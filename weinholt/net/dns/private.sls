;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2011 Göran Weinholt <goran@weinholt.se>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
#!r6rs

;; Stuff that is needed by more than one of the DNS libraries

(library (weinholt net dns private (1 0 20110123))
  (export put-labels parse-labels       ;wire format
          display-dns-label display-dns-string) ;master file format
  (import (rnrs)
          (srfi :26 cut)
          (srfi :39 parameters)
          (weinholt bytevectors)
          (weinholt struct pack))

  (define (displayer lower permitted prefix suffix)
    (lambda (bv p)
      (display prefix p)
      (do ((i 0 (+ i 1)))
          ((= i (bytevector-length bv)))
        (let* ((b (bytevector-u8-ref bv i))
               (c (integer->char b)))
          (cond ((memv c permitted)
                 (display #\\ p)
                 (display c p))
                ((< lower b #x7f)
                 (display c p))
                (else
                 (display #\\ p)
                 (if (< b 100) (display #\0 p))
                 (if (< b 10) (display #\0 p))
                 (display b p)))))
      (display suffix p)))

  (define display-dns-label
    (displayer #x20 '(#\@ #\. #\\ #\( #\) #\; #\$ #\") "" "."))

  (define display-dns-string
    (displayer #x1f '(#\\ #\") "\"" "\""))

  (define put-labels
    (case-lambda
      ((port labels table offset)
       (let lp ((labels labels))
         (cond ((null? labels)
                (put-u8 port 0))
               ((and table (hashtable-ref table (car labels) #f))
                => (lambda (position)
                     (put-bytevector port
                                     (pack "!S"
                                           (fxior (fxarithmetic-shift-left #b11 (+ 8 6))
                                                  position)))))
               (else
                (let ((label (car labels)))
                  (unless (< 0 (bytevector-length label) 64)
                    (error 'format-labels "Invalid DNS label" labels label))
                  (when table           ;compression table
                    (let ((pos (+ (port-position port) offset)))
                      (when (< pos #b0011111111111111)
                        (hashtable-set! table label pos))))
                  (put-u8 port (bytevector-length label))
                  (put-bytevector port label)
                  (lp (cdr labels)))))))
      ((port labels)
       (put-labels port labels #f 0))))

  ;; Parses labels in the wire format. This needs access to the whole
  ;; query if there are any compressed labels.
  (define (parse-labels bv start)
    (define who 'parse-labels)
    (let lp ((start start)
             (ret '())
             (acclen 0)
             (used-offsets '())
             (end #f)) ;the end offset of the label (does not follow pointers)
      (when (> start (bytevector-length bv))
        (error who "invalid pointer in a name" start))
      ;; Detect pointer loops. If I was much too clever for anyone's
      ;; good I might have translated these into circular lists
      ;; instead, and even supported them in put-dns-message. Who
      ;; wants an infinite domain name?
      (when (memv start used-offsets)
        (error who "looping name"))
      (let* ((len (bytevector-u8-ref bv start))
             (tag (fxbit-field len 6 8)))
        (cond ((zero? len)
               (values (reverse ret) (or end (+ start 1))))
              ((zero? tag)              ;normal label
               (when (> (+ acclen len 1) 255)
                 (error who "overlong name" acclen))
               (lp (+ start 1 len)
                   (cons (subbytevector bv (+ start 1) (+ start 1 len))
                         ret)
                   (+ acclen len 1) (cons start used-offsets) end))
              ((= #b11 tag)             ;pointer
               (lp (fxior (bitwise-arithmetic-shift (fxand len #b111111) 8)
                          (bytevector-u8-ref bv (+ start 1)))
                   ret acclen (cons start used-offsets)
                   (or end (+ start 2))))
              ;; TODO: #b01 EDNS0 rfc2671
              (else (error who "reserved bits in a length field" len)))))))
