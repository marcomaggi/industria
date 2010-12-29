;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010 Göran Weinholt <goran@weinholt.se>
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

;; Internet address parsing and formatting

;; RFC 4291 IP Version 6 Addressing Architecture.
;; RFC 5952 A Recommendation for IPv6 Address Text Representation.

(library (weinholt text internet (1 0 20101229))
  (export ipv4->string #;string->ipv4
          ipv6->string string->ipv6)
  (import (rnrs)
          (only (srfi :13 strings) string-join string-prefix?)
          (weinholt struct pack)
          (weinholt text strings))

  (define (ipv4->string addr)
    (string-join (map number->string (bytevector->u8-list addr))
                 "."))

  ;; TODO: this is too simple, needs checks
  ;; (define (string->ipv4 str)
  ;;   (u8-list->bytevector (map string->number (string-split str #\. 4))))
  
  (define (word i addr) (unpack "!uS" addr (fx+ i i)))

  (define (compression-index addr)
    ;; Finds the largest span of zero words. Chooses the first span
    ;; if two spans are of equal length.
    (let lp ((i 0) (start -1) (len 0) (start* -1) (len* 0))
      (cond ((fx=? i 8)
             (if (fx>? len len*)
                 (values start len)
                 (values start* len*)))
            ((fxzero? (word i addr))
             (lp (fx+ i 1) (if (fx=? start -1) i start)
                 (fx+ len 1) start* len*))
            ((fx>? len len*)
             (lp (fx+ i 1) -1 0 start len))
            (else
             (lp (fx+ i 1) -1 0 start* len*)))))

  ;; TODO: emit embedded IPv4 addresses
  (define (ipv6->string addr)
    (call-with-string-output-port
      (lambda (p)
        (let-values (((cidx* clen) (compression-index addr)))
          (let ((cidx (if (fx=? clen 1) -1 cidx*)))
            (do ((i 0 (if (fx=? i cidx) (fx+ i clen) (fx+ i 1))))
                ((fx=? i 8)
                 (when (fx=? i (+ cidx clen)) (display #\: p)))
              (cond ((fx=? i cidx)
                     (display #\: p))
                    (else
                     (unless (fxzero? i) (put-char p #\:))
                     (display (string-downcase
                               (number->string (word i addr) 16))
                              p)))))))))

  ;; Returns a bytevector or #f.
  (define (string->ipv6 str)
    (define (parse str start)
      (let ((addr (make-bytevector 16 0))
            (se (string-length str)))
        (let lp ((si start) (ai 0) (nibbles 0) (cidx #f) (word 0))
          (cond ((= si se)
                 (cond ((positive? nibbles)
                        ;; Trailing word
                        (cond ((< ai 16)
                               (pack! "!uS" addr ai word)
                               (lp si (+ ai 2) 0 cidx 0))
                              (else #f)))
                       (cidx
                        ;; The string used compression, move the words
                        ;; to the right.
                        (let ((didx (- 16 (- ai cidx))))
                          (bytevector-copy! addr cidx addr didx (- ai cidx))
                          (do ((i cidx (+ i 2)))
                              ((= i didx) addr)
                            (bytevector-u16-native-set! addr i 0))))
                       ((= ai 16) addr)
                       (else #f)))      ;too many/few words
                ((char=? #\: (string-ref str si))
                 (cond ((zero? nibbles)
                        ;; Compression
                        (and (not cidx)
                             (lp (+ si 1) ai nibbles ai word)))
                       ((< ai 14)
                        (pack! "!uS" addr ai word)
                        (lp (+ si 1) (+ ai 2) 0 cidx 0))
                       (else #f)))      ;bad place for a colon
                ((string->number (string (string-ref str si)) 16)
                 => (lambda (n)
                      (and (< nibbles 4)
                           (lp (+ si 1) ai (+ nibbles 1) cidx
                               (fxior n (fxarithmetic-shift-left word 4))))))
                ;; TODO: handle embedded IPv4 addresses
                (else #f)))))
    (if (string-prefix? ":" str)
        (and (string-prefix? "::" str)
             (parse str 1))
        (parse str 0))))

