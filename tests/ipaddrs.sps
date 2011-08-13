#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
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

(import (rnrs)
        (only (srfi :1 lists) iota)
        (srfi :78 lightweight-testing)
        (weinholt text internet)
        (weinholt text strings)
        (weinholt struct pack))

(check (ipv6->string (string->ipv6 "::")) => "::")

(check (ipv6->string (string->ipv6 "::1")) => "::1")

(check (ipv6->string (string->ipv6 "2001:db8::")) => "2001:db8::")

;; Test the recommendations from RFC 5952
(check (ipv6->string (string->ipv6 "2001:0db8::0001"))
       => "2001:db8::1")

(check (ipv6->string (string->ipv6 "2001:0db8::1:0000"))
       => "2001:db8::1:0")

(check (ipv6->string (string->ipv6 "2001:db8:0:0:0:0:2:1"))
       => "2001:db8::2:1")

(check (ipv6->string (string->ipv6 "2001:db8::0:1"))
       => "2001:db8::1")

(check (ipv6->string (string->ipv6 "2001:db8::1:1:1:1:1"))
       => "2001:db8:0:1:1:1:1:1")

(check (ipv6->string (string->ipv6 "2001:0:0:1:0:0:0:1"))
       => "2001:0:0:1::1")

(check (ipv6->string (string->ipv6 "2001:db8:0:0:1:0:0:1"))
       => "2001:db8::1:0:0:1")

;; This procedure only works for valid addresses: no error checking.
(define (string->ipv6* str)
  (let ((words (string-split str #\:))
        (addr (make-bytevector 16 0)))
    (let lp ((i 0) (dir 1) (words words))
      (cond ((null? words) addr)
            ((equal? (car words) "")
             (lp 7 -1 (reverse (cdr words))))
            (else
             (pack! "!uS" addr (+ i i) (string->number (car words) 16))
             (lp (fx+ i dir) dir (cdr words)))))))

(do ((i 0 (+ i 1)))
    ((= i (expt 2 8)))
  (let ((addr
         (uint-list->bytevector
          (map (lambda (bit) (if (fxbit-set? i bit) 1 0))
               (iota 8))
          (endianness big) 2)))
    (unless (and (equal? addr
                         (string->ipv6 (ipv6->string addr)))
                 (equal? (string->ipv6 (ipv6->string addr))
                         (string->ipv6* (ipv6->string addr))))
      (error 'blah "blah blah blah!!" addr))))


(check-report)
