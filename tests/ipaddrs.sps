#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2010, 2011 Göran Weinholt <goran@weinholt.se>

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
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
