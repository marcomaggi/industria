#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2009, 2010 Göran Weinholt <goran@weinholt.se>

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

(import (weinholt crypto crc)
        (weinholt compression adler-32)
        (srfi :78 lightweight-testing)
        (rnrs))

;; Simple tests on the pre-defined CRCs:

(define-crc crc-32)
(check (crc-32-self-test) => 'success)

(define-crc crc-16)
(check (crc-16-self-test) => 'success)

(define-crc crc-16/ccitt)
(check (crc-16/ccitt-self-test) => 'success)

(define-crc crc-32c)
(check (crc-32c-self-test) => 'success)

(define-crc crc-24)
(check (crc-24-self-test) => 'success)

(define-crc crc-64)
(check (crc-64-self-test) => 'success)

;; Tests the other procedures

(check (crc-32c-finish
        (crc-32c-update (crc-32c-update (crc-32c-init)
                                        (string->utf8 "12345"))
                        (string->utf8 "6789")))
       => #xE3069283)

(check (crc-32c-finish
        (crc-32c-update (crc-32c-update (crc-32c-init)
                                        (string->utf8 "XX12345") 2)
                        (string->utf8 "XX6789XX")  2 6))
       => #xE3069283)

(check (crc-32c (string->utf8 "123456789"))
       => #xE3069283)

;; Test the syntax for defining new CRCs

(define-crc crc-test (24 23 18 17 14 11 10 7 6 5 4 3 1 0)
            #xB704CE #f #f 0 #x21CF02)  ;CRC-24

(check (crc-test-self-test) => 'success)

;; And last a test for Adler-32
(check (adler-32-self-test) => 'success)

(check-report)
