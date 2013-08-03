#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2013 Göran Weinholt <goran@weinholt.se>

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
        (weinholt crypto uuid)
        (srfi :78 lightweight-testing))

;;; RFC Errata ID: 3476

(check (uuid->string (md5-uuid uuid-namespace-dns "www.widgets.com"))
       => "3d813cbb-47fb-32ba-91df-831e1593ac29")

(check (uuid->string (md5-uuid uuid-namespace-dns "www.example.com"))
       => "5df41881-3aed-3515-88a7-2f4a814cf09e")

;;; Check that time-based UUIDs are at least somewhat unique

(display (uuid->string (time-uuid))) (newline)
(display "\n; Checking time-based UUIDs... ")

(let ((ht (make-hashtable (lambda (bv)
                            (bytevector-uint-ref bv 0 (endianness big) 16))
                          equal?))
      (n (expt 2 15)))
  (do ((i 0 (+ i 1)))
      ((= i n) (display "100%\n"))
    (when (zero? (mod i (expt 2 12)))
      (display (round (* 100 (/ i n))))
      (display "%... "))
    (hashtable-set! ht (time-uuid) #t))
  (check (hashtable-size ht) => n))
(display (uuid->string (time-uuid))) (newline)

(check-report)
