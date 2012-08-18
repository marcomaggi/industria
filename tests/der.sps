#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2010 Göran Weinholt <goran@weinholt.se>

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

(import (srfi :78 lightweight-testing)
        (rnrs)
        (prefix (weinholt struct der) der:))

(define (SubjectAltName)
  `(sequence-of 1 +inf.0 ,(GeneralName)))

(define (GeneralName)
  `(choice #;(otherName (implicit context 0 ,(OtherName)))
           (rfc822Name (implicit context 1 ia5-string))
           (dNSName (implicit context 2 ia5-string))
           #;etc...))

(check
 (der:translate (der:decode #vu8(48 30 130 15 119 119 119 46 119 101 105 110 104 111 108 116
                                    46 115 101 130 11 119 101 105 110 104 111 108 116 46 115 101))
                (SubjectAltName))
 => '("www.weinholt.se" "weinholt.se"))

;; TODO: needs more tests, to say the least.

(check-report)
