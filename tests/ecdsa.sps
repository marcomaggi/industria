#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2011 Göran Weinholt <goran@weinholt.se>

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
        (srfi :78 lightweight-testing)
        (weinholt crypto ec)
        (weinholt crypto ec dsa)
        (weinholt crypto sha-1))

;; Test from SECG's GEC 2

(define secp160r1
  (make-elliptic-prime-curve
   #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFFFFFF
   -3
   #x1C97BEFC54BD7A8B65ACF89F81D4D4ADC565FA45
   #x024A96B5688EF573284664698968C38BB913CBFC82
   #x0100000000000000000001F4C8F927AED3CA752257
   #x01))

(check (ecdsa-verify-signature (sha-1->bytevector (sha-1 (string->utf8 "abc")))
                               (ecdsa-private->public
                                (make-ecdsa-private-key
                                 secp160r1
                                 971761939728640320549601132085879836204587084162))
                               1176954224688105769566774212902092897866168635793
                               299742580584132926933316745664091704165278518100)
       => #t)

;; Test all of ecdsa-sha2: make a new key, make a signature and verify it.
(check (let*-values (((data) (sha-1->bytevector (sha-1 #vu8(1 2 3))))
                     ((key) (make-ecdsa-private-key secp256r1))
                     ((r s) (ecdsa-create-signature data key)))
         (ecdsa-verify-signature data (ecdsa-private->public key) r s))
       => #t)

(check-report)
