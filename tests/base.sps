#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2009, 2010, 2011, 2013 Göran Weinholt <goran@weinholt.se>

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
        (weinholt text base64))

(define (string->base64 x)
  (base64-encode (string->utf8 x)))

;; From RFC 4658

(check (string->base64 "") => "")

(check (string->base64 "f") => "Zg==")

(check (string->base64 "fo") => "Zm8=")

(check (string->base64 "foo") => "Zm9v")

(check (string->base64 "foob") => "Zm9vYg==")

(check (string->base64 "fooba") => "Zm9vYmE=")

(check (string->base64 "foobar") => "Zm9vYmFy")

;; Non-strict mode

(check (base64-decode "ABC= " base64-alphabet #f #f) => #vu8(0 16))

(check (base64-decode "ABC =" base64-alphabet #f #f) => #vu8(0 16))

(check (base64-decode "AB==C=" base64-alphabet #f #f) => #vu8(0 16))

(check (base64-decode "AB==C =" base64-alphabet #f #f) => #vu8(0 16))

(check (base64-decode "A B = = C = " base64-alphabet #f #f) => #vu8(0 16))

;; ad-hoc

(define (base64-linewrapped str)
  (let ((bv (string->utf8 str)))
    (base64-encode bv 0 (bytevector-length bv) 76 #f)))

(check (base64-linewrapped
        "My name is Ozymandias, king of kings:\n\
         Look on my works, ye Mighty, and despair!")
       =>
       "TXkgbmFtZSBpcyBPenltYW5kaWFzLCBraW5nIG9mIGtpbmdzOgpMb29rIG9uIG15IHdvcmtzLCB5\n\
        ZSBNaWdodHksIGFuZCBkZXNwYWlyIQ==")

;; ascii armor

(check (call-with-values
         (lambda ()
           (get-delimited-base64
            (open-string-input-port
             "-----BEGIN EXAMPLE-----\n\
AAECAwQFBg==\n\
-----END EXAMPLE-----\n")))
         list)
         => '("EXAMPLE" #vu8(0 1 2 3 4 5 6)))

;; ignoring header and crc-24 checksum
(check (call-with-values
         (lambda ()
           (get-delimited-base64
            (open-string-input-port
             "Example follows\n\
\n\
-----BEGIN EXAMPLE-----\n\
Header: data\n\
Header2: data2\n\
 etc
foo
\n\
AAECAwQFBg==\n\
=2wOb\n\
-----END EXAMPLE-----\n")))
         list)
         => '("EXAMPLE" #vu8(0 1 2 3 4 5 6)))

(let-values (((p extract) (open-string-output-port))
             ((str) "Crusoe's Law: With every new C++ standard, its syntax\n\
                     asymptotically approaches that of a PERL regex."))
  (put-delimited-base64 p "TEST" (string->utf8 str))
  (let-values (((type str*) (get-delimited-base64 (open-string-input-port
                                                   (string-append
                                                    "This is garbage\n"
                                                    (extract))))))
    (check type => "TEST")
    (check (utf8->string str*) => str)
    #f))

(check-report)
