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

(import (weinholt crypto password)
        (srfi :78 lightweight-testing)
        (rnrs))

;;; DES

(check (crypt "foodbard" "..") => "..o6avrdNBOA6")

(check (crypt "test" "..") => "..9sjyf8zL76k")

(check (crypt "X" "..") => "..XhpOnw6KMZg")

(check (crypt "foobar" "Ax") => "AxTdjVtckZ0Rs")

(check (crypt "ZZZZ" "zz") => "zz/CBDeUpwD26")

(check (crypt "" "..") => "..X8NBuQ4l6uQ")

(check (crypt "" "ZZ") => "ZZvIHp4MBMwSE")

;;; MD5

(check (crypt "hello" "$1$oKnN0HHt$") => "$1$oKnN0HHt$Aul2g/J4edgga3WE/03cN/")

(check (crypt "this is a password longer than 16 characters" "$1$oKnN0HHt$")
       => "$1$oKnN0HHt$KtM1JhHfFNyQOq5OgbGo.1")

;;; SHA from http://people.redhat.com/drepper/SHA-crypt.txt

;; SHA-256

;; (check (crypt "Hello world!" "$5$saltstring") =>
;;        "$5$saltstring$5B8vYYiY.CVt1RlTTf8KbXBH3hsxY/GNooZaBBGWEc5")

;; (check (crypt "Hello world!"
;;               "$5$rounds=10000$saltstringsaltstring")
;;        =>
;;        "$5$rounds=10000$saltstringsaltst$3xv.VbSHBb41AL9AvLeujZkZRBAwqFMz2.opqey6IcA")

;; (check (crypt "This is just a test"
;;               "$5$rounds=5000$toolongsaltstring")
;;        =>
;;        "$5$rounds=5000$toolongsaltstrin$Un/5jzAHMgOGZ5.mWJpuVolil07guHPvOW8mGRcvxa5")

;; (check (crypt "a very much longer text to encrypt.  This one even stretches over morethan one line."
;;               "$5$rounds=1400$anotherlongsaltstring")
;;        =>
;;        "$5$rounds=1400$anotherlongsalts$Rx.j8H.h8HjEDGomFU8bDkXm3XIUnzyxf12oP84Bnq1")

;; (check (crypt "we have a short salt string but not a short password"
;;               "$5$rounds=77777$short")
;;        =>
;;        "$5$rounds=77777$short$JiO1O3ZpDAxGJeaDIuqCoEFysAe1mZNJRs3pw0KQRd/")

;; (check (crypt "a short string"
;;               "$5$rounds=123456$asaltof16chars..")
;;        =>
;;        "$5$rounds=123456$asaltof16chars..$gP3VQ/6X7UUEW3HkBn2w1/Ptq2jxPyzV/cZKmF/wJvD")

;; (check (crypt "the minimum number is still observed"
;;               "$5$rounds=10$roundstoolow")
;;        =>
;;        "$5$rounds=1000$roundstoolow$yfvwcWrQ8l/K0DAWyuPMDNHpIVlTQebY9l/gL972bIC")

;; ;; SHA-512

;; (check (crypt "Hello world!" "$6$saltstring")
;;        =>
;;        "$6$saltstring$svn8UoSVapNtMuq1ukKS4tPQd8iKwSMHWjl/O817G3uBnIFNjnQJuesI68u4OTLiBFdcbYEdFCoEOfaS35inz1")

;; (check (crypt "Hello world!"
;;               "$6$rounds=10000$saltstringsaltstring")
;;        =>
;;        "$6$rounds=10000$saltstringsaltst$OW1/O6BYHV6BcXZu8QVeXbDWra3Oeqh0sbHbbMCVNSnCM/UrjmM0Dp8vOuZeHBy/YTBmSK6H9qs/y3RnOaw5v.")

;; (check (crypt "This is just a test"
;;               "$6$rounds=5000$toolongsaltstring")
;;        =>
;;        "$6$rounds=5000$toolongsaltstrin$lQ8jolhgVRVhY4b5pZKaysCLi0QBxGoNeKQzQ3glMhwllF7oGDZxUhx1yxdYcz/e1JSbq3y6JMxxl8audkUEm0")

;; (check (crypt "a very much longer text to encrypt.  This one even stretches over morethan one line."
;;               "$6$rounds=1400$anotherlongsaltstring")
;;        =>
;;        "$6$rounds=1400$anotherlongsalts$POfYwTEok97VWcjxIiSOjiykti.o/pQs.wPvMxQ6Fm7I6IoYN3CmLs66x9t0oSwbtEW7o7UmJEiDwGqd8p4ur1")

;; (check (crypt "we have a short salt string but not a short password"
;;               "$6$rounds=77777$short")
;;        =>
;;        "$6$rounds=77777$short$WuQyW2YR.hBNpjjRhpYD/ifIw05xdfeEyQoMxIXbkvr0gge1a1x3yRULJ5CCaUeOxFmtlcGZelFl5CxtgfiAc0")

;; (check (crypt "a short string"
;;               "$6$rounds=123456$asaltof16chars..")
;;        =>
;;        "$6$rounds=123456$asaltof16chars..$BtCwjqMJGx5hrJhZywWvt0RLE8uZ4oPwcelCjmw2kSYu.Ec6ycULevoBK25fs2xXgMNrCzIMVcgEJAstJeonj1")

;; (check (crypt "the minimum number is still observed"
;;               "$6$rounds=10$roundstoolow")
;;        =>
;;        "$6$rounds=1000$roundstoolow$kUMsbe306n21p9R.FRkW3IGn.S9NPN0x50YhH1xhLsPuWGsUSklZt58jaTfF4ZEQpyUNGc0dqbpBYYBaHHrsX.")


(check-report)
