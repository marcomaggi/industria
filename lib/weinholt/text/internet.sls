;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2011, 2012 Göran Weinholt <goran@weinholt.se>

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

;; Internet address parsing and formatting

;; RFC 4291 IP Version 6 Addressing Architecture.
;; RFC 5952 A Recommendation for IPv6 Address Text Representation.

(library (weinholt text internet)
  (export ipv4->string string->ipv4
          ipv6->string string->ipv6)
  (import (rnrs)
          (only (srfi :13 strings) string-join string-prefix?)
          (weinholt struct pack)
          (weinholt text strings))

  (define (ipv4->string addr)
    (call-with-string-output-port
      (lambda (o)
        (define (write-octet i)
          (put-datum o (bytevector-u8-ref addr i)))
        (define (write-dot)
          (put-char o #\.))
        (write-octet 0) (write-dot)
        (write-octet 1) (write-dot)
        (write-octet 2) (write-dot)
        (write-octet 3))))

  ;; Accepts leading zeros, like in: 192.000.002.000
  (define (string->ipv4 str)
    (let-values (((o extract) (open-bytevector-output-port)))
      (let ((i (open-string-input-port str)))
        (define (parse-octet)
          (let lp ((octet 0) (n 3))
            (let ((c (lookahead-char i)))
              (if (and (positive? n) (char? c) (char<=? #\0 c #\9))
                  (lp (+ (* octet 10)
                         (- (char->integer (get-char i))
                            (char->integer #\0)))
                      (- n 1))
                  (cond ((and (<= 0 octet 255) (< n 3))
                         (put-u8 o octet)
                         #t)
                        (else #f))))))
        (define (parse-dot)
          (eqv? (get-char i) #\.))
        (and (parse-octet) (parse-dot)
             (parse-octet) (parse-dot)
             (parse-octet) (parse-dot)
             (parse-octet) (eof-object? (get-char i))
             (extract)))))

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

