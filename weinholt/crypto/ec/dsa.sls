;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2011 Göran Weinholt <goran@weinholt.se>
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

;; Elliptic Curve Digital Signature Algorithm

(library (weinholt crypto ec dsa (1 0 20110203))
  (export make-ecdsa-public-key ecdsa-public-key?
          ecdsa-public-key-curve
          ecdsa-public-key-Q
          ecdsa-public-key-length

          make-ecdsa-private-key
          ecdsa-private-key-d
          ecdsa-private-key-Q

          ecdsa-private->public

          ecdsa-verify-signature
          ecdsa-create-signature
          
          make-ecdsa-sha-2-public-key ecdsa-sha-2-public-key?
          make-ecdsa-sha-2-private-key ecdsa-sha-2-private-key?
          ecdsa-sha-2-verify-signature
          ecdsa-sha-2-create-signature)
  (import (rnrs)
          (weinholt bytevectors)
          (weinholt crypto ec)
          (weinholt crypto entropy)
          (weinholt crypto math)
          (weinholt crypto sha-2))

  (define-record-type ecdsa-public-key
    (fields curve Q)
    (protocol
     (lambda (p)
       (lambda (curve Q)
         (p curve (->elliptic-point Q curve))))))

  (define-record-type ecdsa-private-key
    (opaque #t)
    (nongenerative ecdsa-private-key-2fb1085c-38ad-48ba-98de-463ab54036d3)
    (fields curve
            d                           ;private
            Q)                          ;public
    (protocol
     (lambda (p)
       (case-lambda
         ((curve)
          (let-values (((d Q) (make-random-key curve)))
            (p curve d Q)))
         ((curve d)
          (p curve d (ec* d (elliptic-curve-G curve) curve)))
         ((curve d Q)
          (p curve d Q))))))

  (define (ecdsa-private->public key)
    (let ((curve (ecdsa-private-key-curve key))
          (Q (ecdsa-private-key-Q key)))
      (if (ecdsa-sha-2-private-key? key)
          (make-ecdsa-sha-2-public-key curve Q)
          (make-ecdsa-public-key curve Q))))

  (define (ecdsa-public-key-length key)
    (bitwise-length (elliptic-curve-n (ecdsa-public-key-curve key))))

  (define (make-random n)
    ;; Generate a random number less than q
    (let ((c (bytevector->uint (make-random-bytevector
                                (fxdiv (fx+ (bitwise-length n) 7) 8)))))
      (if (< 0 c n)
          c
          (make-random n))))

  (define (make-random-key curve)
    (let* ((k (make-random (elliptic-curve-n curve)))
           (R (ec* k (elliptic-curve-G curve) curve)))
      (if (pair? R)
          (values k R)
          (make-random-key curve))))

  (define (hash->integer hash n)
    (let* ((H (bytevector->uint hash))
           (hlen (* 8 (bytevector-length hash)))
           (nlen (bitwise-length n)))
      (if (>= nlen hlen)
          H
          ;; This is weird, but the leftmost nlen bits are apparently
          ;; what should be returned in this case, so....
          (bitwise-arithmetic-shift-right H (- hlen nlen)))))
  
  ;; Returns #t if the signature is valid.
  (define (ecdsa-verify-signature hash key r s)
    (let* ((curve (ecdsa-public-key-curve key))
           (n (elliptic-curve-n curve))
           (G (elliptic-curve-G curve))
           (Q (ecdsa-public-key-Q key)))
      (and (< 0 r n)
           (< 0 s n)
           (let* ((w (expt-mod s -1 n))
                  (e (hash->integer hash n))
                  (u1 (mod (* e w) n))
                  (u2 (mod (* r w) n))
                  (X (ec+ (ec* u1 G curve)
                          (ec* u2 Q curve)
                          curve)))
             (and (pair? X)
                  (let ((v (mod (car X) n)))
                    (= v r)))))))

  (define (ecdsa-create-signature hash privkey)
    (let* ((curve (ecdsa-private-key-curve privkey))
           (n (elliptic-curve-n curve)))
      (let lp ()
        (let-values (((k R) (make-random-key curve)))
          (let ((r (mod (car R) n)))
            (if (zero? r)
                (lp)                  ;retry
                (let* ((e (hash->integer hash n))
                       (s (div-mod (+ e (* r (ecdsa-private-key-d privkey))) k n)))
                  (if (zero? s)
                      (lp)            ;retry
                      (values r s)))))))))

  ;; These things are for working with ecdsa-sha2-* from RFC 5656. The
  ;; special record type lets a program indicate that SHA-2 should be
  ;; used, but it's up to the program to check that it is. Presumably
  ;; other hash algorithms could be defined in the future.

  (define-record-type ecdsa-sha-2-public-key
    (parent ecdsa-public-key))

  (define-record-type ecdsa-sha-2-private-key
    (parent ecdsa-private-key))

  (define (sha-2* message key)
    ;; RFC 5656, 6.2.1
    (let ((b (ecdsa-public-key-length key)))
      (cond ((<= b 256) (sha-256->bytevector (sha-256 message)))
            ((and (< 256 b) (<= b 384))
             (sha-384->bytevector (sha-384 message)))
            ((< 384 b)
             (sha-512->bytevector (sha-512 message))))))

  (define (ecdsa-sha-2-verify-signature message key r s)
    (ecdsa-verify-signature (sha-2* message key) key r s))

  (define (ecdsa-sha-2-create-signature message privkey)
    (ecdsa-create-signature (sha-2* message (ecdsa-private->public privkey)) privkey)))
