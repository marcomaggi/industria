;; -*- mode: scheme; coding: utf-8 -*-
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

;; Procedures that read and write SSH 2 public keys

;; 4253 The Secure Shell (SSH) Transport Layer Protocol. T. Ylonen, C.
;;      Lonvick, Ed.. January 2006. (Format: TXT=68263 bytes) (Status:
;;      PROPOSED STANDARD)

;; 4716 The Secure Shell (SSH) Public Key File Format. J. Galbraith, R.
;;      Thayer. November 2006. (Format: TXT=18395 bytes) (Status:
;;      INFORMATIONAL)

;; TODO: http://permalink.gmane.org/gmane.ietf.secsh/6520 ?

(library (weinholt crypto ssh-public-key (1 1 20110201))
  (export get-ssh-public-key
          ssh-public-key->bytevector
          ssh-public-key-fingerprint
          ssh-public-key-random-art
          ssh-public-key-algorithm)
  (import (only (srfi :13 strings) string-pad
                string-join string-prefix?)
          (except (rnrs) put-string)
          (weinholt bytevectors)
          (weinholt crypto dsa)
          (weinholt crypto ec)
          (weinholt crypto ec dsa)
          (weinholt crypto md5)
          (weinholt crypto rsa)
          (weinholt struct pack)
          (weinholt text base64)
          (weinholt text random-art))

  (define (mpnegative? bv)
    (and (> (bytevector-length bv) 1)
         (fxbit-set? (bytevector-u8-ref bv 0) 7)))

  (define (get-mpint p)
    (let ((bv (get-bytevector-n p (get-unpack p "!L"))))
      (when (mpnegative? bv)
        (error 'get-ssh-public-key "Refusing to read a negative mpint"))
      (bytevector->uint bv)))

  (define (put-mpint p i)
    (let ((bv (uint->bytevector i)))
      (cond ((mpnegative? bv)
             ;; Prevent this from being considered a negative number
             (put-bytevector p (pack "!L" (+ 1 (bytevector-length bv))))
             (put-u8 p 0)
             (put-bytevector p bv))
            (else
             (put-bytevector p (pack "!L" (bytevector-length bv)))
             (put-bytevector p bv)))))

  (define (get-string p)
    (utf8->string (get-bytevector-n p (get-unpack p "!L"))))

  (define (put-string p s)
    (let ((bv (string->utf8 s)))
      (put-bytevector p (pack "!L" (bytevector-length bv)))
      (put-bytevector p bv)))

  ;; ssh-dss           REQUIRED     sign   Raw DSS Key
  ;; ssh-rsa           RECOMMENDED  sign   Raw RSA Key
  ;; pgp-sign-rsa      OPTIONAL     sign   OpenPGP certificates (RSA key)
  ;; pgp-sign-dss      OPTIONAL     sign   OpenPGP certificates (DSS key)

  ;; Reads a binary SSH public key. They are normally Base64 encoded
  ;; when stored in files.
  (define (get-ssh-public-key p)
    (define who 'get-ssh-public-key)
    (let ((type (get-string p)))
      (cond ((string=? type "ssh-rsa")
             (let* ((e (get-mpint p))
                    (n (get-mpint p)))
               (make-rsa-public-key n e)))
            ((string=? type "ssh-dss")
             (let* ((p* (get-mpint p))
                    (q (get-mpint p))
                    (g (get-mpint p))
                    (y (get-mpint p)))
               (make-dsa-public-key p* q g y)))
            ((string-prefix? "ecdsa-sha2-" type)
             (let* ((id (get-string p)) ;curve ID
                    (Q (get-mpint p)))  ;public point
               (make-ecdsa-sha-2-public-key (id->curve id who) Q)))
            (else
             (error 'get-ssh-public-key
                    "Unknown public key algorithm"
                    type p)))))

  (define (id->curve x who)
    (cond ((string=? x "nistp256") nistp256)
          ((string=? x "nistp384") nistp384)
          ((string=? x "nistp521") nistp521)
          (else
           (error who "Unknown elliptic curve" x))))

  (define (curve->id x who)
    (cond ((elliptic-curve=? x nistp256) "nistp256")
          ((elliptic-curve=? x nistp384) "nistp384")
          ((elliptic-curve=? x nistp521) "nistp521")
          ;; For every other curve, its ASN.1 OID in ASCII is used
          (else
           (error who "Unknown elliptic curve" x))))

  (define (ssh-public-key-algorithm key)
    (define who 'ssh-public-key-algorithm )
    (cond ((rsa-public-key? key) "ssh-rsa")
          ((dsa-public-key? key) "ssh-dss")
          ((ecdsa-sha-2-public-key? key)
           (string-append "ecdsa-sha2-"
                          (curve->id (ecdsa-public-key-curve key) who)))
          (else
           (error 'ssh-public-key-algorithm
                  "Unknown public key algorithm"
                  key))))

  (define (ssh-public-key->bytevector key)
    (define who 'ssh-public-key->bytevector)
    (call-with-bytevector-output-port
      (lambda (p)
        (cond ((rsa-public-key? key)
               (put-string p "ssh-rsa")
               (put-mpint p (rsa-public-key-e key))
               (put-mpint p (rsa-public-key-n key)))
              ((dsa-public-key? key)
               (put-string p "ssh-dss")
               (put-mpint p (dsa-public-key-p key))
               (put-mpint p (dsa-public-key-q key))
               (put-mpint p (dsa-public-key-g key))
               (put-mpint p (dsa-public-key-y key)))
              ((ecdsa-sha-2-public-key? key)
               ;; This does not use point compression. If point
               ;; compression could be used, then each ECDSA key would
               ;; have two different fingerprints.
               (let ((id (curve->id (ecdsa-public-key-curve key) who))
                     (Q (elliptic-point->bytevector (ecdsa-public-key-Q key)
                                                    (ecdsa-public-key-curve key))))
                 (put-string p (string-append "ecdsa-sha2-" id))
                 (put-string p id)
                 (put-bytevector p (pack "!L" (bytevector-length Q)))
                 (put-bytevector p Q)))
              (else
               (error who "Unknown public key algorithm" key))))))

  (define (ssh-public-key-fingerprint key)
    (string-join
     (map (lambda (b)
            (string-pad (string-downcase (number->string b 16)) 2 #\0))
          (bytevector->u8-list
           (md5->bytevector (md5 (ssh-public-key->bytevector key)))))
     ":" 'infix))

  ;; TODO: bubblebabble

  (define (ssh-public-key-random-art key)
    (let-values (((prefix length)
                  (cond ((rsa-public-key? key)
                         (values "RSA" rsa-public-key-length))
                        ((dsa-public-key? key)
                         (values "DSA" dsa-public-key-length))
                        ((ecdsa-public-key? key)
                         (values "ECDSA" ecdsa-public-key-length))
                        (else
                         (values "UNKNOWN" (lambda (x) +nan.0))))))
      (random-art (md5->bytevector (md5 (ssh-public-key->bytevector key)))
                  (string-append prefix " " (number->string (length key)))))))
