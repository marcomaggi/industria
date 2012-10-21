;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2010, 2012 Göran Weinholt <goran@weinholt.se>

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

;; Cryptographical algorithms for the TLS library

(library (weinholt net tls algorithms (1 0 20121021))
  (export
    tls-prf-md5-sha1
    tls-prf-sha256

    null-cipher-suite supported-cipher-suites

    make-cs
    cs-name cs-id cs-kex cs-cipher cs-mac
    cs-key-length cs-iv-size cs-block-size
    cs-expand-ekey cs-expand-dkey
    cs-hash-size #;cs-mac-key-length

    cs-compute-mac cs-encrypt! cs-decrypt!
    )
  (import
    (except (rnrs) bytevector=?)
    ;;(srfi :39 parameters)
    (rename (weinholt bytevectors)
            (bytevector=?/constant-time bytevector=?))
    (weinholt crypto aes)
    (weinholt crypto arcfour)
    (weinholt crypto des)
    ;;(weinholt crypto dh)
    (weinholt crypto dsa)
    ;;(weinholt crypto entropy)
    (weinholt crypto md5)
    (weinholt crypto rsa)
    (weinholt crypto sha-1))

;;; Pseudo random functions

  (define (tls-prf-sha256 bytes secret label seeds)
    ;; TODO: TLS 1.2
    (error 'tls-prf-sha256 "not implemented"))

  (define (p-hash hash->bytevector hmac hash-length length secret seeds)
    ;; Expands the secret and seeds to at least `length' bytes, which
    ;; will be rounded to a multiple of the hash-length.
    (define (gen-A i max prev)
      (if (fx=? i max)
          '()
          (let ((a (hash->bytevector (hmac secret prev))))
            (cons prev (gen-A (fx+ i 1) max a)))))
    (bytevector-concatenate
     (map (lambda (a) (hash->bytevector (apply hmac secret a seeds)))
          (gen-A 0
                 (div (+ length (- hash-length 1)) hash-length)
                 (hash->bytevector (apply hmac secret seeds))))))

  (define (tls-prf-md5-sha1 bytes secret label seeds)
    ;; Performs this operation from RFC4346:
    ;; PRF(secret, label, seed) = P_MD5(S1, label + seed) XOR
    ;;                          P_SHA-1(S2, label + seed)
    (let* ((half-length (div (+ (bytevector-length secret) 1) 2))
           (s1 (subbytevector secret 0 half-length))
           (s2 (subbytevector secret half-length))
           (seeds (cons label seeds)))
      (do ((p1 (p-hash md5->bytevector hmac-md5 16 bytes s1 seeds))
           (p2 (p-hash sha-1->bytevector hmac-sha-1 20 bytes s2 seeds))
           (i 0 (fx+ i 1))
           (ret (make-bytevector bytes)))
          ((= i bytes) ret)
        (bytevector-u8-set! ret i (fxxor (bytevector-u8-ref p1 i)
                                         (bytevector-u8-ref p2 i))))))


;;; Cipher suites

  (define-record-type cs
    (fields name                        ;algorithm name
            id                          ;algorithm identifier (u16)
            kex                         ;key exchange algorithm
            cipher                      ;cipher algorithm
            mac                         ;MAC algorithm
            key-length                  ;cipher key length
            iv-size                     ;IV size
            block-size                  ;cipher block size
            expand-ekey                 ;expand encryption key schedule
            expand-dkey                 ;expand decryption key schedule
            hash-size                   ;MAC hash size
            mac-key-length)             ;MAC key length
    (protocol
     (lambda (p)
       (lambda (name id kex cipher mac)
         (let-values (((key-length iv-size block-size
                                   expand-ekey expand-dkey)
                       ;; Cipher parameters
                       (case cipher
                         ((#f)
                          (values 0 0 #f
                                  'no-keys
                                  'no-keys))
                         ((rc4-128)
                          (values 16 0 #f
                                  expand-arcfour-key
                                  expand-arcfour-key))
                         ((tdea)
                          (values 24 8 8
                                  tdea-permute-key
                                  tdea-permute-key))
                         ((aes-128-cbc)
                          (values 16 16 16
                                  expand-aes-key
                                  (lambda (k)
                                    (reverse-aes-schedule
                                     (expand-aes-key k)))))
                         ((aes-256-cbc)
                          (values 32 16 16
                                  expand-aes-key
                                  (lambda (k)
                                    (reverse-aes-schedule
                                     (expand-aes-key k)))))
                         (else
                          (error 'make-cs "Unknown cipher algorithm" cipher))))
                      ((hash-size mac-key-length)
                       ;; MAC parameters
                       (case mac
                         ((#f) (values 0 0))
                         ((md5) (values 16 16))
                         ((sha-1) (values 20 20))
                         ((sha-256) (values 32 32))
                         (else
                          (error 'make-cs "Unknown MAC algorithm" mac)))))
           (p name id kex cipher mac
              key-length iv-size block-size expand-ekey expand-dkey
              hash-size mac-key-length))))))

  (define null-cipher-suite (make-cs 'NULL-WITH-NULL-NULL #x0000 #f #f #f))

  ;; All the supported cipher suites in order of priority. The NULL
  ;; cipher must be first.
  ;; http://www.iana.org/assignments/tls-parameters/tls-parameters.txt
  (define %supported-cipher-suites
    (list
     ;; Ephemeral Diffie-Hellman gives Perfect Forward Secrecy.
     ;; (make-cs 'DH-DSS-WITH-AES-128-CBC-SHA #x0030 'dh-dss 'aes-128-cbc 'sha-1)
     ;; (make-cs 'DH-RSA-WITH-AES-128-CBC-SHA #x0031 'dh-rsa 'aes-128-cbc 'sha-1)
     (make-cs 'DHE-DSS-WITH-AES-128-CBC-SHA #x0032 'dhe-dss 'aes-128-cbc 'sha-1)
     (make-cs 'DHE-RSA-WITH-AES-128-CBC-SHA #x0033 'dhe-rsa 'aes-128-cbc 'sha-1)
     ;; (make-cs 'DH-DSS-WITH-AES-256-CBC-SHA #x0036 'dh-dss 'aes-256-cbc 'sha-1)
     ;; (make-cs 'DH-RSA-WITH-AES-256-CBC-SHA #x0037 'dh-rsa 'aes-256-cbc 'sha-1)
     (make-cs 'DHE-DSS-WITH-AES-256-CBC-SHA #x0038 'dhe-dss 'aes-256-cbc 'sha-1)
     (make-cs 'DHE-RSA-WITH-AES-256-CBC-SHA #x0039 'dhe-rsa 'aes-256-cbc 'sha-1)

     ;; TODO: version 1.2:
     ;; (make-cs 'DH-DSS-WITH-AES-128-CBC-SHA256 #x003E 'dh-dss 'aes-128-cbc 'sha-256)
     ;; (make-cs 'DH-RSA-WITH-AES-128-CBC-SHA256 #x003F 'dh-rsa 'aes-128-cbc 'sha-256)
     ;; (make-cs 'DHE-DSS-WITH-AES-128-CBC-SHA256 #x0040 'dhe-dss 'aes-128-cbc 'sha-256)
     ;; (make-cs 'DHE-RSA-WITH-AES-128-CBC-SHA256 #x0067 'dhe-rsa 'aes-128-cbc 'sha-256)
     ;; (make-cs 'DH-DSS-WITH-AES-256-CBC-SHA256 #x0068 'dh-dss 'aes-256-cbc 'sha-256)
     ;; (make-cs 'DH-RSA-WITH-AES-256-CBC-SHA256 #x0069 'dh-rsa 'aes-256-cbc 'sha-256)
     ;; (make-cs 'DHE-DSS-WITH-AES-256-CBC-SHA256 #x006A 'dhe-dss 'aes-256-cbc 'sha-256)
     ;; (make-cs 'DHE-RSA-WITH-AES-256-CBC-SHA256 #x006B 'dhe-rsa 'aes-256-cbc 'sha-256)

     (make-cs 'RSA-WITH-AES-128-CBC-SHA #x002F 'rsa 'aes-128-cbc 'sha-1)
     (make-cs 'RSA-WITH-AES-256-CBC-SHA #x0035 'rsa 'aes-256-cbc 'sha-1)
     (make-cs 'RSA-WITH-RC4-128-SHA #x0005 'rsa 'rc4-128 'sha-1)
     (make-cs 'RSA-WITH-RC4-128-MD5 #x0004 'rsa 'rc4-128 'md5)
     ;; TODO: version 1.2:
     ;; (make-cs 'RSA-WITH-AES-128-CBC-SHA256 #x003C 'rsa 'aes-128-cbc 'sha-256)
     ;; (make-cs 'RSA-WITH-AES-256-CBC-SHA256 #x003D 'rsa 'aes-256-cbc 'sha-256)
     ;; 3DES is last because it's so very slow
     ;; (make-cs 'DH-DSS-WITH-3DES-EDE-CBC-SHA #x000D 'dh-dss 'tdea 'sha-1)
     ;; (make-cs 'DH-RSA-WITH-3DES-EDE-CBC-SHA #x0010 'dh-rsa 'tdea 'sha-1)
     (make-cs 'DHE-DSS-WITH-3DES-EDE-CBC-SHA #x0013 'dhe-dss 'tdea 'sha-1)
     (make-cs 'DHE-RSA-WITH-3DES-EDE-CBC-SHA #x0016 'dhe-rsa 'tdea 'sha-1)
     (make-cs 'RSA-WITH-3DES-EDE-CBC-SHA #x000A 'rsa 'tdea 'sha-1))
    )

  (define (supported-cipher-suites)
    ;; TODO: should maybe be a parameter, or saved in the tls conn, or something
    %supported-cipher-suites)

  (define (cs-compute-mac cs secret header data)
    (case (cs-mac cs)
      ((sha-1)
       (sha-1->bytevector (hmac-sha-1 secret header data)))
      ((md5)
       (md5->bytevector (hmac-md5 secret header data)))
      ((#f) #vu8())
      (else
       (error 'cs-compute-mac
              "You forgot to put in the new MAC algorithm!" cs))))

  (define (cs-encrypt! cs source source-index target target-index len key IV)
    (case (cs-cipher cs)
      ((aes-128-cbc aes-256-cbc)
       (aes-cbc-encrypt! source source-index
                         target target-index
                         len key IV))
      ((rc4-128)
       (arcfour! source source-index
                 target target-index
                 len key))
      ((tdea)
       ;; Stupid API
       (assert (and (eqv? source target)
                    (eqv? source-index target-index)))
       (tdea-cbc-encipher! source key IV source-index len))
      ((#f) #f)              ;null algorithm
      (else
       (error 'cs-encrypt!
              "You forgot to put in the new cipher algorithm!"
              (cs-cipher cs)))))

  (define (cs-decrypt! cs source source-index target target-index len key IV)
    (case (cs-cipher cs)
      ((aes-128-cbc aes-256-cbc)
       (aes-cbc-decrypt! source source-index
                         target target-index
                         len key IV))
      ((rc4-128)
       (arcfour! source source-index
                 target target-index
                 len key))
      ((tdea)
       ;; Stupid API
       (assert (and (eqv? source target)
                    (eqv? source-index target-index)))
       (tdea-cbc-decipher! source key IV source-index len))
      (else
       (error 'cs-decrypt!
              "You forgot to put in the new cipher algorithm!"
              (cs-cipher cs))))))
