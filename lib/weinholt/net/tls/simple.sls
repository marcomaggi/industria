;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2012 Göran Weinholt <goran@weinholt.se>

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

;; Custom input and output ports that start a TLS connection

;; TODO: avoid all this extra copying

(library (weinholt net tls simple)
  (export tls-connect start-tls)
  (import (rnrs)
          (weinholt bytevectors)
          (weinholt net tcp)
          (weinholt net tls))

  (define-syntax print
    (syntax-rules ()
      #;
      ((_ . args)
       (begin
         (for-each display (list . args))
         (newline)))
      ((_ . args) (values))))

  (define-syntax trace
    (syntax-rules ()
      #;
      ((_ x)
       (begin
         (let ((v x))
           (display "TLS: ")
           (display v)
           (newline)
           v)))
      ((_ x) x)))

  ;; TODO: let tls.sls handle fragmentation
  (define maxlen (expt 2 14))



  (define tls-connect
    (case-lambda
      ((host service)
       (tls-connect host service #f))
      ((host service client-certificates)
       (let-values (((in out) (tcp-connect host service)))
         (start-tls host service in out client-certificates)))))

  (define start-tls
    (case-lambda
      ((host service in out)
       (start-tls host service in out #f))
      ((host service in out client-certificates)
       (let ((s (make-tls-wrapper in out host))
             (send-cert? #f)
             (other-closed? #f)
             (unread #f)
             (offset 0))
         (define (fail msg . irritants)
           (close-output-port out)
           (close-input-port in)
           (raise
             (condition
              (make-error)
              (make-i/o-error)
              (make-i/o-port-error in)
              (make-message-condition msg)
              (make-irritants-condition irritants))))

         (define (read! bytevector start count)
           ;; Read up to `count' bytes from the source, write them to
           ;; `bytevector' at index `start'. Return the number of bytes
           ;; read (zero means end of file).
           (define (return data offset*)
             (let* ((valid (- (bytevector-length data) offset*))
                    (returned (min count valid)))
               (cond ((= returned valid)
                      (set! unread #f)
                      (set! offset 0))
                     (else
                      (set! unread data)
                      (set! offset (+ offset returned))))
               (bytevector-copy! data offset* bytevector start returned)
               returned))
           (if unread
               (return unread offset)
               (let lp ()
                 (let ((r (get-tls-record s)))
                   (cond ((and (pair? r) (eq? (car r) 'application-data))
                          (if (zero? (bytevector-length (cadr r)))
                              (lp)
                              (return (cadr r) 0)))
                         ((condition? r)
                          ;; XXX: other alerts can also cause it to
                          ;; close... but in error.
                          (if (equal? (condition-irritants r) '((0 . close-notify)))
                              0
                              (lp)))
                         ((eof-object? r)
                          0)
                         ;; FIXME: do something about this record...
                         (else (lp)))))))

         (define (write! bytevector start count)
           ;; Send up to `count' bytes from `bytevector' at index
           ;; `start'. Returns the number of bytes written. A zero count
           ;; should send a close-notify.
           (cond ((zero? count)
                  (put-tls-alert-record s 1 0)
                  (flush-tls-output s)
                  0)
                 (else
                  (do ((rem count (- rem maxlen))
                       (idx start (+ idx maxlen)))
                      ((<= rem 0)
                       (flush-tls-output s)
                       count)
                    (put-tls-application-data s (subbytevector bytevector idx
                                                               (+ idx (min maxlen rem))))))))

         (define (close)
           (cond (other-closed?
                  (put-tls-alert-record s 1 0)
                  (guard (con
                          ((i/o-error? con) #f))
                    (flush-tls-output s)
                    (close-output-port out))
                  (close-input-port in))
                 (else
                  ;; This is so that each port can be closed independently.
                  (set! other-closed? #t))))
         (define (expect want)
           (let ((got (trace (get-tls-record s))))
             (unless (eq? got want)
               (fail (string-append "Expected " (symbol->string want))
                     got))))

         ;; Start negotiation
         (put-tls-handshake-client-hello s)
         (flush-tls-output s)

         (expect 'handshake-server-hello)

         (let lp ((allowed '(handshake-certificate
                             handshake-server-key-exchange
                             handshake-certificate-request
                             handshake-server-hello-done)))
           (let ((data (trace (get-tls-record s))))
             (when (eof-object? data)
               (fail "The server disconnected during the handshake"))
             (unless (memq data allowed)
               (fail "The server did the handshake in the wrong order"
                     data))
             (case data
               ((handshake-certificate-request)
                (set! send-cert? #t)
                (lp '(handshake-server-hello-done)))
               ((handshake-server-hello-done) #t)
               (else
                (lp (cdr (memq data allowed)))))))

         (print "server handshake done. client sends its own handshake now")
         (when send-cert?
           (print "sending client certificate")
           (put-tls-handshake-certificate s client-certificates))
         (put-tls-handshake-client-key-exchange s)
         (when (and client-certificates (not (null? client-certificates)))
           (put-tls-handshake-certificate-verify s))
         (put-tls-change-cipher-spec s)
         (put-tls-handshake-finished s)
         (flush-tls-output s)

         (expect 'change-cipher-spec)
         (expect 'handshake-finished)

         (let ((id (string-append "tls " host ":" service)))
           (values (make-custom-binary-input-port id read! #f #f close)
                   (make-custom-binary-output-port id write! #f #f close)
                   s)))))))
