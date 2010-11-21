;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2010 Göran Weinholt <goran@weinholt.se>
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

;; Entropic helpers.

;; Everything that uses entropy in Industria must get it here.

;; TODO: procedures for estimating entropy.

;; TODO: support for EGD?

;; TODO: Should probably not use srfi-27 but something that works the
;; same everywhere.

(library (weinholt crypto entropy (1 0 20101121))
  (export make-random-bytevector
          bytevector-randomize!
          random-positive-byte
          string-random-case
          random-integer)               ;re-exported
  (import (rnrs)
          (only (srfi :13 strings) string-map)
          (srfi :26 cut)
          (srfi :27 random-bits))

  (define make-random-bytevector
    (lambda (n)
      (let ((bv (make-bytevector n)))
        (bytevector-randomize! bv)
        bv)))

  (define /dev/urandom
    (and (file-exists? "/dev/urandom")
         (open-file-input-port "/dev/urandom"
                               (file-options)
                               (buffer-mode none))))

  ;; The same interface as bytevector-copy! except with no source
  ;; arguments.
  (define bytevector-randomize!
    (if /dev/urandom
        (case-lambda
          ((bv) (bytevector-randomize! bv 0 (bytevector-length bv)))
          ((bv start) (bytevector-randomize! bv start (bytevector-length bv)))
          ((bv start count)
           (let lp ((start start)
                    (count count))
             (unless (zero? count)
               (let ((n (get-bytevector-n! /dev/urandom bv start count)))
                 (lp (+ start n) (- count n)))))))
        (let* ((s (make-random-source))
               (make-int (random-source-make-integers s)))
          (case-lambda
            ((bv) (bytevector-randomize! bv 0 (bytevector-length bv)))
            ((bv start) (bytevector-randomize! bv start (bytevector-length bv)))
            ((bv start count)
             (random-source-randomize! s)
             (do ((start start (+ start 1))
                  (count count (- count 1)))
                 ((zero? count))
               (bytevector-u8-set! bv start (make-int 255))))))))

  (define random-positive-byte
    (if /dev/urandom
        (lambda ()
          (let lp ()
            (let ((v (get-u8 /dev/urandom)))
              (if (zero? v) (lp) v))))
        (let* ((s (make-random-source))
               (make-int (random-source-make-integers s)))
          (random-source-randomize! s)
          (lambda () (+ 1 (make-int 254))))))
  
  (define rand
    (let ((s (make-random-source)))
      (random-source-randomize! s)
      (random-source-make-integers s)))

  (define (random-boolean) (zero? (rand 2)))
  
  (define (string-random-case name)
    (string-map (cut (if (random-boolean) char-upcase char-downcase) <>)
                name))


  )
