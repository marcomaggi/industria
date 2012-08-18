;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2008, 2009 Göran Weinholt <goran@weinholt.se>

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

(library (weinholt assembler x86-misc (1 0 0))
    (export make-modr/m make-sib bitwidth<=
            number->bytevector)
    (import (rnrs))

  (define (number->bytevector imm size)
    ;; Takes a number that fits in the signed or unsigned range of
    ;; `size' bits and encodes it in a bytevector of that many bits.
    (unless (<= (- (expt 2 (- size 1))) imm (- (expt 2 size) 1))
      (error 'number->bytevector "The given number does not fit in the given number of bits"
             imm size))
    (case size
      ((8) (let ((bv (make-bytevector 1)))
             (bytevector-u8-set! bv 0 (bitwise-and imm #xff))
             bv))
      ((16) (let ((bv (make-bytevector 2)))
              (bytevector-u16-set! bv 0 (bitwise-and imm #xffff) (endianness little))
              bv))
      ((32) (let ((bv (make-bytevector 4)))
              (bytevector-u32-set! bv 0 (bitwise-and imm #xffffffff) (endianness little))
              bv))
      ((64) (let ((bv (make-bytevector 8)))
              (bytevector-u64-set! bv 0 (bitwise-and imm #xffffffffffffffff) (endianness little))
              bv))
      (else
       (let ((bv (make-bytevector (bitwise-arithmetic-shift-right size 3))))
         (bytevector-uint-set! bv
                               0
                               (bitwise-and imm (- (bitwise-arithmetic-shift-left 1 size) 1))
                               (endianness little)
                               (bitwise-arithmetic-shift-right size 3))
         bv))))
  
  (define (make-modr/m mod reg r/m)
    (fxior (fxarithmetic-shift-left (fxand mod #b11) 6)
           (fxarithmetic-shift-left (fxand reg #b111) 3)
           (fxand r/m #b111)))

  (define (make-sib scale index base)
    (fxior (fxarithmetic-shift-left (case scale
                                      ((1) #b00)
                                      ((2) #b01)
                                      ((4) #b10)
                                      ((8) #b11)
                                      (else (error 'make-sib "invalid scale" scale)))
                                    6)
           (fxarithmetic-shift-left (fxand index #b111) 3)
           (fxand base #b111)))

  (define (bitwidth<= x l u)
    (<= (- (expt 2 l)) x (- (expt 2 u) 1))))
