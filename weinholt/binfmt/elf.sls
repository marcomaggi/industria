;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2008, 2009, 2010, 2011 Göran Weinholt <goran@weinholt.se>
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

;; Routines for reading the Executable and Linkable Format (ELF)

(library (weinholt binfmt elf (1 0 20110430))
  (export is-elf-image?
          open-elf-image

          make-elf-image elf-image?
          elf-image-port elf-image-word-size elf-image-endianness
          elf-image-os-abi elf-image-abi-version elf-image-type
          elf-image-machine elf-image-entry elf-image-phoff
          elf-image-shoff elf-image-flags elf-image-ehsize
          elf-image-phentsize elf-image-phnum elf-image-shentsize
          elf-image-shnum elf-image-shstrndx

          make-elf-section elf-section?
          elf-section-name elf-section-type elf-section-flags
          elf-section-addr elf-section-offset elf-section-size
          elf-section-link elf-section-info elf-section-addralign
          elf-section-entsize

          make-elf-segment elf-segment?
          elf-segment-type elf-segment-flags elf-segment-offset
          elf-segment-vaddr elf-segment-paddr elf-segment-filesz
          elf-segment-memsz elf-segment-align

          make-elf-symbol elf-symbol?
          elf-symbol-name elf-symbol-other elf-symbol-shndx
          elf-symbol-value elf-symbol-size elf-symbol-binding
          elf-symbol-type

          elf-image-section-by-name
          elf-image-sections
          elf-image-symbols

          ELF-MAGIC

          ;; elf-image-word-size, elf-image-endianness
          ELFCLASS32 ELFCLASS64 ELFDATA2LSB ELFDATA2MSB

          ;; elf-image-type
          ET-NONE ET-REL ET-EXEC ET-DYN ET-CORE
          ET-LOOS ET-HIOS ET-LOPROC ET-HIPROC

          ;; elf-image-version
          EV-CURRENT

          ;; elf-image-os-abi
          ELFOSABI-SYSV ELFOSABI-HPUX ELFOSABI-NETBSD ELFOSABI-LINUX
          ELFOSABI-SOLARIS ELFOSABI-AIX ELFOSABI-IRIX ELFOSABI-FREEBSD
          ELFOSABI-TRU64 ELFOSABI-MODESTO ELFOSABI-OPENBSD ELFOSABI-OPENVMS
          ELFOSABI-NSK ELFOSABI-AROS

          ;; elf-image-machine
          EM-NONE EM-M32 EM-SPARC EM-386 EM-68K EM-88K EM-860
          EM-MIPS EM-MIPS-RS3-LE EM-PARISC EM-SPARC32PLUS EM-PPC
          EM-PPC64 EM-S390 EM-ARM EM-SPARCV9 EM-IA-64 EM-68HC12
          EM-X86-64 EM-68HC11
          elf-machine-names

          ;; elf-section-type, elf-section-link, elf-section-info
          SHT-NULL SHT-PROGBITS SHT-SYMTAB SHT-STRTAB SHT-RELA SHT-HASH
          SHT-DYNAMIC SHT-NOTE SHT-NOBITS SHT-REL SHT-SHLIB SHT-DYNSYM
          SHT-LOOS SHT-HIOS SHT-LOPROC SHT-HIPROC

          ;; elf-section-flags
          SHF-WRITE SHF-ALLOC SHF-EXECINSTR SHF-MASKOS SHF-MASKPROC

          SHN-UNDEF SHN-ABS SHN-COMMON

          ;; elf-segment-type
          PT-NULL PT-LOAD PT-DYNAMIC PT-INTERP PT-NOTE PT-PHDR
          PT-LOPROC PT-HIPROC

          ;; elf-segment-flags bitfield
          PF-R PF-W PF-X PF-MASKOS PF-MASKPROC

          ;; elf-symbol-binding
          STB-LOCAL STB-GLOBAL STB-WEAK
          STB-LOOS STB-HIOS STB-LOPROC STB-HIPROC

          ;; elf-symbol-type
          STT-NOTYPE STT-OBJECT STT-FUNC STT-SECTION STT-FILE
          STT-LOOS STT-HIOS STT-LOPROC STT-HIPROC)
  (import (rnrs)
          (only (srfi :1 lists) iota)
          (weinholt struct pack (1 (>= 3))))

;;; Utilities

  (define ELF-MAGIC #x7f454c46)         ;"\x7f;ELF"

  (define (print . x) (for-each display x) (newline))

  (define (file f)
    (if (input-port? f) f (open-file-input-port f)))

  (define (asciiz->string bv offset)
    (call-with-string-output-port
      (lambda (p)
        (let lp ((offset offset))
          (let ((byte (bytevector-u8-ref bv offset)))
            (unless (zero? byte)
              (put-char p (integer->char byte))
              (lp (+ offset 1))))))))

  (define (utf8z->string bv offset)
    (utf8->string
     (call-with-bytevector-output-port
       (lambda (p)
         (let lp ((offset offset))
           (let ((byte (bytevector-u8-ref bv offset)))
             (unless (zero? byte)
               (put-u8 p byte)
               (lp (+ offset 1)))))))))

;;;

  ;; Takes a filename or a binary input port and returns #t if the
  ;; file looks like an ELF image.
  (define (is-elf-image? f)
    (let* ((f (file f))
           (pos (port-position f)))
      (set-port-position! f 0)
      (let ((bv (get-bytevector-n f 16)))
        (set-port-position! f pos)
        (and (bytevector? bv)
             (= (bytevector-length bv) 16)
             (let-values (((magic word-size endian version)
                           (unpack "!LCCC9x" bv)))
               (and (= magic ELF-MAGIC)
                    (<= 1 word-size 2)
                    (<= 1 endian 2)
                    (= version 1)))))))

;;; Image headers

  ;; Word size and endianness
  (define ELFCLASS32 1)
  (define ELFCLASS64 2)
  (define ELFDATA2LSB 1)
  (define ELFDATA2MSB 2)

  ;; Object file types
  (define ET-NONE 0)
  (define ET-REL 1)                     ;relocatable
  (define ET-EXEC 2)                    ;executable
  (define ET-DYN 3)                     ;shared object
  (define ET-CORE 4)                    ;core dump
  (define ET-LOOS #xfe00)               ;environment-specific
  (define ET-HIOS #xfeff)
  (define ET-LOPROC #xff00)             ;processor-specific
  (define ET-HIPROC #xffff)

  ;; ELF version
  (define EV-CURRENT 1)

  (define ELFOSABI-SYSV 0)
  (define ELFOSABI-HPUX 1)
  (define ELFOSABI-NETBSD 2)
  (define ELFOSABI-LINUX 3)
  (define ELFOSABI-SOLARIS 6)
  (define ELFOSABI-AIX 7)
  (define ELFOSABI-IRIX 8)
  (define ELFOSABI-FREEBSD 9)
  (define ELFOSABI-TRU64 10)
  (define ELFOSABI-MODESTO 11)
  (define ELFOSABI-OPENBSD 12)
  (define ELFOSABI-OPENVMS 13)
  (define ELFOSABI-NSK 14)
  (define ELFOSABI-AROS 15)

  ;; There's a gigantic list of architectures that is missing here
  (define EM-NONE 0)
  (define EM-M32 1)
  (define EM-SPARC 2)
  (define EM-386 3)
  (define EM-68K 4)
  (define EM-88K 5)
  (define EM-860 7)
  (define EM-MIPS 8)
  (define EM-MIPS-RS3-LE 10)
  (define EM-PARISC 15)
  (define EM-SPARC32PLUS 18)
  (define EM-PPC 20)
  (define EM-PPC64 21)
  (define EM-S390 22)
  (define EM-ARM 40)
  (define EM-SPARCV9 43)
  (define EM-IA-64 50)
  (define EM-68HC12 53)
  (define EM-X86-64 62)
  (define EM-68HC11 70)

  (define elf-machine-names
    (list (cons EM-NONE "No machine")
          (cons EM-M32 "AT&T WE32100")
          (cons EM-SPARC "SPARC")
          (cons EM-386 "Intel 80386")
          (cons EM-68K "Motorola 68000")
          (cons EM-88K "Motorola 88000")
          (cons EM-860 "Intel 80860")
          (cons EM-MIPS "MIPS I Architecture")
          (cons EM-MIPS-RS3-LE "MIPS RS3000 Little-endian")
          (cons EM-PARISC "Hewlett-Packard PA-RISC")
          (cons EM-SPARC32PLUS "Enhanced instruction set SPARC")
          (cons EM-PPC "PowerPC")
          (cons EM-PPC64 "64-bit PowerPC")
          (cons EM-S390 "IBM System/390 Processor")
          (cons EM-ARM "Advanced RISC Machines ARM")
          (cons EM-SPARCV9 "SPARC V9")
          (cons EM-IA-64 "Intel IA-64 Processor Architecture")
          (cons EM-68HC12 "Motorola M68HC12")
          (cons EM-X86-64 "AMD x86-64 architecture")
          (cons EM-68HC11 "Motorola MC68HC11 microcontroller")))

  (define-record-type elf-image
    (fields port word-size endianness os-abi abi-version
            type machine version
            entry                 ;program entry point
            phoff shoff           ;offset to program headers / section
            flags                 ;processor-specific flags
            ehsize                ;elf header size
            phentsize phnum       ;program header entries
            shentsize shnum       ;section headers
            shstrndx))            ;section idx of section name table

  (define (open-elf-image fn)
    (let ((port (file fn)))
      (unless (is-elf-image? port)
        (error 'open-elf-image "Not an ELF image" fn))
      (set-port-position! port 0)
      (get-elf-image port)))

  (define (get-elf-image port)
    (let*-values (((word-size endianness os-abi abi-version)
                   (get-unpack port "4xCCxCC7x"))
                  (x
                   (if (= word-size ELFCLASS32)
                       (if (= endianness ELFDATA2LSB)
                           (get-unpack port "<SSLLLLLSSSSSS")
                           (get-unpack port ">SSLLLLLSSSSSS"))
                       (if (= endianness ELFDATA2LSB)
                           (get-unpack port "<SSLQQQLSSSSSS")
                           (get-unpack port ">SSLQQQLSSSSSS")))))
      (apply make-elf-image port word-size endianness os-abi abi-version x)))

;;; Section headers

  ;; Section types
  (define SHT-NULL 0)
  (define SHT-PROGBITS 1)
  (define SHT-SYMTAB 2)
  (define SHT-STRTAB 3)
  (define SHT-RELA 4)
  (define SHT-HASH 5)
  (define SHT-DYNAMIC 6)
  (define SHT-NOTE 7)
  (define SHT-NOBITS 8)
  (define SHT-REL 9)
  (define SHT-SHLIB 10)
  (define SHT-DYNSYM 11)
  (define SHT-LOOS #x60000000)
  (define SHT-HIOS #x6FFFFFFF)
  (define SHT-LOPROC #x70000000)
  (define SHT-HIPROC #x7FFFFFFF)

  ;; Section flags
  (define SHF-WRITE #b1)
  (define SHF-ALLOC #b10)
  (define SHF-EXECINSTR #b100)
  (define SHF-MASKOS #xf000000)
  (define SHF-MASKPROC #xf0000000)

  ;; Special section indexes
  (define SHN-UNDEF 0)
  (define SHN-ABS #xFFF1)
  (define SHN-COMMON #xFFF2)

  (define-record-type elf-section
    (fields name type flags addr offset size link info addralign entsize))

  (define (get-elf-section image index)
    (unless (or (< index 0) (< index (elf-image-shnum image)))
      (error 'get-elf-section "Index out of bounds" index))
    (let ((port (elf-image-port image)))
      (set-port-position! port (+ (elf-image-shoff image)
                                  (* (elf-image-shentsize image) index)))
      (call-with-values (lambda ()
                          (let ((bv (get-bytevector-n port
                                                      (elf-image-shentsize image))))
                            (if (= (elf-image-word-size image) 1)
                                (if (= (elf-image-endianness image) 1)
                                    (unpack "<10L" bv)
                                    (unpack ">10L" bv))
                                (if (= (elf-image-endianness image) 1)
                                    (unpack "<LL4QLLQQ" bv)
                                    (unpack ">LL4QLLQQ" bv)))))
        make-elf-section)))

;;; Program headers

  (define PT-NULL 0)                    ;to be ignored
  (define PT-LOAD 1)                    ;loadable segment
  (define PT-DYNAMIC 2)                 ;dynamic linking info
  (define PT-INTERP 3)                  ;interpreter pathname
  (define PT-NOTE 4)                    ;note section
  (define PT-PHDR 6)                    ;the program header
  (define PT-LOPROC #x70000000)         ;processor-specific
  (define PT-HIPROC #x7fffffff)

  (define PF-X #b1)                     ;executable
  (define PF-W #b10)                    ;writable
  (define PF-R #b100)                   ;readable
  (define PF-MASKOS #xFF0000)           ;os specific flags
  (define PF-MASKPROC #xFF000000)       ;processor specific flags

  (define-record-type elf-segment
    (fields type
            flags
            offset                      ;file offset program segment
            vaddr                       ;virtual address
            paddr                       ;physical address (not really used)
            filesz                      ;size in file image
            memsz                       ;size in memory image
            align))                     ;required alignment

  (define (get-elf-segment image index)
    (unless (or (< index 0) (< index (elf-image-phnum image)))
      (error 'get-elf-segment "Index out of bounds" index))
    (let ((port (elf-image-port image)))
      (set-port-position! port (+ (elf-image-phoff image)
                                  (* (elf-image-phentsize image) index)))
      (let ((bv (get-bytevector-n port (elf-image-phentsize image))))
        (if (= (elf-image-word-size image) 1)
            (let-values (((type offset vaddr paddr filesz memsz flags align)
                          (if (= (elf-image-endianness image) 1)
                              (unpack "<8L" bv)
                              (unpack ">8L" bv))))
              (make-elf-segment type flags offset vaddr paddr filesz memsz align))
            (let-values (((type flags offset vaddr paddr filesz memsz align)
                          (if (= (elf-image-endianness image) 1)
                              (unpack "<2L6Q" bv)
                              (unpack ">2L6Q" bv))))
              (make-elf-segment type flags offset vaddr paddr filesz memsz align))))))

;;; Symbol tables

  (define-record-type elf-symbol
    (fields name
            info                        ;4 bits binding, 4 bits type
            other                       ;reserved
            shndx                       ;section index
            value
            size))

  (define (elf-symbol-binding x)
    (bitwise-bit-field (elf-symbol-info x) 0 4))

  (define (elf-symbol-type x)
    (bitwise-bit-field (elf-symbol-info x) 4 8))

  (define STB-LOCAL 0)
  (define STB-GLOBAL 1)
  (define STB-WEAK 2)
  (define STB-LOOS 10)
  (define STB-HIOS 12)
  (define STB-LOPROC 13)
  (define STB-HIPROC 15)

  (define STT-NOTYPE 0)
  (define STT-OBJECT 1)
  (define STT-FUNC 2)
  (define STT-SECTION 3)
  (define STT-FILE 4)
  (define STT-LOOS 10)
  (define STT-HIOS 12)
  (define STT-LOPROC 13)
  (define STT-HIPROC 15)

  (define (elf-image-symbols image)
    (let ((p (elf-image-port image))
          (strtab* (elf-image-section-by-name image ".strtab"))
          (symtab* (elf-image-section-by-name image ".symtab")))
      (and strtab* symtab*
           (let ((strtab (get-elf-section-data image strtab*)))
             (set-port-position! p (elf-section-offset symtab*))
             (do ((table (make-vector (div (elf-section-size symtab*)
                                           (elf-section-entsize symtab*))))
                  (i 0 (+ i 1)))
                 ((= i (vector-length table))
                  table)
               (let ((bv (get-bytevector-n p (elf-section-entsize symtab*))))
                 (if (= (elf-image-word-size image) 1)
                     (let-values (((name value size info other shndx)
                                   (if (= (elf-image-endianness image) 1)
                                       (unpack "<LLLCCS" bv)
                                       (unpack ">LLLCCS" bv))))
                       (vector-set! table i (cons (utf8z->string strtab name)
                                                  (make-elf-symbol
                                                   name info other shndx value size))))
                     (let-values (((name info other shndx value size)
                                   (if (= (elf-image-endianness image) 1)
                                       (unpack "<LCCSQQ" bv)
                                       (unpack ">LCCSQQ" bv))))
                       (vector-set! table i (cons (utf8z->string strtab name)
                                                  (make-elf-symbol
                                                   name info other shndx value size)))))))))))

;;; Helpers

  (define (get-elf-section-data image sh)
    ;; XXX: missing sanity checks
    (let ((port (elf-image-port image)))
      (set-port-position! port (elf-section-offset sh))
      (get-bytevector-n port (elf-section-size sh))))

  (define (get-elf-section-names image)
    ;; FIXME: check for special section indexes and everything else
    ;; required for shstrndx...
    (let ((i (elf-image-shstrndx image)))
      (and (not (= SHN-UNDEF i))
           (let ((shstrtab (get-elf-section image i))
                 (port (elf-image-port image)))
             (unless (= (elf-section-type shstrtab) SHT-STRTAB)
               (error 'get-elf-section-names
                      "Corrupt ELF: shstrtab not a string table" shstrtab))
             (get-elf-section-data image shstrtab)))))

  (define (elf-image-section-by-name image name)
    (let ((sections (elf-image-sections image)))
      (cond ((and sections (assoc name sections)) => cdr)
            (else #f))))

  (define (elf-image-sections image)
    (let ((section-names (get-elf-section-names image)))
      (map (lambda (i)
             (let* ((sh (get-elf-section image i))
                    (name (asciiz->string section-names (elf-section-name sh))))
               (cons name sh)))
           (iota (elf-image-shnum image)))))

  (define (elf-image-segments image)
    (map (lambda (i)
           (get-elf-segment image i))
         (iota (elf-image-phnum image)))))
