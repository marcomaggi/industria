;; libraries.scm --
;;
;;This file is meant  to be included by the build dependencies script,  so it must be
;;in the sources search path.

(define-constant INCLUDE-LIBRARY-BUILD-HIERARCHIES
  '((weinholt)))

(define-constant INCLUDE-LIBRARY-DEPENDENCIES-HIERARCHIES
  '())

(define-constant INCLUDE-INSTALL-RULES
  #t)

;;These are the library files generated by the "configure" script starting from ".in"
;;models.
;;
(define-constant FROM-MODELS-SOURCE-FILES
  '())

;;These are the  library files generated by some automated  process; for example, the
;;"features"  program  that inspects  the  availability  of host  facilities  through
;;preprocessor constant generated by the GNU Autoconf infrastructure.
;;
(define-constant BUILT-SOURCE-FILES
  '())

;;This is the table of libraries to compile.  The table is a list of entries:
;;
;;   (?entry ...)
;;
;;each ?ENTRY having one of the formats:
;;
;;   ((?want-feature ...) ?library-name ...)
;;
;;where: each ?WANT-FEATURE is a symbol  defined in the "configure.ac" model using
;;AM_CONDITIONAL  from the  GNU  Automake infrastructure;  ?LIBRARY-NAME  is an  R6RS
;;library name specification.   If no ?WANT-FEATURE is present: the  libraries are to
;;be always processed.
;;
(define-constant LIBRARIES-SPECS
  '((()
     (weinholt archive tar)
     (weinholt assembler elf)
     (weinholt assembler x86-misc)
     (weinholt assembler x86-operands)
     (weinholt assembler x86)
     (weinholt binfmt elf)
     (weinholt bytevectors)
     (weinholt compression adler-32)
     (weinholt compression bitstream)
     (weinholt compression gzip)
     (weinholt compression huffman)
     (weinholt compression inflate)
     (weinholt compression lzma2)
     (weinholt compression lzma)
     (weinholt compression sliding-buffer)
     (weinholt compression xz)
     (weinholt compression zip)
     (weinholt compression zip extra)
     (weinholt compression zip)
     (weinholt compression zlib)
     (weinholt crypto aes private)
     (weinholt crypto aes)
     (weinholt crypto arcfour)
     (weinholt crypto blowfish)
     (weinholt crypto crc)
     (weinholt crypto des)
     (weinholt crypto dh)
     (weinholt crypto dsa)
     (weinholt crypto ec dsa)
     (weinholt crypto ec)
     (weinholt crypto entropy)
     (weinholt crypto hmac)
     (weinholt crypto math)
     (weinholt crypto md5)
     (weinholt crypto openpgp)
     (weinholt crypto password)
     (weinholt crypto rsa)
     (weinholt crypto sha-1)
     (weinholt crypto sha-2)
     (weinholt crypto ssh-public-key)
     (weinholt crypto uuid)
     (weinholt crypto x509)
     (weinholt disassembler i8080)
     (weinholt disassembler m68hc12)
     (weinholt disassembler mips)
     (weinholt disassembler private)
     (weinholt disassembler x86-opcodes)
     (weinholt disassembler x86)
     (weinholt net buffer)
     (weinholt net dns numbers)
     (weinholt net dns private)
     (weinholt net dns)
     (weinholt net dns types)
     (weinholt net irc fish)
     (weinholt net irc)
     (weinholt net otr)
     (weinholt net ssh algorithms)
     (weinholt net ssh connection)
     (weinholt net ssh kex-dh-gex)
     (weinholt net ssh kexdh)
     (weinholt net ssh private)
     (weinholt net ssh)
     (weinholt net ssh transport)
     (weinholt net ssh userauth)
     (weinholt net tcp)
     (weinholt net tls algorithms)
     (weinholt net tls simple)
     (weinholt net tls)
     (weinholt struct der)
     (weinholt struct pack-aux)
     (weinholt struct pack)
     (weinholt text base64)
     (weinholt text hexdump)
     (weinholt text internet)
     (weinholt text punycode)
     (weinholt text random-art)
     (weinholt text strings))))

;;; end of file
;; Local Variables:
;; mode: vicare
;; End: