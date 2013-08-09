;; compile-all.sps --
;;
;; Compilation script for Vicare Scheme.

#!r6rs
(import
    (only (weinholt archive tar))
  (only (weinholt assembler elf))
  (only (weinholt assembler x86-misc))
  (only (weinholt assembler x86-operands))
  (only (weinholt assembler x86))
  (only (weinholt binfmt elf))
  (only (weinholt bytevectors))
  (only (weinholt compression adler-32))
  (only (weinholt compression bitstream))
  (only (weinholt compression gzip))
  (only (weinholt compression huffman))
  (only (weinholt compression inflate))
  (only (weinholt compression lzma2))
  (only (weinholt compression lzma))
  (only (weinholt compression sliding-buffer))
  (only (weinholt compression xz))
  (only (weinholt compression zip))
  (only (weinholt compression zip extra))
  (only (weinholt compression zip))
  (only (weinholt compression zlib))
  (only (weinholt crypto aes))
  (only (weinholt crypto aes private))
  (only (weinholt crypto aes))
  (only (weinholt crypto arcfour))
  (only (weinholt crypto blowfish))
  (only (weinholt crypto crc))
  (only (weinholt crypto des))
  (only (weinholt crypto dh))
  (only (weinholt crypto dsa))
  (only (weinholt crypto ec))
  (only (weinholt crypto ec dsa))
  (only (weinholt crypto ec))
  (only (weinholt crypto entropy))
  (only (weinholt crypto hmac))
  (only (weinholt crypto math))
  (only (weinholt crypto md5))
  (only (weinholt crypto openpgp))
  (only (weinholt crypto password))
  (only (weinholt crypto rsa))
  (only (weinholt crypto sha-1))
  (only (weinholt crypto sha-2))
  (only (weinholt crypto ssh-public-key))
  (only (weinholt crypto uuid))
  (only (weinholt crypto x509))
  (only (weinholt disassembler i8080))
  (only (weinholt disassembler m68hc12))
  (only (weinholt disassembler mips))
  (only (weinholt disassembler private))
  (only (weinholt disassembler x86-opcodes))
  (only (weinholt disassembler x86))
  (only (weinholt net buffer))
  (only (weinholt net dns numbers))
  (only (weinholt net dns private))
  (only (weinholt net dns))
  (only (weinholt net dns types))
  (only (weinholt net irc fish))
  (only (weinholt net irc))
  (only (weinholt net otr))
  (only (weinholt net ssh algorithms))
  (only (weinholt net ssh connection))
  (only (weinholt net ssh kex-dh-gex))
  (only (weinholt net ssh kexdh))
  (only (weinholt net ssh private))
  (only (weinholt net ssh))
  (only (weinholt net ssh transport))
  (only (weinholt net ssh userauth))
  (only (weinholt net tcp))
  (only (weinholt net tls algorithms))
  (only (weinholt net tls simple))
  (only (weinholt net tls))
  (only (weinholt struct der))
  (only (weinholt struct pack-aux))
  (only (weinholt struct pack))
  (only (weinholt text base64))
  (only (weinholt text hexdump))
  (only (weinholt text internet))
  (only (weinholt text punycode))
  (only (weinholt text random-art))
  (only (weinholt text strings)))

;;; end of file
