## dependencies.make --
#
# Automatically built.

lib/weinholt/archive/tar.fasl: \
		lib/weinholt/archive/tar.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_archive_tar_fasldir = $(bundledlibsdir)/weinholt/archive
lib_weinholt_archive_tar_slsdir  = $(bundledlibsdir)/weinholt/archive
nodist_lib_weinholt_archive_tar_fasl_DATA = lib/weinholt/archive/tar.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_archive_tar_sls_DATA = lib/weinholt/archive/tar.sls
endif
EXTRA_DIST += lib/weinholt/archive/tar.sls
CLEANFILES += lib/weinholt/archive/tar.fasl

lib/weinholt/assembler/elf.fasl: \
		lib/weinholt/assembler/elf.sls \
		lib/weinholt/binfmt/elf.fasl \
		lib/weinholt/struct/pack.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_assembler_elf_fasldir = $(bundledlibsdir)/weinholt/assembler
lib_weinholt_assembler_elf_slsdir  = $(bundledlibsdir)/weinholt/assembler
nodist_lib_weinholt_assembler_elf_fasl_DATA = lib/weinholt/assembler/elf.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_assembler_elf_sls_DATA = lib/weinholt/assembler/elf.sls
endif
EXTRA_DIST += lib/weinholt/assembler/elf.sls
CLEANFILES += lib/weinholt/assembler/elf.fasl

lib/weinholt/binfmt/elf.fasl: \
		lib/weinholt/binfmt/elf.sls \
		lib/weinholt/struct/pack.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_binfmt_elf_fasldir = $(bundledlibsdir)/weinholt/binfmt
lib_weinholt_binfmt_elf_slsdir  = $(bundledlibsdir)/weinholt/binfmt
nodist_lib_weinholt_binfmt_elf_fasl_DATA = lib/weinholt/binfmt/elf.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_binfmt_elf_sls_DATA = lib/weinholt/binfmt/elf.sls
endif
EXTRA_DIST += lib/weinholt/binfmt/elf.sls
CLEANFILES += lib/weinholt/binfmt/elf.fasl

lib/weinholt/struct/pack.fasl: \
		lib/weinholt/struct/pack.sls \
		lib/weinholt/struct/pack-aux.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_struct_pack_fasldir = $(bundledlibsdir)/weinholt/struct
lib_weinholt_struct_pack_slsdir  = $(bundledlibsdir)/weinholt/struct
nodist_lib_weinholt_struct_pack_fasl_DATA = lib/weinholt/struct/pack.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_struct_pack_sls_DATA = lib/weinholt/struct/pack.sls
endif
EXTRA_DIST += lib/weinholt/struct/pack.sls
CLEANFILES += lib/weinholt/struct/pack.fasl

lib/weinholt/struct/pack-aux.fasl: \
		lib/weinholt/struct/pack-aux.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_struct_pack_aux_fasldir = $(bundledlibsdir)/weinholt/struct
lib_weinholt_struct_pack_aux_slsdir  = $(bundledlibsdir)/weinholt/struct
nodist_lib_weinholt_struct_pack_aux_fasl_DATA = lib/weinholt/struct/pack-aux.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_struct_pack_aux_sls_DATA = lib/weinholt/struct/pack-aux.sls
endif
EXTRA_DIST += lib/weinholt/struct/pack-aux.sls
CLEANFILES += lib/weinholt/struct/pack-aux.fasl

lib/weinholt/assembler/x86-misc.fasl: \
		lib/weinholt/assembler/x86-misc.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_assembler_x86_misc_fasldir = $(bundledlibsdir)/weinholt/assembler
lib_weinholt_assembler_x86_misc_slsdir  = $(bundledlibsdir)/weinholt/assembler
nodist_lib_weinholt_assembler_x86_misc_fasl_DATA = lib/weinholt/assembler/x86-misc.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_assembler_x86_misc_sls_DATA = lib/weinholt/assembler/x86-misc.sls
endif
EXTRA_DIST += lib/weinholt/assembler/x86-misc.sls
CLEANFILES += lib/weinholt/assembler/x86-misc.fasl

lib/weinholt/assembler/x86-operands.fasl: \
		lib/weinholt/assembler/x86-operands.sls \
		lib/weinholt/assembler/x86-misc.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_assembler_x86_operands_fasldir = $(bundledlibsdir)/weinholt/assembler
lib_weinholt_assembler_x86_operands_slsdir  = $(bundledlibsdir)/weinholt/assembler
nodist_lib_weinholt_assembler_x86_operands_fasl_DATA = lib/weinholt/assembler/x86-operands.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_assembler_x86_operands_sls_DATA = lib/weinholt/assembler/x86-operands.sls
endif
EXTRA_DIST += lib/weinholt/assembler/x86-operands.sls
CLEANFILES += lib/weinholt/assembler/x86-operands.fasl

lib/weinholt/assembler/x86.fasl: \
		lib/weinholt/assembler/x86.sls \
		lib/weinholt/assembler/x86-operands.fasl \
		lib/weinholt/assembler/x86-misc.fasl \
		lib/weinholt/disassembler/x86-opcodes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_assembler_x86_fasldir = $(bundledlibsdir)/weinholt/assembler
lib_weinholt_assembler_x86_slsdir  = $(bundledlibsdir)/weinholt/assembler
nodist_lib_weinholt_assembler_x86_fasl_DATA = lib/weinholt/assembler/x86.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_assembler_x86_sls_DATA = lib/weinholt/assembler/x86.sls
endif
EXTRA_DIST += lib/weinholt/assembler/x86.sls
CLEANFILES += lib/weinholt/assembler/x86.fasl

lib/weinholt/disassembler/x86-opcodes.fasl: \
		lib/weinholt/disassembler/x86-opcodes.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_disassembler_x86_opcodes_fasldir = $(bundledlibsdir)/weinholt/disassembler
lib_weinholt_disassembler_x86_opcodes_slsdir  = $(bundledlibsdir)/weinholt/disassembler
nodist_lib_weinholt_disassembler_x86_opcodes_fasl_DATA = lib/weinholt/disassembler/x86-opcodes.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_disassembler_x86_opcodes_sls_DATA = lib/weinholt/disassembler/x86-opcodes.sls
endif
EXTRA_DIST += lib/weinholt/disassembler/x86-opcodes.sls
CLEANFILES += lib/weinholt/disassembler/x86-opcodes.fasl

lib/weinholt/bytevectors.fasl: \
		lib/weinholt/bytevectors.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_bytevectors_fasldir = $(bundledlibsdir)/weinholt
lib_weinholt_bytevectors_slsdir  = $(bundledlibsdir)/weinholt
nodist_lib_weinholt_bytevectors_fasl_DATA = lib/weinholt/bytevectors.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_bytevectors_sls_DATA = lib/weinholt/bytevectors.sls
endif
EXTRA_DIST += lib/weinholt/bytevectors.sls
CLEANFILES += lib/weinholt/bytevectors.fasl

lib/weinholt/compression/adler-32.fasl: \
		lib/weinholt/compression/adler-32.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_compression_adler_32_fasldir = $(bundledlibsdir)/weinholt/compression
lib_weinholt_compression_adler_32_slsdir  = $(bundledlibsdir)/weinholt/compression
nodist_lib_weinholt_compression_adler_32_fasl_DATA = lib/weinholt/compression/adler-32.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_compression_adler_32_sls_DATA = lib/weinholt/compression/adler-32.sls
endif
EXTRA_DIST += lib/weinholt/compression/adler-32.sls
CLEANFILES += lib/weinholt/compression/adler-32.fasl

lib/weinholt/compression/bitstream.fasl: \
		lib/weinholt/compression/bitstream.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_compression_bitstream_fasldir = $(bundledlibsdir)/weinholt/compression
lib_weinholt_compression_bitstream_slsdir  = $(bundledlibsdir)/weinholt/compression
nodist_lib_weinholt_compression_bitstream_fasl_DATA = lib/weinholt/compression/bitstream.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_compression_bitstream_sls_DATA = lib/weinholt/compression/bitstream.sls
endif
EXTRA_DIST += lib/weinholt/compression/bitstream.sls
CLEANFILES += lib/weinholt/compression/bitstream.fasl

lib/weinholt/compression/gzip.fasl: \
		lib/weinholt/compression/gzip.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/crc.fasl \
		lib/weinholt/compression/inflate.fasl \
		lib/weinholt/struct/pack.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_compression_gzip_fasldir = $(bundledlibsdir)/weinholt/compression
lib_weinholt_compression_gzip_slsdir  = $(bundledlibsdir)/weinholt/compression
nodist_lib_weinholt_compression_gzip_fasl_DATA = lib/weinholt/compression/gzip.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_compression_gzip_sls_DATA = lib/weinholt/compression/gzip.sls
endif
EXTRA_DIST += lib/weinholt/compression/gzip.sls
CLEANFILES += lib/weinholt/compression/gzip.fasl

lib/weinholt/crypto/crc.fasl: \
		lib/weinholt/crypto/crc.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_crc_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_crc_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_crc_fasl_DATA = lib/weinholt/crypto/crc.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_crc_sls_DATA = lib/weinholt/crypto/crc.sls
endif
EXTRA_DIST += lib/weinholt/crypto/crc.sls
CLEANFILES += lib/weinholt/crypto/crc.fasl

lib/weinholt/compression/inflate.fasl: \
		lib/weinholt/compression/inflate.sls \
		lib/weinholt/compression/bitstream.fasl \
		lib/weinholt/compression/huffman.fasl \
		lib/weinholt/compression/sliding-buffer.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_compression_inflate_fasldir = $(bundledlibsdir)/weinholt/compression
lib_weinholt_compression_inflate_slsdir  = $(bundledlibsdir)/weinholt/compression
nodist_lib_weinholt_compression_inflate_fasl_DATA = lib/weinholt/compression/inflate.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_compression_inflate_sls_DATA = lib/weinholt/compression/inflate.sls
endif
EXTRA_DIST += lib/weinholt/compression/inflate.sls
CLEANFILES += lib/weinholt/compression/inflate.fasl

lib/weinholt/compression/huffman.fasl: \
		lib/weinholt/compression/huffman.sls \
		lib/weinholt/compression/bitstream.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_compression_huffman_fasldir = $(bundledlibsdir)/weinholt/compression
lib_weinholt_compression_huffman_slsdir  = $(bundledlibsdir)/weinholt/compression
nodist_lib_weinholt_compression_huffman_fasl_DATA = lib/weinholt/compression/huffman.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_compression_huffman_sls_DATA = lib/weinholt/compression/huffman.sls
endif
EXTRA_DIST += lib/weinholt/compression/huffman.sls
CLEANFILES += lib/weinholt/compression/huffman.fasl

lib/weinholt/compression/sliding-buffer.fasl: \
		lib/weinholt/compression/sliding-buffer.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_compression_sliding_buffer_fasldir = $(bundledlibsdir)/weinholt/compression
lib_weinholt_compression_sliding_buffer_slsdir  = $(bundledlibsdir)/weinholt/compression
nodist_lib_weinholt_compression_sliding_buffer_fasl_DATA = lib/weinholt/compression/sliding-buffer.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_compression_sliding_buffer_sls_DATA = lib/weinholt/compression/sliding-buffer.sls
endif
EXTRA_DIST += lib/weinholt/compression/sliding-buffer.sls
CLEANFILES += lib/weinholt/compression/sliding-buffer.fasl

lib/weinholt/compression/lzma2.fasl: \
		lib/weinholt/compression/lzma2.sls \
		lib/weinholt/compression/lzma.fasl \
		lib/weinholt/compression/sliding-buffer.fasl \
		lib/weinholt/struct/pack.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_compression_lzma2_fasldir = $(bundledlibsdir)/weinholt/compression
lib_weinholt_compression_lzma2_slsdir  = $(bundledlibsdir)/weinholt/compression
nodist_lib_weinholt_compression_lzma2_fasl_DATA = lib/weinholt/compression/lzma2.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_compression_lzma2_sls_DATA = lib/weinholt/compression/lzma2.sls
endif
EXTRA_DIST += lib/weinholt/compression/lzma2.sls
CLEANFILES += lib/weinholt/compression/lzma2.fasl

lib/weinholt/compression/lzma.fasl: \
		lib/weinholt/compression/lzma.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/compression/sliding-buffer.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_compression_lzma_fasldir = $(bundledlibsdir)/weinholt/compression
lib_weinholt_compression_lzma_slsdir  = $(bundledlibsdir)/weinholt/compression
nodist_lib_weinholt_compression_lzma_fasl_DATA = lib/weinholt/compression/lzma.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_compression_lzma_sls_DATA = lib/weinholt/compression/lzma.sls
endif
EXTRA_DIST += lib/weinholt/compression/lzma.sls
CLEANFILES += lib/weinholt/compression/lzma.fasl

lib/weinholt/compression/xz.fasl: \
		lib/weinholt/compression/xz.sls \
		lib/weinholt/compression/lzma2.fasl \
		lib/weinholt/crypto/crc.fasl \
		lib/weinholt/crypto/sha-2.fasl \
		lib/weinholt/struct/pack.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_compression_xz_fasldir = $(bundledlibsdir)/weinholt/compression
lib_weinholt_compression_xz_slsdir  = $(bundledlibsdir)/weinholt/compression
nodist_lib_weinholt_compression_xz_fasl_DATA = lib/weinholt/compression/xz.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_compression_xz_sls_DATA = lib/weinholt/compression/xz.sls
endif
EXTRA_DIST += lib/weinholt/compression/xz.sls
CLEANFILES += lib/weinholt/compression/xz.fasl

lib/weinholt/crypto/sha-2.fasl: \
		lib/weinholt/crypto/sha-2.sls \
		lib/weinholt/crypto/hmac.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_sha_2_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_sha_2_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_sha_2_fasl_DATA = lib/weinholt/crypto/sha-2.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_sha_2_sls_DATA = lib/weinholt/crypto/sha-2.sls
endif
EXTRA_DIST += lib/weinholt/crypto/sha-2.sls
CLEANFILES += lib/weinholt/crypto/sha-2.fasl

lib/weinholt/crypto/hmac.fasl: \
		lib/weinholt/crypto/hmac.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_hmac_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_hmac_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_hmac_fasl_DATA = lib/weinholt/crypto/hmac.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_hmac_sls_DATA = lib/weinholt/crypto/hmac.sls
endif
EXTRA_DIST += lib/weinholt/crypto/hmac.sls
CLEANFILES += lib/weinholt/crypto/hmac.fasl

lib/weinholt/compression/zip.fasl: \
		lib/weinholt/compression/zip.sls \
		lib/weinholt/struct/pack.fasl \
		lib/weinholt/crypto/crc.fasl \
		lib/weinholt/compression/inflate.fasl \
		lib/weinholt/compression/zip/extra.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_compression_zip_fasldir = $(bundledlibsdir)/weinholt/compression
lib_weinholt_compression_zip_slsdir  = $(bundledlibsdir)/weinholt/compression
nodist_lib_weinholt_compression_zip_fasl_DATA = lib/weinholt/compression/zip.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_compression_zip_sls_DATA = lib/weinholt/compression/zip.sls
endif
EXTRA_DIST += lib/weinholt/compression/zip.sls
CLEANFILES += lib/weinholt/compression/zip.fasl

lib/weinholt/compression/zip/extra.fasl: \
		lib/weinholt/compression/zip/extra.vicare.sls \
		lib/weinholt/text/strings.fasl \
		lib/weinholt/struct/pack.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_compression_zip_extra_fasldir = $(bundledlibsdir)/weinholt/compression/zip
lib_weinholt_compression_zip_extra_vicare_slsdir  = $(bundledlibsdir)/weinholt/compression/zip
nodist_lib_weinholt_compression_zip_extra_fasl_DATA = lib/weinholt/compression/zip/extra.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_compression_zip_extra_vicare_sls_DATA = lib/weinholt/compression/zip/extra.vicare.sls
endif
EXTRA_DIST += lib/weinholt/compression/zip/extra.vicare.sls
CLEANFILES += lib/weinholt/compression/zip/extra.fasl

lib/weinholt/text/strings.fasl: \
		lib/weinholt/text/strings.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_text_strings_fasldir = $(bundledlibsdir)/weinholt/text
lib_weinholt_text_strings_slsdir  = $(bundledlibsdir)/weinholt/text
nodist_lib_weinholt_text_strings_fasl_DATA = lib/weinholt/text/strings.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_text_strings_sls_DATA = lib/weinholt/text/strings.sls
endif
EXTRA_DIST += lib/weinholt/text/strings.sls
CLEANFILES += lib/weinholt/text/strings.fasl

lib/weinholt/compression/zlib.fasl: \
		lib/weinholt/compression/zlib.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/compression/adler-32.fasl \
		lib/weinholt/compression/inflate.fasl \
		lib/weinholt/struct/pack.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_compression_zlib_fasldir = $(bundledlibsdir)/weinholt/compression
lib_weinholt_compression_zlib_slsdir  = $(bundledlibsdir)/weinholt/compression
nodist_lib_weinholt_compression_zlib_fasl_DATA = lib/weinholt/compression/zlib.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_compression_zlib_sls_DATA = lib/weinholt/compression/zlib.sls
endif
EXTRA_DIST += lib/weinholt/compression/zlib.sls
CLEANFILES += lib/weinholt/compression/zlib.fasl

lib/weinholt/crypto/aes/private.fasl: \
		lib/weinholt/crypto/aes/private.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_aes_private_fasldir = $(bundledlibsdir)/weinholt/crypto/aes
lib_weinholt_crypto_aes_private_slsdir  = $(bundledlibsdir)/weinholt/crypto/aes
nodist_lib_weinholt_crypto_aes_private_fasl_DATA = lib/weinholt/crypto/aes/private.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_aes_private_sls_DATA = lib/weinholt/crypto/aes/private.sls
endif
EXTRA_DIST += lib/weinholt/crypto/aes/private.sls
CLEANFILES += lib/weinholt/crypto/aes/private.fasl

lib/weinholt/crypto/aes.fasl: \
		lib/weinholt/crypto/aes.sls \
		lib/weinholt/crypto/aes/private.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_aes_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_aes_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_aes_fasl_DATA = lib/weinholt/crypto/aes.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_aes_sls_DATA = lib/weinholt/crypto/aes.sls
endif
EXTRA_DIST += lib/weinholt/crypto/aes.sls
CLEANFILES += lib/weinholt/crypto/aes.fasl

lib/weinholt/crypto/arcfour.fasl: \
		lib/weinholt/crypto/arcfour.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_arcfour_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_arcfour_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_arcfour_fasl_DATA = lib/weinholt/crypto/arcfour.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_arcfour_sls_DATA = lib/weinholt/crypto/arcfour.sls
endif
EXTRA_DIST += lib/weinholt/crypto/arcfour.sls
CLEANFILES += lib/weinholt/crypto/arcfour.fasl

lib/weinholt/crypto/blowfish.fasl: \
		lib/weinholt/crypto/blowfish.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_blowfish_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_blowfish_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_blowfish_fasl_DATA = lib/weinholt/crypto/blowfish.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_blowfish_sls_DATA = lib/weinholt/crypto/blowfish.sls
endif
EXTRA_DIST += lib/weinholt/crypto/blowfish.sls
CLEANFILES += lib/weinholt/crypto/blowfish.fasl

lib/weinholt/crypto/des.fasl: \
		lib/weinholt/crypto/des.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_des_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_des_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_des_fasl_DATA = lib/weinholt/crypto/des.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_des_sls_DATA = lib/weinholt/crypto/des.sls
endif
EXTRA_DIST += lib/weinholt/crypto/des.sls
CLEANFILES += lib/weinholt/crypto/des.fasl

lib/weinholt/crypto/dh.fasl: \
		lib/weinholt/crypto/dh.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/entropy.fasl \
		lib/weinholt/crypto/math.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_dh_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_dh_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_dh_fasl_DATA = lib/weinholt/crypto/dh.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_dh_sls_DATA = lib/weinholt/crypto/dh.sls
endif
EXTRA_DIST += lib/weinholt/crypto/dh.sls
CLEANFILES += lib/weinholt/crypto/dh.fasl

lib/weinholt/crypto/entropy.fasl: \
		lib/weinholt/crypto/entropy.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_entropy_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_entropy_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_entropy_fasl_DATA = lib/weinholt/crypto/entropy.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_entropy_sls_DATA = lib/weinholt/crypto/entropy.sls
endif
EXTRA_DIST += lib/weinholt/crypto/entropy.sls
CLEANFILES += lib/weinholt/crypto/entropy.fasl

lib/weinholt/crypto/math.fasl: \
		lib/weinholt/crypto/math.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_math_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_math_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_math_fasl_DATA = lib/weinholt/crypto/math.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_math_sls_DATA = lib/weinholt/crypto/math.sls
endif
EXTRA_DIST += lib/weinholt/crypto/math.sls
CLEANFILES += lib/weinholt/crypto/math.fasl

lib/weinholt/crypto/dsa.fasl: \
		lib/weinholt/crypto/dsa.sls \
		lib/weinholt/struct/der.fasl \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/entropy.fasl \
		lib/weinholt/crypto/math.fasl \
		lib/weinholt/text/base64.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_dsa_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_dsa_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_dsa_fasl_DATA = lib/weinholt/crypto/dsa.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_dsa_sls_DATA = lib/weinholt/crypto/dsa.sls
endif
EXTRA_DIST += lib/weinholt/crypto/dsa.sls
CLEANFILES += lib/weinholt/crypto/dsa.fasl

lib/weinholt/struct/der.fasl: \
		lib/weinholt/struct/der.sls \
		lib/weinholt/bytevectors.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_struct_der_fasldir = $(bundledlibsdir)/weinholt/struct
lib_weinholt_struct_der_slsdir  = $(bundledlibsdir)/weinholt/struct
nodist_lib_weinholt_struct_der_fasl_DATA = lib/weinholt/struct/der.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_struct_der_sls_DATA = lib/weinholt/struct/der.sls
endif
EXTRA_DIST += lib/weinholt/struct/der.sls
CLEANFILES += lib/weinholt/struct/der.fasl

lib/weinholt/text/base64.fasl: \
		lib/weinholt/text/base64.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_text_base64_fasldir = $(bundledlibsdir)/weinholt/text
lib_weinholt_text_base64_slsdir  = $(bundledlibsdir)/weinholt/text
nodist_lib_weinholt_text_base64_fasl_DATA = lib/weinholt/text/base64.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_text_base64_sls_DATA = lib/weinholt/text/base64.sls
endif
EXTRA_DIST += lib/weinholt/text/base64.sls
CLEANFILES += lib/weinholt/text/base64.fasl

lib/weinholt/crypto/ec/dsa.fasl: \
		lib/weinholt/crypto/ec/dsa.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/ec.fasl \
		lib/weinholt/crypto/entropy.fasl \
		lib/weinholt/crypto/math.fasl \
		lib/weinholt/crypto/sha-2.fasl \
		lib/weinholt/struct/der.fasl \
		lib/weinholt/text/base64.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_ec_dsa_fasldir = $(bundledlibsdir)/weinholt/crypto/ec
lib_weinholt_crypto_ec_dsa_slsdir  = $(bundledlibsdir)/weinholt/crypto/ec
nodist_lib_weinholt_crypto_ec_dsa_fasl_DATA = lib/weinholt/crypto/ec/dsa.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_ec_dsa_sls_DATA = lib/weinholt/crypto/ec/dsa.sls
endif
EXTRA_DIST += lib/weinholt/crypto/ec/dsa.sls
CLEANFILES += lib/weinholt/crypto/ec/dsa.fasl

lib/weinholt/crypto/ec.fasl: \
		lib/weinholt/crypto/ec.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/math.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_ec_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_ec_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_ec_fasl_DATA = lib/weinholt/crypto/ec.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_ec_sls_DATA = lib/weinholt/crypto/ec.sls
endif
EXTRA_DIST += lib/weinholt/crypto/ec.sls
CLEANFILES += lib/weinholt/crypto/ec.fasl

lib/weinholt/crypto/md5.fasl: \
		lib/weinholt/crypto/md5.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_md5_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_md5_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_md5_fasl_DATA = lib/weinholt/crypto/md5.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_md5_sls_DATA = lib/weinholt/crypto/md5.sls
endif
EXTRA_DIST += lib/weinholt/crypto/md5.sls
CLEANFILES += lib/weinholt/crypto/md5.fasl

lib/weinholt/crypto/openpgp.fasl: \
		lib/weinholt/crypto/openpgp.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/dsa.fasl \
		lib/weinholt/crypto/md5.fasl \
		lib/weinholt/crypto/rsa.fasl \
		lib/weinholt/crypto/sha-1.fasl \
		lib/weinholt/crypto/sha-2.fasl \
		lib/weinholt/text/base64.fasl \
		lib/weinholt/struct/pack.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_openpgp_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_openpgp_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_openpgp_fasl_DATA = lib/weinholt/crypto/openpgp.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_openpgp_sls_DATA = lib/weinholt/crypto/openpgp.sls
endif
EXTRA_DIST += lib/weinholt/crypto/openpgp.sls
CLEANFILES += lib/weinholt/crypto/openpgp.fasl

lib/weinholt/crypto/rsa.fasl: \
		lib/weinholt/crypto/rsa.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/entropy.fasl \
		lib/weinholt/crypto/math.fasl \
		lib/weinholt/struct/der.fasl \
		lib/weinholt/text/base64.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_rsa_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_rsa_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_rsa_fasl_DATA = lib/weinholt/crypto/rsa.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_rsa_sls_DATA = lib/weinholt/crypto/rsa.sls
endif
EXTRA_DIST += lib/weinholt/crypto/rsa.sls
CLEANFILES += lib/weinholt/crypto/rsa.fasl

lib/weinholt/crypto/sha-1.fasl: \
		lib/weinholt/crypto/sha-1.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_sha_1_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_sha_1_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_sha_1_fasl_DATA = lib/weinholt/crypto/sha-1.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_sha_1_sls_DATA = lib/weinholt/crypto/sha-1.sls
endif
EXTRA_DIST += lib/weinholt/crypto/sha-1.sls
CLEANFILES += lib/weinholt/crypto/sha-1.fasl

lib/weinholt/crypto/password.fasl: \
		lib/weinholt/crypto/password.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/des.fasl \
		lib/weinholt/crypto/md5.fasl \
		lib/weinholt/text/base64.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_password_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_password_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_password_fasl_DATA = lib/weinholt/crypto/password.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_password_sls_DATA = lib/weinholt/crypto/password.sls
endif
EXTRA_DIST += lib/weinholt/crypto/password.sls
CLEANFILES += lib/weinholt/crypto/password.fasl

lib/weinholt/crypto/ssh-public-key.fasl: \
		lib/weinholt/crypto/ssh-public-key.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/dsa.fasl \
		lib/weinholt/crypto/ec.fasl \
		lib/weinholt/crypto/ec/dsa.fasl \
		lib/weinholt/crypto/md5.fasl \
		lib/weinholt/crypto/rsa.fasl \
		lib/weinholt/struct/pack.fasl \
		lib/weinholt/text/base64.fasl \
		lib/weinholt/text/random-art.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_ssh_public_key_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_ssh_public_key_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_ssh_public_key_fasl_DATA = lib/weinholt/crypto/ssh-public-key.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_ssh_public_key_sls_DATA = lib/weinholt/crypto/ssh-public-key.sls
endif
EXTRA_DIST += lib/weinholt/crypto/ssh-public-key.sls
CLEANFILES += lib/weinholt/crypto/ssh-public-key.fasl

lib/weinholt/text/random-art.fasl: \
		lib/weinholt/text/random-art.sls \
		lib/weinholt/crypto/dsa.fasl \
		lib/weinholt/crypto/rsa.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_text_random_art_fasldir = $(bundledlibsdir)/weinholt/text
lib_weinholt_text_random_art_slsdir  = $(bundledlibsdir)/weinholt/text
nodist_lib_weinholt_text_random_art_fasl_DATA = lib/weinholt/text/random-art.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_text_random_art_sls_DATA = lib/weinholt/text/random-art.sls
endif
EXTRA_DIST += lib/weinholt/text/random-art.sls
CLEANFILES += lib/weinholt/text/random-art.fasl

lib/weinholt/crypto/uuid.fasl: \
		lib/weinholt/crypto/uuid.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/entropy.fasl \
		lib/weinholt/crypto/md5.fasl \
		lib/weinholt/crypto/sha-1.fasl \
		lib/weinholt/struct/pack.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_uuid_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_uuid_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_uuid_fasl_DATA = lib/weinholt/crypto/uuid.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_uuid_sls_DATA = lib/weinholt/crypto/uuid.sls
endif
EXTRA_DIST += lib/weinholt/crypto/uuid.sls
CLEANFILES += lib/weinholt/crypto/uuid.fasl

lib/weinholt/crypto/x509.fasl: \
		lib/weinholt/crypto/x509.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/dsa.fasl \
		lib/weinholt/crypto/md5.fasl \
		lib/weinholt/crypto/rsa.fasl \
		lib/weinholt/crypto/sha-1.fasl \
		lib/weinholt/crypto/sha-2.fasl \
		lib/weinholt/struct/der.fasl \
		lib/weinholt/struct/pack.fasl \
		lib/weinholt/text/base64.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_crypto_x509_fasldir = $(bundledlibsdir)/weinholt/crypto
lib_weinholt_crypto_x509_slsdir  = $(bundledlibsdir)/weinholt/crypto
nodist_lib_weinholt_crypto_x509_fasl_DATA = lib/weinholt/crypto/x509.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_crypto_x509_sls_DATA = lib/weinholt/crypto/x509.sls
endif
EXTRA_DIST += lib/weinholt/crypto/x509.sls
CLEANFILES += lib/weinholt/crypto/x509.fasl

lib/weinholt/disassembler/i8080.fasl: \
		lib/weinholt/disassembler/i8080.sls \
		lib/weinholt/disassembler/private.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_disassembler_i8080_fasldir = $(bundledlibsdir)/weinholt/disassembler
lib_weinholt_disassembler_i8080_slsdir  = $(bundledlibsdir)/weinholt/disassembler
nodist_lib_weinholt_disassembler_i8080_fasl_DATA = lib/weinholt/disassembler/i8080.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_disassembler_i8080_sls_DATA = lib/weinholt/disassembler/i8080.sls
endif
EXTRA_DIST += lib/weinholt/disassembler/i8080.sls
CLEANFILES += lib/weinholt/disassembler/i8080.fasl

lib/weinholt/disassembler/private.fasl: \
		lib/weinholt/disassembler/private.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_disassembler_private_fasldir = $(bundledlibsdir)/weinholt/disassembler
lib_weinholt_disassembler_private_slsdir  = $(bundledlibsdir)/weinholt/disassembler
nodist_lib_weinholt_disassembler_private_fasl_DATA = lib/weinholt/disassembler/private.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_disassembler_private_sls_DATA = lib/weinholt/disassembler/private.sls
endif
EXTRA_DIST += lib/weinholt/disassembler/private.sls
CLEANFILES += lib/weinholt/disassembler/private.fasl

lib/weinholt/disassembler/m68hc12.fasl: \
		lib/weinholt/disassembler/m68hc12.sls \
		lib/weinholt/disassembler/private.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_disassembler_m68hc12_fasldir = $(bundledlibsdir)/weinholt/disassembler
lib_weinholt_disassembler_m68hc12_slsdir  = $(bundledlibsdir)/weinholt/disassembler
nodist_lib_weinholt_disassembler_m68hc12_fasl_DATA = lib/weinholt/disassembler/m68hc12.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_disassembler_m68hc12_sls_DATA = lib/weinholt/disassembler/m68hc12.sls
endif
EXTRA_DIST += lib/weinholt/disassembler/m68hc12.sls
CLEANFILES += lib/weinholt/disassembler/m68hc12.fasl

lib/weinholt/disassembler/mips.fasl: \
		lib/weinholt/disassembler/mips.sls \
		lib/weinholt/disassembler/private.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_disassembler_mips_fasldir = $(bundledlibsdir)/weinholt/disassembler
lib_weinholt_disassembler_mips_slsdir  = $(bundledlibsdir)/weinholt/disassembler
nodist_lib_weinholt_disassembler_mips_fasl_DATA = lib/weinholt/disassembler/mips.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_disassembler_mips_sls_DATA = lib/weinholt/disassembler/mips.sls
endif
EXTRA_DIST += lib/weinholt/disassembler/mips.sls
CLEANFILES += lib/weinholt/disassembler/mips.fasl

lib/weinholt/disassembler/x86.fasl: \
		lib/weinholt/disassembler/x86.sls \
		lib/weinholt/disassembler/private.fasl \
		lib/weinholt/disassembler/x86-opcodes.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_disassembler_x86_fasldir = $(bundledlibsdir)/weinholt/disassembler
lib_weinholt_disassembler_x86_slsdir  = $(bundledlibsdir)/weinholt/disassembler
nodist_lib_weinholt_disassembler_x86_fasl_DATA = lib/weinholt/disassembler/x86.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_disassembler_x86_sls_DATA = lib/weinholt/disassembler/x86.sls
endif
EXTRA_DIST += lib/weinholt/disassembler/x86.sls
CLEANFILES += lib/weinholt/disassembler/x86.fasl

lib/weinholt/net/buffer.fasl: \
		lib/weinholt/net/buffer.sls \
		lib/weinholt/struct/pack.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_buffer_fasldir = $(bundledlibsdir)/weinholt/net
lib_weinholt_net_buffer_slsdir  = $(bundledlibsdir)/weinholt/net
nodist_lib_weinholt_net_buffer_fasl_DATA = lib/weinholt/net/buffer.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_buffer_sls_DATA = lib/weinholt/net/buffer.sls
endif
EXTRA_DIST += lib/weinholt/net/buffer.sls
CLEANFILES += lib/weinholt/net/buffer.fasl

lib/weinholt/net/dns/numbers.fasl: \
		lib/weinholt/net/dns/numbers.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_dns_numbers_fasldir = $(bundledlibsdir)/weinholt/net/dns
lib_weinholt_net_dns_numbers_slsdir  = $(bundledlibsdir)/weinholt/net/dns
nodist_lib_weinholt_net_dns_numbers_fasl_DATA = lib/weinholt/net/dns/numbers.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_dns_numbers_sls_DATA = lib/weinholt/net/dns/numbers.sls
endif
EXTRA_DIST += lib/weinholt/net/dns/numbers.sls
CLEANFILES += lib/weinholt/net/dns/numbers.fasl

lib/weinholt/net/dns/private.fasl: \
		lib/weinholt/net/dns/private.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/struct/pack.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_dns_private_fasldir = $(bundledlibsdir)/weinholt/net/dns
lib_weinholt_net_dns_private_slsdir  = $(bundledlibsdir)/weinholt/net/dns
nodist_lib_weinholt_net_dns_private_fasl_DATA = lib/weinholt/net/dns/private.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_dns_private_sls_DATA = lib/weinholt/net/dns/private.sls
endif
EXTRA_DIST += lib/weinholt/net/dns/private.sls
CLEANFILES += lib/weinholt/net/dns/private.fasl

lib/weinholt/net/dns.fasl: \
		lib/weinholt/net/dns.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/entropy.fasl \
		lib/weinholt/net/dns/numbers.fasl \
		lib/weinholt/net/dns/types.fasl \
		lib/weinholt/net/dns/private.fasl \
		lib/weinholt/struct/pack.fasl \
		lib/weinholt/text/punycode.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_dns_fasldir = $(bundledlibsdir)/weinholt/net
lib_weinholt_net_dns_slsdir  = $(bundledlibsdir)/weinholt/net
nodist_lib_weinholt_net_dns_fasl_DATA = lib/weinholt/net/dns.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_dns_sls_DATA = lib/weinholt/net/dns.sls
endif
EXTRA_DIST += lib/weinholt/net/dns.sls
CLEANFILES += lib/weinholt/net/dns.fasl

lib/weinholt/net/dns/types.fasl: \
		lib/weinholt/net/dns/types.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/net/dns/numbers.fasl \
		lib/weinholt/net/dns/private.fasl \
		lib/weinholt/struct/pack.fasl \
		lib/weinholt/text/base64.fasl \
		lib/weinholt/text/internet.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_dns_types_fasldir = $(bundledlibsdir)/weinholt/net/dns
lib_weinholt_net_dns_types_slsdir  = $(bundledlibsdir)/weinholt/net/dns
nodist_lib_weinholt_net_dns_types_fasl_DATA = lib/weinholt/net/dns/types.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_dns_types_sls_DATA = lib/weinholt/net/dns/types.sls
endif
EXTRA_DIST += lib/weinholt/net/dns/types.sls
CLEANFILES += lib/weinholt/net/dns/types.fasl

lib/weinholt/text/internet.fasl: \
		lib/weinholt/text/internet.sls \
		lib/weinholt/struct/pack.fasl \
		lib/weinholt/text/strings.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_text_internet_fasldir = $(bundledlibsdir)/weinholt/text
lib_weinholt_text_internet_slsdir  = $(bundledlibsdir)/weinholt/text
nodist_lib_weinholt_text_internet_fasl_DATA = lib/weinholt/text/internet.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_text_internet_sls_DATA = lib/weinholt/text/internet.sls
endif
EXTRA_DIST += lib/weinholt/text/internet.sls
CLEANFILES += lib/weinholt/text/internet.fasl

lib/weinholt/text/punycode.fasl: \
		lib/weinholt/text/punycode.sls \
		lib/weinholt/bytevectors.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_text_punycode_fasldir = $(bundledlibsdir)/weinholt/text
lib_weinholt_text_punycode_slsdir  = $(bundledlibsdir)/weinholt/text
nodist_lib_weinholt_text_punycode_fasl_DATA = lib/weinholt/text/punycode.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_text_punycode_sls_DATA = lib/weinholt/text/punycode.sls
endif
EXTRA_DIST += lib/weinholt/text/punycode.sls
CLEANFILES += lib/weinholt/text/punycode.fasl

lib/weinholt/net/irc/fish.fasl: \
		lib/weinholt/net/irc/fish.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/blowfish.fasl \
		lib/weinholt/crypto/dh.fasl \
		lib/weinholt/crypto/sha-2.fasl \
		lib/weinholt/struct/pack.fasl \
		lib/weinholt/text/base64.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_irc_fish_fasldir = $(bundledlibsdir)/weinholt/net/irc
lib_weinholt_net_irc_fish_slsdir  = $(bundledlibsdir)/weinholt/net/irc
nodist_lib_weinholt_net_irc_fish_fasl_DATA = lib/weinholt/net/irc/fish.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_irc_fish_sls_DATA = lib/weinholt/net/irc/fish.sls
endif
EXTRA_DIST += lib/weinholt/net/irc/fish.sls
CLEANFILES += lib/weinholt/net/irc/fish.fasl

lib/weinholt/net/irc.fasl: \
		lib/weinholt/net/irc.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/text/strings.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_irc_fasldir = $(bundledlibsdir)/weinholt/net
lib_weinholt_net_irc_slsdir  = $(bundledlibsdir)/weinholt/net
nodist_lib_weinholt_net_irc_fasl_DATA = lib/weinholt/net/irc.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_irc_sls_DATA = lib/weinholt/net/irc.sls
endif
EXTRA_DIST += lib/weinholt/net/irc.sls
CLEANFILES += lib/weinholt/net/irc.fasl

lib/weinholt/net/otr.fasl: \
		lib/weinholt/net/otr.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/aes.fasl \
		lib/weinholt/crypto/dsa.fasl \
		lib/weinholt/crypto/dh.fasl \
		lib/weinholt/crypto/entropy.fasl \
		lib/weinholt/crypto/math.fasl \
		lib/weinholt/crypto/sha-1.fasl \
		lib/weinholt/crypto/sha-2.fasl \
		lib/weinholt/struct/pack.fasl \
		lib/weinholt/text/base64.fasl \
		lib/weinholt/text/strings.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_otr_fasldir = $(bundledlibsdir)/weinholt/net
lib_weinholt_net_otr_slsdir  = $(bundledlibsdir)/weinholt/net
nodist_lib_weinholt_net_otr_fasl_DATA = lib/weinholt/net/otr.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_otr_sls_DATA = lib/weinholt/net/otr.sls
endif
EXTRA_DIST += lib/weinholt/net/otr.sls
CLEANFILES += lib/weinholt/net/otr.fasl

lib/weinholt/net/ssh/algorithms.fasl: \
		lib/weinholt/net/ssh/algorithms.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/aes.fasl \
		lib/weinholt/crypto/arcfour.fasl \
		lib/weinholt/crypto/blowfish.fasl \
		lib/weinholt/crypto/des.fasl \
		lib/weinholt/crypto/sha-1.fasl \
		lib/weinholt/crypto/md5.fasl \
		lib/weinholt/net/buffer.fasl \
		lib/weinholt/net/ssh/kexdh.fasl \
		lib/weinholt/net/ssh/kex-dh-gex.fasl \
		lib/weinholt/struct/pack.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_ssh_algorithms_fasldir = $(bundledlibsdir)/weinholt/net/ssh
lib_weinholt_net_ssh_algorithms_slsdir  = $(bundledlibsdir)/weinholt/net/ssh
nodist_lib_weinholt_net_ssh_algorithms_fasl_DATA = lib/weinholt/net/ssh/algorithms.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_ssh_algorithms_sls_DATA = lib/weinholt/net/ssh/algorithms.sls
endif
EXTRA_DIST += lib/weinholt/net/ssh/algorithms.sls
CLEANFILES += lib/weinholt/net/ssh/algorithms.fasl

lib/weinholt/net/ssh/kexdh.fasl: \
		lib/weinholt/net/ssh/kexdh.sls \
		lib/weinholt/crypto/dh.fasl \
		lib/weinholt/crypto/sha-1.fasl \
		lib/weinholt/crypto/ssh-public-key.fasl \
		lib/weinholt/net/ssh/private.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_ssh_kexdh_fasldir = $(bundledlibsdir)/weinholt/net/ssh
lib_weinholt_net_ssh_kexdh_slsdir  = $(bundledlibsdir)/weinholt/net/ssh
nodist_lib_weinholt_net_ssh_kexdh_fasl_DATA = lib/weinholt/net/ssh/kexdh.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_ssh_kexdh_sls_DATA = lib/weinholt/net/ssh/kexdh.sls
endif
EXTRA_DIST += lib/weinholt/net/ssh/kexdh.sls
CLEANFILES += lib/weinholt/net/ssh/kexdh.fasl

lib/weinholt/net/ssh/private.fasl: \
		lib/weinholt/net/ssh/private.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/dsa.fasl \
		lib/weinholt/crypto/ec.fasl \
		lib/weinholt/crypto/ec/dsa.fasl \
		lib/weinholt/crypto/rsa.fasl \
		lib/weinholt/crypto/sha-1.fasl \
		lib/weinholt/crypto/sha-2.fasl \
		lib/weinholt/crypto/ssh-public-key.fasl \
		lib/weinholt/net/buffer.fasl \
		lib/weinholt/struct/pack.fasl \
		lib/weinholt/text/strings.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_ssh_private_fasldir = $(bundledlibsdir)/weinholt/net/ssh
lib_weinholt_net_ssh_private_slsdir  = $(bundledlibsdir)/weinholt/net/ssh
nodist_lib_weinholt_net_ssh_private_fasl_DATA = lib/weinholt/net/ssh/private.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_ssh_private_sls_DATA = lib/weinholt/net/ssh/private.sls
endif
EXTRA_DIST += lib/weinholt/net/ssh/private.sls
CLEANFILES += lib/weinholt/net/ssh/private.fasl

lib/weinholt/net/ssh/kex-dh-gex.fasl: \
		lib/weinholt/net/ssh/kex-dh-gex.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/dh.fasl \
		lib/weinholt/crypto/entropy.fasl \
		lib/weinholt/crypto/math.fasl \
		lib/weinholt/crypto/sha-1.fasl \
		lib/weinholt/crypto/sha-2.fasl \
		lib/weinholt/crypto/ssh-public-key.fasl \
		lib/weinholt/net/ssh/private.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_ssh_kex_dh_gex_fasldir = $(bundledlibsdir)/weinholt/net/ssh
lib_weinholt_net_ssh_kex_dh_gex_slsdir  = $(bundledlibsdir)/weinholt/net/ssh
nodist_lib_weinholt_net_ssh_kex_dh_gex_fasl_DATA = lib/weinholt/net/ssh/kex-dh-gex.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_ssh_kex_dh_gex_sls_DATA = lib/weinholt/net/ssh/kex-dh-gex.sls
endif
EXTRA_DIST += lib/weinholt/net/ssh/kex-dh-gex.sls
CLEANFILES += lib/weinholt/net/ssh/kex-dh-gex.fasl

lib/weinholt/net/ssh/connection.fasl: \
		lib/weinholt/net/ssh/connection.sls \
		lib/weinholt/net/buffer.fasl \
		lib/weinholt/net/ssh/private.fasl \
		lib/weinholt/struct/pack.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_ssh_connection_fasldir = $(bundledlibsdir)/weinholt/net/ssh
lib_weinholt_net_ssh_connection_slsdir  = $(bundledlibsdir)/weinholt/net/ssh
nodist_lib_weinholt_net_ssh_connection_fasl_DATA = lib/weinholt/net/ssh/connection.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_ssh_connection_sls_DATA = lib/weinholt/net/ssh/connection.sls
endif
EXTRA_DIST += lib/weinholt/net/ssh/connection.sls
CLEANFILES += lib/weinholt/net/ssh/connection.fasl

lib/weinholt/net/ssh.fasl: \
		lib/weinholt/net/ssh.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/entropy.fasl \
		lib/weinholt/crypto/ssh-public-key.fasl \
		lib/weinholt/net/buffer.fasl \
		lib/weinholt/net/ssh/algorithms.fasl \
		lib/weinholt/net/ssh/connection.fasl \
		lib/weinholt/net/ssh/kexdh.fasl \
		lib/weinholt/net/ssh/private.fasl \
		lib/weinholt/net/ssh/transport.fasl \
		lib/weinholt/net/ssh/userauth.fasl \
		lib/weinholt/struct/pack.fasl \
		lib/weinholt/text/hexdump.fasl \
		lib/weinholt/text/strings.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_ssh_fasldir = $(bundledlibsdir)/weinholt/net
lib_weinholt_net_ssh_slsdir  = $(bundledlibsdir)/weinholt/net
nodist_lib_weinholt_net_ssh_fasl_DATA = lib/weinholt/net/ssh.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_ssh_sls_DATA = lib/weinholt/net/ssh.sls
endif
EXTRA_DIST += lib/weinholt/net/ssh.sls
CLEANFILES += lib/weinholt/net/ssh.fasl

lib/weinholt/net/ssh/transport.fasl: \
		lib/weinholt/net/ssh/transport.sls \
		lib/weinholt/net/buffer.fasl \
		lib/weinholt/net/ssh/private.fasl \
		lib/weinholt/struct/pack.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_ssh_transport_fasldir = $(bundledlibsdir)/weinholt/net/ssh
lib_weinholt_net_ssh_transport_slsdir  = $(bundledlibsdir)/weinholt/net/ssh
nodist_lib_weinholt_net_ssh_transport_fasl_DATA = lib/weinholt/net/ssh/transport.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_ssh_transport_sls_DATA = lib/weinholt/net/ssh/transport.sls
endif
EXTRA_DIST += lib/weinholt/net/ssh/transport.sls
CLEANFILES += lib/weinholt/net/ssh/transport.fasl

lib/weinholt/net/ssh/userauth.fasl: \
		lib/weinholt/net/ssh/userauth.sls \
		lib/weinholt/crypto/dsa.fasl \
		lib/weinholt/crypto/rsa.fasl \
		lib/weinholt/crypto/ssh-public-key.fasl \
		lib/weinholt/net/buffer.fasl \
		lib/weinholt/net/ssh/private.fasl \
		lib/weinholt/struct/pack.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_ssh_userauth_fasldir = $(bundledlibsdir)/weinholt/net/ssh
lib_weinholt_net_ssh_userauth_slsdir  = $(bundledlibsdir)/weinholt/net/ssh
nodist_lib_weinholt_net_ssh_userauth_fasl_DATA = lib/weinholt/net/ssh/userauth.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_ssh_userauth_sls_DATA = lib/weinholt/net/ssh/userauth.sls
endif
EXTRA_DIST += lib/weinholt/net/ssh/userauth.sls
CLEANFILES += lib/weinholt/net/ssh/userauth.fasl

lib/weinholt/text/hexdump.fasl: \
		lib/weinholt/text/hexdump.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_text_hexdump_fasldir = $(bundledlibsdir)/weinholt/text
lib_weinholt_text_hexdump_slsdir  = $(bundledlibsdir)/weinholt/text
nodist_lib_weinholt_text_hexdump_fasl_DATA = lib/weinholt/text/hexdump.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_text_hexdump_sls_DATA = lib/weinholt/text/hexdump.sls
endif
EXTRA_DIST += lib/weinholt/text/hexdump.sls
CLEANFILES += lib/weinholt/text/hexdump.fasl

lib/weinholt/net/tcp.fasl: \
		lib/weinholt/net/tcp.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_tcp_fasldir = $(bundledlibsdir)/weinholt/net
lib_weinholt_net_tcp_vicare_slsdir  = $(bundledlibsdir)/weinholt/net
nodist_lib_weinholt_net_tcp_fasl_DATA = lib/weinholt/net/tcp.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_tcp_vicare_sls_DATA = lib/weinholt/net/tcp.vicare.sls
endif
EXTRA_DIST += lib/weinholt/net/tcp.vicare.sls
CLEANFILES += lib/weinholt/net/tcp.fasl

lib/weinholt/net/tls/algorithms.fasl: \
		lib/weinholt/net/tls/algorithms.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/aes.fasl \
		lib/weinholt/crypto/arcfour.fasl \
		lib/weinholt/crypto/des.fasl \
		lib/weinholt/crypto/dsa.fasl \
		lib/weinholt/crypto/md5.fasl \
		lib/weinholt/crypto/rsa.fasl \
		lib/weinholt/crypto/sha-1.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_tls_algorithms_fasldir = $(bundledlibsdir)/weinholt/net/tls
lib_weinholt_net_tls_algorithms_slsdir  = $(bundledlibsdir)/weinholt/net/tls
nodist_lib_weinholt_net_tls_algorithms_fasl_DATA = lib/weinholt/net/tls/algorithms.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_tls_algorithms_sls_DATA = lib/weinholt/net/tls/algorithms.sls
endif
EXTRA_DIST += lib/weinholt/net/tls/algorithms.sls
CLEANFILES += lib/weinholt/net/tls/algorithms.fasl

lib/weinholt/net/tls/simple.fasl: \
		lib/weinholt/net/tls/simple.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/net/tcp.fasl \
		lib/weinholt/net/tls.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_tls_simple_fasldir = $(bundledlibsdir)/weinholt/net/tls
lib_weinholt_net_tls_simple_slsdir  = $(bundledlibsdir)/weinholt/net/tls
nodist_lib_weinholt_net_tls_simple_fasl_DATA = lib/weinholt/net/tls/simple.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_tls_simple_sls_DATA = lib/weinholt/net/tls/simple.sls
endif
EXTRA_DIST += lib/weinholt/net/tls/simple.sls
CLEANFILES += lib/weinholt/net/tls/simple.fasl

lib/weinholt/net/tls.fasl: \
		lib/weinholt/net/tls.sls \
		lib/weinholt/bytevectors.fasl \
		lib/weinholt/crypto/dh.fasl \
		lib/weinholt/crypto/dsa.fasl \
		lib/weinholt/crypto/entropy.fasl \
		lib/weinholt/crypto/md5.fasl \
		lib/weinholt/crypto/rsa.fasl \
		lib/weinholt/crypto/sha-1.fasl \
		lib/weinholt/crypto/x509.fasl \
		lib/weinholt/net/buffer.fasl \
		lib/weinholt/net/tls/algorithms.fasl \
		lib/weinholt/struct/pack.fasl \
		lib/weinholt/text/base64.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_weinholt_net_tls_fasldir = $(bundledlibsdir)/weinholt/net
lib_weinholt_net_tls_slsdir  = $(bundledlibsdir)/weinholt/net
nodist_lib_weinholt_net_tls_fasl_DATA = lib/weinholt/net/tls.fasl
if WANT_INSTALL_SOURCES
dist_lib_weinholt_net_tls_sls_DATA = lib/weinholt/net/tls.sls
endif
EXTRA_DIST += lib/weinholt/net/tls.sls
CLEANFILES += lib/weinholt/net/tls.fasl


### end of file
# Local Variables:
# mode: makefile-automake
# End:
