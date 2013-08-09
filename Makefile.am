## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS		= -I autotools
AUTOMAKE_OPTIONS	= foreign
SUBDIRS			= documentation tests
EXTRA_DIST		= INSTALL configure.sh
dist_doc_DATA		= README NEWS LICENSE.txt

vicare_test_RULES	= test tests demo demos

.PHONY: $(vicare_test_RULES)

$(vicare_test_RULES):
	cd tests ; $(MAKE) $(@)

## --------------------------------------------------------------------

libvicaredir	= $(libdir)/vicare-scheme
libvicare2dir	= $(libdir)/vicare-scheme

dist_pkglibexec_SCRIPTS		= compile-all.sps

# These files are hand-written.
nobase_dist_libvicare_DATA = \
	weinholt/archive/tar.sls				\
	weinholt/assembler/elf.sls				\
	weinholt/assembler/x86-misc.sls				\
	weinholt/assembler/x86-operands.sls			\
	weinholt/assembler/x86.sls				\
	weinholt/binfmt/elf.sls					\
	weinholt/bytevectors.sls				\
	weinholt/compression/adler-32.sls			\
	weinholt/compression/bitstream.sls			\
	weinholt/compression/gzip.sls				\
	weinholt/compression/huffman.sls			\
	weinholt/compression/inflate.sls			\
	weinholt/compression/lzma2.sls				\
	weinholt/compression/lzma.sls				\
	weinholt/compression/sliding-buffer.sls			\
	weinholt/compression/xz.sls				\
	weinholt/compression/zip/extra.ikarus.sls		\
	weinholt/compression/zip/extra.sls			\
	weinholt/compression/zip/extra.ypsilon.sls		\
	weinholt/compression/zip.sls				\
	weinholt/compression/zlib.sls				\
	weinholt/crypto/aes/private.sls				\
	weinholt/crypto/aes.sls					\
	weinholt/crypto/arcfour.sls				\
	weinholt/crypto/blowfish.sls				\
	weinholt/crypto/crc.sls					\
	weinholt/crypto/des.sls					\
	weinholt/crypto/dh.sls					\
	weinholt/crypto/dsa.sls					\
	weinholt/crypto/ec/dsa.sls				\
	weinholt/crypto/ec.sls					\
	weinholt/crypto/entropy.sls				\
	weinholt/crypto/hmac.sls				\
	weinholt/crypto/math.sls				\
	weinholt/crypto/md5.sls					\
	weinholt/crypto/openpgp.sls				\
	weinholt/crypto/password.sls				\
	weinholt/crypto/rsa.sls					\
	weinholt/crypto/sha-1.sls				\
	weinholt/crypto/sha-2.sls				\
	weinholt/crypto/ssh-public-key.sls			\
	weinholt/crypto/uuid.sls				\
	weinholt/crypto/x509.sls				\
	weinholt/disassembler/i8080.sls				\
	weinholt/disassembler/m68hc12.sls			\
	weinholt/disassembler/mips.sls				\
	weinholt/disassembler/private.sls			\
	weinholt/disassembler/x86-opcodes.sls			\
	weinholt/disassembler/x86.sls				\
	weinholt/net/buffer.sls					\
	weinholt/net/dns/numbers.sls				\
	weinholt/net/dns/private.sls				\
	weinholt/net/dns.sls					\
	weinholt/net/dns/types.sls				\
	weinholt/net/irc/fish.sls				\
	weinholt/net/irc.sls					\
	weinholt/net/otr.sls					\
	weinholt/net/ssh/algorithms.sls				\
	weinholt/net/ssh/connection.sls				\
	weinholt/net/ssh/kex-dh-gex.sls				\
	weinholt/net/ssh/kexdh.sls				\
	weinholt/net/ssh/private.sls				\
	weinholt/net/ssh.sls					\
	weinholt/net/ssh/transport.sls				\
	weinholt/net/ssh/userauth.sls				\
	weinholt/net/tcp.chezscheme.sls				\
	weinholt/net/tcp.guile.sls				\
	weinholt/net/tcp.ikarus.sls				\
	weinholt/net/tcp.ironscheme.sls				\
	weinholt/net/tcp.larceny.sls				\
	weinholt/net/tcp.mosh.sls				\
	weinholt/net/tcp.mzscheme.sls				\
	weinholt/net/tcp.sagittarius.sls			\
	weinholt/net/tcp.sls					\
	weinholt/net/tcp.vicare.sls				\
	weinholt/net/tcp.ypsilon.sls				\
	weinholt/net/tls/algorithms.sls				\
	weinholt/net/tls/simple.sls				\
	weinholt/net/tls.sls					\
	weinholt/struct/der.sls					\
	weinholt/struct/pack-aux.sls				\
	weinholt/struct/pack.sls				\
	weinholt/text/base64.sls				\
	weinholt/text/hexdump.sls				\
	weinholt/text/internet.sls				\
	weinholt/text/punycode.sls				\
	weinholt/text/random-art.sls				\
	weinholt/text/strings.sls

#page
#### generation of fasl files under the build directory

VICARE_FASL_DIRECTORY		= $(builddir)/fasl
VICARE_COMPILE_LIBRARY_PATH	= $(builddir):$(srcdir):$(VICARE_LIBRARY_PATH)
VICARE_COMPILE_FASL_PATH	= $(VICARE_FASL_DIRECTORY):$(VICARE_FASL_PATH)

# The variable "VFLAGS" is for the user  to be set on the command line
# of "make" or "configure", for example:
#
#   make VFLAGS="-g -O2 --print-loaded-libraries"
#
VFLAGS		= @VFLAGS@
VICARE		= @VICARE@
VICARE_COMPILE_FLAGS	= --fasl-directory $(VICARE_FASL_DIRECTORY) $(VFLAGS)
VICARE_COMPILE_ENV	= VICARE_LIBRARY_PATH=$(VICARE_COMPILE_LIBRARY_PATH)	\
			  VICARE_FASL_PATH=$(VICARE_COMPILE_FASL_PATH)
VICARE_COMPILE_RUN	= $(VICARE_COMPILE_ENV) $(VICARE) $(VICARE_COMPILE_FLAGS)

VICARE_COMPILE_INST_FLAGS = --print-loaded-libraries
VICARE_COMPILE_INST_ENV	= VICARE_LIBRARY_PATH=$(VICARE_LIBRARY_PATH)
VICARE_COMPILE_INST_RUN	= $(VICARE_COMPILE_INST_ENV) $(VICARE) $(VICARE_COMPILE_INST_FLAGS)

VICARE_COMPILE_OPTIONS	= --compile-dependencies $(srcdir)/compile-all.sps

.PHONY : vfasl vfasl-installed

vfasl:
	test -d $(VICARE_FASL_DIRECTORY) || $(MKDIR_P) $(VICARE_FASL_DIRECTORY)
	$(VICARE_COMPILE_RUN) $(VICARE_COMPILE_OPTIONS)

vfasl-installed:
	$(VICARE_COMPILE_INST_RUN) $(VICARE_COMPILE_OPTIONS)

clean-local:
	-rm -rf $(VICARE_FASL_DIRECTORY)

### end of file
