## Process this file with automake to produce Makefile.in

VICARE			= @VICARE@
testdir			= $(srcdir)
libvicaredir		= $(libdir)/vicare-scheme
VICARE_FASL_DIRECTORY	= $(builddir)/../fasl

VICARE_TEST_SEARCH_PATH	= $(srcdir):$(srcdir)/..:$(builddir)/..:$(VICARE_LIBRARY_PATH)
VICARE_TEST_FASL_PATH	= $(VICARE_FASL_DIRECTORY):$(VICARE_FASL_PATH)

# The variable "VFLAGS" is for the user  to be set on the command line
# of "make" or "configure", for example:
#
#   make VFLAGS="-g -O2 --print-loaded-libraries"
#
VFLAGS		= @VFLAGS@

# Enable timing of test files  execution.  We cannot include this in the
# separator  because  it needs  to  be  placed  between the  environment
# variables and the actual command line.
if WANT_TIME_TESTS
VICARE_TIME_TESTS	= $(TIME_PROGRAM) -p
endif

VICARE_TEST_FLAGS	= $(VFLAGS)
VICARE_TEST_ENV		= \
	VICARE_LIBRARY_PATH=$(VICARE_TEST_SEARCH_PATH); export VICARE_LIBRARY_PATH;	\
	VICARE_FASL_PATH=$(VICARE_TEST_FASL_PATH); export VICARE_FASL_PATH;		\
	VICARE_SRC_DIR=$(srcdir); export VICARE_SRC_DIR					\
	VICARE_BUILDDIR=$(builddir); export VICARE_BUILDDIR;				\
	CHECK_TEST_NAME=$(name); export CHECK_TEST_NAME;
VICARE_TEST_RUN		= $(VICARE_TEST_ENV) $(VICARE_TIME_TESTS) $(GDB) $(VICARE) $(VICARE_TEST_FLAGS)

VICARE_SELECTED_TESTS	= $(testdir)/*$(file)*.sps
VICARE_SELECTED_DEMOS	= $(testdir)/demo-*$(file)*.sps

.PHONY: test tests demo demos

test tests:
	@echo
	@for f in $(VICARE_SELECTED_TESTS); do $(VICARE_TEST_RUN) $$f; done
	@echo

demo demos:
	@echo
	@for f in $(VICARE_SELECTED_DEMOS); do $(VICARE_TEST_RUN) $$f; done
	@echo

#page
#### extra files to be distributed

EXTRA_DIST = \
	aes.sps						\
	arcfour.sps					\
	base.sps					\
	blowfish.sps					\
	cpu12disasm.sps					\
	crc.sps						\
	der.sps						\
	des.sps						\
	dh.sps						\
	dns.sps						\
	dsa.sps						\
	ecdsa.sps					\
	gzip.sps					\
	hmac.sps					\
	huffman.sps					\
	ipaddrs.sps					\
	irc.sps						\
	md5.sps						\
	otr.sps						\
	pack.sps					\
	password.sps					\
	punycode.sps					\
	sha-1.sps					\
	sha-256.sps					\
	sha-512.sps					\
	ssh-kex.sps					\
	uuid.sps					\
	x509.sps					\
	x86asm.sps					\
	x86disasm.sps					\
	zlib.sps

#page
#### interface to "make check"

# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#
# We must make sure that this ends with a semicolon!!!
AM_TESTS_ENVIRONMENT	= VICARE_CHECK_QUIET=yes; export VICARE_CHECK_QUIET; $(VICARE_TEST_ENV)
TEST_EXTENSIONS		= .sps
SPS_LOG_COMPILER	= $(VICARE)
AM_SPS_LOG_FLAGS	= $(VFLAGS)

TESTS			= \
	aes.sps						\
	arcfour.sps					\
	base.sps					\
	blowfish.sps					\
	cpu12disasm.sps					\
	crc.sps						\
	der.sps						\
	des.sps						\
	dh.sps						\
	dns.sps						\
	dsa.sps						\
	ecdsa.sps					\
	gzip.sps					\
	hmac.sps					\
	huffman.sps					\
	ipaddrs.sps					\
	irc.sps						\
	md5.sps						\
	otr.sps						\
	pack.sps					\
	password.sps					\
	punycode.sps					\
	sha-1.sps					\
	sha-256.sps					\
	sha-512.sps					\
	ssh-kex.sps					\
	uuid.sps					\
	x509.sps					\
	x86asm.sps					\
	x86disasm.sps					\
	zlib.sps

XFAIL_TESTS	= x86asm.sps otr.sps

#page
#### interface to "make installcheck"

VICARE_INSTCHECK_LIBDIR		= $(libvicaredir)
VICARE_INSTCHECK_SEARCH_PATH	= $(DESTDIR)$(srcdir):$(DESTDIR)$(VICARE_INSTCHECK_LIBDIR):$(VICARE_LIBRARY_PATH)
VICARE_INSTCHECK_FASL_PATH	= $(VICARE_FASL_PATH)

VICARE_INSTCHECK_FLAGS	= --print-loaded-libraries $(VFLAGS)
VICARE_INSTCHECK_ENV	= VICARE_LIBRARY_PATH=$(VICARE_INSTCHECK_SEARCH_PATH)	\
			  VICARE_FASL_PATH=$(VICARE_INSTCHECK_FASL_PATH)
VICARE_INSTCHECK_RUN	= $(VICARE_INSTCHECK_ENV) $(VICARE) $(VICARE_INSTCHECK_FLAGS)

installcheck-local:
	@for f in $(TESTS); do $(VICARE_INSTCHECK_RUN) $(srcdir)/$$f; done

### end of file
