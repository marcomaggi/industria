# Industria

Industria is a collection of portable R6RS Scheme libraries for
cryptography (AES, HMAC, SHA-1, SHA-2, RSA, DSA, ECDSA, etc.), zlib/xz
decompression, Off-The-Record messaging, bytevector pack/unpack
syntax, TLS connections via custom binary ports, Secure Shell, and a
few more things.

The assembler, disassembler and binary format libraries have been
moved to the [machine-code](https://github.com/weinholt/machine-code)
project.

# Current status

This project is being split into smaller projects.
See [issue #4](https://github.com/weinholt/industria/issues/4).

# Documentation

The latest [released manual is available online](https://weinholt.se/industria/manual/).

The sources for the manual are available in Texinfo format in the
documentation directory. Use these commands from that directory to
build the manual:

```bash
makeinfo industria.texinfo                    # info format
makeinfo --plaintext industria.texinfo        # text format
makeinfo --no-split --html industria.texinfo  # html format
texi2pdf industria.texinfo                    # pdf format
```
