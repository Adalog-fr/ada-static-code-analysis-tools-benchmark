# TLSAda — Ada bindings to libtls

[![builds.sr.ht status](https://builds.sr.ht/~nytpu/tlsada.svg)](https://builds.sr.ht/~nytpu/tlsada?)
[![license: MPL-2.0](https://img.shields.io/badge/license-MPL--2.0-informational.svg)](LICENSE)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/tlsada.json)](https://alire.ada.dev/crates/tlsada.html)

Thick Ada bindings to [libtls](https://man.openbsd.org/tls_init.3), "a new TLS
library, designed to make it easier to write foolproof applications".  It
supports any libtls installation, both the libtls provided by
[LibreSSL](https://www.libressl.org/) and the OpenSSL port
[LibreTLS](https://git.causal.agency/libretls/about/).

These bindings mirror the
[philosophy of libtls](https://www.openbsd.org/papers/linuxconfau2017-libtls/):

- As easy to use as possible.
- Safe and secure **by default**.
- Consistent, obvious and well documented.


## Documentation

See the package specifications, which have copious doc comments:

- [`TLS`](src/tls.ads)
  - [`TLS.Configure`](src/tls-configure.ads)
  - [`TLS.Contexts`](src/tls-contexts.ads)
    - [`TLS.Contexts.Client`](src/tls-contexts-client.ads)
    - [`TLS.Contexts.Server`](src/tls-contexts-server.ads)

Also see the example programs:

- [Client Example](src/example/client_example.adb)
- [Server Example](src/example/server_example.adb)

As well as the thick library, thin bindings to the entire libtls library are
also available: [`libTLS_Bindings`](src/generated/libtls_bindings.ads)


## Using

Use [Alire](https://alire.ada.dev/):
`alr with tlsada`, then `with "tlsada.gpr";` in your GPRBuild file.

Alternately, add this repo as a subtree or submodule in your project and
include `with "path/to/tlsada.gpr";` in your GPRbuild file.

You may also install the library & specification in a system library directory
and include it from there.


## Compiling
### Requirements

- [GPRbuild](https://github.com/AdaCore/gprbuild)
- An Ada 2012 compiler and standard library
- libTLS provider such as [LibreSSL](https://www.libressl.org/) or
  [LibreTLS](https://git.causal.agency/libretls/about/).
- An implementation of `GNAT.Sockets`
- POSIX-compatible make(1) (optional).
  Most makes (including GNU Make and BSD Make) support the POSIX standard.


### Building

    git clone https://git.sr.ht/~nytpu/tlsada && cd tlsada
    make
    make example # builds example programs
    sudo make install

You may also use plain `gprbuild` and `gprinstall` commands rather than the
convenience makefile.


## Contributing

The upstream URL of this project is
<https://git.sr.ht/~nytpu/tlsada>.
Send suggestions, bugs, patches, and other contributions to
<~nytpu/public-inbox@lists.sr.ht>.
For help sending a patch through email, see
<https://git-send-email.io>.
You can browse the list archives at
<https://lists.sr.ht/~nytpu/public-inbox>.

If you have a very large set of changes, please use
[`git request-pull`](https://git-scm.com/docs/git-request-pull)
rather than sending a large patchset.


## Copyright

Copyright (C) 2022 nytpu <alex [at] nytpu.com>.

Licensed under the terms of the Mozilla Public License version 2.0.
You can view a copy of the MPL in
[LICENSE](LICENSE)
or at
<https://www.mozilla.org/en-US/MPL/2.0/>.
