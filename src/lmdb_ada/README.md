# lmdb-ada — Ada 2012 Bindings to the Lightning Memory-Mapped Database

[![builds.sr.ht status](https://builds.sr.ht/~nytpu/lmdb-ada.svg)](https://builds.sr.ht/~nytpu/lmdb-ada?)
[![license: BSD-2-Clause AND OLDAP-2.8+](https://img.shields.io/badge/license-BSD--2--Clause%20AND%20OLDAP--2.8%2B-informational)](#copyright)

Thick Ada bindings to the
[Lightning Memory-Mapped Database](https://www.symas.com/lmdb)
(LMDB)
library,
a very high-performance and resilient embedded key-value store.

Note:
The bindings are currently complete,
however there are some functions that are not yet bound to,
mostly functions intended for extremely-high-performance applications that
custom-tune low-level database parameters for their specific data.
All the core functionality of the library *is* implemented in these bindings,
and it is suitable for all but these atypical use-cases.
Contributions implementing bindings to these functions are welcome if you find
them necessary.


## Documentation

See the package specifications, which have copious doc comments:
- [`LMDB`](src/lmdb.ads)
  - [`LMDB.Utils`](src/lmdb-utils.ads)
    (helper functions for converting to/from `Stream_Element_Array`s)

You may also want to look at the
[example program](src/example/example.adb).

As well as the thick library,
thin bindings to the entire LMDB library are also available:
[`lmdb_h`](src/lmdb_h.ads)


## Using

Use [Alire](https://alire.ada.dev/):
`alr with lmdb_ada`, then `with "lmdb.gpr";` in your GPRBuild file.

Alternately, add this repo as a subtree or submodule in your project and
include `with "path/to/lmdb.gpr";` in your GPRbuild file.

You may also install the library & specification in a system library directory
and include it from there.


## Compiling
### Requirements

- [GPRbuild](https://github.com/AdaCore/gprbuild)
- An Ada 2012 compiler and standard library
- A C compiler and standard library.
- [GNAT Components Collection (GNATCOLL), Core Libraries](https://github.com/AdaCore/gnatcoll-core)
- POSIX-compatible make(1) (optional).
  Most makes (including GNU Make and BSD Make) support the POSIX standard.


### Building

    git clone https://git.sr.ht/~nytpu/lmdb-ada && cd lmdb-ada
    make
    make example  # builds example program
    sudo make install

You may also use plain `gprbuild` and `gprinstall` commands rather than the
convenience makefile.


## Contributing

The upstream URL of this project is
<https://git.sr.ht/~nytpu/lmdb-ada>.
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

Copyright (c) 2022 nytpu <alex [at] nytpu.com>

Licensed under the terms of the BSD 2-Clause license.  You can view a copy of
the licenses in [`LICENSE`](LICENSE).

LMDB is Copyright 2011-2021 Howard Chu, Symas Corp.

LMDB is licensed under the OpenLDAP Public License.
You can view a copy of the copyright notice and license in
[`src/liblmdb/COPYRIGHT`](src/liblmdb/COPYRIGHT)
and
[`src/liblmdb/LICENSE`](src/liblmdb/LICENSE),
respectively.
