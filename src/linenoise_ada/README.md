# linenoise-ada — Ada 2012 Bindings to the Linenoise Library

[![builds.sr.ht status](https://builds.sr.ht/~nytpu/linenoise-ada.svg)](https://builds.sr.ht/~nytpu/linenoise-ada?)
[![license: MPL-2.0](https://img.shields.io/badge/license-MPL--2.0-informational.svg)](LICENSE)

Medium-thickness Ada bindings to the
[Linenoise](https://github.com/yhirose/linenoise)
line-editing library.

The Linenoise version vendored with this library is patched to support UTF-8
input as well as US-ASCII.


## Documentation

The specification should be well-documented:
[`linenoise.ads`](src/linenoise.ads).

You may also want to look at the example program:
[`example/`](src/example/).


## Using

Use [Alire](https://alire.ada.dev/):
`alr with linenoise_ada`, then `with "linenoise.gpr";` in your GPRBuild file.

Alternately, add this repo as a subtree or submodule in your project and
include `with "path/to/linenoise.gpr";` in your GPRbuild file.

You may also install the library & specification in a system library directory
and include it from there.


## Compiling
### Requirements

- [GPRbuild](https://github.com/AdaCore/gprbuild)
- An Ada 2012 compiler and standard library
- A C compiler and standard library.
- POSIX-compatible make(1) (optional).
  Most makes (including GNU Make and BSD Make) support the POSIX standard.


### Building

    git clone https://git.sr.ht/~nytpu/linenoise-ada && cd linenoise-ada
    make
    make example  # builds example program
    sudo make install

You may also use plain `gprbuild` and `gprinstall` commands rather than the
convenience makefile.


## Contributing

The upstream URL of this project is
<https://git.sr.ht/~nytpu/linenoise-ada>.
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

Copyright (c) 2010-2014, Salvatore Sanfilippo <antirez at gmail dot com>

Copyright (c) 2010-2013, Pieter Noordhuis <pcnoordhuis at gmail dot com>

Copyright (c) 2022 yhirose

Copyright (c) 2022 nytpu <alex [at] nytpu.com>

Licensed under the terms of the BSD 2-Clause license.
You can view a copy of the license in [`LICENSE`](LICENSE).

This repository vendors sources from this repository:
<https://github.com/yhirose/linenoise>.
