# uri-mime-ada — Ada 2012 URI parsing and manipulation

[![builds.sr.ht status](https://builds.sr.ht/~nytpu/uri-mime-ada.svg)](https://builds.sr.ht/~nytpu/uri-mime-ada?)
[![license: MPL-2.0](https://img.shields.io/badge/license-MPL--2.0-informational.svg)](LICENSE)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/uri_mime.json)](https://alire.ada.dev/crates/uri_mime.html)

A URI manipulation library compliant with
[RFC 3986](https://datatracker.ietf.org/doc/html/rfc3986).
Supports parsing raw URIs, resolving relative URIs, path normalization &
manipulation, and percent encoding.

Also supports a *reasonable subset* of MIME Types according to
[RFC 2045 § 5](https://datatracker.ietf.org/doc/html/rfc2045#section-5).


## Documentation

The specifications should be well-documented:
- [`URI`](src/uri.ads)
- [`MIME`](src/mime.ads)


## Using

Use [Alire](https://alire.ada.dev/):
`alr with uri_mime`, then `with "uri_mime.gpr";` in your GPRBuild file.

Alternately, add this repo as a subtree or submodule in your project and
include `with "path/to/uri_mime.gpr";` in your GPRbuild file.

You may also install the library & specification in a system library
directory and include it from there.


## Compiling
### Requirements

- [GPRbuild](https://github.com/AdaCore/gprbuild)
- An Ada 2012 compiler and standard library
- [Ahven](http://ahven.stronglytyped.org/) (For testing, optional)


### Building

    git clone https://git.sr.ht/~nytpu/uri-mime-ada && cd uri-mime-ada
    make test  # optional
    make
    sudo make install

You may also use plain `gprbuild`, `gprinstall` commands rather than the
convenience makefile.


## Contributing

The upstream URL of this project is <https://git.sr.ht/~nytpu/uri-mime-ada>.
Send suggestions, bugs, patches, and other contributions to
<~nytpu/public-inbox@lists.sr.ht>.  For help sending a patch through email, see
<https://git-send-email.io>.  You can browse the list archives at
<https://lists.sr.ht/~nytpu/public-inbox>.

If you have a very large set of changes, please use
[`git request-pull`](https://git-scm.com/docs/git-request-pull) rather than
sending a large patchset.


## Copyright

Copyright (C) 2022 nytpu <alex [at] nytpu.com>.

Licensed under the terms of the Mozilla Public License version 2.0.  You can
view a copy of the MPL in [LICENSE](LICENSE) or at
<https://www.mozilla.org/en-US/MPL/2.0/>.
