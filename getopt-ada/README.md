# getopt-ada — POSIX getopt(3) for Ada

[![builds.sr.ht status](https://builds.sr.ht/~nytpu/getopt-ada.svg)](https://builds.sr.ht/~nytpu/getopt-ada?)
[![license: MPL-2.0](https://img.shields.io/badge/license-MPL--2.0-informational.svg)](LICENSE)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/getopt.json)](https://alire.ada.dev/crates/getopt.html)

A package to implement a
[POSIX-compliant getopt](https://pubs.opengroup.org/onlinepubs/9699919799/functions/getopt.html)
for Ada.

If you need substantially complex option parsing then it is recommended to
instead use
[`GNAT.Command_Line`](https://gcc.gnu.org/onlinedocs/gnat_rm/GNAT-Command_005fLine-g-comlin-ads.html#GNAT-Command_005fLine-g-comlin-ads),
however in general this package is preferred by the author as it is truly
POSIX-compliant and is portable Ada 2012 rather than an implementation
extension.

It should work on Windows and other non-POSIX systems (anything that implements
`Ada.Command_Line`), although it will still use the non-standard "-" for
options rather than the flag character usual for the system.


## Documentation

The specification is well-documented:
[`getopts.ads`](src/getopts.ads)

Also see the example program:
[`src/example/example.adb`](src/example/example.adb)


## Using

Use [Alire](https://alire.ada.dev/):
`alr with getopt`, then `with "getopt.gpr";` in your GPRBuild file.

Add this repo as a subtree or submodule in your project and include
`with "path/to/getopt.gpr";` in your GPRbuild file.

Alternately you could install the library & specification in a system library
directory and include it from there.


## Compiling
### Requirements

- [GPRbuild](https://github.com/AdaCore/gprbuild)
- An Ada 2012 compiler and standard library
- POSIX-compatible make(1) (optional).
  Most makes (including GNU Make and BSD Make) support the POSIX standard.


### Building

    git clone https://git.sr.ht/~nytpu/getopt-ada && cd getopt-ada
    make
    make example  # builds example program
    sudo make install

You may also use plain `gprbuild` and `gprinstall` commands rather than the
convenience makefile.


## Contributing

The upstream URL of this project is
<https://git.sr.ht/~nytpu/getopt-ada>.
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

Copyright (C) 2021–2022 nytpu <alex [at] nytpu.com>.

Licensed under the terms of the Mozilla Public License version 2.0.
You can view a copy of the MPL in
[LICENSE](LICENSE)
or at
<https://www.mozilla.org/en-US/MPL/2.0/>.
