# DEF Engineering Framework (DEF)

## Overview

DEF is a low-level programming language designed for transparent compatibility with C for integration with existing C projects, as well as being good at the things C is good at.  DEF, however, has a number of language features missing from C, related to concurrency.  It includes Cilk-style parallel constructs, native support for transactions, and includes integrated memory reclamation for concurrent data structures.

Additionally, DEF contains modern language features that C is missing (tuples, parallel and concurrency constructs, array slices, and more), and a syntax that exposes features that are difficult to use in C, such as function pointers.  A description of the language can be found at the [DEF wiki](http://projects.csail.mit.edu/def/wiki/index.php?title=Main_Page).

This repository contains the source for the `def` compiler, a suite of unit and regression tests, and additional tools for language support.

## Build Instructions

DEF is currently supported only on GNU/Linux.  Instructions are for Ubuntu (or Debian-like) systems, but they should be easily generalizable to systems with other package managers.

### Packages

**OCaml and libraries:**
* The OCaml compiler can be acquired with: `sudo apt-get install ocaml ocaml-nox`
* OPAM (OCaml package manager): `sudo apt-get install opam`
* Menhir package: `opam install menhir`
* Cmdliner package (for defghi): `opam install cmdliner`

**Forkscan:**
* Clone the repository and build: https://github.com/Willtor/forkscan
* Instructions are in README.md (or visit the Github site).

**LLVM:**
* DEF requires the TAPIR extension to LLVM, which you have to build yourself.  The meta-package can be found at: https://github.com/wsmoses/Tapir-Meta
* Follow the instructions for building.  Caution: The debug binaries can get quite large, leading to long build-times.  For better results, try building the release version.
* You do not need to install TAPIR.
* Set the environment variable, TAPIRPATH, to your /path/to/tapir, the parent of the build subdirectory that was created.

### Compiler Binary

Build the `def` compiler binary and `defghi` utility with:

```
% make
% sudo make install
```

If you choose not to install it, the built binaries are in the repo at `build/bin/`.

## Contributions

Contributions are extremely welcome.  Clone the repository and create pull requests.

**Comments or bugs:** Contact the original author and maintainer, William M. Leiserson, at willtor@mit.edu.
