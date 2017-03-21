# DEF Engineering Framework (DEF)

## Overview

DEF is a low-level programming language designed for transparent compatibility with C for integration with existing C projects, as well as being good at the things C is good at.  DEF, however, has a number of language features missing from C (tuples, parallel and concurrency constructs, array slices, and more), and a syntax that exposes features that are difficult to use in C, such as function pointers.

This repository contains the source for the `defc` compiler, a suite of unit and regression tests, and additional tools for language support.

## Build Instructions

DEF is currently supported only on GNU/Linux.  Instructions are for Ubuntu (or Debian-like) systems, but they should be easily generalizable to systems with other package managers.

### Packages

**OCaml and libraries:**
* The OCaml compiler can be acquired with: `sudo apt-get install ocaml ocaml-nox`
* OPAM (OCaml package manager): `sudo apt-get install opam`
* LLVM and Menhir packages: `opam install llvm menhir`

**Forkscan:**
* Forkscan library requires JE Malloc: `sudo apt-get install libjemalloc-dev`
* Clone the repository and build:
```
% git clone git@githubmit.edu:willtor/forkgc.git
% cd forkgc
% git checkout -b ForkGC origin/ForkGC
% make
% sudo make install
```

**LLVM:**
* DEFC, at present requires llvm-3.9: `sudo apt-get install llvm-3.9 llvm-3.9-dev`

### Compiler Binary

Build the `defc` binary with:

```
% make
% sudo make install
```

If you choose not to install it, the built binary is in the repo at `bin/defc`.

## Contributions

Contributions are extremely welcome.  Clone the repository and create pull requests.

**Comments or bugs:** Contact the original author and maintainer, William M. Leiserson, at willtor@mit.edu.
