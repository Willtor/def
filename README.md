# DEF Engineering Framework (DEF)

## Build Instructions

DEFC is currently supported only on GNU/Linux.  Instructions are for Ubuntu (or Debian-like) systems, but they should be easily generalizable to systems with other package managers.

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
