# DEF Engineering Framework (DEF)

## Build Instructions

DEF requires LLVM-3.9 as a target, and an OCaml compiler along with the LLVM toolkit for OCaml.  In Debian-based systems, you can install LLVM-3.9 with:

```
% sudo apt-get install llvm-3.9 llvm-3.9-dev
```

To build the defc binary, just type ***make***.  The binary will appear in a new bin subdirectory.
