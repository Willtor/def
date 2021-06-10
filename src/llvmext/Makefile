TARGET = def

LLVM_VER ?= 4.0

ifneq ($(TAPIRPATH),)
	TAPIROCAML = $(TAPIRPATH)/build/lib/ocaml
	TAPIRC = $(TAPIRPATH)/include
	TAPIRCPP = $(TAPIRPATH)/build/include
else
	TAPIROCAML = /
	TAPIRC = /usr/include/llvm-c-$(LLVM_VER)
	TAPIRCPP = /usr/include/llvm-$(LLVM_VER)
endif

CIMPORT_PATH = ../cimport

include $(CIMPORT_PATH)/cimport.mk

OCAMLC = OCAMLPATH=$(TAPIROCAML) ocamlfind ocamlopt -g -opaque
PACKAGE = -package cmdliner,llvm,llvm.analysis,llvm.scalar_opts,llvm.ipo,llvm.target,llvm.irreader,llvm.bitreader,llvm.bitwriter,llvm.tapir_opts,llvm.passmgr_builder,llvm.all_backends,str,unix
CLANGPP = clang++-$(LLVM_VER)
CPPFLAGS = -fPIC -std=c++11 -fno-exceptions -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -I$(TAPIRC) -I$(TAPIRCPP)

SRC = llvmext.ml
OBJ = cmpxchg.o cilky.o metadata.o $(SRC:.ml=.cmx)

.PHONY: all
all:	$(SRC:.ml=.mli) $(SRC:.ml=.cmi) $(OBJ)

%.o:	%.cpp
	$(CLANGPP) -o $@ -c $(CPPFLAGS) $<

%.cmx: %.ml
	$(OCAMLC) $(PACKAGE) -I $(CIMPORT_PATH) -c $<

%.cmi: %.mli
	$(OCAMLC) $(PACKAGE) -I $(CIMPORT_PATH) -c $<

llvmext.mli: llvmext.ml
	$(OCAMLC) $(PACKAGE) -i $< > $@
