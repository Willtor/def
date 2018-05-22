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

OCAMLC = OCAMLPATH=$(TAPIROCAML) ocamlfind ocamlopt -g
PACKAGE = -package cmdliner,llvm,llvm.analysis,llvm.scalar_opts,llvm.ipo,llvm.target,llvm.irreader,llvm.bitreader,llvm.bitwriter,llvm.tapir_opts,llvm.passmgr_builder,llvm.all_backends,str,unix
LINKPKG = -linkpkg $(PACKAGE) -cclib -lz
APPENDLINK = -cclib -lLLVMTransformUtils $(addprefix -cclib ,$(CIMPORT_LIBS))
CLANGPP = clang++-$(LLVM_VER)
CPPFLAGS = -fPIC -std=c++11 -fno-exceptions -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -I$(TAPIRC) -I$(TAPIRCPP)

SRC = llvmext.ml operator.ml config.ml util.ml report.ml osspecific.ml link.ml types.ml ast.ml cfg.ml lower.ml scrubber.ml iropt.ml irfactory.ml build.ml main.ml

COMMONDIR = ../common
COMMONOBJ = version.cmx error.cmx parsetree.cmx deflex.cmx defparse.cmx
OBJ = cmpxchg.o cilky.o metadata.o $(addprefix $(CIMPORT_PATH)/,$(CIMPORT_OBJ)) $(COMMONOBJ) $(SRC:.ml=.cmx)
INTERFACE_OBJ = $(COMMONOBJ:.cmx=.cmi) $(SRC:.ml=.cmi)

$(TARGET): $(INTERFACE_OBJ) $(OBJ)
	$(OCAMLC) -o $@ $(LINKPKG) $(OBJ) $(APPENDLINK)

%.o:	%.cpp
	$(CLANGPP) -o $@ -c $(CPPFLAGS) $<

%.cmx: %.ml
	$(OCAMLC) $(PACKAGE) -I $(CIMPORT_PATH) -c $<

%.cmi: %.mli
	$(OCAMLC) $(PACKAGE) -I $(CIMPORT_PATH) -c $<

%.cmi: $(COMMONDIR)/%.cmi
	cp $< $@

%.cmx: $(COMMONDIR)/%.cmx
	cp $< $@
	cp $(<:.cmx=.o) $(@:.cmx=.o)

report.mli: report.ml
	$(OCAMLC) -i $< > $@

config.mli: config.ml
	$(OCAMLC) -i $< > $@

llvmext.mli: llvmext.ml
	$(OCAMLC) $(PACKAGE) -i $< > $@
