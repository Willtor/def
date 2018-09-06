TAPIRCPP = $(TAPIRPATH)/build/include
TAPIRBUILDCPP = $(TAPIRPATH)/include
TAPIRCLANG = $(TAPIRPATH)/tools/clang/include
TAPIRBUILDCLANG = $(TAPIRPATH)/build/tools/clang/include

OCAMLSRC = cimportext.ml
CSRC = cimport.cpp
OBJ = $(CSRC:.cpp=.o) $(OCAMLSRC:.ml=.cmi) $(OCAMLSRC:.ml=.cmx)

OCAMLC = OCAMLPATH=$(TAPIROCAML) ocamlfind ocamlopt -g -opaque
CXX = clang++
CPPFLAGS = -fPIC -std=c++11 -fno-rtti -fno-exceptions -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -I$(TAPIRCPP) -I$(TAPIRBUILDCPP) -I$(TAPIRCLANG) -I$(TAPIRBUILDCLANG)

all: $(OBJ)

%.o: %.cpp
	$(CXX) -o $@ -c $(CPPFLAGS) $<

%.cmx: %.ml
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

cimportext.mli: cimportext.ml
	$(OCAMLC) -i $< > $@
