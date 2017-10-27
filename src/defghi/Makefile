TARGET = defghi

OCAMLC = OCAMLPATH=$(TAPIROCAML) ocamlfind ocamlopt -g
PACKAGE = -package str,unix
LINKPKG = -linkpkg $(PACKAGE)

SRC = main.ml

COMMONDIR = ../common
COMMONOBJ = version.cmx error.cmx parsetree.cmx deflex.cmx defparse.cmx
OBJ = $(COMMONOBJ) $(SRC:.ml=.cmx)
INTERFACE_OBJ = $(COMMONOBJ:.cmx=.cmi) $(SRC:.ml=.cmi)

$(TARGET): $(INTERFACE_OBJ) $(OBJ)
	$(OCAMLC) -o $@ $(LINKPKG) $(OBJ)

%.o:	%.cpp
	$(CLANGPP) -o $@ -c $(CPPFLAGS) $<

%.cmx: %.ml
	$(OCAMLC) $(PACKAGE) -c $<

%.cmi: %.mli
	$(OCAMLC) $(PACKAGE) -c $<

%.cmi: $(COMMONDIR)/%.cmi
	cp $< $@

%.cmx: $(COMMONDIR)/%.cmx
	cp $< $@
	cp $(<:.cmx=.o) $(@:.cmx=.o)
