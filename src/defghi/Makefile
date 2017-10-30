TARGET = defghi

OCAMLC = OCAMLPATH=$(TAPIROCAML) ocamlfind ocamlopt -g
PACKAGE = -package cmdliner,str,unix
LINKPKG = -linkpkg $(PACKAGE)

SRC = main.ml

COMMONDIR = ../common
COMMONOBJ = version.cmx error.cmx parsetree.cmx deflex.cmx defparse.cmx
OBJ = $(COMMONOBJ) $(SRC:.ml=.cmx)
INTERFACE_OBJ = $(COMMONOBJ:.cmx=.cmi) $(SRC:.ml=.cmi)

$(TARGET): $(INTERFACE_OBJ) $(OBJ)
	$(OCAMLC) -o $@ $(LINKPKG) $(OBJ)

%.cmx: %.ml
	$(OCAMLC) $(PACKAGE) -c $<

%.cmx: $(COMMONDIR)/%.cmx
	cp $< $@
	cp $(<:.cmx=.o) $(@:.cmx=.o)

%.cmi: %.mli
	$(OCAMLC) $(PACKAGE) -c $<

%.cmi: $(COMMONDIR)/%.cmi
	cp $< $@
