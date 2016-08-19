TARGET = defc

OCAMLC = ocamlfind ocamlopt
PACKAGE = -package llvm,llvm.analysis
LINKPKG = -linkpkg $(PACKAGE)

SRC = util.ml ast.ml defparse.mly deflex.mll scrubber.ml irfactory.ml main.ml
GENERATED = ast.mli defparse.ml defparse.mli deflex.ml deflex.mli

FILES1 = $(SRC:.mly=.ml)
FILES2 = $(FILES1:.mll=.ml)

OBJ = $(FILES2:.ml=.cmx)
INTERFACE_OBJ = $(FILES2:.ml=.cmi)

$(TARGET): $(INTERFACE_OBJ) $(OBJ)
	$(OCAMLC) -o $@ $(LINKPKG) $(OBJ)

clean:
	rm -f $(TARGET) $(OBJ) $(GENERATED) *.cmx *.cmi *.o

%.cmx: %.ml
	$(OCAMLC) $(PACKAGE) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

%.ml: %.mll
	ocamllex $<

%.ml: %.mly
	menhir $<

ast.mli: ast.ml
	$(OCAMLC) -i $< > $@

defparse.mli: defparse.ml
	$(OCAMLC) -i $< > $@

deflex.mli: deflex.ml
	$(OCAMLC) -i $< > $@
