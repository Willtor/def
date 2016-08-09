TARGET = defc

SRC = ast.ml verify.ml defparse.mly deflex.mll unparse.ml main.ml
GENERATED = ast.mli defparse.ml defparse.mli deflex.ml deflex.mli

FILES1 = $(SRC:.mly=.ml)
FILES2 = $(FILES1:.mll=.ml)

OBJ = $(FILES2:.ml=.cmx)
INTERFACE_OBJ = $(FILES2:.ml=.cmi)

$(TARGET): $(INTERFACE_OBJ) $(OBJ)
	ocamlfind ocamlopt -o $@ $(OBJ)

clean:
	rm -f $(TARGET) $(OBJ) $(GENERATED) *.cmx *.cmi *.o

%.cmx: %.ml
	ocamlfind ocamlopt -c $<

%.cmi: %.mli
	ocamlfind ocamlopt -c $<

%.ml: %.mll
	ocamllex $<

%.ml: %.mly
	menhir $<

ast.mli: ast.ml
	ocamlfind ocamlopt -i $< > $@

defparse.mli: defparse.ml
	ocamlfind ocamlopt -i $< > $@

deflex.mli: deflex.ml
	ocamlfind ocamlopt -i $< > $@
