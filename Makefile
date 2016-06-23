TARGET = defc

SRC = ast.ml defparse.mly deflex.mll main.ml
GENERATED = ast.mli defparse.ml defparse.mli deflex.ml deflex.mli

FILES1 = $(SRC:.mly=.ml)
FILES2 = $(FILES1:.mll=.ml)

OBJ = $(FILES2:.ml=.cmx)
INTERFACE_OBJ = $(FILES2:.ml=.cmi)

$(TARGET): $(INTERFACE_OBJ) $(OBJ)
	ocamlopt -o $@ $(OBJ)

clean:
	rm -f $(TARGET) $(OBJ) $(GENERATED) *.cmx *.cmi *.o

%.cmx: %.ml
	ocamlopt -c $<

%.cmi: %.mli
	ocamlopt -c $<

%.ml: %.mll
	ocamllex $<

%.ml: %.mly
	menhir $<

%.mli: %.ml
	ocamlopt -i $< > $@
