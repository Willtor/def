OCAMLC = ocamlfind ocamlopt -g
PACKAGE = -package str,unix

SRC = version.ml operator.ml error.ml util.ml parsetree.ml stuinterp.ml defparse.mly deflex.mll frontend.ml
FILES1 = $(SRC:.mly=.ml)
FILES2 = $(FILES1:.mll=.ml)
COMMON_OBJS = $(FILES2:.ml=.cmx)
COMMON_INTERFACES = $(FILES2:.ml=.cmi)

all: $(COMMON_INTERFACES) $(COMMON_OBJS)

%.cmx: %.ml
	$(OCAMLC) $(PACKAGE) -c $<

%.cmi: %.mli
	$(OCAMLC) $(PACKAGE) -c $<

%.ml: %.mll
	ocamllex $<

%.ml: %.mly
	menhir --infer $<

defparse.mli: defparse.ml
	$(OCAMLC) -i $< > $@

deflex.mli: deflex.ml
	$(OCAMLC) -i $< > $@

version.mli: version.ml
	$(OCAMLC) -i $< > $@
