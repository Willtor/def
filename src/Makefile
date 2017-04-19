TARGET = def

OCAMLC = ocamlfind ocamlopt -g
PACKAGE = -package llvm,llvm.analysis,llvm.scalar_opts,str,unix
LINKPKG = -linkpkg $(PACKAGE)
CLANGPP = clang++-3.9
CPPFLAGS = -fPIC -std=c++11 -fno-exceptions -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -I/usr/include/llvm-c-3.9 -I/usr/include/llvm-3.9

SRC = version.ml llvmext.ml util.ml report.ml cmdline.ml link.ml types.ml ast.ml header.ml cfg.ml lower.ml defparse.mly deflex.mll scrubber.ml iropt.ml irfactory.ml main.ml
GENERATED = version.mli llvmext.mli report.mli ast.mli defparse.ml defparse.mli deflex.ml deflex.mli

FILES1 = $(SRC:.mly=.ml)
FILES2 = $(FILES1:.mll=.ml)

OBJ = cmpxchg.o $(FILES2:.ml=.cmx)
INTERFACE_OBJ = $(FILES2:.ml=.cmi)

$(TARGET): $(INTERFACE_OBJ) $(OBJ)
	$(OCAMLC) -o $@ $(LINKPKG) $(OBJ)

clean:
	rm -f $(TARGET) $(OBJ) $(GENERATED) *.cmx *.cmi *.o

%.o:	%.cpp
	$(CLANGPP) -o $@ -c $(CPPFLAGS) $<

%.cmx: %.ml
	$(OCAMLC) $(PACKAGE) -c $<

%.cmi: %.mli
	$(OCAMLC) $(PACKAGE) -c $<

%.ml: %.mll
	ocamllex $<

%.ml: %.mly
	menhir --infer $<

ast.mli: ast.ml
	$(OCAMLC) -i $< > $@

report.mli: report.ml
	$(OCAMLC) -i $< > $@

defparse.mli: defparse.ml
	$(OCAMLC) -i $< > $@

deflex.mli: deflex.ml
	$(OCAMLC) -i $< > $@

llvmext.mli: llvmext.ml
	$(OCAMLC) $(PACKAGE) -i $< > $@

version.mli: version.ml
	$(OCAMLC) -i $< > $@
