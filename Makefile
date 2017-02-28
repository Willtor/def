BINDIR = bin
BUILDDIR = build
BUILDDEFC = $(BUILDDIR)/defc
DEFC = bin/defc
SRCDIR = src
SRCFILES = 		\
	cmpxchg.cpp	\
	ast.ml		\
	cfg.ml		\
	cfg.mli		\
	deflex.mll	\
	defparse.mly	\
	irfactory.ml	\
	irfactory.mli	\
	iropt.ml	\
	iropt.mli	\
	llvmext.ml	\
	lower.ml	\
	lower.mli	\
	main.ml		\
	main.mli	\
	Makefile	\
	report.ml	\
	scrubber.ml	\
	scrubber.mli	\
	types.ml	\
	types.mli	\
	util.ml		\
	util.mli

BUILDSRC = $(addprefix $(BUILDDIR)/,$(SRCFILES))

all: $(DEFC)

$(BINDIR):
	mkdir -p $@

$(BUILDDIR):
	mkdir -p $@
#	cp $(SRCDIR)/*.ml $(SRCDIR)/*.mli $(SRCDIR)/*.mll $(SRCDIR)/*.mly $@
#	cp $(SRCDIR)/Makefile $@
#	ocamldep $(BUILDDIR)/*.ml $(BUILDDIR)/*.mli >> $(BUILDDIR)/Makefile

$(DEFC): $(BUILDDEFC) $(BINDIR)
	cp $< $@

$(BUILDDEFC): $(BUILDDIR) $(BUILDSRC)
	make -C $<

clean:
	rm -rf $(BINDIR) $(BUILDDIR)

$(BUILDDIR)/%: $(SRCDIR)/%
	cp $< $@
