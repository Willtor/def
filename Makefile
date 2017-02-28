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

$(DEFC): $(BUILDDEFC) $(BINDIR)
	cp $< $@

$(BUILDDEFC): $(BUILDDIR) $(BUILDSRC)
	make -C $<

clean:
	rm -rf $(BINDIR) $(BUILDDIR)

$(BUILDDIR)/Makefile: $(SRCDIR)/Makefile
	cp $< $@
	(cd $(BUILDDIR); ocamldep *.ml *.mli >> Makefile)

$(BUILDDIR)/%: $(SRCDIR)/%
	cp $< $@
