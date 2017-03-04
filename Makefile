BINDIR = bin
INSTALL_DIR = /usr/local
BUILDDIR = build
BUILDDEFC = $(BUILDDIR)/defc
DEFC = defc
SRCDIR = src
SRCFILES = 		\
	cmdline.ml	\
	cmdline.mli	\
	cmpxchg.cpp	\
	ast.ml		\
	cfg.ml		\
	cfg.mli		\
	deflex.mll	\
	defparse.mly	\
	header.ml	\
	header.mli	\
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

all: $(BINDIR)/$(DEFC)

install: $(INSTALL_DIR)/bin/$(DEFC)

$(INSTALL_DIR)/bin/$(DEFC): $(BINDIR)/$(DEFC)
	cp $< $@

$(BINDIR):
	mkdir -p $@

$(BUILDDIR):
	mkdir -p $@

$(BINDIR)/$(DEFC): $(BUILDDEFC) $(BINDIR)
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
