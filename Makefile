BINDIR = bin
INSTALL_DIR = /usr/local
BUILDDIR = build
DEF = def
BUILDDEF = $(BUILDDIR)/$(DEF)
SRCDIR = src
SRCFILES = 		\
	ast.ml		\
	cmdline.ml	\
	cmdline.mli	\
	cmpxchg.cpp	\
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
	link.ml		\
	link.mli	\
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
	util.mli	\
	version.ml

BUILDSRC = $(addprefix $(BUILDDIR)/,$(SRCFILES))

all: $(BINDIR)/$(DEF)

install: $(INSTALL_DIR)/bin/$(DEF)

$(INSTALL_DIR)/bin/$(DEF): $(BINDIR)/$(DEF)
	cp $< $@

$(BINDIR):
	mkdir -p $@

$(BUILDDIR):
	mkdir -p $@

$(BINDIR)/$(DEF): $(BUILDDEF) $(BINDIR)
	cp $< $@

$(BUILDDEF): $(BUILDDIR) $(BUILDSRC)
	make -C $<

clean:
	rm -rf $(BINDIR) $(BUILDDIR)

$(BUILDDIR)/Makefile: $(SRCDIR)/Makefile
	cp $< $@
	(cd $(BUILDDIR); ocamldep *.ml *.mli >> Makefile)

$(BUILDDIR)/version.ml:
	bash version_info.sh ocaml $(LLVM_VER) > $@

$(BUILDDIR)/%: $(SRCDIR)/%
	cp $< $@
