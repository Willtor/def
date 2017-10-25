LLVM_VER ?= 4.0

INSTALL_DIR = /usr/local
BUILDDIR = build
DEFDIR = $(BUILDDIR)/def
BINDIR = $(BUILDDIR)/bin
DEF = def
BUILDDEF = $(DEFDIR)/$(DEF)
SRCDIR = src/def
SRCFILES = 		\
	ast.ml		\
	build.ml	\
	build.mli	\
	cmdline.ml	\
	cmdline.mli	\
	cilky.cpp	\
	cmpxchg.cpp	\
	cfg.ml		\
	cfg.mli		\
	config.ml	\
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
	parsetree.ml	\
	parsetree.mli	\
	report.ml	\
	scrubber.ml	\
	scrubber.mli	\
	templates.ml	\
	templates.mli	\
	types.ml	\
	types.mli	\
	util.ml		\
	util.mli	\
	version.ml

BUILDSRC = $(addprefix $(DEFDIR)/,$(SRCFILES))

all: $(BINDIR)/$(DEF)

install: $(INSTALL_DIR)/bin/$(DEF)

$(INSTALL_DIR)/bin/$(DEF): $(BINDIR)/$(DEF)
	cp $< $(INSTALL_DIR)/bin/`bash version_info.sh patch`
	ln -f -s `bash version_info.sh patch` $(INSTALL_DIR)/bin/`bash version_info.sh minor`
	ln -f -s `bash version_info.sh minor` $@

$(BINDIR):
	mkdir -p $@

$(DEFDIR):
	mkdir -p $@

$(BINDIR)/$(DEF): $(BUILDDEF) $(BINDIR)
	cp $< $@

$(BUILDDEF): $(DEFDIR) $(BUILDSRC)
	make -C $<

clean:
	rm -rf $(BINDIR) $(BUILDDIR)

$(DEFDIR)/Makefile: $(SRCDIR)/Makefile
	cp $< $@
	(cd $(DEFDIR); ocamldep *.ml *.mli >> Makefile)

$(DEFDIR)/version.ml:
	bash version_info.sh ocaml $(LLVM_VER) > $@

$(DEFDIR)/%: $(SRCDIR)/%
	cp $< $@
