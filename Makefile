LLVM_VER ?= 4.0

INSTALL_DIR = /usr/local
BUILDDIR = build
COMMONDIR = $(BUILDDIR)/common
DEFDIR = $(BUILDDIR)/def
BINDIR = $(BUILDDIR)/bin
DEF = def
BUILDDEF = $(DEFDIR)/$(DEF)

COMMONSRCDIR = src/common
COMMONFILES =		\
	deflex.mll	\
	defparse.mly	\
	error.ml	\
	error.mli	\
	Makefile	\
	parsetree.ml	\
	parsetree.mli	\
	version.ml

DEFSRCDIR = src/def
DEFFILES = 		\
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
	templates.ml	\
	templates.mli	\
	types.ml	\
	types.mli	\
	util.ml		\
	util.mli

COMMONSRC = $(addprefix $(COMMONDIR)/,$(COMMONFILES))
BUILDSRC = $(addprefix $(DEFDIR)/,$(DEFFILES))

all: $(BUILDDIR) $(BUILDDIR)/version.t $(BINDIR)/$(DEF)

install: $(INSTALL_DIR)/bin/$(DEF)

$(INSTALL_DIR)/bin/$(DEF): $(BINDIR)/$(DEF)
	cp $< $(INSTALL_DIR)/bin/`bash version_info.sh patch`
	ln -f -s `bash version_info.sh patch` $(INSTALL_DIR)/bin/`bash version_info.sh minor`
	ln -f -s `bash version_info.sh minor` $@

$(BUILDDIR):
	mkdir -p $@

$(BUILDDIR)/version.t:
	bash version_info.sh patch $(LLVM_VER) > $@

$(BINDIR):
	mkdir -p $@

$(COMMONDIR):
	mkdir -p $@

$(DEFDIR):
	mkdir -p $@

$(BINDIR)/$(DEF): $(BUILDDEF) $(BINDIR)
	cp $< $@

$(BUILDDEF): $(COMMONDIR) $(DEFDIR) $(COMMONSRC) $(BUILDSRC)
	make -C $(COMMONDIR)
	make -C $(DEFDIR)

clean:
	rm -rf $(BUILDDIR)

$(DEFDIR)/Makefile: $(DEFSRCDIR)/Makefile
	cp $< $@
	(cd $(DEFDIR); ocamldep *.ml *.mli >> Makefile)

$(COMMONDIR)/version.ml:
	bash version_info.sh ocaml $(LLVM_VER) > $@

$(COMMONDIR)/%: $(COMMONSRCDIR)/%
	cp $< $@

$(DEFDIR)/%: $(DEFSRCDIR)/%
	cp $< $@
