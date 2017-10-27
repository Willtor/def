INSTALL_DIR = /usr/local
BUILDDIR = build
COMMON_BUILD_DIR = $(BUILDDIR)/common
DEF_BUILD_DIR = $(BUILDDIR)/def
BINDIR = $(BUILDDIR)/bin

DEF = def

COMMON_SRC_DIR = src/common
COMMONFILES =		\
	deflex.mll	\
	defparse.mly	\
	error.ml	\
	error.mli	\
	Makefile	\
	parsetree.ml	\
	parsetree.mli	\
	version.ml

DEF_SRC_DIR = src/def
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

COMMON_SRC = $(addprefix $(COMMON_BUILD_DIR)/,$(COMMONFILES))
BUILDSRC = $(addprefix $(DEF_BUILD_DIR)/,$(DEFFILES))

all: $(BUILDDIR) $(BUILDDIR)/version.t $(BINDIR)/$(DEF)

install: $(INSTALL_DIR)/bin/$(DEF)

$(INSTALL_DIR)/bin/$(DEF): $(BINDIR)/$(DEF)
	cp $< $(INSTALL_DIR)/bin/`bash version_info.sh patch`
	ln -f -s `bash version_info.sh patch` $(INSTALL_DIR)/bin/`bash version_info.sh minor`
	ln -f -s `bash version_info.sh minor` $@

$(BUILDDIR):
	mkdir -p $@

$(BUILDDIR)/version.t:
	bash version_info.sh patch > $@

$(BINDIR):
	mkdir -p $@

$(COMMON_BUILD_DIR):
	mkdir -p $@

$(DEF_BUILD_DIR):
	mkdir -p $@

$(BINDIR)/$(DEF): $(DEF_BUILD_DIR)/$(DEF) $(BINDIR)
	cp $< $@

$(DEF_BUILD_DIR)/$(DEF): $(COMMON_BUILD_DIR) $(DEF_BUILD_DIR) $(COMMON_SRC) $(BUILDSRC)
	make -C $(COMMON_BUILD_DIR)
	make -C $(DEF_BUILD_DIR)

clean:
	rm -rf $(BUILDDIR)

$(DEF_BUILD_DIR)/Makefile: $(DEF_SRC_DIR)/Makefile
	cp $< $@
	(cd $(DEF_BUILD_DIR); ocamldep *.ml *.mli >> Makefile)

$(COMMON_BUILD_DIR)/version.ml:
	bash version_info.sh ocaml > $@

$(COMMON_BUILD_DIR)/%: $(COMMON_SRC_DIR)/%
	cp $< $@

$(DEF_BUILD_DIR)/%: $(DEF_SRC_DIR)/%
	cp $< $@
