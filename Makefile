INSTALL_DIR = /usr/local
BUILDDIR = build
COMMON_BUILD_DIR = $(BUILDDIR)/common
CIMPORT_BUILD_DIR = $(BUILDDIR)/cimport
LLVMEXT_BUILD_DIR = $(BUILDDIR)/llvmext
DEF_BUILD_DIR = $(BUILDDIR)/def
DEFGHI_BUILD_DIR = $(BUILDDIR)/defghi
LIBHYTM_BUILD_DIR = $(BUILDDIR)/libhytm
BINDIR = $(BUILDDIR)/bin
LIBDIR = $(BUILDDIR)/lib
TESTDIR = tests

DEF = def
DEFGHI = defghi
LIBHYTM = libhytm.a

INCLUDEFILES =		\
	include/transaction.defi

COMMON_SRC_DIR = src/common
COMMONFILES =		\
	deflex.mll	\
	defparse.mly	\
	error.ml	\
	error.mli	\
	frontend.ml	\
	frontend.mli	\
	ismerr.ml	\
	Makefile	\
	operator.ml	\
	operator.mli	\
	parsetree.ml	\
	parsetree.mli	\
	isminterp.ml	\
	isminterp.mli	\
	util.ml		\
	util.mli	\
	version.ml

CIMPORT_SRC_DIR = src/cimport
CIMPORTFILES =		\
	cimportext.ml	\
	cimport.cpp	\
	cimport.mk	\
	Makefile

LLVMEXT_SRC_DIR = src/llvmext
LLVMEXTFILES =		\
	cilky.cpp	\
	cmpxchg.cpp	\
	llvmext.ml	\
	Makefile	\
	metadata.cpp

DEF_SRC_DIR = src/def
DEFFILES = 		\
	ast.ml		\
	ast.mli		\
	build.ml	\
	build.mli	\
	config.ml	\
	irfactory.ml	\
	irfactory.mli	\
	iropt.ml	\
	iropt.mli	\
	link.ml		\
	link.mli	\
	main.ml		\
	main.mli	\
	Makefile	\
	osspecific.ml	\
	osspecific.mli	\
	report.ml	\
	scrubber.ml	\
	scrubber.mli	\
	types.ml	\
	types.mli

DEFGHI_SRC_DIR = src/defghi
DEFGHIFILES =		\
	defi.ml		\
	defi.mli	\
	doc.ml		\
	header.ml	\
	header.mli	\
	main.ml		\
	main.mli	\
	Makefile

LIBHYTM_SRC_DIR = src/libhytm
LIBHYTMFILES =		\
	Makefile	\
	software_tran.def	\
	rhnorec.def

COMMON_SRC = $(addprefix $(COMMON_BUILD_DIR)/,$(COMMONFILES))
CIMPORT_SRC = $(addprefix $(CIMPORT_BUILD_DIR)/,$(CIMPORTFILES))
LLVMEXT_SRC = $(addprefix $(LLVMEXT_BUILD_DIR)/,$(LLVMEXTFILES))
DEF_SRC = $(addprefix $(DEF_BUILD_DIR)/,$(DEFFILES))
DEFGHI_SRC = $(addprefix $(DEFGHI_BUILD_DIR)/,$(DEFGHIFILES))
LIBHYTM_SRC = $(addprefix $(LIBHYTM_BUILD_DIR)/,$(LIBHYTMFILES))

all: $(BUILDDIR) $(BUILDDIR)/version.t $(BINDIR)/$(DEF) $(BINDIR)/$(DEFGHI) \
	$(LIBDIR)/$(LIBHYTM)

install: $(addprefix $(INSTALL_DIR)/,$(INCLUDEFILES))	\
	$(INSTALL_DIR)/bin/$(DEF)	\
	$(INSTALL_DIR)/bin/$(DEFGHI)	\
	$(INSTALL_DIR)/lib/$(LIBHYTM)

test: $(BUILDDIR)/bin/$(DEF)
	make -C $(TESTDIR)

testclean:
	make -C $(TESTDIR) clean

$(INSTALL_DIR)/bin/$(DEF): $(BINDIR)/$(DEF)
	cp $< $(INSTALL_DIR)/bin/`bash version_info.sh patch`
	ln -f -s `bash version_info.sh patch` $(INSTALL_DIR)/bin/`bash version_info.sh minor`
	ln -f -s `bash version_info.sh minor` $@

$(INSTALL_DIR)/bin/$(DEFGHI): $(BINDIR)/$(DEFGHI)
	cp $< $@

$(INSTALL_DIR)/lib/$(LIBHYTM): $(LIBDIR)/$(LIBHYTM)
	cp $< $@

$(INSTALL_DIR)/include/%: include/%
	cp $< $@

$(BUILDDIR):
	mkdir -p $@

$(BUILDDIR)/version.t:
	bash version_info.sh patch > $@

$(BINDIR):
	mkdir -p $@

$(LIBDIR):
	mkdir -p $@

$(COMMON_BUILD_DIR):
	mkdir -p $@

$(LLVMEXT_BUILD_DIR):
	mkdir -p $@

$(CIMPORT_BUILD_DIR):
	mkdir -p $@

$(DEF_BUILD_DIR):
	mkdir -p $@

$(DEFGHI_BUILD_DIR):
	mkdir -p $@

$(LIBHYTM_BUILD_DIR):
	mkdir -p $@

$(BINDIR)/$(DEF): $(DEF_BUILD_DIR)/$(DEF) $(BINDIR)
	cp $< $@

$(BINDIR)/$(DEFGHI): $(DEFGHI_BUILD_DIR)/$(DEFGHI) $(BINDIR)
	cp $< $@

$(LIBDIR)/$(LIBHYTM): $(LIBHYTM_BUILD_DIR)/$(LIBHYTM) $(LIBDIR)
	cp $< $@

$(DEF_BUILD_DIR)/$(DEF): $(COMMON_BUILD_DIR) $(CIMPORT_BUILD_DIR) $(LLVMEXT_BUILD_DIR) $(DEF_BUILD_DIR) $(COMMON_SRC) $(CIMPORT_SRC) $(LLVMEXT_SRC) $(DEF_SRC)
	make -C $(COMMON_BUILD_DIR)
	make -C $(CIMPORT_BUILD_DIR)
	make -C $(LLVMEXT_BUILD_DIR)
	make -C $(DEF_BUILD_DIR)

$(DEFGHI_BUILD_DIR)/$(DEFGHI): $(COMMON_BUILD_DIR) $(DEFGHI_BUILD_DIR) $(COMMON_SRC) $(DEFGHI_SRC)
	make -C $(COMMON_BUILD_DIR)
	make -C $(DEFGHI_BUILD_DIR)

$(LIBHYTM_BUILD_DIR)/$(LIBHYTM): $(LIBHYTM_BUILD_DIR) $(LIBHYTM_SRC)
	make -C $(LIBHYTM_BUILD_DIR) DEF=../../$(BINDIR)/$(DEF)

clean:
	rm -rf $(BUILDDIR)

$(DEF_BUILD_DIR)/Makefile: $(DEF_SRC_DIR)/Makefile
	cp $< $@
	(cd $(DEF_BUILD_DIR); ocamldep *.ml *.mli >> Makefile)

$(COMMON_BUILD_DIR)/version.ml:
	bash version_info.sh ocaml > $@

$(COMMON_BUILD_DIR)/%: $(COMMON_SRC_DIR)/%
	cp $< $@

$(CIMPORT_BUILD_DIR)/%: $(CIMPORT_SRC_DIR)/%
	cp $< $@

$(LLVMEXT_BUILD_DIR)/%:	$(LLVMEXT_SRC_DIR)/%
	cp $< $@

$(DEF_BUILD_DIR)/%: $(DEF_SRC_DIR)/%
	cp $< $@

$(DEFGHI_BUILD_DIR)/%: $(DEFGHI_SRC_DIR)/%
	cp $< $@

$(LIBHYTM_BUILD_DIR)/%: $(LIBHYTM_SRC_DIR)/%
	cp $< $@
