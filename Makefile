BINDIR = bin
BUILDDIR = build
BUILDDEFC = $(BUILDDIR)/defc
DEFC = bin/defc
SRCDIR = src

all: $(DEFC)

$(BINDIR):
	mkdir -p $@

$(BUILDDIR):
	mkdir -p $@
	cp $(SRCDIR)/*.ml $(SRCDIR)/*.mli $(SRCDIR)/*.mll $(SRCDIR)/*.mly $@
	cp $(SRCDIR)/Makefile $@

$(DEFC): $(BUILDDEFC) $(BINDIR)
	cp $< $@

$(BUILDDEFC): $(BUILDDIR)
	make -C $<

clean:
	rm -rf $(BINDIR) $(BUILDDIR)
