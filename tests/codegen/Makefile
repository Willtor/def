include ../common.mk

UNIT = volatile_loops
SMOKE =
ALL = $(SMOKE) $(UNIT)

.PHONY: all unit smoke clean

.SECONDARY:

all: $(ALL)

unit: $(UNIT)

smoke: $(SMOKE)

clean:
	rm -f *.o *.ll $(ALL) *.out

%: %.def %.keywords
	$(DEFC) -o $@.ll -S -emit-llvm $<
	$(KEYWORDS) $@

%: %.def %.correct
	$(DEFC) -o $@ $<
	$(CORRECT) $@
