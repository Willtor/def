include ../common.mk

UNIT = type-inference for-generic while-generic global_vars static-array switch-basic \
	switch-tuple while-break
SMOKE =
ALL = $(SMOKE) $(UNIT)

.PHONY: all unit smoke clean

.SECONDARY:

all: $(ALL)

unit: $(UNIT)

smoke: $(SMOKE)

clean:
	rm -f *.o $(ALL) *.out

%: %.def %.correct
	$(DEFC) -o $@ $<
	$(CORRECT) $@
