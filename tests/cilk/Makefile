include ../common.mk

UNIT =
SMOKE = fib
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
