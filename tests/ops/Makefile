DEFC = defc -O0
CORRECT = ../correct.sh
ALL = plus minus
UNIT = $(ALL)
SMOKE =

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
