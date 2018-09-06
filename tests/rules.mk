.PHONY: all unit smoke clean

.SECONDARY:

all: $(ALL)

unit: $(UNIT)

smoke: $(SMOKE)

clean:
	rm -f *.o $(ALL) *.actual *.out *.ll

%: %.def %.correct
	$(DEFC) -o $@ $<
	$(CORRECT) $@

%: %.def %.error
	-$(DEFC) $< > $@.actual 2>&1
	$(ERROR) $@

%: %.def %.keywords
	$(DEFC) -o $@.ll -S -emit-llvm $<
	$(KEYWORDS) $@
