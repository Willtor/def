DEFC = defc -O0
CORRECT = ../correct.sh
TESTS = plus

.PHONY: all clean

.SECONDARY:

all: $(TESTS)

clean:
	rm -f *.o $(TESTS) *.out

%: %.def %.correct
	$(DEFC) -o $@ $<
	$(CORRECT) $@
