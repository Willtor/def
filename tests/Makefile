
DIRECTORIES = ops cilk

.PHONY: all unit smoke clean

all:
	$(foreach set,$(DIRECTORIES),(cd $(set) && make all);)

unit:
	$(foreach set,$(DIRECTORIES),(cd $(set) && make unit);)

smoke:
	$(foreach set,$(DIRECTORIES),(cd $(set) && make smoke);)

clean:
	$(foreach set,$(DIRECTORIES),(cd $(set) && make clean);)
