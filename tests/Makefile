
DIRECTORIES = ops stmts cilk

.PHONY: all unit smoke clean

all:
	$(foreach set,$(DIRECTORIES),(cd $(set) && make all) &&) echo ALL DONE

unit:
	$(foreach set,$(DIRECTORIES),(cd $(set) && make unit) &&) echo UNIT DONE

smoke:
	$(foreach set,$(DIRECTORIES),(cd $(set) && make smoke) &&) echo SMOKE DONE

clean:
	$(foreach set,$(DIRECTORIES),(cd $(set) && make clean) &&) echo CLEAN DONE
