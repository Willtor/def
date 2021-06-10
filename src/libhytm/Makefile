DEF ?= def
AR = ar
TARGET = libhytm.a

SRC = software_tran.def rhnorec.def
OBJ = $(SRC:.def=.o)

.PHONY: all
all: $(TARGET)

%.o: %.def
	$(DEF) -c -o $@ $<

$(TARGET): $(OBJ)
	$(AR) cr $@ $^

.PHONY: clean
clean:
	rm -f $(TARGET) $(OBJ)
