DEF ?= def
AR = ar
TARGET = libhytm.a

SRC = software_tran.def rhnorec.def
OBJ = $(SRC:.def=.o)

all: $(TARGET)

%.o: %.def
	$(DEF) -c -o $@ $<

$(TARGET): $(OBJ)
	$(AR) cr $@ $^

clean:
	rm -f $(TARGET) $(OBJ)
