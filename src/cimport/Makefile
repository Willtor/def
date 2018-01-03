TAPIRCPP = $(TAPIRPATH)/build/include
TAPIRBUILDCPP = $(TAPIRPATH)/include
TAPIRCLANG = $(TAPIRPATH)/tools/clang/include
TAPIRBUILDCLANG = $(TAPIRPATH)/build/tools/clang/include

SRC = cimport.cpp
OBJ = $(SRC:.cpp=.o)

CXX = clang++
CPPFLAGS = -std=c++11 -fno-rtti -fno-exceptions -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -I$(TAPIRCPP) -I$(TAPIRBUILDCPP) -I$(TAPIRCLANG) -I$(TAPIRBUILDCLANG)

all: $(OBJ)

%.o: %.cpp
	$(CXX) -o $@ -c $(CPPFLAGS) $<

clean:
	rm -f $(OBJ)
