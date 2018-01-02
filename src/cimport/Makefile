TARGET = cimport

TAPIRCPP = $(TAPIRPATH)/build/include
TAPIRBUILDCPP = $(TAPIRPATH)/include
TAPIRCLANG = $(TAPIRPATH)/tools/clang/include
TAPIRBUILDCLANG = $(TAPIRPATH)/build/tools/clang/include
TAPIRLIBDIR = $(TAPIRPATH)/build/lib

LIBS =  -lLLVMX86CodeGen	\
	-lLLVMX86AsmPrinter	\
	-lLLVMX86AsmParser	\
	-lLLVMX86Desc	\
	-lLLVMX86Info	\
	-lLLVMX86Disassembler	\
	-lLLVMX86Info	\
	-lLLVMX86Utils	\
	-lLLVMOption	\
	-lLLVMSupport	\
	-lclangAST	\
	-lclangBasic	\
	-lclangDriver	\
	-lclangFrontend	\
	-lclangRewriteFrontend	\
	-lclangStaticAnalyzerFrontend	\
	-lclangTooling	\
	-lLLVMAsmPrinter	\
	-lLLVMDebugInfoCodeView	\
	-lLLVMDebugInfoMSF	\
	-lLLVMGlobalISel	\
	-lLLVMSelectionDAG	\
	-lLLVMCodeGen	\
	-lLLVMBitWriter	\
	-lLLVMScalarOpts	\
	-lLLVMInstCombine	\
	-lLLVMTransformUtils	\
	-lLLVMTarget	\
	-lLLVMAnalysis	\
	-lLLVMX86AsmPrinter	\
	-lLLVMX86Utils	\
	-lLLVMObject	\
	-lLLVMMCDisassembler	\
	-lclangStaticAnalyzerCheckers	\
	-lclangStaticAnalyzerCore	\
	-lclangFrontend	\
	-lclangDriver	\
	-lLLVMOption	\
	-lclangParse	\
	-lLLVMMCParser	\
	-lclangSerialization	\
	-lclangSema	\
	-lclangEdit	\
	-lclangAnalysis	\
	-lLLVMBitReader	\
	-lLLVMProfileData	\
	-lclangASTMatchers	\
	-lclangFormat	\
	-lclangToolingCore	\
	-lclangAST	\
	-lclangRewrite	\
	-lclangLex	\
	-lclangBasic	\
	-lLLVMCore	\
	-lLLVMBinaryFormat	\
	-lLLVMMC	\
	-lLLVMSupport	\
	-lLLVMDemangle

SRC = cimport.cpp main.cpp
OBJ = $(SRC:.cpp=.o)

CXX = clang++
CPPFLAGS = -std=c++11 -fno-rtti -fno-exceptions -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS -I$(TAPIRCPP) -I$(TAPIRBUILDCPP) -I$(TAPIRCLANG) -I$(TAPIRBUILDCLANG)

all: $(TARGET)

$(TARGET): $(OBJ)
	$(CXX) -o $@ -L$(TAPIRLIBDIR) -L. $(OBJ) $(LIBS) -lpthread -lz -ltinfo

%.o: %.cpp
	$(CXX) -o $@ -c $(CPPFLAGS) $<

clean:
	rm -f $(OBJ) $(TARGET)
