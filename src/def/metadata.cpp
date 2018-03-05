#include "caml/callback.h"
#include "caml/custom.h"
#include "llvm-c/Core.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/LegacyPassManager.h"

// FIXME: This code should be integrated into TAPIR.

// Source:
// https://github.com/llvm-mirror/llvm/blob/master/bindings/ocaml/llvm/
//   llvm_ocaml.c
#define Builder_val(v) (*(LLVMBuilderRef *)(Data_custom_val(v)))

#define VALUEREF2METADATA(type, value)                                  \
    dyn_cast<type>(unwrap<MetadataAsValue>(value)->getMetadata())

using namespace llvm;

typedef void * DIBuilderRef;

/** Get the llvm::dwarf::* type.  Note: This needs to be kept consistent with
 *  llvmext.ml.
 */
static
int dwarf_of (int ocaml_dwarf_type) {
    switch (ocaml_dwarf_type) {
    case 0: // FIXME: Suitable internal error.
        fprintf(stderr, "Internal error: DW_INVALID.\n");
        abort();
    case 1: return llvm::dwarf::DW_ATE_boolean;
    case 2: return llvm::dwarf::DW_ATE_signed_char;
    case 3: return llvm::dwarf::DW_ATE_unsigned_char;
    case 4: return llvm::dwarf::DW_ATE_signed;
    case 5: return llvm::dwarf::DW_ATE_unsigned;
    case 6: return llvm::dwarf::DW_ATE_float;
    default:
        fprintf(stderr, "Internal error: Unknown dwarf type.\n");
        abort();
    }
}

static
LLVMValueRef LLVMDIFile (LLVMContextRef ctx, DIBuilderRef dib,
                         char *filename, char *basepath)
{
    LLVMContext &Context = *unwrap(ctx);
    DIBuilder *bldr = (DIBuilder*)dib;
    DIFile *file = bldr->createFile(StringRef(filename),
                                    StringRef(basepath));
    return wrap(MetadataAsValue::get(Context, file));
}

static
LLVMValueRef LLVMDICompileUnit (LLVMContextRef ctx,
                                DIBuilderRef dib,
                                /* lang <- param needed; use C by default. */
                                LLVMValueRef file,
                                StringRef producer,
                                bool is_optimized,
                                StringRef flags,
                                unsigned int rt_version)
{
    LLVMContext &Context = *unwrap(ctx);
    DIBuilder *bldr = (DIBuilder*)dib;
    MDNode *filenode = dyn_cast<MDNode>(unwrap<MetadataAsValue>(file)
                                        ->getMetadata());
    DICompileUnit *cu = bldr->createCompileUnit(dwarf::DW_LANG_C,
                                                static_cast<DIFile*>(filenode),
                                                producer,
                                                is_optimized,
                                                flags,
                                                rt_version);
    return wrap(MetadataAsValue::get(Context, cu));
}

static
LLVMValueRef LLVMDIBasicType (LLVMContextRef ctx,
                              DIBuilderRef dib,
                              char *type_name,
                              int bitwidth,
                              int dwarf_type)
{
    LLVMContext &Context = *unwrap(ctx);
    DIBasicType *type =
        ((DIBuilder*)dib)->createBasicType(StringRef(type_name),
                                           bitwidth,
                                           dwarf_of(dwarf_type));
    return wrap(MetadataAsValue::get(Context, type));
}

static
LLVMValueRef LLVMDIPointerType (LLVMContextRef ctx,
                                DIBuilderRef dib,
                                LLVMValueRef base_type,
                                int size)
{
    LLVMContext &Context = *unwrap(ctx);
    DIDerivedType *type =
        ((DIBuilder*)dib)
        ->createPointerType(VALUEREF2METADATA(DIType, base_type),
                            (uint64_t)size);
    return wrap(MetadataAsValue::get(Context, type));
}

static
LLVMValueRef LLVMDISubroutineType (LLVMContextRef ctx,
                                   DIBuilderRef dib,
                                   value ret_and_params)
{
    LLVMContext &Context = *unwrap(ctx);
    std::vector<Metadata*> pvector;
    int i;
    for (i = 0; Val_int(0) != ret_and_params; ++i) {
        LLVMValueRef param =
            reinterpret_cast<LLVMValueRef>(Field(ret_and_params, 0));
        Metadata *ptype =
            dyn_cast<Metadata>(unwrap<MetadataAsValue>(param)->getMetadata());
        pvector.push_back(ptype);
        ret_and_params = Field(ret_and_params, 1);
    }
    ArrayRef<Metadata*> parray(pvector);
    DITypeRefArray plist = ((DIBuilder*)dib)->getOrCreateTypeArray(parray);
    DISubroutineType *type = ((DIBuilder*)dib)->createSubroutineType(plist);
    return wrap(MetadataAsValue::get(Context, type));
}

static
LLVMValueRef LLVMDIFunction (LLVMContextRef ctx,
                             DIBuilderRef dib,
                             char *fname,
                             LLVMValueRef file,
                             LLVMValueRef scope,
                             int line_no,
                             bool is_local,
                             LLVMValueRef type)
{
    LLVMContext &Context = *unwrap(ctx);
    DISubprogram *fcn =
        ((DIBuilder*)dib)->createFunction(VALUEREF2METADATA(DIScope, scope),
                                          StringRef(fname),
                                          StringRef(""),
                                          VALUEREF2METADATA(DIFile, file),
                                          (unsigned int)line_no,
                                          VALUEREF2METADATA(DISubroutineType,
                                                            type),
                                          is_local,
                                          true,
                                          (unsigned int)line_no);
    ((DIBuilder*)dib)->finalizeSubprogram(fcn);
    return wrap(MetadataAsValue::get(Context, fcn));
}

void LLVMSetSubprogram (LLVMValueRef function, LLVMValueRef metadata)
{
    Function *f = unwrap<Function>(function);
    f->setSubprogram(VALUEREF2METADATA(DISubprogram, metadata));
}

LLVMValueRef LLVMDILexicalBlock (LLVMContextRef ctx,
                                 DIBuilderRef dib,
                                 unsigned int line,
                                 unsigned int column,
                                 LLVMValueRef scope,
                                 LLVMValueRef file)
{
    LLVMContext &Context = *unwrap(ctx);
    DILexicalBlock *block =
        ((DIBuilder*)dib)
        ->createLexicalBlock(VALUEREF2METADATA(DIScope, scope),
                             VALUEREF2METADATA(DIFile, file),
                             line, column);
    return wrap(MetadataAsValue::get(Context, block));
}

LLVMValueRef LLVMDILocation (LLVMContextRef ctx,
                             unsigned int line,
                             unsigned int column,
                             LLVMValueRef scope)
{
    LLVMContext &Context = *unwrap(ctx);
    DILocation *loc =
        DILocation::get(Context, line, column,
                        VALUEREF2METADATA(DIScope, scope));
    return wrap(MetadataAsValue::get(Context, loc));
}

/** Make a DIBuilder for constructing debugging info.
 *  llmodule -> lldibuilder
 */
extern "C"
DIBuilderRef llvm_dibuilder (LLVMModuleRef mdl)
{
    return (DIBuilderRef)new DIBuilder(*unwrap(mdl));
}

/** Return a DIFile metadata object given the file and path.
 *  llcontext -> lldibuilder -> string -> string -> Llvm.llvalue
 */
extern "C"
LLVMValueRef llvm_difile (LLVMContextRef ctx, DIBuilderRef dib,
                          value filename, value basepath)
{
    return LLVMDIFile(ctx, dib, String_val(filename), String_val(basepath));
}

/** Get a compile unit object.
 *  llcontext -> lldibuilder -> llvalue -> string -> bool -> string -> int
 *  -> llvalue
 */
extern "C"
LLVMValueRef llvm_dicompile_unit (value ctx,
                                  value dib,
                                  value file,
                                  value producer,
                                  value is_optimized,
                                  value flags,
                                  value rt_version)
{
    return LLVMDICompileUnit(reinterpret_cast<LLVMContextRef>(ctx),
                             reinterpret_cast<DIBuilderRef>(dib),
                             reinterpret_cast<LLVMValueRef>(file),
                             StringRef(String_val(producer)),
                             Bool_val(is_optimized),
                             StringRef(String_val(flags)),
                             (unsigned int)Int_val(rt_version));
}

/** Bytecode wrapper for llvm_dicompile_unit.
 */
extern "C"
LLVMValueRef llvm_dicompile_unit_bc (value *argv, int argc)
{
    return llvm_dicompile_unit(argv[0], argv[1], argv[2], argv[3], argv[4],
                               argv[5], argv[6]);
}

/** Get a new DIBasicType for the given name, length, and DWARF type.
 *  lldibuilder -> string -> int -> dwarf_type -> llvalue
 */
extern "C"
LLVMValueRef llvm_dibasic_type (value ctx,
                                value dib,
                                value type_name,
                                value bitwidth,
                                value dwarf_type)
{
    return LLVMDIBasicType (reinterpret_cast<LLVMContextRef>(ctx),
                            reinterpret_cast<DIBuilderRef>(dib),
                            String_val(type_name),
                            Int_val(bitwidth),
                            Int_val(dwarf_type));
}

/** Bytecode wrapper for llvm_dibasic_type.
 */
extern "C"
LLVMValueRef llvm_dibasic_type_bc (value *argv, int argc)
{
    return llvm_dibasic_type(argv[0], argv[1], argv[2], argv[3], argv[4]);
}

/** Get a new pointer type to the given base type and pointer size.
 *  llcontext -> lldibuilder -> llvalue -> int -> llvalue
 */
extern "C"
LLVMValueRef llvm_dipointer_type (LLVMContextRef ctx,
                                  DIBuilderRef dib,
                                  LLVMValueRef base_type,
                                  value size)
{
    return LLVMDIPointerType(ctx, dib, base_type, Int_val(size));
}

/** Get a new DISubroutineType for the given return value + parameters.
 *  llcontext -> lldibuilder -> llvalue list -> llvalue
 */
extern "C"
LLVMValueRef llvm_disubroutine_type (LLVMContextRef ctx,
                                     DIBuilderRef dib,
                                     value ret_and_params)
{
    return LLVMDISubroutineType(ctx, dib, ret_and_params);
}

/** Create debugging info for a function.
 *  llcontext -> lldibuilder -> string -> llvalue -> llvalue -> int -> bool
 *  -> llvalue -> llvalue
 */
extern "C"
LLVMValueRef llvm_difunction (value ctx,
                              value dib,
                              value name,
                              value file,
                              value scope,
                              value line_no,
                              value is_local,
                              value type)
{
    return LLVMDIFunction(reinterpret_cast<LLVMContextRef>(ctx),
                          reinterpret_cast<DIBuilderRef>(dib),
                          String_val(name),
                          reinterpret_cast<LLVMValueRef>(file),
                          reinterpret_cast<LLVMValueRef>(scope),
                          Int_val(line_no),
                          Bool_val(is_local),
                          reinterpret_cast<LLVMValueRef>(type));
}

/** Bytecode wrapper for llvm_difunction.
 */
extern "C"
LLVMValueRef llvm_difunction_bc (value *argv, int argc)
{
    return llvm_difunction(argv[0], argv[1], argv[2], argv[3], argv[4],
                           argv[5], argv[6], argv[7]);
}

extern "C"
CAMLprim value llvm_set_subprogram (LLVMValueRef function,
                                    LLVMValueRef metadata)
{
    LLVMSetSubprogram(function, metadata);
    return Val_unit;
}

/** Create a DILexicalBlock for the given position.
 *  llcontext -> lldibuilder -> Lexing.position -> llvalue -> llvalue
 *  -> llvalue
 */
extern "C"
LLVMValueRef llvm_dilexical_block (value ctx,
                                   value dib,
                                   value position,
                                   value scope,
                                   value file)
{
    unsigned int line = Int_val(Field(position, 1));
    unsigned int column =
        Int_val(Field(position, 3)) - Int_val(Field(position, 2));
    return LLVMDILexicalBlock(reinterpret_cast<LLVMContextRef>(ctx),
                              reinterpret_cast<DIBuilderRef>(dib),
                              line,
                              column,
                              reinterpret_cast<LLVMValueRef>(scope),
                              reinterpret_cast<LLVMValueRef>(file));
}

/** Bytecode wrapper for llvm_dilexical_block.
 */
extern "C"
LLVMValueRef llvm_dilexical_block_bc (value *argv, int argc)
{
    return llvm_dilexical_block(argv[0], argv[1], argv[2], argv[3], argv[4]);
}

/** Create a DILocation for the given position.
 *  lldibuilder -> Lexing.position -> llvalue -> llvalue -> llvalue
 */
extern "C"
LLVMValueRef llvm_dilocation (value ctx,
                              value position,
                              value scope)
{
    unsigned int line = Int_val(Field(position, 1));
    unsigned int column =
        Int_val(Field(position, 3)) - Int_val(Field(position, 2));
    return LLVMDILocation(reinterpret_cast<LLVMContextRef>(ctx),
                          line,
                          column,
                          reinterpret_cast<LLVMValueRef>(scope));
}

/** Bytecode wrapper for llvm_dilexical_block.
 */
extern "C"
LLVMValueRef llvm_dilocation_bc (value *argv, int argc)
{
    return llvm_dilocation(argv[0], argv[1], argv[2]);
}
