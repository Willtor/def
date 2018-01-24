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

extern "C"
LLVMValueRef llvm_disubroutine_type (LLVMContextRef ctx,
                                     DIBuilderRef dib,
                                     value ret_and_params)
{
    return LLVMDISubroutineType(ctx, dib, ret_and_params);
}
