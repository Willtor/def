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
