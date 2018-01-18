#include "caml/callback.h"
#include "caml/custom.h"
#include "llvm-c/Core.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/LegacyPassManager.h"

// FIXME: This code should be integrated into TAPIR.

// Source:
// https://github.com/llvm-mirror/llvm/blob/master/bindings/ocaml/llvm/
//   llvm_ocaml.c
#define Builder_val(v) (*(LLVMBuilderRef *)(Data_custom_val(v)))

using namespace llvm;

static
LLVMValueRef LLVMDIFile (LLVMContextRef ctx, char *filename, char *basepath)
{
    LLVMContext &Context = *unwrap(ctx);
    return wrap(MetadataAsValue::get(Context,
                                     DIFile::get(Context,
                                                 StringRef(filename),
                                                 StringRef(basepath))));
}

/** Return a DIFile metadata object given the file and path.
 *  llcontext -> string -> string -> Llvm.llvalue
 */
extern "C"
LLVMValueRef llvm_difile (LLVMContextRef ctx, value filename, value basepath)
{
    return LLVMDIFile(ctx, String_val(filename), String_val(basepath));
}
