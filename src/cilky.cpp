#include "caml/callback.h"
#include "llvm-c/Core.h"
#include "llvm/IR/IRBuilder.h"

// FIXME: This code should be integrated into TAPIR.

// Source:
// https://github.com/llvm-mirror/llvm/blob/master/bindings/ocaml/llvm/
//   llvm_ocaml.c
#define Builder_val(v) (*(LLVMBuilderRef *)(Data_custom_val(v)))

using namespace llvm;

LLVMValueRef LLVMBuildDetach(LLVMBuilderRef B,
                             LLVMBasicBlockRef DetachBB,
                             LLVMBasicBlockRef ContinueBB)
{
    return wrap(unwrap(B)->CreateDetach(unwrap(DetachBB),
                                        unwrap(ContinueBB)));
}

LLVMValueRef LLVMBuildReattach(LLVMBuilderRef B,
                               LLVMBasicBlockRef ReattachBB)
{
    return wrap(unwrap(B)->CreateReattach(unwrap(ReattachBB)));
}

LLVMValueRef LLVMBuildSync(LLVMBuilderRef B,
                           LLVMBasicBlockRef ContinueBB)
{
    return wrap(unwrap(B)->CreateSync(unwrap(ContinueBB)));
}

/* llbasicblock -> llbasicblock -> llbuilder -> llvalue
 */
extern "C"
CAMLprim LLVMValueRef llvm_build_detach(LLVMBasicBlockRef DetachBB,
                                        LLVMBasicBlockRef ContinueBB,
                                        LLVMBuilderRef B)
{
    return LLVMBuildDetach(Builder_val(B), DetachBB, ContinueBB);
}

/* llbasicblock -> llbuilder -> llvalue
 */
extern "C"
CAMLprim LLVMValueRef llvm_build_reattach(LLVMBasicBlockRef ReattachBB,
                                          LLVMBuilderRef B)
{
    return LLVMBuildReattach(Builder_val(B), ReattachBB);
}

/* llbasicblock -> llbuilder -> llvalue
 */
extern "C"
CAMLprim LLVMValueRef llvm_build_sync(LLVMBasicBlockRef ContinueBB,
                                      LLVMBuilderRef B)
{
    return LLVMBuildSync(Builder_val(B), ContinueBB);
}
