#include "caml/callback.h"
#include "llvm-c/Core.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/Tapir.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"

// FIXME: This code should be integrated into TAPIR.

// Source:
// https://github.com/llvm-mirror/llvm/blob/master/bindings/ocaml/llvm/
//   llvm_ocaml.c
#define Builder_val(v) (*(LLVMBuilderRef *)(Data_custom_val(v)))

using namespace llvm;

LLVMValueRef LLVMBuildDetach(LLVMBuilderRef B,
                             LLVMBasicBlockRef DetachBB,
                             LLVMBasicBlockRef ContinueBB,
                             LLVMValueRef SyncRegion)
{
    return wrap(unwrap(B)->CreateDetach(unwrap(DetachBB),
                                        unwrap(ContinueBB),
                                        unwrap(SyncRegion)));
}

LLVMValueRef LLVMBuildReattach(LLVMBuilderRef B,
                               LLVMBasicBlockRef ReattachBB,
                               LLVMValueRef SyncRegion)
{
    return wrap(unwrap(B)->CreateReattach(unwrap(ReattachBB),
                                          unwrap(SyncRegion)));
}

LLVMValueRef LLVMBuildSync(LLVMBuilderRef B,
                           LLVMBasicBlockRef ContinueBB,
                           LLVMValueRef SyncRegion)
{
    return wrap(unwrap(B)->CreateSync(unwrap(ContinueBB),
                                      unwrap(SyncRegion)));
}

void LLVMAddUnifyFunctionExitNodes(LLVMPassManagerRef PM) {
    unwrap(PM)->add(createUnifyFunctionExitNodesPass());
}

void LLVMAddLowerTapirToCilk(LLVMPassManagerRef PM) {
    unwrap(PM)->add(createLowerTapirToCilkPass(true, false));
}

extern LLVMTypeRef LLVMTokenTypeInContext(LLVMContextRef C);

/* llbasicblock -> llbasicblock -> llvalue -> llbuilder -> llvalue
 */
extern "C"
CAMLprim LLVMValueRef llvm_build_detach(LLVMBasicBlockRef DetachBB,
                                        LLVMBasicBlockRef ContinueBB,
                                        LLVMValueRef SyncRegion,
                                        LLVMBuilderRef B)
{
    return LLVMBuildDetach(Builder_val(B), DetachBB, ContinueBB, SyncRegion);
}

/* llbasicblock -> llvalue -> llbuilder -> llvalue
 */
extern "C"
CAMLprim LLVMValueRef llvm_build_reattach(LLVMBasicBlockRef ReattachBB,
                                          LLVMValueRef SyncRegion,
                                          LLVMBuilderRef B)
{
    return LLVMBuildReattach(Builder_val(B), ReattachBB, SyncRegion);
}

/* llbasicblock -> llvalue -> llbuilder -> llvalue
 */
extern "C"
CAMLprim LLVMValueRef llvm_build_sync(LLVMBasicBlockRef ContinueBB,
                                      LLVMValueRef SyncRegion,
                                      LLVMBuilderRef B)
{
    return LLVMBuildSync(Builder_val(B), ContinueBB, SyncRegion);
}

/* [`Module] Llvm.Passmanager.t -> unit
 */
extern "C"
CAMLprim value llvm_add_unify_function_exit_nodes(LLVMPassManagerRef PM) {
    LLVMAddUnifyFunctionExitNodes(PM);
    return Val_unit;
}

/* [`Module] Llvm.PassManager.t -> unit
 */
extern "C"
CAMLprim value llvm_add_lower_tapir_to_cilk(LLVMPassManagerRef PM) {
    LLVMAddLowerTapirToCilk(PM);
    return Val_unit;
}

/* llcontext -> lltype
 */
extern "C"
CAMLprim LLVMTypeRef llvm_token_type(LLVMContextRef Context) {
    return LLVMTokenTypeInContext(Context);
}

/* FIXME: For some reason, we need this with the current Tapir branch.
 */
extern "C"
void LLVMDumpType(LLVMTypeRef Ty) { return; }
