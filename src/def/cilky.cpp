#include "caml/callback.h"
#include "caml/custom.h"
#include "llvm-c/Core.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/Tapir.h"
#include "llvm/Transforms/Tapir/CilkABI.h"
#include "llvm/Transforms/Tapir/TapirUtils.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"

// FIXME: This code should be integrated into TAPIR.

// Source:
// https://github.com/llvm-mirror/llvm/blob/master/bindings/ocaml/llvm/
//   llvm_ocaml.c
#define Builder_val(v) (*(LLVMBuilderRef *)(Data_custom_val(v)))
#define TapirTarget_val(v) (*(TapirTarget **)(Data_custom_val(v)))

using namespace llvm;

static void llvm_delete_tapir_target(value target)
{
    // Generates warning about non-virtual destructor, but TapirTarget is
    // defined by Tapir, so wait for the fix there.
    delete TapirTarget_val(target);
}

static struct custom_operations tapir_target_ops =
    { (char *) "tapir_target",
      llvm_delete_tapir_target,
      custom_compare_default,
      custom_hash_default,
      custom_serialize_default,
      custom_deserialize_default,
      custom_compare_ext_default
    };

static void LLVMAddUnifyFunctionExitNodes(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createUnifyFunctionExitNodesPass());
}

static value LLVMAllocateTapirTarget(TapirTarget *target)
{
    value val = alloc_custom(&tapir_target_ops, sizeof(TapirTarget*), 0, 1);
    TapirTarget_val(val) = target;
    return val;
}

extern LLVMTypeRef LLVMTokenTypeInContext(LLVMContextRef C);


/* [`Module] Llvm.Passmanager.t -> unit
 */
extern "C"
CAMLprim value llvm_add_unify_function_exit_nodes(LLVMPassManagerRef PM) {
    LLVMAddUnifyFunctionExitNodes(PM);
    return Val_unit;
}

extern "C"
CAMLprim value llvm_tapir_cilk_target ()
{
    return LLVMAllocateTapirTarget(new llvm::CilkABI());
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

/* llvalue -> bool
 */
extern "C"
CAMLprim value llvm_is_parallel(LLVMValueRef fcn) {
    llvm::Function *f = (llvm::Function*)unwrap(fcn);

    // Iterate over the basic blocks and look for parallel instructions.
    // We don't actually need to look at every instruction -- detach,
    // reattach, and sync all appear only at the end of the basic block.
    for (llvm::BasicBlock &bb : *f) {
        llvm::Instruction &i = bb.back();
        switch (i.getOpcode()) {
        case Instruction::Detach:
        case Instruction::Reattach:
        case Instruction::Sync:
            return Val_bool(true);
        default:
            break;
        }
    }

    // No parallel instructions.
    return Val_bool(false);
}
