#include "caml/callback.h"
#include "llvm-c/Core.h"
#include "llvm/IR/IRBuilder.h"

// Source:
// https://github.com/llvm-mirror/llvm/blob/master/bindings/ocaml/llvm/
//   llvm_ocaml.c
#define Builder_val(v) (*(LLVMBuilderRef *)(Data_custom_val(v)))

using namespace llvm;

static AtomicOrdering mapFromLLVMOrdering(LLVMAtomicOrdering Ordering) {
    switch (Ordering) {
    case LLVMAtomicOrderingNotAtomic: return AtomicOrdering::NotAtomic;
    case LLVMAtomicOrderingUnordered: return AtomicOrdering::Unordered;
    case LLVMAtomicOrderingMonotonic: return AtomicOrdering::Monotonic;
    case LLVMAtomicOrderingAcquire: return AtomicOrdering::Acquire;
    case LLVMAtomicOrderingRelease: return AtomicOrdering::Release;
    case LLVMAtomicOrderingAcquireRelease:
        return AtomicOrdering::AcquireRelease;
    case LLVMAtomicOrderingSequentiallyConsistent:
        return AtomicOrdering::SequentiallyConsistent;
    }

    llvm_unreachable("Invalid LLVMAtomicOrdering value!");
}

// Code adapted from Jonathan Ragan-Kelley at:
// https://github.com/jrk/llvm/pull/3

static
LLVMValueRef LLVMBuildCmpXchg(LLVMBuilderRef B,
                              LLVMAtomicOrdering SuccOrder,
                              LLVMAtomicOrdering FailOrder,
                              LLVMValueRef Ptr,
                              LLVMValueRef Cmp,
                              LLVMValueRef New)
{
    return
        wrap(unwrap(B)->CreateAtomicCmpXchg(unwrap(Ptr),
                                            unwrap(Cmp),
                                            unwrap(New),
                                            mapFromLLVMOrdering(SuccOrder),
                                            mapFromLLVMOrdering(FailOrder)));
}

/* llvalue -> llvalue -> llvalue -> AtomicOrder.t -> AtomicOrder.t ->
     llbuilder -> llvalue */
extern "C"
CAMLprim LLVMValueRef llvm_build_cmpxchg_native (LLVMValueRef Ptr,
                                                 LLVMValueRef Cmp,
                                                 LLVMValueRef New,
                                                 value SuccOrder,
                                                 value FailOrder,
                                                 value Name,
                                                 value B)
{
    LLVMValueRef Instr =
        LLVMBuildCmpXchg(Builder_val(B),
                         (LLVMAtomicOrdering)Int_val(SuccOrder),
                         (LLVMAtomicOrdering)Int_val(FailOrder),
                         Ptr,
                         Cmp,
                         New);
    LLVMSetValueName(Instr, String_val(Name));
    return Instr;
}

extern "C"
CAMLprim LLVMValueRef llvm_build_cmpxchg_bytecode (value *argv, int argc)
{
    return llvm_build_cmpxchg_native((LLVMValueRef)argv[0],
                                     (LLVMValueRef)argv[1],
                                     (LLVMValueRef)argv[2],
                                     (value)argv[3],
                                     (value)argv[4],
                                     (value)argv[5],
                                     (value)argv[6]);
}
