import Lean

namespace Macros
namespace Preface

namespace AssertType

elab "#assertType " termStx:term " : " typeStx:term : command =>
  open Lean Lean.Elab Command Term in
    liftTermElabM
      try
        let tp <- elabType typeStx
        discard $ elabTermEnsuringType termStx tp
        synthesizeSyntheticMVarsNoPostponing
        logInfo "NO PROBLEM"
      catch | _ => throwError "OH NO"

#assertType 5 : Nat
#assertType [] : Nat

end AssertType

namespace Arith

inductive Arith : Type where
  | add : Arith -> Arith -> Arith
  | mul : Arith -> Arith -> Arith
  | nat : Nat -> Arith
  | var : String -> Arith

declare_syntax_cat arith
syntax num                        : arith
syntax str                        : arith
syntax ident                      : arith
syntax:50 arith:50 " + " arith:51 : arith
syntax:60 arith:60 " * " arith:61 : arith
syntax " ( " arith " ) "          : arith

syntax " ⟪ " arith " ⟫ " : term

macro_rules
  | `(⟪ $s:str ⟫) => `(Arith.var $s)
  | `(⟪ $s:ident ⟫) => `(Arith.var $s)
  | `(⟪ $n:num ⟫) => `(Arith.nat $n)
  | `(⟪ $x:arith + $y:arith ⟫) => `(Arith.add ⟪ $x ⟫ ⟪ $y ⟫)
  | `(⟪ $x:arith * $y:arith ⟫) => `(Arith.mul ⟪ $x ⟫ ⟪ $y ⟫)
  | `(⟪ ( $x:arith ) ⟫) => `(⟪ $x ⟫)

#check ⟪ 1 + 1 ⟫
#check ⟪ "a" + "b" ⟫
-- #check ⟪ a + a ⟫
-- This is not yet working, how to turn ident into str? 

end Arith

end Preface
end Macros
