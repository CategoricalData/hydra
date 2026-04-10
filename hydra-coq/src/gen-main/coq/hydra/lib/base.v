(* Hydra base library: shared axioms for code generation *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.

(* General fixpoint combinator for recursive let bindings.
   Hydra uses general recursion which Coq's termination checker cannot verify.
   This axiom allows recursive definitions to type-check without proving termination. *)
Axiom hydra_fix : forall (X : Type), (X -> X) -> X.
Arguments hydra_fix {X}.

(* Bottom value for partial pattern matches.
   Hydra source code uses partial pattern matching (valid in Haskell);
   Coq requires exhaustive matches. This axiom provides a catch-all value. *)
Axiom hydra_unreachable : forall (X : Type), X.
Arguments hydra_unreachable {X}.


