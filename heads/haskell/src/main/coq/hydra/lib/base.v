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

(* Universal equality. Hydra's type system has no Eq class, so lib primitives
   like `elem`/`nub`/`group` must compare values of unconstrained type. Coq
   cannot provide this for arbitrary types, so it is declared as an axiom.
   Tests that need concrete equality should specialise this via a rewrite
   lemma for the specific element type. *)
Axiom hydra_eq : forall (X : Type), X -> X -> bool.
Arguments hydra_eq {X}.

(* Universal comparison. Same story as `hydra_eq`, used by `sort` and
   `sortOn`. Returns a Coq `comparison` (`Lt`/`Eq`/`Gt`). *)
Axiom hydra_compare : forall (X : Type), X -> X -> comparison.
Arguments hydra_compare {X}.

(* IEEE 754 special float values. Coq's Q (rationals) cannot represent these,
   so they are declared as opaque axioms. Tests that exercise them stall at
   the axiom — matching the behaviour of `hydra_eq` and `hydra_compare` — but
   the enclosing code type-checks. *)
Axiom hydra_posInf : Q.
Axiom hydra_negInf : Q.
Axiom hydra_nan : Q.


