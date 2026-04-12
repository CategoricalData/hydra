(* Extraction and validation for hydra.util types *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.graph hydra.core hydra.errors hydra.util hydra.lib.eithers hydra.extract.core hydra.lib.logic hydra.lib.equality.

Definition comparison (t0 : Type) : t0 -> hydra.graph.Graph -> Term -> (sum) (Error) (Comparison) :=
  fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (term_ : Term) => ((eithers.bind) ((((unitVariant) ("Comparison"%string)) (graph_)) (term_))) (fun (fname : Name) => (((logic.ifElse) (((equality.equal) ((fun w_ => w_) (fname))) ("equalTo"%string))) ((inr) ((Comparison_EqualTo) (tt)))) ((((logic.ifElse) (((equality.equal) ((fun w_ => w_) (fname))) ("lessThan"%string))) ((inr) ((Comparison_LessThan) (tt)))) ((((logic.ifElse) (((equality.equal) ((fun w_ => w_) (fname))) ("greaterThan"%string))) ((inr) ((Comparison_GreaterThan) (tt)))) ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("comparison"%string) ((fun w_ => w_) (fname))))))))).
Arguments comparison {t0}.

