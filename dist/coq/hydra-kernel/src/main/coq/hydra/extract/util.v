(* Extraction and validation for hydra.util types *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.errors hydra.extract.core hydra.graph hydra.lib.eithers hydra.lib.equality hydra.lib.logic hydra.util.

Definition comparison (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) (Comparison) := fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (term_ : Term) => ((eithers.bind) ((((unitVariant) ("hydra.util.Comparison"%string)) (graph_)) (term_))) (fun (fname : Name) => (((logic.ifElse) (((equality.equal) ((fun w_ => w_) (fname))) ("equalTo"%string))) ((inr) ((Comparison_EqualTo) (tt)))) ((((logic.ifElse) (((equality.equal) ((fun w_ => w_) (fname))) ("lessThan"%string))) ((inr) ((Comparison_LessThan) (tt)))) ((((logic.ifElse) (((equality.equal) ((fun w_ => w_) (fname))) ("greaterThan"%string))) ((inr) ((Comparison_GreaterThan) (tt)))) ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("comparison"%string) ((fun w_ => w_) (fname))))))))).
Arguments comparison {t0}.

