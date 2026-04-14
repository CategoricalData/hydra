(* Term encoders for hydra.paths *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.paths hydra.core hydra.encode.core hydra.lib.lists.

Definition subtypeStep : SubtypeStep -> Term :=
  fun x_ => match x_ with
| SubtypeStep_AnnotatedBody v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("annotatedBody"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtypeStep_ApplicationFunction v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("applicationFunction"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtypeStep_ApplicationArgument v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("applicationArgument"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtypeStep_EitherLeft v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("eitherLeft"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtypeStep_EitherRight v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("eitherRight"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtypeStep_ForallBody v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("forallBody"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtypeStep_FunctionDomain v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("functionDomain"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtypeStep_FunctionCodomain v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("functionCodomain"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtypeStep_ListElement v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("listElement"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtypeStep_MapKeys v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("mapKeys"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtypeStep_MapValues v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("mapValues"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtypeStep_MaybeElement v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("maybeElement"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtypeStep_PairFirst v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("pairFirst"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtypeStep_PairSecond v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("pairSecond"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtypeStep_RecordField v_ => (fun (y : Name) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("recordField"%string) ((hydra.encode.core.name) (y))))) (v_)
| SubtypeStep_SetElement v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("setElement"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtypeStep_UnionField v_ => (fun (y : Name) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("unionField"%string) ((hydra.encode.core.name) (y))))) (v_)
| SubtypeStep_WrappedType v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtypeStep"%string) ((Build_Field) ("wrappedType"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.
Definition subtypePath : SubtypePath -> Term :=
  fun (x : SubtypePath) => (Term_Wrap) ((Build_WrappedTerm) ("SubtypePath"%string) ((fun (xs : (list) (SubtypeStep)) => (Term_List) (((lists.map) (subtypeStep)) (xs))) ((fun w_ => w_) (x)))).
Definition subtypeNode : SubtypeNode -> Term :=
  fun (x : SubtypeNode) => (Term_Record) ((Build_Record_) ("SubtypeNode"%string) ((cons) ((Build_Field) ("name"%string) ((hydra.encode.core.name) ((fun r_ => (subtypeNode_name) (r_)) (x)))) ((cons) ((Build_Field) ("label"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (subtypeNode_label) (r_)) (x)))) ((cons) ((Build_Field) ("id"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (subtypeNode_id) (r_)) (x)))) (nil))))).
Definition subtypeEdge : SubtypeEdge -> Term :=
  fun (x : SubtypeEdge) => (Term_Record) ((Build_Record_) ("SubtypeEdge"%string) ((cons) ((Build_Field) ("source"%string) ((subtypeNode) ((fun r_ => (subtypeEdge_source) (r_)) (x)))) ((cons) ((Build_Field) ("path"%string) ((subtypePath) ((fun r_ => (subtypeEdge_path) (r_)) (x)))) ((cons) ((Build_Field) ("target"%string) ((subtypeNode) ((fun r_ => (subtypeEdge_target) (r_)) (x)))) (nil))))).
Definition subtypeGraph : SubtypeGraph -> Term :=
  fun (x : SubtypeGraph) => (Term_Record) ((Build_Record_) ("SubtypeGraph"%string) ((cons) ((Build_Field) ("nodes"%string) ((fun (xs : (list) (SubtypeNode)) => (Term_List) (((lists.map) (subtypeNode)) (xs))) ((fun r_ => (subtypeGraph_nodes) (r_)) (x)))) ((cons) ((Build_Field) ("edges"%string) ((fun (xs : (list) (SubtypeEdge)) => (Term_List) (((lists.map) (subtypeEdge)) (xs))) ((fun r_ => (subtypeGraph_edges) (r_)) (x)))) (nil)))).
Definition subtermStep : SubtermStep -> Term :=
  fun x_ => match x_ with
| SubtermStep_AnnotatedBody v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("annotatedBody"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtermStep_ApplicationFunction v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("applicationFunction"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtermStep_ApplicationArgument v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("applicationArgument"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtermStep_LambdaBody v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("lambdaBody"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtermStep_UnionCasesDefault v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("unionCasesDefault"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtermStep_UnionCasesBranch v_ => (fun (y : Name) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("unionCasesBranch"%string) ((hydra.encode.core.name) (y))))) (v_)
| SubtermStep_LetBody v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("letBody"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtermStep_LetBinding v_ => (fun (y : Name) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("letBinding"%string) ((hydra.encode.core.name) (y))))) (v_)
| SubtermStep_ListElement v_ => (fun (y : Z) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("listElement"%string) ((fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x)))) (y))))) (v_)
| SubtermStep_MapKey v_ => (fun (y : Z) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("mapKey"%string) ((fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x)))) (y))))) (v_)
| SubtermStep_MapValue v_ => (fun (y : Z) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("mapValue"%string) ((fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x)))) (y))))) (v_)
| SubtermStep_MaybeTerm v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("maybeTerm"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtermStep_ProductTerm v_ => (fun (y : Z) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("productTerm"%string) ((fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x)))) (y))))) (v_)
| SubtermStep_RecordField v_ => (fun (y : Name) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("recordField"%string) ((hydra.encode.core.name) (y))))) (v_)
| SubtermStep_SetElement v_ => (fun (y : Z) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("setElement"%string) ((fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x)))) (y))))) (v_)
| SubtermStep_SumTerm v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("sumTerm"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtermStep_TypeLambdaBody v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("typeLambdaBody"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtermStep_TypeApplicationTerm v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("typeApplicationTerm"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtermStep_InjectionTerm v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("injectionTerm"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| SubtermStep_WrappedTerm v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("SubtermStep"%string) ((Build_Field) ("wrappedTerm"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.
Definition subtermPath : SubtermPath -> Term :=
  fun (x : SubtermPath) => (Term_Wrap) ((Build_WrappedTerm) ("SubtermPath"%string) ((fun (xs : (list) (SubtermStep)) => (Term_List) (((lists.map) (subtermStep)) (xs))) ((fun w_ => w_) (x)))).
Definition subtermNode : SubtermNode -> Term :=
  fun (x : SubtermNode) => (Term_Record) ((Build_Record_) ("SubtermNode"%string) ((cons) ((Build_Field) ("name"%string) ((hydra.encode.core.name) ((fun r_ => (subtermNode_name) (r_)) (x)))) ((cons) ((Build_Field) ("label"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (subtermNode_label) (r_)) (x)))) ((cons) ((Build_Field) ("id"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (subtermNode_id) (r_)) (x)))) (nil))))).
Definition subtermEdge : SubtermEdge -> Term :=
  fun (x : SubtermEdge) => (Term_Record) ((Build_Record_) ("SubtermEdge"%string) ((cons) ((Build_Field) ("source"%string) ((subtermNode) ((fun r_ => (subtermEdge_source) (r_)) (x)))) ((cons) ((Build_Field) ("path"%string) ((subtermPath) ((fun r_ => (subtermEdge_path) (r_)) (x)))) ((cons) ((Build_Field) ("target"%string) ((subtermNode) ((fun r_ => (subtermEdge_target) (r_)) (x)))) (nil))))).
Definition subtermGraph : SubtermGraph -> Term :=
  fun (x : SubtermGraph) => (Term_Record) ((Build_Record_) ("SubtermGraph"%string) ((cons) ((Build_Field) ("nodes"%string) ((fun (xs : (list) (SubtermNode)) => (Term_List) (((lists.map) (subtermNode)) (xs))) ((fun r_ => (subtermGraph_nodes) (r_)) (x)))) ((cons) ((Build_Field) ("edges"%string) ((fun (xs : (list) (SubtermEdge)) => (Term_List) (((lists.map) (subtermEdge)) (xs))) ((fun r_ => (subtermGraph_edges) (r_)) (x)))) (nil)))).

