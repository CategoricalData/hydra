(* Term encoders for hydra.variants *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.variants.

Definition eliminationVariant : forall (_ : EliminationVariant) , Term := fun x_ => match x_ with
| EliminationVariant_Record v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.EliminationVariant"%string) ((Build_Field) ("record"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| EliminationVariant_Union v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.EliminationVariant"%string) ((Build_Field) ("union"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| EliminationVariant_Wrap v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.EliminationVariant"%string) ((Build_Field) ("wrap"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.
Definition functionVariant : forall (_ : FunctionVariant) , Term := fun x_ => match x_ with
| FunctionVariant_Elimination v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.FunctionVariant"%string) ((Build_Field) ("elimination"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| FunctionVariant_Lambda v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.FunctionVariant"%string) ((Build_Field) ("lambda"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.
Definition literalVariant : forall (_ : LiteralVariant) , Term := fun x_ => match x_ with
| LiteralVariant_Binary v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.LiteralVariant"%string) ((Build_Field) ("binary"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| LiteralVariant_Boolean v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.LiteralVariant"%string) ((Build_Field) ("boolean"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| LiteralVariant_Decimal v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.LiteralVariant"%string) ((Build_Field) ("decimal"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| LiteralVariant_Float v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.LiteralVariant"%string) ((Build_Field) ("float"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| LiteralVariant_Integer v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.LiteralVariant"%string) ((Build_Field) ("integer"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| LiteralVariant_String v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.LiteralVariant"%string) ((Build_Field) ("string"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.
Definition termVariant : forall (_ : TermVariant) , Term := fun x_ => match x_ with
| TermVariant_Annotated v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("annotated"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_Application v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("application"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_Cases v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("cases"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_Either v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("either"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_Inject v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("inject"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_Lambda v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("lambda"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_Let v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("let"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_List v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("list"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_Literal v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("literal"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_Map v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("map"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_Maybe v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("maybe"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_Pair v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("pair"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_Project v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("project"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_Record v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("record"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_Set v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("set"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_TypeApplication v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("typeApplication"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_TypeLambda v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("typeLambda"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_Unit v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("unit"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_Unwrap v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("unwrap"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_Variable v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("variable"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TermVariant_Wrap v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TermVariant"%string) ((Build_Field) ("wrap"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.
Definition typeVariant : forall (_ : TypeVariant) , Term := fun x_ => match x_ with
| TypeVariant_Annotated v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TypeVariant"%string) ((Build_Field) ("annotated"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TypeVariant_Application v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TypeVariant"%string) ((Build_Field) ("application"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TypeVariant_Either v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TypeVariant"%string) ((Build_Field) ("either"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TypeVariant_Forall v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TypeVariant"%string) ((Build_Field) ("forall"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TypeVariant_Function v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TypeVariant"%string) ((Build_Field) ("function"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TypeVariant_List v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TypeVariant"%string) ((Build_Field) ("list"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TypeVariant_Literal v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TypeVariant"%string) ((Build_Field) ("literal"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TypeVariant_Map v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TypeVariant"%string) ((Build_Field) ("map"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TypeVariant_Maybe v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TypeVariant"%string) ((Build_Field) ("maybe"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TypeVariant_Pair v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TypeVariant"%string) ((Build_Field) ("pair"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TypeVariant_Record v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TypeVariant"%string) ((Build_Field) ("record"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TypeVariant_Set v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TypeVariant"%string) ((Build_Field) ("set"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TypeVariant_Union v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TypeVariant"%string) ((Build_Field) ("union"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TypeVariant_Unit v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TypeVariant"%string) ((Build_Field) ("unit"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TypeVariant_Variable v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TypeVariant"%string) ((Build_Field) ("variable"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TypeVariant_Void v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TypeVariant"%string) ((Build_Field) ("void"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TypeVariant_Wrap v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("hydra.variants.TypeVariant"%string) ((Build_Field) ("wrap"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.

