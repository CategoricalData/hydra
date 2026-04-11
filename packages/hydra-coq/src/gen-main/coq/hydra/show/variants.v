(* String representations of hydra.variants types *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.variants.

Definition typeVariant : TypeVariant -> string :=
  fun x_ => match x_ with
| TypeVariant_Annotated _ => "annotated"%string
| TypeVariant_Application _ => "application"%string
| TypeVariant_Either _ => "either"%string
| TypeVariant_Forall _ => "forall"%string
| TypeVariant_Function _ => "function"%string
| TypeVariant_List _ => "list"%string
| TypeVariant_Literal _ => "literal"%string
| TypeVariant_Map _ => "map"%string
| TypeVariant_Maybe _ => "maybe"%string
| TypeVariant_Pair _ => "pair"%string
| TypeVariant_Record _ => "record"%string
| TypeVariant_Set _ => "set"%string
| TypeVariant_Union _ => "union"%string
| TypeVariant_Unit _ => "unit"%string
| TypeVariant_Variable _ => "variable"%string
| TypeVariant_Void _ => "void"%string
| TypeVariant_Wrap _ => "wrap"%string
end.
Definition termVariant : TermVariant -> string :=
  fun x_ => match x_ with
| TermVariant_Annotated _ => "annotated"%string
| TermVariant_Application _ => "application"%string
| TermVariant_Either _ => "either"%string
| TermVariant_Function _ => "function"%string
| TermVariant_Let _ => "let"%string
| TermVariant_List _ => "list"%string
| TermVariant_Literal _ => "literal"%string
| TermVariant_Map _ => "map"%string
| TermVariant_Maybe _ => "maybe"%string
| TermVariant_Pair _ => "pair"%string
| TermVariant_Record _ => "record"%string
| TermVariant_Set _ => "set"%string
| TermVariant_TypeLambda _ => "typeLambda"%string
| TermVariant_TypeApplication _ => "typeApplication"%string
| TermVariant_Union _ => "union"%string
| TermVariant_Unit _ => "unit"%string
| TermVariant_Variable _ => "variable"%string
| TermVariant_Wrap _ => "wrap"%string
end.

