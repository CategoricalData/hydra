(* The extension to graphs of Hydra's core type system (hydra.core) *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.context hydra.core hydra.errors.
Inductive Graph : Type :=
| Build_Graph : forall (_ : (list) ((prod) (Name) (Term))) , forall (_ : (list) ((prod) (Name) (TypeScheme))) , forall (_ : (list) ((prod) (Name) (TypeVariableMetadata))) , forall (_ : (list) (Name)) , forall (_ : (list) ((prod) (Name) (Term))) , forall (_ : (list) ((prod) (Name) (Primitive))) , forall (_ : (list) ((prod) (Name) (TypeScheme))) , forall (_ : (list) (Name)) , Graph
with Primitive : Type :=
| Build_Primitive : forall (_ : Name) , forall (_ : TypeScheme) , forall (_ : unit) , Primitive.

Definition graph_boundTerms (r_ : Graph) := match r_ with
| Build_Graph f0 f1 f2 f3 f4 f5 f6 f7 => f0
end.

Definition graph_boundTypes (r_ : Graph) := match r_ with
| Build_Graph f0 f1 f2 f3 f4 f5 f6 f7 => f1
end.

Definition graph_classConstraints (r_ : Graph) := match r_ with
| Build_Graph f0 f1 f2 f3 f4 f5 f6 f7 => f2
end.

Definition graph_lambdaVariables (r_ : Graph) := match r_ with
| Build_Graph f0 f1 f2 f3 f4 f5 f6 f7 => f3
end.

Definition graph_metadata (r_ : Graph) := match r_ with
| Build_Graph f0 f1 f2 f3 f4 f5 f6 f7 => f4
end.

Definition graph_primitives (r_ : Graph) := match r_ with
| Build_Graph f0 f1 f2 f3 f4 f5 f6 f7 => f5
end.

Definition graph_schemaTypes (r_ : Graph) := match r_ with
| Build_Graph f0 f1 f2 f3 f4 f5 f6 f7 => f6
end.

Definition graph_typeVariables (r_ : Graph) := match r_ with
| Build_Graph f0 f1 f2 f3 f4 f5 f6 f7 => f7
end.

Definition primitive_name (r_ : Primitive) := match r_ with
| Build_Primitive f0 f1 f2 => f0
end.

Definition primitive_type (r_ : Primitive) := match r_ with
| Build_Primitive f0 f1 f2 => f1
end.

Definition primitive_implementation (r_ : Primitive) := match r_ with
| Build_Primitive f0 f1 f2 => f2
end.

Record TermCoder (a : Type) : Type := Build_TermCoder {
termCoder_type : Type_ ;
termCoder_encode : forall (_ : Context_) , forall (_ : Graph) , forall (_ : Term) , (sum) (Error) (a) ;
termCoder_decode : forall (_ : Context_) , forall (_ : a) , (sum) (Error) (Term) ;
}.

Arguments Build_TermCoder {a}.
Arguments termCoder_type {a}.
Arguments termCoder_encode {a}.
Arguments termCoder_decode {a}.

