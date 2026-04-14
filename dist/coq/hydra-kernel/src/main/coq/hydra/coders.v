(* Abstractions for paired transformations between languages *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.variants hydra.core hydra.context hydra.errors hydra.graph.
Inductive TraversalOrder : Type :=
| TraversalOrder_Pre : forall (_ : unit) , TraversalOrder
| TraversalOrder_Post : forall (_ : unit) , TraversalOrder.

Definition LanguageName : Type := string.

Record LanguageConstraints : Type := Build_LanguageConstraints {
languageConstraints_eliminationVariants : (list) (EliminationVariant) ;
languageConstraints_literalVariants : (list) (LiteralVariant) ;
languageConstraints_floatTypes : (list) (FloatType) ;
languageConstraints_functionVariants : (list) (FunctionVariant) ;
languageConstraints_integerTypes : (list) (IntegerType) ;
languageConstraints_termVariants : (list) (TermVariant) ;
languageConstraints_typeVariants : (list) (TypeVariant) ;
languageConstraints_types : forall (_ : Type_) , bool ;
}.

Record Language : Type := Build_Language {
language_name : LanguageName ;
language_constraints : LanguageConstraints ;
}.

Inductive CoderDirection : Type :=
| CoderDirection_Encode : forall (_ : unit) , CoderDirection
| CoderDirection_Decode : forall (_ : unit) , CoderDirection.

Record Coder (v1 : Type) (v2 : Type) : Type := Build_Coder {
coder_encode : forall (_ : Context_) , forall (_ : v1) , (sum) (Error) (v2) ;
coder_decode : forall (_ : Context_) , forall (_ : v2) , (sum) (Error) (v1) ;
}.

Record Adapter (t1 : Type) (t2 : Type) (v1 : Type) (v2 : Type) : Type := Build_Adapter {
adapter_isLossy : bool ;
adapter_source : t1 ;
adapter_target : t2 ;
adapter_coder : ((Coder) (v1)) (v2) ;
}.

Record AdapterContext : Type := Build_AdapterContext {
adapterContext_graph_ : hydra.graph.Graph ;
adapterContext_language : Language ;
adapterContext_adapters : (list) ((prod) (Name) (((((Adapter) (Type_)) (Type_)) (Term)) (Term))) ;
}.

Record Bicoder (t1 : Type) (t2 : Type) (v1 : Type) (v2 : Type) : Type := Build_Bicoder {
bicoder_encode : forall (_ : t1) , ((((Adapter) (t1)) (t2)) (v1)) (v2) ;
bicoder_decode : forall (_ : t2) , ((((Adapter) (t2)) (t1)) (v2)) (v1) ;
}.

Definition SymmetricAdapter (t : Type) (v : Type) : Type := ((((Adapter) (t)) (t)) (v)) (v).

Definition TypeAdapter : Type := forall (_ : AdapterContext) , forall (_ : Type_) , (sum) (string) (((SymmetricAdapter) (Type_)) (Term)).

Arguments Build_Adapter {t1} {t2} {v1} {v2}.
Arguments adapter_isLossy {t1} {t2} {v1} {v2}.
Arguments adapter_source {t1} {t2} {v1} {v2}.
Arguments adapter_target {t1} {t2} {v1} {v2}.
Arguments adapter_coder {t1} {t2} {v1} {v2}.
Arguments Build_Bicoder {t1} {t2} {v1} {v2}.
Arguments bicoder_encode {t1} {t2} {v1} {v2}.
Arguments bicoder_decode {t1} {t2} {v1} {v2}.
Arguments Build_Coder {v1} {v2}.
Arguments coder_encode {v1} {v2}.
Arguments coder_decode {v1} {v2}.

