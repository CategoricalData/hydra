(* Hydra's core data model, consisting of the fundamental hydra.core.Term type and all of its dependencies. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.
Definition Name : Type :=
  string.

Record Projection : Type := Build_Projection {
  projection_typeName : Name ;
  projection_field : Name
}.

Record TypeVariableMetadata : Type := Build_TypeVariableMetadata {
  typeVariableMetadata_classes : (list) (Name)
}.

Inductive IntegerValue : Type :=
| IntegerValue_Bigint : Z -> IntegerValue
| IntegerValue_Int8 : Z -> IntegerValue
| IntegerValue_Int16 : Z -> IntegerValue
| IntegerValue_Int32 : Z -> IntegerValue
| IntegerValue_Int64 : Z -> IntegerValue
| IntegerValue_Uint8 : Z -> IntegerValue
| IntegerValue_Uint16 : Z -> IntegerValue
| IntegerValue_Uint32 : Z -> IntegerValue
| IntegerValue_Uint64 : Z -> IntegerValue.

Inductive IntegerType : Type :=
| IntegerType_Bigint : unit -> IntegerType
| IntegerType_Int8 : unit -> IntegerType
| IntegerType_Int16 : unit -> IntegerType
| IntegerType_Int32 : unit -> IntegerType
| IntegerType_Int64 : unit -> IntegerType
| IntegerType_Uint8 : unit -> IntegerType
| IntegerType_Uint16 : unit -> IntegerType
| IntegerType_Uint32 : unit -> IntegerType
| IntegerType_Uint64 : unit -> IntegerType.

Inductive FloatValue : Type :=
| FloatValue_Bigfloat : Q -> FloatValue
| FloatValue_Float32 : Q -> FloatValue
| FloatValue_Float64 : Q -> FloatValue.

Inductive Literal : Type :=
| Literal_Binary : string -> Literal
| Literal_Boolean : bool -> Literal
| Literal_Float : FloatValue -> Literal
| Literal_Integer : IntegerValue -> Literal
| Literal_String : string -> Literal.

Inductive FloatType : Type :=
| FloatType_Bigfloat : unit -> FloatType
| FloatType_Float32 : unit -> FloatType
| FloatType_Float64 : unit -> FloatType.

Inductive LiteralType : Type :=
| LiteralType_Binary : unit -> LiteralType
| LiteralType_Boolean : unit -> LiteralType
| LiteralType_Float : FloatType -> LiteralType
| LiteralType_Integer : IntegerType -> LiteralType
| LiteralType_String : unit -> LiteralType.

Inductive AnnotatedTerm : Type :=
| Build_AnnotatedTerm : Term -> (list) ((prod) (Name) (Term)) -> AnnotatedTerm
with Term : Type :=
| Term_Annotated : AnnotatedTerm -> Term
| Term_Application : Application -> Term
| Term_Cases : CaseStatement -> Term
| Term_Either : (sum) (Term) (Term) -> Term
| Term_Inject : Injection -> Term
| Term_Lambda : Lambda -> Term
| Term_Let : Let -> Term
| Term_List : (list) (Term) -> Term
| Term_Literal : Literal -> Term
| Term_Map : (list) ((prod) (Term) (Term)) -> Term
| Term_Maybe : (option) (Term) -> Term
| Term_Pair : (prod) (Term) (Term) -> Term
| Term_Project : Projection -> Term
| Term_Record : Record_ -> Term
| Term_Set : (list) (Term) -> Term
| Term_TypeApplication : TypeApplicationTerm -> Term
| Term_TypeLambda : TypeLambda -> Term
| Term_Unit : unit -> Term
| Term_Unwrap : Name -> Term
| Term_Variable : Name -> Term
| Term_Wrap : WrappedTerm -> Term
with Application : Type :=
| Build_Application : Term -> Term -> Application
with CaseStatement : Type :=
| Build_CaseStatement : Name -> (option) (Term) -> (list) (Field) -> CaseStatement
with Field : Type :=
| Build_Field : Name -> Term -> Field
with Injection : Type :=
| Build_Injection : Name -> Field -> Injection
with Lambda : Type :=
| Build_Lambda : Name -> (option) (Type_) -> Term -> Lambda
with Type_ : Type :=
| Type__Annotated : AnnotatedType -> Type_
| Type__Application : ApplicationType -> Type_
| Type__Either : EitherType -> Type_
| Type__Forall : ForallType -> Type_
| Type__Function : FunctionType -> Type_
| Type__List : Type_ -> Type_
| Type__Literal : LiteralType -> Type_
| Type__Map : MapType -> Type_
| Type__Maybe : Type_ -> Type_
| Type__Pair : PairType -> Type_
| Type__Record : (list) (FieldType) -> Type_
| Type__Set : Type_ -> Type_
| Type__Union : (list) (FieldType) -> Type_
| Type__Unit : unit -> Type_
| Type__Variable : Name -> Type_
| Type__Void : unit -> Type_
| Type__Wrap : Type_ -> Type_
with AnnotatedType : Type :=
| Build_AnnotatedType : Type_ -> (list) ((prod) (Name) (Term)) -> AnnotatedType
with ApplicationType : Type :=
| Build_ApplicationType : Type_ -> Type_ -> ApplicationType
with EitherType : Type :=
| Build_EitherType : Type_ -> Type_ -> EitherType
with FieldType : Type :=
| Build_FieldType : Name -> Type_ -> FieldType
with ForallType : Type :=
| Build_ForallType : Name -> Type_ -> ForallType
with FunctionType : Type :=
| Build_FunctionType : Type_ -> Type_ -> FunctionType
with MapType : Type :=
| Build_MapType : Type_ -> Type_ -> MapType
with PairType : Type :=
| Build_PairType : Type_ -> Type_ -> PairType
with Let : Type :=
| Build_Let : (list) (Binding) -> Term -> Let
with Binding : Type :=
| Build_Binding : Name -> Term -> (option) (TypeScheme) -> Binding
with TypeScheme : Type :=
| Build_TypeScheme : (list) (Name) -> Type_ -> (option) ((list) ((prod) (Name) (TypeVariableMetadata))) -> TypeScheme
with Record_ : Type :=
| Build_Record_ : Name -> (list) (Field) -> Record_
with TypeApplicationTerm : Type :=
| Build_TypeApplicationTerm : Term -> Type_ -> TypeApplicationTerm
with TypeLambda : Type :=
| Build_TypeLambda : Name -> Term -> TypeLambda
with WrappedTerm : Type :=
| Build_WrappedTerm : Name -> Term -> WrappedTerm.

Definition annotatedTerm_body (r_ : AnnotatedTerm) :=
  match r_ with
| Build_AnnotatedTerm f0 f1 => f0
end.

Definition annotatedTerm_annotation (r_ : AnnotatedTerm) :=
  match r_ with
| Build_AnnotatedTerm f0 f1 => f1
end.

Definition application_function (r_ : Application) :=
  match r_ with
| Build_Application f0 f1 => f0
end.

Definition application_argument (r_ : Application) :=
  match r_ with
| Build_Application f0 f1 => f1
end.

Definition caseStatement_typeName (r_ : CaseStatement) :=
  match r_ with
| Build_CaseStatement f0 f1 f2 => f0
end.

Definition caseStatement_default (r_ : CaseStatement) :=
  match r_ with
| Build_CaseStatement f0 f1 f2 => f1
end.

Definition caseStatement_cases (r_ : CaseStatement) :=
  match r_ with
| Build_CaseStatement f0 f1 f2 => f2
end.

Definition field_name (r_ : Field) :=
  match r_ with
| Build_Field f0 f1 => f0
end.

Definition field_term (r_ : Field) :=
  match r_ with
| Build_Field f0 f1 => f1
end.

Definition injection_typeName (r_ : Injection) :=
  match r_ with
| Build_Injection f0 f1 => f0
end.

Definition injection_field (r_ : Injection) :=
  match r_ with
| Build_Injection f0 f1 => f1
end.

Definition lambda_parameter (r_ : Lambda) :=
  match r_ with
| Build_Lambda f0 f1 f2 => f0
end.

Definition lambda_domain (r_ : Lambda) :=
  match r_ with
| Build_Lambda f0 f1 f2 => f1
end.

Definition lambda_body (r_ : Lambda) :=
  match r_ with
| Build_Lambda f0 f1 f2 => f2
end.

Definition annotatedType_body (r_ : AnnotatedType) :=
  match r_ with
| Build_AnnotatedType f0 f1 => f0
end.

Definition annotatedType_annotation (r_ : AnnotatedType) :=
  match r_ with
| Build_AnnotatedType f0 f1 => f1
end.

Definition applicationType_function (r_ : ApplicationType) :=
  match r_ with
| Build_ApplicationType f0 f1 => f0
end.

Definition applicationType_argument (r_ : ApplicationType) :=
  match r_ with
| Build_ApplicationType f0 f1 => f1
end.

Definition eitherType_left (r_ : EitherType) :=
  match r_ with
| Build_EitherType f0 f1 => f0
end.

Definition eitherType_right (r_ : EitherType) :=
  match r_ with
| Build_EitherType f0 f1 => f1
end.

Definition fieldType_name (r_ : FieldType) :=
  match r_ with
| Build_FieldType f0 f1 => f0
end.

Definition fieldType_type (r_ : FieldType) :=
  match r_ with
| Build_FieldType f0 f1 => f1
end.

Definition forallType_parameter (r_ : ForallType) :=
  match r_ with
| Build_ForallType f0 f1 => f0
end.

Definition forallType_body (r_ : ForallType) :=
  match r_ with
| Build_ForallType f0 f1 => f1
end.

Definition functionType_domain (r_ : FunctionType) :=
  match r_ with
| Build_FunctionType f0 f1 => f0
end.

Definition functionType_codomain (r_ : FunctionType) :=
  match r_ with
| Build_FunctionType f0 f1 => f1
end.

Definition mapType_keys (r_ : MapType) :=
  match r_ with
| Build_MapType f0 f1 => f0
end.

Definition mapType_values (r_ : MapType) :=
  match r_ with
| Build_MapType f0 f1 => f1
end.

Definition pairType_first (r_ : PairType) :=
  match r_ with
| Build_PairType f0 f1 => f0
end.

Definition pairType_second (r_ : PairType) :=
  match r_ with
| Build_PairType f0 f1 => f1
end.

Definition let_bindings (r_ : Let) :=
  match r_ with
| Build_Let f0 f1 => f0
end.

Definition let_body (r_ : Let) :=
  match r_ with
| Build_Let f0 f1 => f1
end.

Definition binding_name (r_ : Binding) :=
  match r_ with
| Build_Binding f0 f1 f2 => f0
end.

Definition binding_term (r_ : Binding) :=
  match r_ with
| Build_Binding f0 f1 f2 => f1
end.

Definition binding_type (r_ : Binding) :=
  match r_ with
| Build_Binding f0 f1 f2 => f2
end.

Definition typeScheme_variables (r_ : TypeScheme) :=
  match r_ with
| Build_TypeScheme f0 f1 f2 => f0
end.

Definition typeScheme_type (r_ : TypeScheme) :=
  match r_ with
| Build_TypeScheme f0 f1 f2 => f1
end.

Definition typeScheme_constraints (r_ : TypeScheme) :=
  match r_ with
| Build_TypeScheme f0 f1 f2 => f2
end.

Definition record__typeName (r_ : Record_) :=
  match r_ with
| Build_Record_ f0 f1 => f0
end.

Definition record__fields (r_ : Record_) :=
  match r_ with
| Build_Record_ f0 f1 => f1
end.

Definition typeApplicationTerm_body (r_ : TypeApplicationTerm) :=
  match r_ with
| Build_TypeApplicationTerm f0 f1 => f0
end.

Definition typeApplicationTerm_type (r_ : TypeApplicationTerm) :=
  match r_ with
| Build_TypeApplicationTerm f0 f1 => f1
end.

Definition typeLambda_parameter (r_ : TypeLambda) :=
  match r_ with
| Build_TypeLambda f0 f1 => f0
end.

Definition typeLambda_body (r_ : TypeLambda) :=
  match r_ with
| Build_TypeLambda f0 f1 => f1
end.

Definition wrappedTerm_typeName (r_ : WrappedTerm) :=
  match r_ with
| Build_WrappedTerm f0 f1 => f0
end.

Definition wrappedTerm_body (r_ : WrappedTerm) :=
  match r_ with
| Build_WrappedTerm f0 f1 => f1
end.

