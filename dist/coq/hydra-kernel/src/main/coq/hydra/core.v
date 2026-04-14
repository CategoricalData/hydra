(* Hydra's core data model, consisting of the fundamental hydra.core.Term type and all of its dependencies. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.
Definition Name : Type := string.

Record Projection : Type := Build_Projection {
projection_typeName : Name ;
projection_field : Name ;
}.

Record TypeVariableMetadata : Type := Build_TypeVariableMetadata {
typeVariableMetadata_classes : (list) (Name) ;
}.

Inductive IntegerValue : Type :=
| IntegerValue_Bigint : forall (_ : Z) , IntegerValue
| IntegerValue_Int8 : forall (_ : Z) , IntegerValue
| IntegerValue_Int16 : forall (_ : Z) , IntegerValue
| IntegerValue_Int32 : forall (_ : Z) , IntegerValue
| IntegerValue_Int64 : forall (_ : Z) , IntegerValue
| IntegerValue_Uint8 : forall (_ : Z) , IntegerValue
| IntegerValue_Uint16 : forall (_ : Z) , IntegerValue
| IntegerValue_Uint32 : forall (_ : Z) , IntegerValue
| IntegerValue_Uint64 : forall (_ : Z) , IntegerValue.

Inductive IntegerType : Type :=
| IntegerType_Bigint : forall (_ : unit) , IntegerType
| IntegerType_Int8 : forall (_ : unit) , IntegerType
| IntegerType_Int16 : forall (_ : unit) , IntegerType
| IntegerType_Int32 : forall (_ : unit) , IntegerType
| IntegerType_Int64 : forall (_ : unit) , IntegerType
| IntegerType_Uint8 : forall (_ : unit) , IntegerType
| IntegerType_Uint16 : forall (_ : unit) , IntegerType
| IntegerType_Uint32 : forall (_ : unit) , IntegerType
| IntegerType_Uint64 : forall (_ : unit) , IntegerType.

Inductive FloatValue : Type :=
| FloatValue_Bigfloat : forall (_ : Q) , FloatValue
| FloatValue_Float32 : forall (_ : Q) , FloatValue
| FloatValue_Float64 : forall (_ : Q) , FloatValue.

Inductive Literal : Type :=
| Literal_Binary : forall (_ : string) , Literal
| Literal_Boolean : forall (_ : bool) , Literal
| Literal_Float : forall (_ : FloatValue) , Literal
| Literal_Integer : forall (_ : IntegerValue) , Literal
| Literal_String : forall (_ : string) , Literal.

Inductive FloatType : Type :=
| FloatType_Bigfloat : forall (_ : unit) , FloatType
| FloatType_Float32 : forall (_ : unit) , FloatType
| FloatType_Float64 : forall (_ : unit) , FloatType.

Inductive LiteralType : Type :=
| LiteralType_Binary : forall (_ : unit) , LiteralType
| LiteralType_Boolean : forall (_ : unit) , LiteralType
| LiteralType_Float : forall (_ : FloatType) , LiteralType
| LiteralType_Integer : forall (_ : IntegerType) , LiteralType
| LiteralType_String : forall (_ : unit) , LiteralType.

Inductive AnnotatedTerm : Type :=
| Build_AnnotatedTerm : forall (_ : Term) , forall (_ : (list) ((prod) (Name) (Term))) , AnnotatedTerm
with Term : Type :=
| Term_Annotated : forall (_ : AnnotatedTerm) , Term
| Term_Application : forall (_ : Application) , Term
| Term_Cases : forall (_ : CaseStatement) , Term
| Term_Either : forall (_ : (sum) (Term) (Term)) , Term
| Term_Inject : forall (_ : Injection) , Term
| Term_Lambda : forall (_ : Lambda) , Term
| Term_Let : forall (_ : Let) , Term
| Term_List : forall (_ : (list) (Term)) , Term
| Term_Literal : forall (_ : Literal) , Term
| Term_Map : forall (_ : (list) ((prod) (Term) (Term))) , Term
| Term_Maybe : forall (_ : (option) (Term)) , Term
| Term_Pair : forall (_ : (prod) (Term) (Term)) , Term
| Term_Project : forall (_ : Projection) , Term
| Term_Record : forall (_ : Record_) , Term
| Term_Set : forall (_ : (list) (Term)) , Term
| Term_TypeApplication : forall (_ : TypeApplicationTerm) , Term
| Term_TypeLambda : forall (_ : TypeLambda) , Term
| Term_Unit : forall (_ : unit) , Term
| Term_Unwrap : forall (_ : Name) , Term
| Term_Variable : forall (_ : Name) , Term
| Term_Wrap : forall (_ : WrappedTerm) , Term
with Application : Type :=
| Build_Application : forall (_ : Term) , forall (_ : Term) , Application
with CaseStatement : Type :=
| Build_CaseStatement : forall (_ : Name) , forall (_ : (option) (Term)) , forall (_ : (list) (Field)) , CaseStatement
with Field : Type :=
| Build_Field : forall (_ : Name) , forall (_ : Term) , Field
with Injection : Type :=
| Build_Injection : forall (_ : Name) , forall (_ : Field) , Injection
with Lambda : Type :=
| Build_Lambda : forall (_ : Name) , forall (_ : (option) (Type_)) , forall (_ : Term) , Lambda
with Type_ : Type :=
| Type__Annotated : forall (_ : AnnotatedType) , Type_
| Type__Application : forall (_ : ApplicationType) , Type_
| Type__Either : forall (_ : EitherType) , Type_
| Type__Forall : forall (_ : ForallType) , Type_
| Type__Function : forall (_ : FunctionType) , Type_
| Type__List : forall (_ : Type_) , Type_
| Type__Literal : forall (_ : LiteralType) , Type_
| Type__Map : forall (_ : MapType) , Type_
| Type__Maybe : forall (_ : Type_) , Type_
| Type__Pair : forall (_ : PairType) , Type_
| Type__Record : forall (_ : (list) (FieldType)) , Type_
| Type__Set : forall (_ : Type_) , Type_
| Type__Union : forall (_ : (list) (FieldType)) , Type_
| Type__Unit : forall (_ : unit) , Type_
| Type__Variable : forall (_ : Name) , Type_
| Type__Void : forall (_ : unit) , Type_
| Type__Wrap : forall (_ : Type_) , Type_
with AnnotatedType : Type :=
| Build_AnnotatedType : forall (_ : Type_) , forall (_ : (list) ((prod) (Name) (Term))) , AnnotatedType
with ApplicationType : Type :=
| Build_ApplicationType : forall (_ : Type_) , forall (_ : Type_) , ApplicationType
with EitherType : Type :=
| Build_EitherType : forall (_ : Type_) , forall (_ : Type_) , EitherType
with FieldType : Type :=
| Build_FieldType : forall (_ : Name) , forall (_ : Type_) , FieldType
with ForallType : Type :=
| Build_ForallType : forall (_ : Name) , forall (_ : Type_) , ForallType
with FunctionType : Type :=
| Build_FunctionType : forall (_ : Type_) , forall (_ : Type_) , FunctionType
with MapType : Type :=
| Build_MapType : forall (_ : Type_) , forall (_ : Type_) , MapType
with PairType : Type :=
| Build_PairType : forall (_ : Type_) , forall (_ : Type_) , PairType
with Let : Type :=
| Build_Let : forall (_ : (list) (Binding)) , forall (_ : Term) , Let
with Binding : Type :=
| Build_Binding : forall (_ : Name) , forall (_ : Term) , forall (_ : (option) (TypeScheme)) , Binding
with TypeScheme : Type :=
| Build_TypeScheme : forall (_ : (list) (Name)) , forall (_ : Type_) , forall (_ : (option) ((list) ((prod) (Name) (TypeVariableMetadata)))) , TypeScheme
with Record_ : Type :=
| Build_Record_ : forall (_ : Name) , forall (_ : (list) (Field)) , Record_
with TypeApplicationTerm : Type :=
| Build_TypeApplicationTerm : forall (_ : Term) , forall (_ : Type_) , TypeApplicationTerm
with TypeLambda : Type :=
| Build_TypeLambda : forall (_ : Name) , forall (_ : Term) , TypeLambda
with WrappedTerm : Type :=
| Build_WrappedTerm : forall (_ : Name) , forall (_ : Term) , WrappedTerm.

Definition annotatedTerm_body (r_ : AnnotatedTerm) := match r_ with
| Build_AnnotatedTerm f0 f1 => f0
end.

Definition annotatedTerm_annotation (r_ : AnnotatedTerm) := match r_ with
| Build_AnnotatedTerm f0 f1 => f1
end.

Definition application_function (r_ : Application) := match r_ with
| Build_Application f0 f1 => f0
end.

Definition application_argument (r_ : Application) := match r_ with
| Build_Application f0 f1 => f1
end.

Definition caseStatement_typeName (r_ : CaseStatement) := match r_ with
| Build_CaseStatement f0 f1 f2 => f0
end.

Definition caseStatement_default (r_ : CaseStatement) := match r_ with
| Build_CaseStatement f0 f1 f2 => f1
end.

Definition caseStatement_cases (r_ : CaseStatement) := match r_ with
| Build_CaseStatement f0 f1 f2 => f2
end.

Definition field_name (r_ : Field) := match r_ with
| Build_Field f0 f1 => f0
end.

Definition field_term (r_ : Field) := match r_ with
| Build_Field f0 f1 => f1
end.

Definition injection_typeName (r_ : Injection) := match r_ with
| Build_Injection f0 f1 => f0
end.

Definition injection_field (r_ : Injection) := match r_ with
| Build_Injection f0 f1 => f1
end.

Definition lambda_parameter (r_ : Lambda) := match r_ with
| Build_Lambda f0 f1 f2 => f0
end.

Definition lambda_domain (r_ : Lambda) := match r_ with
| Build_Lambda f0 f1 f2 => f1
end.

Definition lambda_body (r_ : Lambda) := match r_ with
| Build_Lambda f0 f1 f2 => f2
end.

Definition annotatedType_body (r_ : AnnotatedType) := match r_ with
| Build_AnnotatedType f0 f1 => f0
end.

Definition annotatedType_annotation (r_ : AnnotatedType) := match r_ with
| Build_AnnotatedType f0 f1 => f1
end.

Definition applicationType_function (r_ : ApplicationType) := match r_ with
| Build_ApplicationType f0 f1 => f0
end.

Definition applicationType_argument (r_ : ApplicationType) := match r_ with
| Build_ApplicationType f0 f1 => f1
end.

Definition eitherType_left (r_ : EitherType) := match r_ with
| Build_EitherType f0 f1 => f0
end.

Definition eitherType_right (r_ : EitherType) := match r_ with
| Build_EitherType f0 f1 => f1
end.

Definition fieldType_name (r_ : FieldType) := match r_ with
| Build_FieldType f0 f1 => f0
end.

Definition fieldType_type (r_ : FieldType) := match r_ with
| Build_FieldType f0 f1 => f1
end.

Definition forallType_parameter (r_ : ForallType) := match r_ with
| Build_ForallType f0 f1 => f0
end.

Definition forallType_body (r_ : ForallType) := match r_ with
| Build_ForallType f0 f1 => f1
end.

Definition functionType_domain (r_ : FunctionType) := match r_ with
| Build_FunctionType f0 f1 => f0
end.

Definition functionType_codomain (r_ : FunctionType) := match r_ with
| Build_FunctionType f0 f1 => f1
end.

Definition mapType_keys (r_ : MapType) := match r_ with
| Build_MapType f0 f1 => f0
end.

Definition mapType_values (r_ : MapType) := match r_ with
| Build_MapType f0 f1 => f1
end.

Definition pairType_first (r_ : PairType) := match r_ with
| Build_PairType f0 f1 => f0
end.

Definition pairType_second (r_ : PairType) := match r_ with
| Build_PairType f0 f1 => f1
end.

Definition let_bindings (r_ : Let) := match r_ with
| Build_Let f0 f1 => f0
end.

Definition let_body (r_ : Let) := match r_ with
| Build_Let f0 f1 => f1
end.

Definition binding_name (r_ : Binding) := match r_ with
| Build_Binding f0 f1 f2 => f0
end.

Definition binding_term (r_ : Binding) := match r_ with
| Build_Binding f0 f1 f2 => f1
end.

Definition binding_type (r_ : Binding) := match r_ with
| Build_Binding f0 f1 f2 => f2
end.

Definition typeScheme_variables (r_ : TypeScheme) := match r_ with
| Build_TypeScheme f0 f1 f2 => f0
end.

Definition typeScheme_type (r_ : TypeScheme) := match r_ with
| Build_TypeScheme f0 f1 f2 => f1
end.

Definition typeScheme_constraints (r_ : TypeScheme) := match r_ with
| Build_TypeScheme f0 f1 f2 => f2
end.

Definition record__typeName (r_ : Record_) := match r_ with
| Build_Record_ f0 f1 => f0
end.

Definition record__fields (r_ : Record_) := match r_ with
| Build_Record_ f0 f1 => f1
end.

Definition typeApplicationTerm_body (r_ : TypeApplicationTerm) := match r_ with
| Build_TypeApplicationTerm f0 f1 => f0
end.

Definition typeApplicationTerm_type (r_ : TypeApplicationTerm) := match r_ with
| Build_TypeApplicationTerm f0 f1 => f1
end.

Definition typeLambda_parameter (r_ : TypeLambda) := match r_ with
| Build_TypeLambda f0 f1 => f0
end.

Definition typeLambda_body (r_ : TypeLambda) := match r_ with
| Build_TypeLambda f0 f1 => f1
end.

Definition wrappedTerm_typeName (r_ : WrappedTerm) := match r_ with
| Build_WrappedTerm f0 f1 => f0
end.

Definition wrappedTerm_body (r_ : WrappedTerm) := match r_ with
| Build_WrappedTerm f0 f1 => f1
end.

