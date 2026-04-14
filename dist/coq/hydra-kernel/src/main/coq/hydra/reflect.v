(* Reflection functions for working with term, type, and literal type variants, as well as numeric precision. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.variants hydra.core hydra.util hydra.lib.lists.

Definition typeVariants : (list) (TypeVariant) :=
  (cons) ((TypeVariant_Annotated) (tt)) ((cons) ((TypeVariant_Application) (tt)) ((cons) ((TypeVariant_Either) (tt)) ((cons) ((TypeVariant_Function) (tt)) ((cons) ((TypeVariant_Forall) (tt)) ((cons) ((TypeVariant_List) (tt)) ((cons) ((TypeVariant_Literal) (tt)) ((cons) ((TypeVariant_Map) (tt)) ((cons) ((TypeVariant_Wrap) (tt)) ((cons) ((TypeVariant_Maybe) (tt)) ((cons) ((TypeVariant_Pair) (tt)) ((cons) ((TypeVariant_Record) (tt)) ((cons) ((TypeVariant_Set) (tt)) ((cons) ((TypeVariant_Union) (tt)) ((cons) ((TypeVariant_Unit) (tt)) ((cons) ((TypeVariant_Variable) (tt)) ((cons) ((TypeVariant_Void) (tt)) (nil))))))))))))))))).
Definition typeVariant : Type_ -> TypeVariant :=
  fun x_ => match x_ with
| Type__Annotated v_ => (fun (_ : AnnotatedType) => (TypeVariant_Annotated) (tt)) (v_)
| Type__Application v_ => (fun (_ : ApplicationType) => (TypeVariant_Application) (tt)) (v_)
| Type__Either v_ => (fun (_ : EitherType) => (TypeVariant_Either) (tt)) (v_)
| Type__Function v_ => (fun (_ : FunctionType) => (TypeVariant_Function) (tt)) (v_)
| Type__Forall v_ => (fun (_ : ForallType) => (TypeVariant_Forall) (tt)) (v_)
| Type__List v_ => (fun (_ : Type_) => (TypeVariant_List) (tt)) (v_)
| Type__Literal v_ => (fun (_ : LiteralType) => (TypeVariant_Literal) (tt)) (v_)
| Type__Map v_ => (fun (_ : MapType) => (TypeVariant_Map) (tt)) (v_)
| Type__Maybe v_ => (fun (_ : Type_) => (TypeVariant_Maybe) (tt)) (v_)
| Type__Pair v_ => (fun (_ : PairType) => (TypeVariant_Pair) (tt)) (v_)
| Type__Record v_ => (fun (_ : (list) (FieldType)) => (TypeVariant_Record) (tt)) (v_)
| Type__Set v_ => (fun (_ : Type_) => (TypeVariant_Set) (tt)) (v_)
| Type__Union v_ => (fun (_ : (list) (FieldType)) => (TypeVariant_Union) (tt)) (v_)
| Type__Unit _ => (TypeVariant_Unit) (tt)
| Type__Variable v_ => (fun (_ : Name) => (TypeVariant_Variable) (tt)) (v_)
| Type__Void _ => (TypeVariant_Void) (tt)
| Type__Wrap v_ => (fun (_ : Type_) => (TypeVariant_Wrap) (tt)) (v_)
end.
Definition termVariants : (list) (TermVariant) :=
  (cons) ((TermVariant_Annotated) (tt)) ((cons) ((TermVariant_Application) (tt)) ((cons) ((TermVariant_Cases) (tt)) ((cons) ((TermVariant_Either) (tt)) ((cons) ((TermVariant_Lambda) (tt)) ((cons) ((TermVariant_Let) (tt)) ((cons) ((TermVariant_List) (tt)) ((cons) ((TermVariant_Literal) (tt)) ((cons) ((TermVariant_Map) (tt)) ((cons) ((TermVariant_Maybe) (tt)) ((cons) ((TermVariant_Pair) (tt)) ((cons) ((TermVariant_Project) (tt)) ((cons) ((TermVariant_Record) (tt)) ((cons) ((TermVariant_Set) (tt)) ((cons) ((TermVariant_TypeLambda) (tt)) ((cons) ((TermVariant_TypeApplication) (tt)) ((cons) ((TermVariant_Inject) (tt)) ((cons) ((TermVariant_Unit) (tt)) ((cons) ((TermVariant_Unwrap) (tt)) ((cons) ((TermVariant_Variable) (tt)) ((cons) ((TermVariant_Wrap) (tt)) (nil))))))))))))))))))))).
Definition termVariant : Term -> TermVariant :=
  fun x_ => match x_ with
| Term_Annotated v_ => (fun (_ : AnnotatedTerm) => (TermVariant_Annotated) (tt)) (v_)
| Term_Application v_ => (fun (_ : Application) => (TermVariant_Application) (tt)) (v_)
| Term_Cases v_ => (fun (_ : CaseStatement) => (TermVariant_Cases) (tt)) (v_)
| Term_Either v_ => (fun (_ : (sum) (Term) (Term)) => (TermVariant_Either) (tt)) (v_)
| Term_Lambda v_ => (fun (_ : Lambda) => (TermVariant_Lambda) (tt)) (v_)
| Term_Let v_ => (fun (_ : Let) => (TermVariant_Let) (tt)) (v_)
| Term_List v_ => (fun (_ : (list) (Term)) => (TermVariant_List) (tt)) (v_)
| Term_Literal v_ => (fun (_ : Literal) => (TermVariant_Literal) (tt)) (v_)
| Term_Map v_ => (fun (_ : (list) ((prod) (Term) (Term))) => (TermVariant_Map) (tt)) (v_)
| Term_Maybe v_ => (fun (_ : (option) (Term)) => (TermVariant_Maybe) (tt)) (v_)
| Term_Pair v_ => (fun (_ : (prod) (Term) (Term)) => (TermVariant_Pair) (tt)) (v_)
| Term_Project v_ => (fun (_ : Projection) => (TermVariant_Project) (tt)) (v_)
| Term_Record v_ => (fun (_ : Record_) => (TermVariant_Record) (tt)) (v_)
| Term_Set v_ => (fun (_ : (list) (Term)) => (TermVariant_Set) (tt)) (v_)
| Term_TypeApplication v_ => (fun (_ : TypeApplicationTerm) => (TermVariant_TypeApplication) (tt)) (v_)
| Term_TypeLambda v_ => (fun (_ : TypeLambda) => (TermVariant_TypeLambda) (tt)) (v_)
| Term_Inject v_ => (fun (_ : Injection) => (TermVariant_Inject) (tt)) (v_)
| Term_Unit _ => (TermVariant_Unit) (tt)
| Term_Unwrap v_ => (fun (_ : Name) => (TermVariant_Unwrap) (tt)) (v_)
| Term_Variable v_ => (fun (_ : Name) => (TermVariant_Variable) (tt)) (v_)
| Term_Wrap v_ => (fun (_ : WrappedTerm) => (TermVariant_Wrap) (tt)) (v_)
end.
Definition literalVariants : (list) (LiteralVariant) :=
  (cons) ((LiteralVariant_Binary) (tt)) ((cons) ((LiteralVariant_Boolean) (tt)) ((cons) ((LiteralVariant_Float) (tt)) ((cons) ((LiteralVariant_Integer) (tt)) ((cons) ((LiteralVariant_String) (tt)) (nil))))).
Definition literalTypeVariant : LiteralType -> LiteralVariant :=
  fun x_ => match x_ with
| LiteralType_Binary _ => (LiteralVariant_Binary) (tt)
| LiteralType_Boolean _ => (LiteralVariant_Boolean) (tt)
| LiteralType_Float v_ => (fun (_ : FloatType) => (LiteralVariant_Float) (tt)) (v_)
| LiteralType_Integer v_ => (fun (_ : IntegerType) => (LiteralVariant_Integer) (tt)) (v_)
| LiteralType_String _ => (LiteralVariant_String) (tt)
end.
Definition integerValueType : IntegerValue -> IntegerType :=
  fun x_ => match x_ with
| IntegerValue_Bigint v_ => (fun (_ : Z) => (IntegerType_Bigint) (tt)) (v_)
| IntegerValue_Int8 v_ => (fun (_ : Z) => (IntegerType_Int8) (tt)) (v_)
| IntegerValue_Int16 v_ => (fun (_ : Z) => (IntegerType_Int16) (tt)) (v_)
| IntegerValue_Int32 v_ => (fun (_ : Z) => (IntegerType_Int32) (tt)) (v_)
| IntegerValue_Int64 v_ => (fun (_ : Z) => (IntegerType_Int64) (tt)) (v_)
| IntegerValue_Uint8 v_ => (fun (_ : Z) => (IntegerType_Uint8) (tt)) (v_)
| IntegerValue_Uint16 v_ => (fun (_ : Z) => (IntegerType_Uint16) (tt)) (v_)
| IntegerValue_Uint32 v_ => (fun (_ : Z) => (IntegerType_Uint32) (tt)) (v_)
| IntegerValue_Uint64 v_ => (fun (_ : Z) => (IntegerType_Uint64) (tt)) (v_)
end.
Definition integerTypes : (list) (IntegerType) :=
  (cons) ((IntegerType_Bigint) (tt)) ((cons) ((IntegerType_Int8) (tt)) ((cons) ((IntegerType_Int16) (tt)) ((cons) ((IntegerType_Int32) (tt)) ((cons) ((IntegerType_Int64) (tt)) ((cons) ((IntegerType_Uint8) (tt)) ((cons) ((IntegerType_Uint16) (tt)) ((cons) ((IntegerType_Uint32) (tt)) ((cons) ((IntegerType_Uint64) (tt)) (nil))))))))).
Definition integerTypePrecision : IntegerType -> Precision :=
  fun x_ => match x_ with
| IntegerType_Bigint _ => (Precision_Arbitrary) (tt)
| IntegerType_Int8 _ => (Precision_Bits) ((8)%Z)
| IntegerType_Int16 _ => (Precision_Bits) ((16)%Z)
| IntegerType_Int32 _ => (Precision_Bits) ((32)%Z)
| IntegerType_Int64 _ => (Precision_Bits) ((64)%Z)
| IntegerType_Uint8 _ => (Precision_Bits) ((8)%Z)
| IntegerType_Uint16 _ => (Precision_Bits) ((16)%Z)
| IntegerType_Uint32 _ => (Precision_Bits) ((32)%Z)
| IntegerType_Uint64 _ => (Precision_Bits) ((64)%Z)
end.
Definition integerTypeIsSigned : IntegerType -> bool :=
  fun x_ => match x_ with
| IntegerType_Bigint _ => true
| IntegerType_Int8 _ => true
| IntegerType_Int16 _ => true
| IntegerType_Int32 _ => true
| IntegerType_Int64 _ => true
| IntegerType_Uint8 _ => false
| IntegerType_Uint16 _ => false
| IntegerType_Uint32 _ => false
| IntegerType_Uint64 _ => false
end.
Definition functionVariants : (list) (FunctionVariant) :=
  (cons) ((FunctionVariant_Elimination) (tt)) ((cons) ((FunctionVariant_Lambda) (tt)) (nil)).
Definition floatValueType : FloatValue -> FloatType :=
  fun x_ => match x_ with
| FloatValue_Bigfloat v_ => (fun (_ : Q) => (FloatType_Bigfloat) (tt)) (v_)
| FloatValue_Float32 v_ => (fun (_ : Q) => (FloatType_Float32) (tt)) (v_)
| FloatValue_Float64 v_ => (fun (_ : Q) => (FloatType_Float64) (tt)) (v_)
end.
Definition literalType : Literal -> LiteralType :=
  fun x_ => match x_ with
| Literal_Binary v_ => (fun (_ : string) => (LiteralType_Binary) (tt)) (v_)
| Literal_Boolean v_ => (fun (_ : bool) => (LiteralType_Boolean) (tt)) (v_)
| Literal_Float v_ => (fun (arg_ : FloatValue) => (fun (injected_ : FloatType) => (LiteralType_Float) (injected_)) ((floatValueType) (arg_))) (v_)
| Literal_Integer v_ => (fun (arg_ : IntegerValue) => (fun (injected_ : IntegerType) => (LiteralType_Integer) (injected_)) ((integerValueType) (arg_))) (v_)
| Literal_String v_ => (fun (_ : string) => (LiteralType_String) (tt)) (v_)
end.
Definition literalVariant : Literal -> LiteralVariant :=
  fun (arg_ : Literal) => (literalTypeVariant) ((literalType) (arg_)).
Definition floatTypes : (list) (FloatType) :=
  (cons) ((FloatType_Bigfloat) (tt)) ((cons) ((FloatType_Float32) (tt)) ((cons) ((FloatType_Float64) (tt)) (nil))).
Definition literalTypes : (list) (LiteralType) :=
  (lists.concat) ((cons) ((cons) ((LiteralType_Binary) (tt)) ((cons) ((LiteralType_Boolean) (tt)) (nil))) ((cons) (((lists.map) (fun (x : FloatType) => (LiteralType_Float) (x))) (floatTypes)) ((cons) (((lists.map) (fun (x : IntegerType) => (LiteralType_Integer) (x))) (integerTypes)) ((cons) ((cons) ((LiteralType_String) (tt)) (nil)) (nil))))).
Definition floatTypePrecision : FloatType -> Precision :=
  fun x_ => match x_ with
| FloatType_Bigfloat _ => (Precision_Arbitrary) (tt)
| FloatType_Float32 _ => (Precision_Bits) ((32)%Z)
| FloatType_Float64 _ => (Precision_Bits) ((64)%Z)
end.
Definition eliminationVariants : (list) (EliminationVariant) :=
  (cons) ((EliminationVariant_Record) (tt)) ((cons) ((EliminationVariant_Union) (tt)) ((cons) ((EliminationVariant_Wrap) (tt)) (nil))).

