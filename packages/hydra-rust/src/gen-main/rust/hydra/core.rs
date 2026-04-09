#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AnnotatedTerm_Variant {
  pub body: Term,
  pub annotation: BTreeMap<Name, Term>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AnnotatedTerm (pub Rc<AnnotatedTerm_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AnnotatedType_Variant {
  pub body: Type,
  pub annotation: BTreeMap<Name, Term>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AnnotatedType (pub Rc<AnnotatedType_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Application_Variant {
  pub function: Term,
  pub argument: Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Application (pub Rc<Application_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ApplicationType_Variant {
  pub function: Type,
  pub argument: Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ApplicationType (pub Rc<ApplicationType_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Binding_Variant {
  pub name: Name,
  pub term: Term,
  pub type_: Option<TypeScheme>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Binding (pub Rc<Binding_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct CaseStatement_Variant {
  pub type_name: Name,
  pub default_: Option<Term>,
  pub cases: Vec<Field>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct CaseStatement (pub Rc<CaseStatement_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct EitherType_Variant {
  pub left: Type,
  pub right: Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct EitherType (pub Rc<EitherType_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PairType_Variant {
  pub first: Type,
  pub second: Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PairType (pub Rc<PairType_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Elimination_Variant {
  Record(Projection),
  Union(CaseStatement),
  Wrap(Name)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Elimination (pub Rc<Elimination_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Field_Variant {
  pub name: Name,
  pub term: Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Field (pub Rc<Field_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FieldType_Variant {
  pub name: Name,
  pub type_: Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FieldType (pub Rc<FieldType_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum FloatType_Variant {
  Bigfloat,
  Float32,
  Float64}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FloatType (pub Rc<FloatType_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum FloatValue_Variant {
  Bigfloat(OrderedFloat<f64>),
  Float32(OrderedFloat<f32>),
  Float64(OrderedFloat<f64>)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FloatValue (pub Rc<FloatValue_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ForallType_Variant {
  pub parameter: Name,
  pub body: Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ForallType (pub Rc<ForallType_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Function_Variant {
  Elimination(Elimination),
  Lambda(Lambda),
  Primitive(Name)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Function (pub Rc<Function_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionType_Variant {
  pub domain: Type,
  pub codomain: Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionType (pub Rc<FunctionType_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Injection_Variant {
  pub type_name: Name,
  pub field: Field}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Injection (pub Rc<Injection_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntegerType_Variant {
  Bigint,
  Int8,
  Int16,
  Int32,
  Int64,
  Uint8,
  Uint16,
  Uint32,
  Uint64}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntegerType (pub Rc<IntegerType_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntegerValue_Variant {
  Bigint(String),
  Int8(i8),
  Int16(i16),
  Int32(i32),
  Int64(i64),
  Uint8(u8),
  Uint16(u16),
  Uint32(u32),
  Uint64(u64)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntegerValue (pub Rc<IntegerValue_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Lambda_Variant {
  pub parameter: Name,
  pub domain: Option<Type>,
  pub body: Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Lambda (pub Rc<Lambda_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Let_Variant {
  pub bindings: Vec<Binding>,
  pub body: Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Let (pub Rc<Let_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Literal_Variant {
  Binary(Vec<u8>),
  Boolean(bool),
  Float(FloatValue),
  Integer(IntegerValue),
  String(String)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Literal (pub Rc<Literal_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralType_Variant {
  Binary,
  Boolean,
  Float(FloatType),
  Integer(IntegerType),
  String}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LiteralType (pub Rc<LiteralType_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct MapType_Variant {
  pub keys: Type,
  pub values: Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct MapType (pub Rc<MapType_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Name_Variant (pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Name (pub Rc<Name_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Projection_Variant {
  pub type_name: Name,
  pub field: Name}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Projection (pub Rc<Projection_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Record_Variant {
  pub type_name: Name,
  pub fields: Vec<Field>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Record (pub Rc<Record_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Term_Variant {
  Annotated(AnnotatedTerm),
  Application(Application),
  Either(Either<Term, Term>),
  Function(Function),
  Let(Let),
  List(Vec<Term>),
  Literal(Literal),
  Map(BTreeMap<Term, Term>),
  Maybe(Option<Term>),
  Pair((Term, Term)),
  Record(Record),
  Set(BTreeSet<Term>),
  TypeApplication(TypeApplicationTerm),
  TypeLambda(TypeLambda),
  Union(Injection),
  Unit,
  Variable(Name),
  Wrap(WrappedTerm)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Term (pub Rc<Term_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type_Variant {
  Annotated(AnnotatedType),
  Application(ApplicationType),
  Either(EitherType),
  Forall(ForallType),
  Function(FunctionType),
  List(Type),
  Literal(LiteralType),
  Map(MapType),
  Maybe(Type),
  Pair(PairType),
  Record(Vec<FieldType>),
  Set(Type),
  Union(Vec<FieldType>),
  Unit,
  Variable(Name),
  Wrap(Type)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Type (pub Rc<Type_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeApplicationTerm_Variant {
  pub body: Term,
  pub type_: Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeApplicationTerm (pub Rc<TypeApplicationTerm_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeLambda_Variant {
  pub parameter: Name,
  pub body: Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeLambda (pub Rc<TypeLambda_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeScheme_Variant {
  pub variables: Vec<Name>,
  pub type_: Type,
  pub constraints: Option<BTreeMap<Name, TypeVariableMetadata>>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeScheme (pub Rc<TypeScheme_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeVariableMetadata_Variant {
  pub classes: BTreeSet<Name>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeVariableMetadata (pub Rc<TypeVariableMetadata_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct WrappedTerm_Variant {
  pub type_name: Name,
  pub body: Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct WrappedTerm (pub Rc<WrappedTerm_Variant>);
