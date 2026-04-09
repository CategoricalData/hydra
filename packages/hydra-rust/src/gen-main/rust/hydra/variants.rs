#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::core::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum EliminationVariant_Variant {
  Record,
  Union,
  Wrap}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct EliminationVariant (pub Rc<EliminationVariant_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum FunctionVariant_Variant {
  Elimination,
  Lambda,
  Primitive}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionVariant (pub Rc<FunctionVariant_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralVariant_Variant {
  Binary,
  Boolean,
  Float,
  Integer,
  String}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LiteralVariant (pub Rc<LiteralVariant_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TermVariant_Variant {
  Annotated,
  Application,
  Either,
  Function,
  Let,
  List,
  Literal,
  Map,
  Maybe,
  Pair,
  Record,
  Set,
  TypeApplication,
  TypeLambda,
  Union,
  Unit,
  Variable,
  Wrap}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TermVariant (pub Rc<TermVariant_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeVariant_Variant {
  Annotated,
  Application,
  Either,
  Forall,
  Function,
  List,
  Literal,
  Map,
  Maybe,
  Pair,
  Record,
  Set,
  Union,
  Unit,
  Variable,
  Wrap}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeVariant (pub Rc<TypeVariant_Variant>);
