#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::core::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AccessorEdge_Variant {
  pub source: AccessorNode,
  pub path: AccessorPath,
  pub target: AccessorNode}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AccessorEdge (pub Rc<AccessorEdge_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AccessorGraph_Variant {
  pub nodes: Vec<AccessorNode>,
  pub edges: Vec<AccessorEdge>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AccessorGraph (pub Rc<AccessorGraph_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AccessorNode_Variant {
  pub name: crate::hydra::core::Name,
  pub label: String,
  pub id: String}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AccessorNode (pub Rc<AccessorNode_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AccessorPath_Variant (pub Vec<TermAccessor>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AccessorPath (pub Rc<AccessorPath_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TermAccessor_Variant {
  AnnotatedBody,
  ApplicationFunction,
  ApplicationArgument,
  LambdaBody,
  UnionCasesDefault,
  UnionCasesBranch(crate::hydra::core::Name),
  LetBody,
  LetBinding(crate::hydra::core::Name),
  ListElement(i32),
  MapKey(i32),
  MapValue(i32),
  MaybeTerm,
  ProductTerm(i32),
  RecordField(crate::hydra::core::Name),
  SetElement(i32),
  SumTerm,
  TypeLambdaBody,
  TypeApplicationTerm,
  InjectionTerm,
  WrappedTerm}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TermAccessor (pub Rc<TermAccessor_Variant>);
