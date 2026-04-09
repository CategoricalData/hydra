#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::context::*;
use crate::hydra::core::*;
use crate::hydra::error::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Graph_Variant {
  pub bound_terms: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term>,
  pub bound_types: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeScheme>,
  pub class_constraints: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>,
  pub lambda_variables: BTreeSet<crate::hydra::core::Name>,
  pub metadata: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term>,
  pub primitives: BTreeMap<crate::hydra::core::Name, Primitive>,
  pub schema_types: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeScheme>,
  pub type_variables: BTreeSet<crate::hydra::core::Name>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Graph (pub Rc<Graph_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Primitive_Variant {
  pub name: crate::hydra::core::Name,
  pub type_: crate::hydra::core::TypeScheme,
  pub implementation: Rc<dyn Fn(crate::hydra::context::Context) -> Rc<dyn Fn(Graph) -> Rc<dyn Fn(Vec<crate::hydra::core::Term>) -> Either<crate::hydra::context::InContext, crate::hydra::core::Term>>>>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Primitive (pub Rc<Primitive_Variant>);

pub type TermCoder = (crate::hydra::core::Type, Rc<dyn Fn(crate::hydra::context::Context) -> Rc<dyn Fn(Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, A>>>>, Rc<dyn Fn(crate::hydra::context::Context) -> Rc<dyn Fn(A) -> Either<crate::hydra::context::InContext, crate::hydra::core::Term>>>) ;
