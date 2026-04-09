#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::core::*;
use crate::hydra::context::*;

pub type FunctionStructure = (Vec<crate::hydra::core::Name>, Vec<crate::hydra::core::Name>, Vec<crate::hydra::core::Binding>, crate::hydra::core::Term, Vec<crate::hydra::core::Type>, Option<crate::hydra::core::Type>, Env) ;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct InferenceResult_Variant {
  pub term: crate::hydra::core::Term,
  pub type_: crate::hydra::core::Type,
  pub subst: TypeSubst,
  pub class_constraints: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>,
  pub context: crate::hydra::context::Context}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct InferenceResult (pub Rc<InferenceResult_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TermSubst_Variant (pub BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TermSubst (pub Rc<TermSubst_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeConstraint_Variant {
  pub left: crate::hydra::core::Type,
  pub right: crate::hydra::core::Type,
  pub comment: String}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeConstraint (pub Rc<TypeConstraint_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeSubst_Variant (pub BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeSubst (pub Rc<TypeSubst_Variant>);
