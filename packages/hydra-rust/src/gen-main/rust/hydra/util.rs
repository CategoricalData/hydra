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

pub type Adapter = (bool, T1, T2, Coder) ;

pub type Bicoder = (Rc<dyn Fn(T1) -> Adapter>, Rc<dyn Fn(T2) -> Adapter>) ;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum CaseConvention_Variant {
  Camel,
  Pascal,
  LowerSnake,
  UpperSnake}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct CaseConvention (pub Rc<CaseConvention_Variant>);

pub type Coder = (Rc<dyn Fn(crate::hydra::context::Context) -> Rc<dyn Fn(V1) -> Either<crate::hydra::context::InContext, V2>>>, Rc<dyn Fn(crate::hydra::context::Context) -> Rc<dyn Fn(V2) -> Either<crate::hydra::context::InContext, V1>>>) ;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Comparison_Variant {
  LessThan,
  EqualTo,
  GreaterThan}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Comparison (pub Rc<Comparison_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precision_Variant {
  Arbitrary,
  Bits(i32)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Precision (pub Rc<Precision_Variant>);
