#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::core::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value_Variant {
  Array(Vec<Value>),
  Boolean(bool),
  Null,
  Number(OrderedFloat<f64>),
  Object(BTreeMap<String, Value>),
  String(String)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Value (pub Rc<Value_Variant>);
