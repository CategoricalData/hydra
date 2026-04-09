#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::core::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Context_Variant {
  pub trace: Vec<String>,
  pub messages: Vec<String>,
  pub other: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Context (pub Rc<Context_Variant>);

pub type InContext = (E, Context) ;
