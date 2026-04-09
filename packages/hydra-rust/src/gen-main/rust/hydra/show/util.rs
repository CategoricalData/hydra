#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::accessors::*;
use crate::hydra::ast::*;
use crate::hydra::classes::*;
use crate::hydra::coders::*;
use crate::hydra::context::*;
use crate::hydra::core::*;
use crate::hydra::error::*;
use crate::hydra::grammar::*;
use crate::hydra::graph::*;
use crate::hydra::json::model::*;
use crate::hydra::module::*;
use crate::hydra::parsing::*;
use crate::hydra::phantoms::*;
use crate::hydra::query::*;
use crate::hydra::relational::*;
use crate::hydra::tabular::*;
use crate::hydra::testing::*;
use crate::hydra::topology::*;
use crate::hydra::typing::*;
use crate::hydra::util::*;
use crate::hydra::variants::*;

pub fn case_convention(c: crate::hydra::util::CaseConvention) -> String {
  match &*c.clone().0 {
    crate::hydra::util::CaseConvention_Variant::LowerSnake (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("lower_snake_case")},
    crate::hydra::util::CaseConvention_Variant::UpperSnake (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("UPPER_SNAKE_CASE")},
    crate::hydra::util::CaseConvention_Variant::Camel (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("camelCase")},
    crate::hydra::util::CaseConvention_Variant::Pascal (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("PascalCase")}}}
