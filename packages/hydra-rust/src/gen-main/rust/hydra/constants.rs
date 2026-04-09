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

pub fn debug_inference() -> bool {
  true}

pub fn ignored_variable() -> String {
  String::from("_")}

pub fn key_classes() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("classes"))))}

pub fn key_debug_id() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("debugId"))))}

pub fn key_deprecated() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("deprecated"))))}

pub fn key_description() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description"))))}

pub fn key_exclude() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("exclude"))))}

pub fn key_first_class_type() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("firstClassType"))))}

pub fn key_max_length() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("maxLength"))))}

pub fn key_min_length() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("minLength"))))}

pub fn key_preserve_field_name() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("preserveFieldName"))))}

pub fn key_type() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type"))))}

pub fn key_fresh_type_variable_count() -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("freshTypeVariableCount"))))}

pub fn max_int32() -> i32 {
  2147483647i32}

pub fn max_trace_depth() -> i32 {
  5000i32}

pub fn warning_auto_generated_file() -> String {
  String::from("Note: this is an automatically generated file. Do not edit.")}
