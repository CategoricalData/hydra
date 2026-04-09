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

pub fn expect_array(value: crate::hydra::json::model::Value) -> Either<String, Vec<crate::hydra::json::model::Value>> {
  match &*value.clone().0 {
    crate::hydra::json::model::Value_Variant::Array (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("JSON array")), crate::hydra::lib::strings::cat2(String::from(" but found "), show_value(value.clone()))))}}

pub fn expect_number(value: crate::hydra::json::model::Value) -> Either<String, OrderedFloat<f64>> {
  match &*value.clone().0 {
    crate::hydra::json::model::Value_Variant::Number (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("JSON number")), crate::hydra::lib::strings::cat2(String::from(" but found "), show_value(value.clone()))))}}

pub fn expect_object(value: crate::hydra::json::model::Value) -> Either<String, BTreeMap<String, crate::hydra::json::model::Value>> {
  match &*value.clone().0 {
    crate::hydra::json::model::Value_Variant::Object (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("JSON object")), crate::hydra::lib::strings::cat2(String::from(" but found "), show_value(value.clone()))))}}

pub fn expect_string(value: crate::hydra::json::model::Value) -> Either<String, String> {
  match &*value.clone().0 {
    crate::hydra::json::model::Value_Variant::String (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("JSON string")), crate::hydra::lib::strings::cat2(String::from(" but found "), show_value(value.clone()))))}}

pub fn opt(fname: T0, m: BTreeMap<T0, T1>) -> Option<T1> {
  crate::hydra::lib::maps::lookup(fname.clone(), m.clone())}

pub fn opt_array(fname: T0, m: BTreeMap<T0, crate::hydra::json::model::Value>) -> Either<String, Option<Vec<crate::hydra::json::model::Value>>> {
  crate::hydra::lib::maybes::maybe(Right(None), |a: crate::hydra::json::model::Value| crate::hydra::lib::eithers::map(|x: Vec<crate::hydra::json::model::Value>| Some(x.clone()), expect_array(a.clone())), opt(fname.clone(), m.clone()))}

pub fn opt_string(fname: T0, m: BTreeMap<T0, crate::hydra::json::model::Value>) -> Either<String, Option<String>> {
  crate::hydra::lib::maybes::maybe(Right(None), |s: crate::hydra::json::model::Value| crate::hydra::lib::eithers::map(|x: String| Some(x.clone()), expect_string(s.clone())), opt(fname.clone(), m.clone()))}

pub fn require(fname: T0, m: BTreeMap<T0, T1>) -> Either<String, T1> {
  crate::hydra::lib::maybes::maybe(Left(crate::hydra::lib::strings::cat(Vec::from([
    String::from("required attribute "),
    show_value(fname.clone()),
    String::from(" not found")]))), |value: T1| Right(value.clone()), crate::hydra::lib::maps::lookup(fname.clone(), m.clone()))}

pub fn require_array(fname: T0, m: BTreeMap<T0, crate::hydra::json::model::Value>) -> Either<String, Vec<crate::hydra::json::model::Value>> {
  crate::hydra::lib::eithers::bind(require(fname.clone(), m.clone()), expect_array)}

pub fn require_number(fname: T0, m: BTreeMap<T0, crate::hydra::json::model::Value>) -> Either<String, OrderedFloat<f64>> {
  crate::hydra::lib::eithers::bind(require(fname.clone(), m.clone()), expect_number)}

pub fn require_string(fname: T0, m: BTreeMap<T0, crate::hydra::json::model::Value>) -> Either<String, String> {
  crate::hydra::lib::eithers::bind(require(fname.clone(), m.clone()), expect_string)}

pub fn show_value(value: T0) -> String {
  String::from("TODO: implement showValue")}
