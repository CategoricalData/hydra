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

pub fn decode_array(decode_elem: impl Fn(crate::hydra::json::model::Value) -> Either<String, T0> + Clone, v1: crate::hydra::json::model::Value) -> Either<String, Vec<T0>> {
  match &*v1.clone().0 {
    crate::hydra::json::model::Value_Variant::Array (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map_list(decode_elem.clone(), v0_.clone())},
    _ => Left(String::from("expected an array"))}}

pub fn decode_boolean(v1: crate::hydra::json::model::Value) -> Either<String, bool> {
  match &*v1.clone().0 {
    crate::hydra::json::model::Value_Variant::Boolean (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(String::from("expected a boolean"))}}

pub fn decode_field(decode_value: impl Fn(T0) -> Either<String, T1> + Clone, name: String, m: BTreeMap<String, T0>) -> Either<String, T1> {
  crate::hydra::lib::eithers::bind(decode_optional_field(decode_value.clone(), name.clone(), m.clone()), |v1: Option<X>| crate::hydra::lib::maybes::maybe(Left(crate::hydra::lib::strings::cat2(String::from("missing field: "), name.clone())), |f: T1| Right(f.clone()), v1.clone()))}

pub fn decode_object(v1: crate::hydra::json::model::Value) -> Either<String, BTreeMap<String, crate::hydra::json::model::Value>> {
  match &*v1.clone().0 {
    crate::hydra::json::model::Value_Variant::Object (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(String::from("expected an object"))}}

pub fn decode_optional_field(decode_value: impl Fn(T0) -> Either<T1, T2> + Clone, name: T3, m: BTreeMap<T3, T0>) -> Either<T0, Option<T2>> {
  crate::hydra::lib::maybes::maybe(Right(None), |v: T0| crate::hydra::lib::eithers::map(|x: T2| Some(x.clone()), decode_value.clone()(v.clone())), crate::hydra::lib::maps::lookup(name.clone(), m.clone()))}

pub fn decode_string(v1: crate::hydra::json::model::Value) -> Either<String, String> {
  match &*v1.clone().0 {
    crate::hydra::json::model::Value_Variant::String (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(String::from("expected a string"))}}
