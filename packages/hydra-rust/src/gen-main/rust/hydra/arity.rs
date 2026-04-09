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

pub fn function_arity(v1: crate::hydra::core::Function) -> i32 {
  match &*v1.clone().0 {
    crate::hydra::core::Function_Variant::Elimination (v0_) => {
      let v0_ = v0_.clone() ;
      1i32},
    crate::hydra::core::Function_Variant::Lambda (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::math::add(1i32, term_arity(v0_.clone().0.body.clone()))},
    crate::hydra::core::Function_Variant::Primitive (v0_) => {
      let v0_ = v0_.clone() ;
      42i32}}}

pub fn primitive_arity(arg_: crate::hydra::graph::Primitive) -> i32 {
  type_arity(arg_.clone().0.type_.clone().0.type_.clone())}

pub fn term_arity(v1: crate::hydra::core::Term) -> i32 {
  match &*v1.clone().0 {
    crate::hydra::core::Term_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::math::sub(term_arity(v0_.clone().0.function.clone()), 1i32)},
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      function_arity(v0_.clone())},
    _ => 0i32}}

pub fn type_arity(v1: crate::hydra::core::Type) -> i32 {
  match &*v1.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      type_arity(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      type_arity(v0_.clone().0.function.clone())},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      type_arity(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::math::add(1i32, type_arity(v0_.clone().0.codomain.clone()))},
    _ => 0i32}}

pub fn type_scheme_arity(arg_: crate::hydra::core::TypeScheme) -> i32 {
  type_arity(arg_.clone().0.type_.clone())}

pub fn uncurry_type(t: crate::hydra::core::Type) -> Vec<crate::hydra::core::Type> {
  match &*t.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      uncurry_type(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      uncurry_type(v0_.clone().0.function.clone())},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      uncurry_type(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::cons(v0_.clone().0.domain.clone(), uncurry_type(v0_.clone().0.codomain.clone()))},
    _ => Vec::from([
      t.clone()])}}
