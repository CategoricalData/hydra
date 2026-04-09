#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::serialization::*;
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

pub fn colon_op() -> crate::hydra::ast::Op {
  crate::hydra::ast::Op(Rc::new(crate::hydra::ast::Op_Variant {
    symbol: crate::hydra::ast::Symbol(Rc::new(crate::hydra::ast::Symbol_Variant(String::from(":")))),
    padding: crate::hydra::ast::Padding(Rc::new(crate::hydra::ast::Padding_Variant {
      left: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::None)),
      right: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::Space))})),
    precedence: crate::hydra::ast::Precedence(Rc::new(crate::hydra::ast::Precedence_Variant(0i32))),
    associativity: crate::hydra::ast::Associativity(Rc::new(crate::hydra::ast::Associativity_Variant::None))}))}

pub fn json_string(s: String) -> String {
  let hex_escape = |c: i32| {
    let hi = crate::hydra::lib::strings::from_list(crate::hydra::lib::lists::pure(crate::hydra::lib::strings::char_at(crate::hydra::lib::math::div(c.clone(), 16i32), String::from("0123456789abcdef")))) ;
    {
      let lo = crate::hydra::lib::strings::from_list(crate::hydra::lib::lists::pure(crate::hydra::lib::strings::char_at(crate::hydra::lib::math::mod_(c.clone(), 16i32), String::from("0123456789abcdef")))) ;
      crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("\\u00"), hi.clone()), lo.clone())}} ;
  let escape = |c: i32| crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(c.clone(), 34i32), String::from("\\\""), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(c.clone(), 92i32), String::from("\\\\"), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(c.clone(), 8i32), String::from("\\b"), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(c.clone(), 12i32), String::from("\\f"), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(c.clone(), 10i32), String::from("\\n"), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(c.clone(), 13i32), String::from("\\r"), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(c.clone(), 9i32), String::from("\\t"), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::lt(c.clone(), 32i32), hex_escape.clone()(c.clone()), crate::hydra::lib::strings::from_list(crate::hydra::lib::lists::pure(c.clone())))))))))) ;
  let escaped = crate::hydra::lib::strings::cat(crate::hydra::lib::lists::map(escape.clone(), crate::hydra::lib::strings::to_list(s.clone()))) ;
  crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("\""), escaped.clone()), String::from("\""))}

pub fn key_value_to_expr(pair: (String, crate::hydra::json::model::Value)) -> crate::hydra::ast::Expr {
  let key = crate::hydra::lib::pairs::first(pair.clone()) ;
  let value = crate::hydra::lib::pairs::second(pair.clone()) ;
  crate::hydra::serialization::ifx(colon_op, crate::hydra::serialization::cst(json_string(key.clone())), value_to_expr(value.clone()))}

pub fn print_json(value: crate::hydra::json::model::Value) -> String {
  crate::hydra::serialization::print_expr(value_to_expr(value.clone()))}

pub fn value_to_expr(value: crate::hydra::json::model::Value) -> crate::hydra::ast::Expr {
  match &*value.clone().0 {
    crate::hydra::json::model::Value_Variant::Array (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::serialization::bracket_list_adaptive(crate::hydra::lib::lists::map(value_to_expr, v0_.clone()))},
    crate::hydra::json::model::Value_Variant::Boolean (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::serialization::cst(crate::hydra::lib::logic::if_else(v0_.clone(), String::from("true"), String::from("false")))},
    crate::hydra::json::model::Value_Variant::Null (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::serialization::cst(String::from("null"))},
    crate::hydra::json::model::Value_Variant::Number (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let rounded = crate::hydra::lib::literals::bigfloat_to_bigint(v0_.clone()) ;
        crate::hydra::serialization::cst(crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(v0_.clone(), crate::hydra::lib::literals::bigint_to_bigfloat(rounded.clone())), crate::hydra::lib::literals::show_bigint(rounded.clone()), crate::hydra::lib::literals::show_bigfloat(v0_.clone())))}},
    crate::hydra::json::model::Value_Variant::Object (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::serialization::braces_list_adaptive(crate::hydra::lib::lists::map(key_value_to_expr, crate::hydra::lib::maps::to_list(v0_.clone())))},
    crate::hydra::json::model::Value_Variant::String (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::serialization::cst(json_string(v0_.clone()))}}}
