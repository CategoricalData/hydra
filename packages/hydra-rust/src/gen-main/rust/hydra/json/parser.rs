#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::parsers::*;
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

pub fn whitespace() -> crate::hydra::parsing::Parser {
  crate::hydra::parsers::map(|_: Vec<i32>| (), crate::hydra::parsers::many(crate::hydra::parsers::satisfy(|c: i32| crate::hydra::lib::lists::foldl(crate::hydra::lib::logic::or, false, Vec::from([
    crate::hydra::lib::equality::equal(c.clone(), 32i32),
    crate::hydra::lib::equality::equal(c.clone(), 9i32),
    crate::hydra::lib::equality::equal(c.clone(), 10i32),
    crate::hydra::lib::equality::equal(c.clone(), 13i32)])))))}

pub fn token(p: crate::hydra::parsing::Parser) -> crate::hydra::parsing::Parser {
  crate::hydra::parsers::bind(p.clone(), |x: T0| crate::hydra::parsers::bind(whitespace, |_: ()| crate::hydra::parsers::pure(x.clone())))}

pub fn json_null() -> crate::hydra::parsing::Parser {
  crate::hydra::parsers::map(|_: String| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Null)), token(crate::hydra::parsers::string(String::from("null"))))}

pub fn json_bool() -> crate::hydra::parsing::Parser {
  crate::hydra::parsers::alt(crate::hydra::parsers::map(|_: String| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Boolean(true))), token(crate::hydra::parsers::string(String::from("true")))), crate::hydra::parsers::map(|_: String| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Boolean(false))), token(crate::hydra::parsers::string(String::from("false")))))}

pub fn digit() -> crate::hydra::parsing::Parser {
  crate::hydra::parsers::satisfy(|c: i32| crate::hydra::lib::logic::and(crate::hydra::lib::equality::gte(c.clone(), 48i32), crate::hydra::lib::equality::lte(c.clone(), 57i32)))}

pub fn digits() -> crate::hydra::parsing::Parser {
  crate::hydra::parsers::map(crate::hydra::lib::strings::from_list, crate::hydra::parsers::some(digit))}

pub fn json_integer_part() -> crate::hydra::parsing::Parser {
  crate::hydra::parsers::bind(crate::hydra::parsers::optional(crate::hydra::parsers::char_(45i32)), |sign: Option<i32>| crate::hydra::parsers::bind(digits, |digits: String| crate::hydra::parsers::pure(crate::hydra::lib::maybes::maybe(digits.clone(), |_: i32| crate::hydra::lib::strings::cat2(String::from("-"), digits.clone()), sign.clone()))))}

pub fn json_fraction_part() -> crate::hydra::parsing::Parser {
  crate::hydra::parsers::optional(crate::hydra::parsers::bind(crate::hydra::parsers::char_(46i32), |_: i32| crate::hydra::parsers::map(|d: String| crate::hydra::lib::strings::cat2(String::from("."), d.clone()), digits)))}

pub fn json_exponent_part() -> crate::hydra::parsing::Parser {
  crate::hydra::parsers::optional(crate::hydra::parsers::bind(crate::hydra::parsers::satisfy(|c: i32| crate::hydra::lib::logic::or(crate::hydra::lib::equality::equal(c.clone(), 101i32), crate::hydra::lib::equality::equal(c.clone(), 69i32))), |_: i32| crate::hydra::parsers::bind(crate::hydra::parsers::optional(crate::hydra::parsers::satisfy(|c: i32| crate::hydra::lib::logic::or(crate::hydra::lib::equality::equal(c.clone(), 43i32), crate::hydra::lib::equality::equal(c.clone(), 45i32)))), |sign: Option<i32>| crate::hydra::parsers::map(|digits: String| crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("e"), crate::hydra::lib::maybes::maybe(String::from(""), |arg_: i32| crate::hydra::lib::strings::from_list(crate::hydra::lib::lists::pure(arg_.clone())), sign.clone())), digits.clone()), digits))))}

pub fn json_number() -> crate::hydra::parsing::Parser {
  token(crate::hydra::parsers::bind(json_integer_part, |int_part: String| crate::hydra::parsers::bind(json_fraction_part, |frac_part: Option<String>| crate::hydra::parsers::bind(json_exponent_part, |exp_part: Option<String>| {
    let num_str = crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(int_part.clone(), crate::hydra::lib::maybes::maybe(String::from(""), crate::hydra::lib::equality::identity, frac_part.clone())), crate::hydra::lib::maybes::maybe(String::from(""), crate::hydra::lib::equality::identity, exp_part.clone())) ;
    crate::hydra::parsers::pure(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Number(crate::hydra::lib::maybes::maybe(0.0, crate::hydra::lib::equality::identity, crate::hydra::lib::literals::read_bigfloat(num_str.clone()))))))}))))}

pub fn json_escape_char() -> crate::hydra::parsing::Parser {
  crate::hydra::parsers::choice(Vec::from([
    crate::hydra::parsers::map(|_: i32| 34i32, crate::hydra::parsers::char_(34i32)),
    crate::hydra::parsers::map(|_: i32| 92i32, crate::hydra::parsers::char_(92i32)),
    crate::hydra::parsers::map(|_: i32| 47i32, crate::hydra::parsers::char_(47i32)),
    crate::hydra::parsers::map(|_: i32| 8i32, crate::hydra::parsers::char_(98i32)),
    crate::hydra::parsers::map(|_: i32| 12i32, crate::hydra::parsers::char_(102i32)),
    crate::hydra::parsers::map(|_: i32| 10i32, crate::hydra::parsers::char_(110i32)),
    crate::hydra::parsers::map(|_: i32| 13i32, crate::hydra::parsers::char_(114i32)),
    crate::hydra::parsers::map(|_: i32| 9i32, crate::hydra::parsers::char_(116i32))]))}

pub fn json_string_char() -> crate::hydra::parsing::Parser {
  crate::hydra::parsers::alt(crate::hydra::parsers::bind(crate::hydra::parsers::char_(92i32), |_: i32| json_escape_char), crate::hydra::parsers::satisfy(|c: i32| crate::hydra::lib::logic::and(crate::hydra::lib::logic::not(crate::hydra::lib::equality::equal(c.clone(), 34i32)), crate::hydra::lib::logic::not(crate::hydra::lib::equality::equal(c.clone(), 92i32)))))}

pub fn json_string() -> crate::hydra::parsing::Parser {
  token(crate::hydra::parsers::bind(crate::hydra::parsers::char_(34i32), |_: i32| crate::hydra::parsers::bind(crate::hydra::parsers::many(json_string_char), |chars: Vec<i32>| crate::hydra::parsers::bind(crate::hydra::parsers::char_(34i32), |_2: i32| crate::hydra::parsers::pure(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::String(crate::hydra::lib::strings::from_list(chars.clone())))))))))}

pub fn json_array() -> crate::hydra::parsing::Parser {
  crate::hydra::parsers::map(|x: Vec<crate::hydra::json::model::Value>| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Array(x.clone()))), crate::hydra::parsers::between(token(crate::hydra::parsers::char_(91i32)), token(crate::hydra::parsers::char_(93i32)), crate::hydra::parsers::sep_by(crate::hydra::parsers::lazy(|_: ()| json_value), token(crate::hydra::parsers::char_(44i32)))))}

pub fn json_key_value() -> crate::hydra::parsing::Parser {
  crate::hydra::parsers::bind(token(crate::hydra::parsers::bind(crate::hydra::parsers::char_(34i32), |_: i32| crate::hydra::parsers::bind(crate::hydra::parsers::many(json_string_char), |chars: Vec<i32>| crate::hydra::parsers::bind(crate::hydra::parsers::char_(34i32), |_2: i32| crate::hydra::parsers::pure(crate::hydra::lib::strings::from_list(chars.clone())))))), |key: String| crate::hydra::parsers::bind(token(crate::hydra::parsers::char_(58i32)), |_: i32| crate::hydra::parsers::map(|v: crate::hydra::json::model::Value| (key.clone(), v.clone()), crate::hydra::parsers::lazy(|_2: ()| json_value))))}

pub fn json_object() -> crate::hydra::parsing::Parser {
  crate::hydra::parsers::map(|arg_: Vec<(String, crate::hydra::json::model::Value)>| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Object(crate::hydra::lib::maps::from_list(arg_.clone())))), crate::hydra::parsers::between(token(crate::hydra::parsers::char_(123i32)), token(crate::hydra::parsers::char_(125i32)), crate::hydra::parsers::sep_by(json_key_value, token(crate::hydra::parsers::char_(44i32)))))}

pub fn json_value() -> crate::hydra::parsing::Parser {
  crate::hydra::parsers::choice(Vec::from([
    json_null,
    json_bool,
    json_number,
    json_string,
    json_array,
    json_object]))}

pub fn parse_json(input: String) -> crate::hydra::parsing::ParseResult {
  crate::hydra::parsers::bind(whitespace, |_: ()| crate::hydra::parsers::bind(json_value, |v: crate::hydra::json::model::Value| crate::hydra::parsers::bind(whitespace, |_2: ()| crate::hydra::parsers::bind(crate::hydra::parsers::eof, |_3: ()| crate::hydra::parsers::pure(v.clone()))))).0.0.clone()}
