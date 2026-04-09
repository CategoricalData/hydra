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

pub fn capitalize(v1: String) -> String {
  map_first_letter(crate::hydra::lib::strings::to_upper, v1.clone())}

pub fn convert_case(from: crate::hydra::util::CaseConvention, to: crate::hydra::util::CaseConvention, original: String) -> String {
  let parts = {
    let by_caps = {
      let split_on_uppercase = |acc: Vec<Vec<i32>>, c: i32| crate::hydra::lib::lists::concat2(crate::hydra::lib::logic::if_else(crate::hydra::lib::chars::is_upper(c.clone()), Vec::from([
        Vec::from([])]), Vec::from([])), crate::hydra::lib::lists::cons(crate::hydra::lib::lists::cons(c.clone(), crate::hydra::lib::lists::head(acc.clone())), crate::hydra::lib::lists::tail(acc.clone()))) ;
      crate::hydra::lib::lists::map(crate::hydra::lib::strings::from_list, crate::hydra::lib::lists::foldl(split_on_uppercase.clone(), Vec::from([
        Vec::from([])]), crate::hydra::lib::lists::reverse(crate::hydra::lib::strings::to_list(decapitalize(original.clone())))))} ;
    let by_underscores = crate::hydra::lib::strings::split_on(String::from("_"), original.clone()) ;
    match &*from.clone().0 {
      crate::hydra::util::CaseConvention_Variant::Camel (v0_) => {
        let v0_ = v0_.clone() ;
        by_caps.clone()},
      crate::hydra::util::CaseConvention_Variant::Pascal (v0_) => {
        let v0_ = v0_.clone() ;
        by_caps.clone()},
      crate::hydra::util::CaseConvention_Variant::LowerSnake (v0_) => {
        let v0_ = v0_.clone() ;
        by_underscores.clone()},
      crate::hydra::util::CaseConvention_Variant::UpperSnake (v0_) => {
        let v0_ = v0_.clone() ;
        by_underscores.clone()}}} ;
  match &*to.clone().0 {
    crate::hydra::util::CaseConvention_Variant::Camel (v0_) => {
      let v0_ = v0_.clone() ;
      decapitalize(crate::hydra::lib::strings::cat(crate::hydra::lib::lists::map(|arg_: String| capitalize(crate::hydra::lib::strings::to_lower(arg_.clone())), parts.clone())))},
    crate::hydra::util::CaseConvention_Variant::Pascal (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat(crate::hydra::lib::lists::map(|arg_: String| capitalize(crate::hydra::lib::strings::to_lower(arg_.clone())), parts.clone()))},
    crate::hydra::util::CaseConvention_Variant::LowerSnake (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::intercalate(String::from("_"), crate::hydra::lib::lists::map(crate::hydra::lib::strings::to_lower, parts.clone()))},
    crate::hydra::util::CaseConvention_Variant::UpperSnake (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::intercalate(String::from("_"), crate::hydra::lib::lists::map(crate::hydra::lib::strings::to_upper, parts.clone()))}}}

pub fn convert_case_camel_to_lower_snake(v1: String) -> String {
  convert_case(crate::hydra::util::CaseConvention(Rc::new(crate::hydra::util::CaseConvention_Variant::Camel)), crate::hydra::util::CaseConvention(Rc::new(crate::hydra::util::CaseConvention_Variant::LowerSnake)), v1.clone())}

pub fn convert_case_camel_to_upper_snake(v1: String) -> String {
  convert_case(crate::hydra::util::CaseConvention(Rc::new(crate::hydra::util::CaseConvention_Variant::Camel)), crate::hydra::util::CaseConvention(Rc::new(crate::hydra::util::CaseConvention_Variant::UpperSnake)), v1.clone())}

pub fn convert_case_pascal_to_upper_snake(v1: String) -> String {
  convert_case(crate::hydra::util::CaseConvention(Rc::new(crate::hydra::util::CaseConvention_Variant::Pascal)), crate::hydra::util::CaseConvention(Rc::new(crate::hydra::util::CaseConvention_Variant::UpperSnake)), v1.clone())}

pub fn decapitalize(v1: String) -> String {
  map_first_letter(crate::hydra::lib::strings::to_lower, v1.clone())}

pub fn escape_with_underscore(reserved: BTreeSet<String>, s: String) -> String {
  crate::hydra::lib::logic::if_else(crate::hydra::lib::sets::member(s.clone(), reserved.clone()), crate::hydra::lib::strings::cat2(s.clone(), String::from("_")), s.clone())}

pub fn indent_lines(s: String) -> String {
  let indent = |l: String| crate::hydra::lib::strings::cat2(String::from("    "), l.clone()) ;
  crate::hydra::lib::strings::unlines(crate::hydra::lib::lists::map(indent.clone(), crate::hydra::lib::strings::lines(s.clone())))}

pub fn java_style_comment(s: String) -> String {
  crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("/**\n"), String::from(" * ")), s.clone()), String::from("\n */"))}

pub fn map_first_letter(mapping: impl Fn(String) -> String + Clone, s: String) -> String {
  let list = crate::hydra::lib::strings::to_list(s.clone()) ;
  let first_letter = mapping.clone()(crate::hydra::lib::strings::from_list(crate::hydra::lib::lists::pure(crate::hydra::lib::lists::head(list.clone())))) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::strings::null(s.clone()), s.clone(), crate::hydra::lib::strings::cat2(first_letter.clone(), crate::hydra::lib::strings::from_list(crate::hydra::lib::lists::tail(list.clone()))))}

pub fn non_alnum_to_underscores(input: String) -> String {
  let is_alnum = |c: i32| crate::hydra::lib::logic::or(crate::hydra::lib::logic::and(crate::hydra::lib::equality::gte(c.clone(), 65i32), crate::hydra::lib::equality::lte(c.clone(), 90i32)), crate::hydra::lib::logic::or(crate::hydra::lib::logic::and(crate::hydra::lib::equality::gte(c.clone(), 97i32), crate::hydra::lib::equality::lte(c.clone(), 122i32)), crate::hydra::lib::logic::and(crate::hydra::lib::equality::gte(c.clone(), 48i32), crate::hydra::lib::equality::lte(c.clone(), 57i32)))) ;
  let replace = |p: (Vec<i32>, bool), c: i32| {
    let s = crate::hydra::lib::pairs::first(p.clone()) ;
    {
      let b = crate::hydra::lib::pairs::second(p.clone()) ;
      crate::hydra::lib::logic::if_else(is_alnum.clone()(c.clone()), (crate::hydra::lib::lists::cons(c.clone(), s.clone()), false), crate::hydra::lib::logic::if_else(b.clone(), (s.clone(), true), (crate::hydra::lib::lists::cons(95i32, s.clone()), true)))}} ;
  let result = crate::hydra::lib::lists::foldl(replace.clone(), (Vec::from([]), false), crate::hydra::lib::strings::to_list(input.clone())) ;
  crate::hydra::lib::strings::from_list(crate::hydra::lib::lists::reverse(crate::hydra::lib::pairs::first(result.clone())))}

pub fn sanitize_with_underscores(reserved: BTreeSet<String>, s: String) -> String {
  escape_with_underscore(reserved.clone(), non_alnum_to_underscores(s.clone()))}

pub fn show_list(f: impl Fn(T0) -> String + Clone, els: Vec<T0>) -> String {
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("["),
    crate::hydra::lib::strings::intercalate(String::from(", "), crate::hydra::lib::lists::map(f.clone(), els.clone())),
    String::from("]")]))}

pub fn strip_leading_and_trailing_whitespace(s: String) -> String {
  crate::hydra::lib::strings::from_list(crate::hydra::lib::lists::drop_while(crate::hydra::lib::chars::is_space, crate::hydra::lib::lists::reverse(crate::hydra::lib::lists::drop_while(crate::hydra::lib::chars::is_space, crate::hydra::lib::lists::reverse(crate::hydra::lib::strings::to_list(s.clone()))))))}

pub fn with_character_aliases(original: String) -> String {
  let aliases = crate::hydra::lib::maps::from_list(Vec::from([
    (32i32, String::from("sp")),
    (33i32, String::from("excl")),
    (34i32, String::from("quot")),
    (35i32, String::from("num")),
    (36i32, String::from("dollar")),
    (37i32, String::from("percnt")),
    (38i32, String::from("amp")),
    (39i32, String::from("apos")),
    (40i32, String::from("lpar")),
    (41i32, String::from("rpar")),
    (42i32, String::from("ast")),
    (43i32, String::from("plus")),
    (44i32, String::from("comma")),
    (45i32, String::from("minus")),
    (46i32, String::from("period")),
    (47i32, String::from("sol")),
    (58i32, String::from("colon")),
    (59i32, String::from("semi")),
    (60i32, String::from("lt")),
    (61i32, String::from("equals")),
    (62i32, String::from("gt")),
    (63i32, String::from("quest")),
    (64i32, String::from("commat")),
    (91i32, String::from("lsqb")),
    (92i32, String::from("bsol")),
    (93i32, String::from("rsqb")),
    (94i32, String::from("circ")),
    (95i32, String::from("lowbar")),
    (96i32, String::from("grave")),
    (123i32, String::from("lcub")),
    (124i32, String::from("verbar")),
    (125i32, String::from("rcub")),
    (126i32, String::from("tilde"))])) ;
  let alias = |c: i32| crate::hydra::lib::maybes::from_maybe(crate::hydra::lib::lists::pure(c.clone()), crate::hydra::lib::maybes::map(crate::hydra::lib::strings::to_list, crate::hydra::lib::maps::lookup(c.clone(), aliases.clone()))) ;
  crate::hydra::lib::strings::from_list(crate::hydra::lib::lists::filter(crate::hydra::lib::chars::is_alpha_num, crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(alias.clone(), crate::hydra::lib::strings::to_list(original.clone())))))}

pub fn wrap_line(maxlen: i32, input: String) -> String {
  let helper = |prev: Vec<Vec<i32>>, rem: Vec<i32>| {
    let trunc = crate::hydra::lib::lists::take(maxlen.clone(), rem.clone()) ;
    let span_result = crate::hydra::lib::lists::span(|c: i32| crate::hydra::lib::logic::and(crate::hydra::lib::logic::not(crate::hydra::lib::equality::equal(c.clone(), 32i32)), crate::hydra::lib::logic::not(crate::hydra::lib::equality::equal(c.clone(), 9i32))), crate::hydra::lib::lists::reverse(trunc.clone())) ;
    let prefix = crate::hydra::lib::lists::reverse(crate::hydra::lib::pairs::second(span_result.clone())) ;
    let suffix = crate::hydra::lib::lists::reverse(crate::hydra::lib::pairs::first(span_result.clone())) ;
    crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::lte(crate::hydra::lib::lists::length(rem.clone()), maxlen.clone()), crate::hydra::lib::lists::reverse(crate::hydra::lib::lists::cons(rem.clone(), prev.clone())), crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(prefix.clone()), helper.clone()(crate::hydra::lib::lists::cons(trunc.clone(), prev.clone()), crate::hydra::lib::lists::drop(maxlen.clone(), rem.clone())), helper.clone()(crate::hydra::lib::lists::cons(crate::hydra::lib::lists::init(prefix.clone()), prev.clone()), crate::hydra::lib::lists::concat2(suffix.clone(), crate::hydra::lib::lists::drop(maxlen.clone(), rem.clone())))))} ;
  crate::hydra::lib::strings::from_list(crate::hydra::lib::lists::intercalate(Vec::from([
    10i32]), helper.clone()(Vec::from([]), crate::hydra::lib::strings::to_list(input.clone()))))}
