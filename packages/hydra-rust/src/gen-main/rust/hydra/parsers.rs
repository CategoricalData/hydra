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

pub fn alt(p1: crate::hydra::parsing::Parser, p2: crate::hydra::parsing::Parser) -> crate::hydra::parsing::Parser {
  let parse = |input: String| |v1: crate::hydra::parsing::ParseResult| match &*v1.clone().0 {
    crate::hydra::parsing::ParseResult_Variant::Success (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::parsing::ParseResult(Rc::new(crate::hydra::parsing::ParseResult_Variant::Success(v0_.clone())))},
    crate::hydra::parsing::ParseResult_Variant::Failure (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(v0_.clone().0.remainder.clone(), input.clone()), p2.clone().0.0.clone(), crate::hydra::parsing::ParseResult(Rc::new(crate::hydra::parsing::ParseResult_Variant::Failure(v0_.clone()))))}}(p1.clone().0.0.clone()) ;
  crate::hydra::parsing::Parser(Rc::new(crate::hydra::parsing::Parser_Variant(parse.clone())))}

pub fn any_char() -> crate::hydra::parsing::Parser {
  satisfy(|_: i32| true)}

pub fn apply(pf: crate::hydra::parsing::Parser, pa: crate::hydra::parsing::Parser) -> crate::hydra::parsing::Parser {
  let parse = |input: String| |v1: crate::hydra::parsing::ParseResult| match &*v1.clone().0 {
    crate::hydra::parsing::ParseResult_Variant::Success (v0_) => {
      let v0_ = v0_.clone() ;
      |v12: crate::hydra::parsing::ParseResult| match &*v12.clone().0 {
        crate::hydra::parsing::ParseResult_Variant::Success (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::parsing::ParseResult(Rc::new(crate::hydra::parsing::ParseResult_Variant::Success(crate::hydra::parsing::ParseSuccess(Rc::new(crate::hydra::parsing::ParseSuccess_Variant {
            value: v0_.clone().0.value.clone(),
            remainder: v0_.clone().0.remainder.clone()})))))},
        crate::hydra::parsing::ParseResult_Variant::Failure (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::parsing::ParseResult(Rc::new(crate::hydra::parsing::ParseResult_Variant::Failure(v0_.clone())))}}(pa.clone().0.0.clone())},
    crate::hydra::parsing::ParseResult_Variant::Failure (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::parsing::ParseResult(Rc::new(crate::hydra::parsing::ParseResult_Variant::Failure(v0_.clone())))}}(pf.clone().0.0.clone()) ;
  crate::hydra::parsing::Parser(Rc::new(crate::hydra::parsing::Parser_Variant(parse.clone())))}

pub fn between(open: crate::hydra::parsing::Parser, close: crate::hydra::parsing::Parser, p: crate::hydra::parsing::Parser) -> crate::hydra::parsing::Parser {
  bind(open.clone(), |_: T0| bind(p.clone(), |x: T2| bind(close.clone(), |_2: T1| pure(x.clone()))))}

pub fn bind(pa: crate::hydra::parsing::Parser, f: impl Fn(T0) -> crate::hydra::parsing::Parser + Clone) -> crate::hydra::parsing::Parser {
  let parse = |input: String| |v1: crate::hydra::parsing::ParseResult| match &*v1.clone().0 {
    crate::hydra::parsing::ParseResult_Variant::Success (v0_) => {
      let v0_ = v0_.clone() ;
      f.clone()(v0_.clone().0.value.clone()).0.0.clone()},
    crate::hydra::parsing::ParseResult_Variant::Failure (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::parsing::ParseResult(Rc::new(crate::hydra::parsing::ParseResult_Variant::Failure(v0_.clone())))}}(pa.clone().0.0.clone()) ;
  crate::hydra::parsing::Parser(Rc::new(crate::hydra::parsing::Parser_Variant(parse.clone())))}

pub fn char_(c: i32) -> crate::hydra::parsing::Parser {
  satisfy(|x: i32| crate::hydra::lib::equality::equal(x.clone(), c.clone()))}

pub fn choice(ps: Vec<crate::hydra::parsing::Parser>) -> crate::hydra::parsing::Parser {
  crate::hydra::lib::lists::foldl(alt, fail(String::from("no choice matched")), ps.clone())}

pub fn eof() -> crate::hydra::parsing::Parser {
  crate::hydra::parsing::Parser(Rc::new(crate::hydra::parsing::Parser_Variant(|input: String| crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(input.clone(), String::from("")), crate::hydra::parsing::ParseResult(Rc::new(crate::hydra::parsing::ParseResult_Variant::Success(crate::hydra::parsing::ParseSuccess(Rc::new(crate::hydra::parsing::ParseSuccess_Variant {
    value: (),
    remainder: String::from("")}))))), crate::hydra::parsing::ParseResult(Rc::new(crate::hydra::parsing::ParseResult_Variant::Failure(crate::hydra::parsing::ParseError(Rc::new(crate::hydra::parsing::ParseError_Variant {
    message: String::from("expected end of input"),
    remainder: input.clone()})))))))))}

pub fn fail(msg: String) -> crate::hydra::parsing::Parser {
  crate::hydra::parsing::Parser(Rc::new(crate::hydra::parsing::Parser_Variant(|input: String| crate::hydra::parsing::ParseResult(Rc::new(crate::hydra::parsing::ParseResult_Variant::Failure(crate::hydra::parsing::ParseError(Rc::new(crate::hydra::parsing::ParseError_Variant {
    message: msg.clone(),
    remainder: input.clone()}))))))))}

pub fn lazy(f: impl Fn(()) -> crate::hydra::parsing::Parser + Clone) -> crate::hydra::parsing::Parser {
  crate::hydra::parsing::Parser(Rc::new(crate::hydra::parsing::Parser_Variant(|input: String| f.clone()(()).0.0.clone())))}

pub fn many(p: crate::hydra::parsing::Parser) -> crate::hydra::parsing::Parser {
  alt(some(p.clone()), pure(Vec::from([])))}

pub fn map(f: impl Fn(T0) -> T1 + Clone, pa: crate::hydra::parsing::Parser) -> crate::hydra::parsing::Parser {
  let parse = |input: String| |v1: crate::hydra::parsing::ParseResult| match &*v1.clone().0 {
    crate::hydra::parsing::ParseResult_Variant::Success (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::parsing::ParseResult(Rc::new(crate::hydra::parsing::ParseResult_Variant::Success(crate::hydra::parsing::ParseSuccess(Rc::new(crate::hydra::parsing::ParseSuccess_Variant {
        value: f.clone()(v0_.clone().0.value.clone()),
        remainder: v0_.clone().0.remainder.clone()})))))},
    crate::hydra::parsing::ParseResult_Variant::Failure (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::parsing::ParseResult(Rc::new(crate::hydra::parsing::ParseResult_Variant::Failure(v0_.clone())))}}(pa.clone().0.0.clone()) ;
  crate::hydra::parsing::Parser(Rc::new(crate::hydra::parsing::Parser_Variant(parse.clone())))}

pub fn optional(p: crate::hydra::parsing::Parser) -> crate::hydra::parsing::Parser {
  alt(map(crate::hydra::lib::maybes::pure, p.clone()), pure(None))}

pub fn pure(a: T0) -> crate::hydra::parsing::Parser {
  crate::hydra::parsing::Parser(Rc::new(crate::hydra::parsing::Parser_Variant(|input: String| crate::hydra::parsing::ParseResult(Rc::new(crate::hydra::parsing::ParseResult_Variant::Success(crate::hydra::parsing::ParseSuccess(Rc::new(crate::hydra::parsing::ParseSuccess_Variant {
    value: a.clone(),
    remainder: input.clone()}))))))))}

pub fn run_parser(p: crate::hydra::parsing::Parser, input: String) -> crate::hydra::parsing::ParseResult {
  p.clone().0.0.clone()}

pub fn satisfy(pred: impl Fn(i32) -> bool + Clone) -> crate::hydra::parsing::Parser {
  let parse = |input: String| {
    let codes = crate::hydra::lib::strings::to_list(input.clone()) ;
    crate::hydra::lib::maybes::maybe(crate::hydra::parsing::ParseResult(Rc::new(crate::hydra::parsing::ParseResult_Variant::Failure(crate::hydra::parsing::ParseError(Rc::new(crate::hydra::parsing::ParseError_Variant {
      message: String::from("unexpected end of input"),
      remainder: input.clone()}))))), |c: i32| {
      let rest = crate::hydra::lib::strings::from_list(crate::hydra::lib::lists::drop(1i32, codes.clone())) ;
      crate::hydra::lib::logic::if_else(pred.clone()(c.clone()), crate::hydra::parsing::ParseResult(Rc::new(crate::hydra::parsing::ParseResult_Variant::Success(crate::hydra::parsing::ParseSuccess(Rc::new(crate::hydra::parsing::ParseSuccess_Variant {
        value: c.clone(),
        remainder: rest.clone()}))))), crate::hydra::parsing::ParseResult(Rc::new(crate::hydra::parsing::ParseResult_Variant::Failure(crate::hydra::parsing::ParseError(Rc::new(crate::hydra::parsing::ParseError_Variant {
        message: String::from("character did not satisfy predicate"),
        remainder: input.clone()}))))))}, crate::hydra::lib::lists::safe_head(codes.clone()))} ;
  crate::hydra::parsing::Parser(Rc::new(crate::hydra::parsing::Parser_Variant(parse.clone())))}

pub fn sep_by(p: crate::hydra::parsing::Parser, sep: crate::hydra::parsing::Parser) -> crate::hydra::parsing::Parser {
  alt(sep_by1(p.clone(), sep.clone()), pure(Vec::from([])))}

pub fn sep_by1(p: crate::hydra::parsing::Parser, sep: crate::hydra::parsing::Parser) -> crate::hydra::parsing::Parser {
  bind(p.clone(), |x: T0| bind(many(bind(sep.clone(), |_: T1| p.clone())), |xs: Vec<T0>| pure(crate::hydra::lib::lists::cons(x.clone(), xs.clone()))))}

pub fn some(p: crate::hydra::parsing::Parser) -> crate::hydra::parsing::Parser {
  bind(p.clone(), |x: T0| bind(many(p.clone()), |xs: Vec<T0>| pure(crate::hydra::lib::lists::cons(x.clone(), xs.clone()))))}

pub fn string(str_: String) -> crate::hydra::parsing::Parser {
  crate::hydra::parsing::Parser(Rc::new(crate::hydra::parsing::Parser_Variant(|input: String| {
    let str_codes = crate::hydra::lib::strings::to_list(str_.clone()) ;
    {
      let input_codes = crate::hydra::lib::strings::to_list(input.clone()) ;
      {
        let str_len = crate::hydra::lib::lists::length(str_codes.clone()) ;
        {
          let input_prefix = crate::hydra::lib::lists::take(str_len.clone(), input_codes.clone()) ;
          crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(str_codes.clone(), input_prefix.clone()), crate::hydra::parsing::ParseResult(Rc::new(crate::hydra::parsing::ParseResult_Variant::Success(crate::hydra::parsing::ParseSuccess(Rc::new(crate::hydra::parsing::ParseSuccess_Variant {
            value: str_.clone(),
            remainder: crate::hydra::lib::strings::from_list(crate::hydra::lib::lists::drop(str_len.clone(), input_codes.clone()))}))))), crate::hydra::parsing::ParseResult(Rc::new(crate::hydra::parsing::ParseResult_Variant::Failure(crate::hydra::parsing::ParseError(Rc::new(crate::hydra::parsing::ParseError_Variant {
            message: crate::hydra::lib::strings::cat2(String::from("expected: "), str_.clone()),
            remainder: input.clone()}))))))}}}})))}
