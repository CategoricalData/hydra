#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::lexical::*;
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

pub fn decode_either(left_decoder: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T0>> + Clone, right_decoder: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T1>> + Clone, g: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, Either<T0, T1>> {
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|x: String| crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(x.clone()))), |x: crate::hydra::core::Term| x.clone(), crate::hydra::lexical::strip_and_dereference_term_either(g.clone(), term.clone())), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::either(|lv: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|x: T0| Left(x.clone()), left_decoder.clone()(g.clone(), lv.clone())), |rv: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|x: T1| Right(x.clone()), right_decoder.clone()(g.clone(), rv.clone())), v0_.clone())},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected either value")))))})}

pub fn decode_list(elem_decoder: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T0>> + Clone, g: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, Vec<T0>> {
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|x: String| crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(x.clone()))), |x: crate::hydra::core::Term| x.clone(), crate::hydra::lexical::strip_and_dereference_term_either(g.clone(), term.clone())), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map_list(|v1: crate::hydra::core::Term| elem_decoder.clone()(g.clone(), v1.clone()), v0_.clone())},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected list")))))})}

pub fn decode_map(key_decoder: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T0>> + Clone, val_decoder: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T1>> + Clone, g: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, BTreeMap<T0, T1>> {
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|x: String| crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(x.clone()))), |x: crate::hydra::core::Term| x.clone(), crate::hydra::lexical::strip_and_dereference_term_either(g.clone(), term.clone())), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(crate::hydra::lib::maps::from_list, crate::hydra::lib::eithers::map_list(|kv: (crate::hydra::core::Term, crate::hydra::core::Term)| crate::hydra::lib::eithers::bind(key_decoder.clone()(g.clone(), crate::hydra::lib::pairs::first(kv.clone())), |k: T0| crate::hydra::lib::eithers::map(|v: T1| (k.clone(), v.clone()), val_decoder.clone()(g.clone(), crate::hydra::lib::pairs::second(kv.clone())))), crate::hydra::lib::maps::to_list(v0_.clone())))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected map")))))})}

pub fn decode_maybe(elem_decoder: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T0>> + Clone, g: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, Option<T0>> {
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|x: String| crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(x.clone()))), |x: crate::hydra::core::Term| x.clone(), crate::hydra::lexical::strip_and_dereference_term_either(g.clone(), term.clone())), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map_maybe(|v1: crate::hydra::core::Term| elem_decoder.clone()(g.clone(), v1.clone()), v0_.clone())},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected optional value")))))})}

pub fn decode_pair(first_decoder: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T0>> + Clone, second_decoder: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T1>> + Clone, g: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, (T0, T1)> {
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|x: String| crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(x.clone()))), |x: crate::hydra::core::Term| x.clone(), crate::hydra::lexical::strip_and_dereference_term_either(g.clone(), term.clone())), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(first_decoder.clone()(g.clone(), crate::hydra::lib::pairs::first(v0_.clone())), |f: T0| crate::hydra::lib::eithers::map(|s: T1| (f.clone(), s.clone()), second_decoder.clone()(g.clone(), crate::hydra::lib::pairs::second(v0_.clone()))))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected pair")))))})}

pub fn decode_set(elem_decoder: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T0>> + Clone, g: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, BTreeSet<T0>> {
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|x: String| crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(x.clone()))), |x: crate::hydra::core::Term| x.clone(), crate::hydra::lexical::strip_and_dereference_term_either(g.clone(), term.clone())), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(crate::hydra::lib::sets::from_list, crate::hydra::lib::eithers::map_list(|v1: crate::hydra::core::Term| elem_decoder.clone()(g.clone(), v1.clone()), crate::hydra::lib::sets::to_list(v0_.clone())))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected set")))))})}

pub fn decode_unit(g: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, ()> {
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|x: String| crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(x.clone()))), |x: crate::hydra::core::Term| x.clone(), crate::hydra::lexical::strip_and_dereference_term_either(g.clone(), term.clone())), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      Right(())},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected a unit value")))))})}

pub fn decode_wrapped(body_decoder: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T0>> + Clone, g: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T0> {
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|x: String| crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(x.clone()))), |x: crate::hydra::core::Term| x.clone(), crate::hydra::lexical::strip_and_dereference_term_either(g.clone(), term.clone())), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      body_decoder.clone()(g.clone(), v0_.clone().0.body.clone())},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped value")))))})}

pub fn require_field(field_name: String, decoder: impl Fn(T0) -> Rc<dyn Fn(T1) -> Either<crate::hydra::error::DecodingError, T2>> + Clone, field_map: BTreeMap<crate::hydra::core::Name, T1>, g: T0) -> Either<crate::hydra::error::DecodingError, T2> {
  crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
    String::from("missing field "),
    field_name.clone(),
    String::from(" in record")])))))), |field_term: T1| decoder.clone()(g.clone(), field_term.clone()), crate::hydra::lib::maps::lookup(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(field_name.clone()))), field_map.clone()))}

pub fn to_field_map(record: crate::hydra::core::Record) -> BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term> {
  crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|f: crate::hydra::core::Field| (f.clone().0.name.clone(), f.clone().0.term.clone()), record.clone().0.fields.clone()))}
