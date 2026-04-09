#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::encode::core::*;
use crate::hydra::encode::context::*;
use crate::hydra::typing::*;

pub fn function_structure(env: impl Fn(T0) -> crate::hydra::core::Term + Clone, x: crate::hydra::typing::FunctionStructure) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.typing.FunctionStructure")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeParams")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(crate::hydra::encode::core::name, x.clone().0.type_params.clone()))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("params")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(crate::hydra::encode::core::name, x.clone().0.params.clone()))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("bindings")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(crate::hydra::encode::core::binding, x.clone().0.bindings.clone()))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("body")))),
        term: crate::hydra::encode::core::term(x.clone().0.body.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("domains")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(crate::hydra::encode::core::type_, x.clone().0.domains.clone()))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("codomain")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(crate::hydra::lib::maybes::map(crate::hydra::encode::core::type_, x.clone().0.codomain.clone()))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("environment")))),
        term: env.clone()(x.clone().0.environment.clone())}))])})))))}

pub fn inference_result(x: crate::hydra::typing::InferenceResult) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.typing.InferenceResult")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("term")))),
        term: crate::hydra::encode::core::term(x.clone().0.term.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
        term: crate::hydra::encode::core::type_(x.clone().0.type_.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("subst")))),
        term: type_subst(x.clone().0.subst.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("classConstraints")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(crate::hydra::lib::maps::bimap(crate::hydra::encode::core::name, crate::hydra::encode::core::type_variable_metadata, x.clone().0.class_constraints.clone()))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("context")))),
        term: crate::hydra::encode::context::context(x.clone().0.context.clone())}))])})))))}

pub fn term_subst(x: crate::hydra::typing::TermSubst) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.typing.TermSubst")))),
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(crate::hydra::lib::maps::bimap(crate::hydra::encode::core::name, crate::hydra::encode::core::term, x.clone().0.0.clone()))))})))))}

pub fn type_constraint(x: crate::hydra::typing::TypeConstraint) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.typing.TypeConstraint")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("left")))),
        term: crate::hydra::encode::core::type_(x.clone().0.left.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("right")))),
        term: crate::hydra::encode::core::type_(x.clone().0.right.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("comment")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x.clone().0.comment.clone()))))))}))])})))))}

pub fn type_subst(x: crate::hydra::typing::TypeSubst) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.typing.TypeSubst")))),
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(crate::hydra::lib::maps::bimap(crate::hydra::encode::core::name, crate::hydra::encode::core::type_, x.clone().0.0.clone()))))})))))}
