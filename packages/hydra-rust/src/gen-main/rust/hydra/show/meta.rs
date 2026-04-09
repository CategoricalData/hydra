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

pub fn term_variant(v1: crate::hydra::variants::TermVariant) -> String {
  match &*v1.clone().0 {
    crate::hydra::variants::TermVariant_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("annotated")},
    crate::hydra::variants::TermVariant_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("application")},
    crate::hydra::variants::TermVariant_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("either")},
    crate::hydra::variants::TermVariant_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("function")},
    crate::hydra::variants::TermVariant_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("let")},
    crate::hydra::variants::TermVariant_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("list")},
    crate::hydra::variants::TermVariant_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("literal")},
    crate::hydra::variants::TermVariant_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("map")},
    crate::hydra::variants::TermVariant_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("maybe")},
    crate::hydra::variants::TermVariant_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("pair")},
    crate::hydra::variants::TermVariant_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("record")},
    crate::hydra::variants::TermVariant_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("set")},
    crate::hydra::variants::TermVariant_Variant::TypeLambda (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("typeLambda")},
    crate::hydra::variants::TermVariant_Variant::TypeApplication (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("typeApplication")},
    crate::hydra::variants::TermVariant_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("union")},
    crate::hydra::variants::TermVariant_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("unit")},
    crate::hydra::variants::TermVariant_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("variable")},
    crate::hydra::variants::TermVariant_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("wrap")}}}

pub fn type_variant(v1: crate::hydra::variants::TypeVariant) -> String {
  match &*v1.clone().0 {
    crate::hydra::variants::TypeVariant_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("annotated")},
    crate::hydra::variants::TypeVariant_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("application")},
    crate::hydra::variants::TypeVariant_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("either")},
    crate::hydra::variants::TypeVariant_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("forall")},
    crate::hydra::variants::TypeVariant_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("function")},
    crate::hydra::variants::TypeVariant_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("list")},
    crate::hydra::variants::TypeVariant_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("literal")},
    crate::hydra::variants::TypeVariant_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("map")},
    crate::hydra::variants::TypeVariant_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("maybe")},
    crate::hydra::variants::TypeVariant_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("pair")},
    crate::hydra::variants::TypeVariant_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("record")},
    crate::hydra::variants::TypeVariant_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("set")},
    crate::hydra::variants::TypeVariant_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("union")},
    crate::hydra::variants::TypeVariant_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("unit")},
    crate::hydra::variants::TypeVariant_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("variable")},
    crate::hydra::variants::TypeVariant_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("wrap")}}}
