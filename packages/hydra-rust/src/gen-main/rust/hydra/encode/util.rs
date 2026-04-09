#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::encode::context::*;
use crate::hydra::encode::core::*;
use crate::hydra::encode::error::*;
use crate::hydra::util::*;

pub fn case_convention(v1: crate::hydra::util::CaseConvention) -> crate::hydra::core::Term {
  match &*v1.clone().0 {
    crate::hydra::util::CaseConvention_Variant::Camel (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.CaseConvention")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("camel")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))},
    crate::hydra::util::CaseConvention_Variant::Pascal (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.CaseConvention")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("pascal")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))},
    crate::hydra::util::CaseConvention_Variant::LowerSnake (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.CaseConvention")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("lowerSnake")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))},
    crate::hydra::util::CaseConvention_Variant::UpperSnake (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.CaseConvention")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("upperSnake")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))}}}

pub fn comparison(v1: crate::hydra::util::Comparison) -> crate::hydra::core::Term {
  match &*v1.clone().0 {
    crate::hydra::util::Comparison_Variant::LessThan (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.Comparison")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("lessThan")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))},
    crate::hydra::util::Comparison_Variant::EqualTo (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.Comparison")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("equalTo")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))},
    crate::hydra::util::Comparison_Variant::GreaterThan (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.Comparison")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("greaterThan")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))}}}

pub fn precision(v1: crate::hydra::util::Precision) -> crate::hydra::core::Term {
  match &*v1.clone().0 {
    crate::hydra::util::Precision_Variant::Arbitrary (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.Precision")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("arbitrary")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))},
    crate::hydra::util::Precision_Variant::Bits (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.Precision")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("bits")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int32(v0_.clone())))))))))}))})))))}}}
