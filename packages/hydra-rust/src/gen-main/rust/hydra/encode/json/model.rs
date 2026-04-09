#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::encode::core::*;
use crate::hydra::json::model::*;

pub fn value(v1: crate::hydra::json::model::Value) -> crate::hydra::core::Term {
  match &*v1.clone().0 {
    crate::hydra::json::model::Value_Variant::Array (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.json.model.Value")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("array")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(value, v0_.clone()))))}))})))))},
    crate::hydra::json::model::Value_Variant::Boolean (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.json.model.Value")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("boolean")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Boolean(v0_.clone()))))))}))})))))},
    crate::hydra::json::model::Value_Variant::Null (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.json.model.Value")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("null")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))},
    crate::hydra::json::model::Value_Variant::Number (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.json.model.Value")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("number")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Float(crate::hydra::core::FloatValue(Rc::new(crate::hydra::core::FloatValue_Variant::Bigfloat(v0_.clone())))))))))}))})))))},
    crate::hydra::json::model::Value_Variant::Object (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.json.model.Value")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("object")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(crate::hydra::lib::maps::bimap(|x: String| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x.clone())))))), value, v0_.clone()))))}))})))))},
    crate::hydra::json::model::Value_Variant::String (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.json.model.Value")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("string")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(v0_.clone()))))))}))})))))}}}
