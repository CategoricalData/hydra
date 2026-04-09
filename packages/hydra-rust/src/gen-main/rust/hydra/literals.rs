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

pub fn bigfloat_to_float_value(ft: crate::hydra::core::FloatType, bf: OrderedFloat<f64>) -> crate::hydra::core::FloatValue {
  match &*ft.clone().0 {
    crate::hydra::core::FloatType_Variant::Bigfloat (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::FloatValue(Rc::new(crate::hydra::core::FloatValue_Variant::Bigfloat(bf.clone())))},
    crate::hydra::core::FloatType_Variant::Float32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::FloatValue(Rc::new(crate::hydra::core::FloatValue_Variant::Float32(crate::hydra::lib::literals::bigfloat_to_float32(bf.clone()))))},
    crate::hydra::core::FloatType_Variant::Float64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::FloatValue(Rc::new(crate::hydra::core::FloatValue_Variant::Float64(crate::hydra::lib::literals::bigfloat_to_float64(bf.clone()))))}}}

pub fn bigint_to_integer_value(it: crate::hydra::core::IntegerType, bi: String) -> crate::hydra::core::IntegerValue {
  match &*it.clone().0 {
    crate::hydra::core::IntegerType_Variant::Bigint (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Bigint(bi.clone())))},
    crate::hydra::core::IntegerType_Variant::Int8 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int8(crate::hydra::lib::literals::bigint_to_int8(bi.clone()))))},
    crate::hydra::core::IntegerType_Variant::Int16 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int16(crate::hydra::lib::literals::bigint_to_int16(bi.clone()))))},
    crate::hydra::core::IntegerType_Variant::Int32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int32(crate::hydra::lib::literals::bigint_to_int32(bi.clone()))))},
    crate::hydra::core::IntegerType_Variant::Int64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int64(crate::hydra::lib::literals::bigint_to_int64(bi.clone()))))},
    crate::hydra::core::IntegerType_Variant::Uint8 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Uint8(crate::hydra::lib::literals::bigint_to_uint8(bi.clone()))))},
    crate::hydra::core::IntegerType_Variant::Uint16 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Uint16(crate::hydra::lib::literals::bigint_to_uint16(bi.clone()))))},
    crate::hydra::core::IntegerType_Variant::Uint32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Uint32(crate::hydra::lib::literals::bigint_to_uint32(bi.clone()))))},
    crate::hydra::core::IntegerType_Variant::Uint64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Uint64(crate::hydra::lib::literals::bigint_to_uint64(bi.clone()))))}}}

pub fn float_value_to_bigfloat(v1: crate::hydra::core::FloatValue) -> OrderedFloat<f64> {
  match &*v1.clone().0 {
    crate::hydra::core::FloatValue_Variant::Bigfloat (v0_) => {
      let v0_ = v0_.clone() ;
      v0_.clone()},
    crate::hydra::core::FloatValue_Variant::Float32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::literals::float32_to_bigfloat(v0_.clone())},
    crate::hydra::core::FloatValue_Variant::Float64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::literals::float64_to_bigfloat(v0_.clone())}}}

pub fn integer_value_to_bigint(v1: crate::hydra::core::IntegerValue) -> String {
  match &*v1.clone().0 {
    crate::hydra::core::IntegerValue_Variant::Bigint (v0_) => {
      let v0_ = v0_.clone() ;
      v0_.clone()},
    crate::hydra::core::IntegerValue_Variant::Int8 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::literals::int8_to_bigint(v0_.clone())},
    crate::hydra::core::IntegerValue_Variant::Int16 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::literals::int16_to_bigint(v0_.clone())},
    crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::literals::int32_to_bigint(v0_.clone())},
    crate::hydra::core::IntegerValue_Variant::Int64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::literals::int64_to_bigint(v0_.clone())},
    crate::hydra::core::IntegerValue_Variant::Uint8 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::literals::uint8_to_bigint(v0_.clone())},
    crate::hydra::core::IntegerValue_Variant::Uint16 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::literals::uint16_to_bigint(v0_.clone())},
    crate::hydra::core::IntegerValue_Variant::Uint32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::literals::uint32_to_bigint(v0_.clone())},
    crate::hydra::core::IntegerValue_Variant::Uint64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::literals::uint64_to_bigint(v0_.clone())}}}
