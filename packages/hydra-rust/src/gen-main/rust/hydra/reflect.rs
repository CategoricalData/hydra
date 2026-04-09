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

pub fn elimination_variant(v1: crate::hydra::core::Elimination) -> crate::hydra::variants::EliminationVariant {
  match &*v1.clone().0 {
    crate::hydra::core::Elimination_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::EliminationVariant(Rc::new(crate::hydra::variants::EliminationVariant_Variant::Record))},
    crate::hydra::core::Elimination_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::EliminationVariant(Rc::new(crate::hydra::variants::EliminationVariant_Variant::Union))},
    crate::hydra::core::Elimination_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::EliminationVariant(Rc::new(crate::hydra::variants::EliminationVariant_Variant::Wrap))}}}

pub fn elimination_variants() -> Vec<crate::hydra::variants::EliminationVariant> {
  Vec::from([
    crate::hydra::variants::EliminationVariant(Rc::new(crate::hydra::variants::EliminationVariant_Variant::Record)),
    crate::hydra::variants::EliminationVariant(Rc::new(crate::hydra::variants::EliminationVariant_Variant::Union)),
    crate::hydra::variants::EliminationVariant(Rc::new(crate::hydra::variants::EliminationVariant_Variant::Wrap))])}

pub fn float_type_precision(v1: crate::hydra::core::FloatType) -> crate::hydra::util::Precision {
  match &*v1.clone().0 {
    crate::hydra::core::FloatType_Variant::Bigfloat (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::util::Precision(Rc::new(crate::hydra::util::Precision_Variant::Arbitrary))},
    crate::hydra::core::FloatType_Variant::Float32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::util::Precision(Rc::new(crate::hydra::util::Precision_Variant::Bits(32i32)))},
    crate::hydra::core::FloatType_Variant::Float64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::util::Precision(Rc::new(crate::hydra::util::Precision_Variant::Bits(64i32)))}}}

pub fn float_types() -> Vec<crate::hydra::core::FloatType> {
  Vec::from([
    crate::hydra::core::FloatType(Rc::new(crate::hydra::core::FloatType_Variant::Bigfloat)),
    crate::hydra::core::FloatType(Rc::new(crate::hydra::core::FloatType_Variant::Float32)),
    crate::hydra::core::FloatType(Rc::new(crate::hydra::core::FloatType_Variant::Float64))])}

pub fn float_value_type(v1: crate::hydra::core::FloatValue) -> crate::hydra::core::FloatType {
  match &*v1.clone().0 {
    crate::hydra::core::FloatValue_Variant::Bigfloat (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::FloatType(Rc::new(crate::hydra::core::FloatType_Variant::Bigfloat))},
    crate::hydra::core::FloatValue_Variant::Float32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::FloatType(Rc::new(crate::hydra::core::FloatType_Variant::Float32))},
    crate::hydra::core::FloatValue_Variant::Float64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::FloatType(Rc::new(crate::hydra::core::FloatType_Variant::Float64))}}}

pub fn function_variant(v1: crate::hydra::core::Function) -> crate::hydra::variants::FunctionVariant {
  match &*v1.clone().0 {
    crate::hydra::core::Function_Variant::Elimination (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::FunctionVariant(Rc::new(crate::hydra::variants::FunctionVariant_Variant::Elimination))},
    crate::hydra::core::Function_Variant::Lambda (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::FunctionVariant(Rc::new(crate::hydra::variants::FunctionVariant_Variant::Lambda))},
    crate::hydra::core::Function_Variant::Primitive (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::FunctionVariant(Rc::new(crate::hydra::variants::FunctionVariant_Variant::Primitive))}}}

pub fn function_variants() -> Vec<crate::hydra::variants::FunctionVariant> {
  Vec::from([
    crate::hydra::variants::FunctionVariant(Rc::new(crate::hydra::variants::FunctionVariant_Variant::Elimination)),
    crate::hydra::variants::FunctionVariant(Rc::new(crate::hydra::variants::FunctionVariant_Variant::Lambda)),
    crate::hydra::variants::FunctionVariant(Rc::new(crate::hydra::variants::FunctionVariant_Variant::Primitive))])}

pub fn integer_type_is_signed(v1: crate::hydra::core::IntegerType) -> bool {
  match &*v1.clone().0 {
    crate::hydra::core::IntegerType_Variant::Bigint (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    crate::hydra::core::IntegerType_Variant::Int8 (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    crate::hydra::core::IntegerType_Variant::Int16 (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    crate::hydra::core::IntegerType_Variant::Int32 (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    crate::hydra::core::IntegerType_Variant::Int64 (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    crate::hydra::core::IntegerType_Variant::Uint8 (v0_) => {
      let v0_ = v0_.clone() ;
      false},
    crate::hydra::core::IntegerType_Variant::Uint16 (v0_) => {
      let v0_ = v0_.clone() ;
      false},
    crate::hydra::core::IntegerType_Variant::Uint32 (v0_) => {
      let v0_ = v0_.clone() ;
      false},
    crate::hydra::core::IntegerType_Variant::Uint64 (v0_) => {
      let v0_ = v0_.clone() ;
      false}}}

pub fn integer_type_precision(v1: crate::hydra::core::IntegerType) -> crate::hydra::util::Precision {
  match &*v1.clone().0 {
    crate::hydra::core::IntegerType_Variant::Bigint (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::util::Precision(Rc::new(crate::hydra::util::Precision_Variant::Arbitrary))},
    crate::hydra::core::IntegerType_Variant::Int8 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::util::Precision(Rc::new(crate::hydra::util::Precision_Variant::Bits(8i32)))},
    crate::hydra::core::IntegerType_Variant::Int16 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::util::Precision(Rc::new(crate::hydra::util::Precision_Variant::Bits(16i32)))},
    crate::hydra::core::IntegerType_Variant::Int32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::util::Precision(Rc::new(crate::hydra::util::Precision_Variant::Bits(32i32)))},
    crate::hydra::core::IntegerType_Variant::Int64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::util::Precision(Rc::new(crate::hydra::util::Precision_Variant::Bits(64i32)))},
    crate::hydra::core::IntegerType_Variant::Uint8 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::util::Precision(Rc::new(crate::hydra::util::Precision_Variant::Bits(8i32)))},
    crate::hydra::core::IntegerType_Variant::Uint16 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::util::Precision(Rc::new(crate::hydra::util::Precision_Variant::Bits(16i32)))},
    crate::hydra::core::IntegerType_Variant::Uint32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::util::Precision(Rc::new(crate::hydra::util::Precision_Variant::Bits(32i32)))},
    crate::hydra::core::IntegerType_Variant::Uint64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::util::Precision(Rc::new(crate::hydra::util::Precision_Variant::Bits(64i32)))}}}

pub fn integer_types() -> Vec<crate::hydra::core::IntegerType> {
  Vec::from([
    crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Bigint)),
    crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int8)),
    crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int16)),
    crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int32)),
    crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int64)),
    crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint8)),
    crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint16)),
    crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint32)),
    crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint64))])}

pub fn integer_value_type(v1: crate::hydra::core::IntegerValue) -> crate::hydra::core::IntegerType {
  match &*v1.clone().0 {
    crate::hydra::core::IntegerValue_Variant::Bigint (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Bigint))},
    crate::hydra::core::IntegerValue_Variant::Int8 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int8))},
    crate::hydra::core::IntegerValue_Variant::Int16 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int16))},
    crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int32))},
    crate::hydra::core::IntegerValue_Variant::Int64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int64))},
    crate::hydra::core::IntegerValue_Variant::Uint8 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint8))},
    crate::hydra::core::IntegerValue_Variant::Uint16 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint16))},
    crate::hydra::core::IntegerValue_Variant::Uint32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint32))},
    crate::hydra::core::IntegerValue_Variant::Uint64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint64))}}}

pub fn literal_type(v1: crate::hydra::core::Literal) -> crate::hydra::core::LiteralType {
  match &*v1.clone().0 {
    crate::hydra::core::Literal_Variant::Binary (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Binary))},
    crate::hydra::core::Literal_Variant::Boolean (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Boolean))},
    crate::hydra::core::Literal_Variant::Float (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Float(float_value_type(v0_.clone()))))},
    crate::hydra::core::Literal_Variant::Integer (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Integer(integer_value_type(v0_.clone()))))},
    crate::hydra::core::Literal_Variant::String (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String))}}}

pub fn literal_type_variant(v1: crate::hydra::core::LiteralType) -> crate::hydra::variants::LiteralVariant {
  match &*v1.clone().0 {
    crate::hydra::core::LiteralType_Variant::Binary (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::LiteralVariant(Rc::new(crate::hydra::variants::LiteralVariant_Variant::Binary))},
    crate::hydra::core::LiteralType_Variant::Boolean (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::LiteralVariant(Rc::new(crate::hydra::variants::LiteralVariant_Variant::Boolean))},
    crate::hydra::core::LiteralType_Variant::Float (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::LiteralVariant(Rc::new(crate::hydra::variants::LiteralVariant_Variant::Float))},
    crate::hydra::core::LiteralType_Variant::Integer (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::LiteralVariant(Rc::new(crate::hydra::variants::LiteralVariant_Variant::Integer))},
    crate::hydra::core::LiteralType_Variant::String (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::LiteralVariant(Rc::new(crate::hydra::variants::LiteralVariant_Variant::String))}}}

pub fn literal_types() -> Vec<crate::hydra::core::LiteralType> {
  crate::hydra::lib::lists::concat(Vec::from([
    Vec::from([
      crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Binary)),
      crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Boolean))]),
    crate::hydra::lib::lists::map(|x: crate::hydra::core::FloatType| crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Float(x.clone()))), float_types),
    crate::hydra::lib::lists::map(|x: crate::hydra::core::IntegerType| crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Integer(x.clone()))), integer_types),
    Vec::from([
      crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String))])]))}

pub fn literal_variant(arg_: crate::hydra::core::Literal) -> crate::hydra::variants::LiteralVariant {
  literal_type_variant(literal_type(arg_.clone()))}

pub fn literal_variants() -> Vec<crate::hydra::variants::LiteralVariant> {
  Vec::from([
    crate::hydra::variants::LiteralVariant(Rc::new(crate::hydra::variants::LiteralVariant_Variant::Binary)),
    crate::hydra::variants::LiteralVariant(Rc::new(crate::hydra::variants::LiteralVariant_Variant::Boolean)),
    crate::hydra::variants::LiteralVariant(Rc::new(crate::hydra::variants::LiteralVariant_Variant::Float)),
    crate::hydra::variants::LiteralVariant(Rc::new(crate::hydra::variants::LiteralVariant_Variant::Integer)),
    crate::hydra::variants::LiteralVariant(Rc::new(crate::hydra::variants::LiteralVariant_Variant::String))])}

pub fn term_variant(v1: crate::hydra::core::Term) -> crate::hydra::variants::TermVariant {
  match &*v1.clone().0 {
    crate::hydra::core::Term_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Annotated))},
    crate::hydra::core::Term_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Application))},
    crate::hydra::core::Term_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Either))},
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Function))},
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Let))},
    crate::hydra::core::Term_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::List))},
    crate::hydra::core::Term_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Literal))},
    crate::hydra::core::Term_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Map))},
    crate::hydra::core::Term_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Maybe))},
    crate::hydra::core::Term_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Pair))},
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Record))},
    crate::hydra::core::Term_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Set))},
    crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::TypeApplication))},
    crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::TypeLambda))},
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Union))},
    crate::hydra::core::Term_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Unit))},
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Variable))},
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Wrap))}}}

pub fn term_variants() -> Vec<crate::hydra::variants::TermVariant> {
  Vec::from([
    crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Annotated)),
    crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Application)),
    crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Either)),
    crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Function)),
    crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::List)),
    crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Literal)),
    crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Map)),
    crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Maybe)),
    crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Pair)),
    crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Record)),
    crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Set)),
    crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::TypeLambda)),
    crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::TypeApplication)),
    crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Union)),
    crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Unit)),
    crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Variable)),
    crate::hydra::variants::TermVariant(Rc::new(crate::hydra::variants::TermVariant_Variant::Wrap))])}

pub fn type_variant(v1: crate::hydra::core::Type) -> crate::hydra::variants::TypeVariant {
  match &*v1.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Annotated))},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Application))},
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Either))},
    crate::hydra::core::Type_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Function))},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Forall))},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::List))},
    crate::hydra::core::Type_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Literal))},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Map))},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Maybe))},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Pair))},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Record))},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Set))},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Union))},
    crate::hydra::core::Type_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Unit))},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Variable))},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Wrap))}}}

pub fn type_variants() -> Vec<crate::hydra::variants::TypeVariant> {
  Vec::from([
    crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Annotated)),
    crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Application)),
    crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Either)),
    crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Function)),
    crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Forall)),
    crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::List)),
    crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Literal)),
    crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Map)),
    crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Wrap)),
    crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Maybe)),
    crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Pair)),
    crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Record)),
    crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Set)),
    crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Union)),
    crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Unit)),
    crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Variable))])}
