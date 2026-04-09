#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::constants::*;
use crate::hydra::decode::core::*;
use crate::hydra::show::core::*;
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

pub fn graph_to_schema(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, els: Vec<crate::hydra::core::Binding>) -> Either<crate::hydra::context::InContext, BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>> {
  let to_pair = |el: crate::hydra::core::Binding| {
    let name = el.clone().0.name.clone() ;
    crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|_wc_e: crate::hydra::error::DecodingError| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: _wc_e.clone(),
      context: cx.clone()})), |_wc_a: crate::hydra::core::Type| _wc_a.clone(), crate::hydra::decode::core::type_(graph.clone(), el.clone().0.term.clone())), |t: crate::hydra::core::Type| Right((name.clone(), t.clone())))} ;
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(to_pair.clone(), els.clone()), |pairs: Vec<(crate::hydra::core::Name, crate::hydra::core::Type)>| Right(crate::hydra::lib::maps::from_list(pairs.clone())))}

pub fn instantiate_template(cx: crate::hydra::context::Context, minimal: bool, schema: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>, tname: crate::hydra::core::Name, t: crate::hydra::core::Type) -> Either<crate::hydra::context::InContext, crate::hydra::core::Term> {
  let inst = |tn: crate::hydra::core::Name, v1: crate::hydra::core::Type| instantiate_template(cx.clone(), minimal.clone(), schema.clone(), tn.clone(), v1.clone()) ;
  let no_poly = Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(String::from("Polymorphic and function types are not currently supported"))))))),
    context: cx.clone()}))) ;
  let for_float = |ft: crate::hydra::core::FloatType| match &*ft.clone().0 {
    crate::hydra::core::FloatType_Variant::Bigfloat (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::FloatValue(Rc::new(crate::hydra::core::FloatValue_Variant::Bigfloat(0.0)))},
    crate::hydra::core::FloatType_Variant::Float32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::FloatValue(Rc::new(crate::hydra::core::FloatValue_Variant::Float32(0.0f32)))},
    crate::hydra::core::FloatType_Variant::Float64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::FloatValue(Rc::new(crate::hydra::core::FloatValue_Variant::Float64(0.0)))}} ;
  let for_integer = |it: crate::hydra::core::IntegerType| match &*it.clone().0 {
    crate::hydra::core::IntegerType_Variant::Bigint (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Bigint(String::from("0:bigint"))))},
    crate::hydra::core::IntegerType_Variant::Int8 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int8(0i8)))},
    crate::hydra::core::IntegerType_Variant::Int16 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int16(0i16)))},
    crate::hydra::core::IntegerType_Variant::Int32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int32(0i32)))},
    crate::hydra::core::IntegerType_Variant::Int64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int64(0i64)))},
    crate::hydra::core::IntegerType_Variant::Uint8 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Uint8(0u8)))},
    crate::hydra::core::IntegerType_Variant::Uint16 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Uint16(0u16)))},
    crate::hydra::core::IntegerType_Variant::Uint32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Uint32(0u32)))},
    crate::hydra::core::IntegerType_Variant::Uint64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Uint64(0u64)))}} ;
  let for_literal = |lt: crate::hydra::core::LiteralType| match &*lt.clone().0 {
    crate::hydra::core::LiteralType_Variant::Binary (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from(""))))},
    crate::hydra::core::LiteralType_Variant::Boolean (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Boolean(false)))},
    crate::hydra::core::LiteralType_Variant::Integer (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(for_integer.clone()(v0_.clone()))))},
    crate::hydra::core::LiteralType_Variant::Float (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Float(for_float.clone()(v0_.clone()))))},
    crate::hydra::core::LiteralType_Variant::String (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from(""))))}} ;
  match &*t.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      inst.clone()(tname.clone(), v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      no_poly.clone()},
    crate::hydra::core::Type_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      no_poly.clone()},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      no_poly.clone()},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(minimal.clone(), Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(Vec::from([]))))), crate::hydra::lib::eithers::bind(inst.clone()(tname.clone(), v0_.clone()), |e: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(Vec::from([
        e.clone()])))))))},
    crate::hydra::core::Type_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(for_literal.clone()(v0_.clone())))))},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let kt = v0_.clone().0.keys.clone() ;
        {
          let vt = v0_.clone().0.values.clone() ;
          crate::hydra::lib::logic::if_else(minimal.clone(), Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(crate::hydra::lib::maps::empty)))), crate::hydra::lib::eithers::bind(inst.clone()(tname.clone(), kt.clone()), |ke: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(inst.clone()(tname.clone(), vt.clone()), |ve: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(crate::hydra::lib::maps::singleton(ke.clone(), ve.clone()))))))))}}},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(minimal.clone(), Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(None)))), crate::hydra::lib::eithers::bind(inst.clone()(tname.clone(), v0_.clone()), |e: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(Some(e.clone())))))))},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let to_field = |ft: crate::hydra::core::FieldType| crate::hydra::lib::eithers::bind(inst.clone()(tname.clone(), ft.clone().0.type_.clone()), |e: crate::hydra::core::Term| Right(crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: ft.clone().0.name.clone(),
          term: e.clone()})))) ;
        crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(to_field.clone(), v0_.clone()), |dfields: Vec<crate::hydra::core::Field>| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
          type_name: tname.clone(),
          fields: dfields.clone()})))))))}},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(minimal.clone(), Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Set(crate::hydra::lib::sets::empty)))), crate::hydra::lib::eithers::bind(inst.clone()(tname.clone(), v0_.clone()), |e: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Set(crate::hydra::lib::sets::from_list(Vec::from([
        e.clone()]))))))))},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
        object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(String::from("Type variable "), crate::hydra::lib::strings::cat2(crate::hydra::show::core::term(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(v0_.clone())))), String::from(" not found in schema"))))))))),
        context: cx.clone()}))), |v1: crate::hydra::core::Type| inst.clone()(v0_.clone(), v1.clone()), crate::hydra::lib::maps::lookup(v0_.clone(), schema.clone()))},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(inst.clone()(tname.clone(), v0_.clone()), |e: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
        type_name: tname.clone(),
        body: e.clone()})))))))}}}
