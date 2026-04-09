#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::lexical::*;
use crate::hydra::rewriting::*;
use crate::hydra::show::core::*;
use crate::hydra::show::error::*;
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

pub fn bigfloat(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, OrderedFloat<f64>> {
  crate::hydra::lib::eithers::bind(literal(cx.clone(), graph.clone(), t.clone()), |l: crate::hydra::core::Literal| crate::hydra::lib::eithers::bind(float_literal(cx.clone(), l.clone()), |f: crate::hydra::core::FloatValue| bigfloat_value(cx.clone(), f.clone())))}

pub fn bigfloat_value(cx: crate::hydra::context::Context, v: crate::hydra::core::FloatValue) -> Either<crate::hydra::context::InContext, OrderedFloat<f64>> {
  match &*v.clone().0 {
    crate::hydra::core::FloatValue_Variant::Bigfloat (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("bigfloat")), String::from(" but found ")), crate::hydra::show::core::float(v.clone())))))))),
      context: cx.clone()})))}}

pub fn bigint(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, String> {
  crate::hydra::lib::eithers::bind(literal(cx.clone(), graph.clone(), t.clone()), |l: crate::hydra::core::Literal| crate::hydra::lib::eithers::bind(integer_literal(cx.clone(), l.clone()), |i: crate::hydra::core::IntegerValue| bigint_value(cx.clone(), i.clone())))}

pub fn bigint_value(cx: crate::hydra::context::Context, v: crate::hydra::core::IntegerValue) -> Either<crate::hydra::context::InContext, String> {
  match &*v.clone().0 {
    crate::hydra::core::IntegerValue_Variant::Bigint (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("bigint")), String::from(" but found ")), crate::hydra::show::core::integer(v.clone())))))))),
      context: cx.clone()})))}}

pub fn binary(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, Vec<u8>> {
  crate::hydra::lib::eithers::bind(literal(cx.clone(), graph.clone(), t.clone()), |l: crate::hydra::core::Literal| binary_literal(cx.clone(), l.clone()))}

pub fn binary_literal(cx: crate::hydra::context::Context, v: crate::hydra::core::Literal) -> Either<crate::hydra::context::InContext, Vec<u8>> {
  match &*v.clone().0 {
    crate::hydra::core::Literal_Variant::Binary (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("binary")), String::from(" but found ")), crate::hydra::show::core::literal(v.clone())))))))),
      context: cx.clone()})))}}

pub fn boolean(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, bool> {
  crate::hydra::lib::eithers::bind(literal(cx.clone(), graph.clone(), t.clone()), |l: crate::hydra::core::Literal| boolean_literal(cx.clone(), l.clone()))}

pub fn boolean_literal(cx: crate::hydra::context::Context, v: crate::hydra::core::Literal) -> Either<crate::hydra::context::InContext, bool> {
  match &*v.clone().0 {
    crate::hydra::core::Literal_Variant::Boolean (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("boolean")), String::from(" but found ")), crate::hydra::show::core::literal(v.clone())))))))),
      context: cx.clone()})))}}

pub fn case_field(cx: crate::hydra::context::Context, name: crate::hydra::core::Name, n: String, graph: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Field> {
  let field_name = crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(n.clone()))) ;
  crate::hydra::lib::eithers::bind(cases(cx.clone(), name.clone(), graph.clone(), term.clone()), |cs: crate::hydra::core::CaseStatement| {
    let matching = crate::hydra::lib::lists::filter(|f: crate::hydra::core::Field| crate::hydra::lib::equality::equal(f.clone().0.name.clone().0.0.clone(), field_name.clone().0.0.clone()), cs.clone().0.cases.clone()) ;
    crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(matching.clone()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(String::from("not enough cases"))))))),
      context: cx.clone()}))), Right(crate::hydra::lib::lists::head(matching.clone())))})}

pub fn cases(cx: crate::hydra::context::Context, name: crate::hydra::core::Name, graph: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::CaseStatement> {
  crate::hydra::lib::eithers::bind(crate::hydra::lexical::strip_and_dereference_term(cx.clone(), graph.clone(), term0.clone()), |term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Function_Variant::Elimination (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::Elimination_Variant::Union (v0_) => {
              let v0_ = v0_.clone() ;
              crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(v0_.clone().0.type_name.clone().0.0.clone(), name.clone().0.0.clone()), Right(v0_.clone()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
                object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), crate::hydra::lib::strings::cat2(String::from("case statement for type "), name.clone().0.0.clone())), String::from(" but found ")), crate::hydra::show::core::term(term.clone())))))))),
                context: cx.clone()}))))},
            _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
              object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("case statement")), String::from(" but found ")), crate::hydra::show::core::term(term.clone())))))))),
              context: cx.clone()})))}},
        _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
          object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("case statement")), String::from(" but found ")), crate::hydra::show::core::term(term.clone())))))))),
          context: cx.clone()})))}},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("case statement")), String::from(" but found ")), crate::hydra::show::core::term(term.clone())))))))),
      context: cx.clone()})))})}

pub fn field(cx: crate::hydra::context::Context, fname: crate::hydra::core::Name, mapping: impl Fn(crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, T0> + Clone, graph: crate::hydra::graph::Graph, fields: Vec<crate::hydra::core::Field>) -> Either<crate::hydra::context::InContext, T0> {
  let matching_fields = crate::hydra::lib::lists::filter(|f: crate::hydra::core::Field| crate::hydra::lib::equality::equal(f.clone().0.name.clone().0.0.clone(), fname.clone().0.0.clone()), fields.clone()) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(matching_fields.clone()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), crate::hydra::lib::strings::cat2(String::from("field "), fname.clone().0.0.clone())), String::from(" but found ")), String::from("no matching field")))))))),
    context: cx.clone()}))), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(crate::hydra::lib::lists::length(matching_fields.clone()), 1i32), crate::hydra::lib::eithers::bind(crate::hydra::lexical::strip_and_dereference_term(cx.clone(), graph.clone(), crate::hydra::lib::lists::head(matching_fields.clone()).0.term.clone()), |stripped: crate::hydra::core::Term| mapping.clone()(stripped.clone())), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("single field")), String::from(" but found ")), crate::hydra::lib::strings::cat2(String::from("multiple fields named "), fname.clone().0.0.clone())))))))),
    context: cx.clone()})))))}

pub fn float32(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, OrderedFloat<f32>> {
  crate::hydra::lib::eithers::bind(literal(cx.clone(), graph.clone(), t.clone()), |l: crate::hydra::core::Literal| crate::hydra::lib::eithers::bind(float_literal(cx.clone(), l.clone()), |f: crate::hydra::core::FloatValue| float32_value(cx.clone(), f.clone())))}

pub fn float32_value(cx: crate::hydra::context::Context, v: crate::hydra::core::FloatValue) -> Either<crate::hydra::context::InContext, OrderedFloat<f32>> {
  match &*v.clone().0 {
    crate::hydra::core::FloatValue_Variant::Float32 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("float32")), String::from(" but found ")), crate::hydra::show::core::float(v.clone())))))))),
      context: cx.clone()})))}}

pub fn float64(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, OrderedFloat<f64>> {
  crate::hydra::lib::eithers::bind(literal(cx.clone(), graph.clone(), t.clone()), |l: crate::hydra::core::Literal| crate::hydra::lib::eithers::bind(float_literal(cx.clone(), l.clone()), |f: crate::hydra::core::FloatValue| float64_value(cx.clone(), f.clone())))}

pub fn float64_value(cx: crate::hydra::context::Context, v: crate::hydra::core::FloatValue) -> Either<crate::hydra::context::InContext, OrderedFloat<f64>> {
  match &*v.clone().0 {
    crate::hydra::core::FloatValue_Variant::Float64 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("float64")), String::from(" but found ")), crate::hydra::show::core::float(v.clone())))))))),
      context: cx.clone()})))}}

pub fn float_literal(cx: crate::hydra::context::Context, lit: crate::hydra::core::Literal) -> Either<crate::hydra::context::InContext, crate::hydra::core::FloatValue> {
  match &*lit.clone().0 {
    crate::hydra::core::Literal_Variant::Float (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("floating-point value")), String::from(" but found ")), crate::hydra::show::core::literal(lit.clone())))))))),
      context: cx.clone()})))}}

pub fn float_value(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::FloatValue> {
  crate::hydra::lib::eithers::bind(literal(cx.clone(), graph.clone(), t.clone()), |l: crate::hydra::core::Literal| float_literal(cx.clone(), l.clone()))}

pub fn either_term(cx: crate::hydra::context::Context, left_fun: impl Fn(crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, T0> + Clone, right_fun: impl Fn(crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, T1> + Clone, graph: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, Either<T0, T1>> {
  crate::hydra::lib::eithers::bind(crate::hydra::lexical::strip_and_dereference_term(cx.clone(), graph.clone(), term0.clone()), |term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::either(|l: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|x: T0| Left(x.clone()), left_fun.clone()(l.clone())), |r: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|x: T1| Right(x.clone()), right_fun.clone()(r.clone())), v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("either value")), String::from(" but found ")), crate::hydra::show::core::term(term.clone())))))))),
      context: cx.clone()})))})}

pub fn either_type(cx: crate::hydra::context::Context, typ: crate::hydra::core::Type) -> Either<crate::hydra::context::InContext, crate::hydra::core::EitherType> {
  let stripped = crate::hydra::rewriting::deannotate_type(typ.clone()) ;
  match &*stripped.clone().0 {
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("either type")), String::from(" but found ")), crate::hydra::show::core::type_(typ.clone())))))))),
      context: cx.clone()})))}}

pub fn function_type(cx: crate::hydra::context::Context, typ: crate::hydra::core::Type) -> Either<crate::hydra::context::InContext, crate::hydra::core::FunctionType> {
  let stripped = crate::hydra::rewriting::deannotate_type(typ.clone()) ;
  match &*stripped.clone().0 {
    crate::hydra::core::Type_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("function type")), String::from(" but found ")), crate::hydra::show::core::type_(typ.clone())))))))),
      context: cx.clone()})))}}

pub fn injection(cx: crate::hydra::context::Context, expected: crate::hydra::core::Name, graph: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Field> {
  crate::hydra::lib::eithers::bind(crate::hydra::lexical::strip_and_dereference_term(cx.clone(), graph.clone(), term0.clone()), |term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(v0_.clone().0.type_name.clone().0.0.clone(), expected.clone().0.0.clone()), Right(v0_.clone().0.field.clone()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
        object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), crate::hydra::lib::strings::cat2(String::from("injection of type "), expected.clone().0.0.clone())), String::from(" but found ")), v0_.clone().0.type_name.clone().0.0.clone()))))))),
        context: cx.clone()}))))},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("injection")), String::from(" but found ")), crate::hydra::show::core::term(term.clone())))))))),
      context: cx.clone()})))})}

pub fn int16(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, i16> {
  crate::hydra::lib::eithers::bind(literal(cx.clone(), graph.clone(), t.clone()), |l: crate::hydra::core::Literal| crate::hydra::lib::eithers::bind(integer_literal(cx.clone(), l.clone()), |i: crate::hydra::core::IntegerValue| int16_value(cx.clone(), i.clone())))}

pub fn int16_value(cx: crate::hydra::context::Context, v: crate::hydra::core::IntegerValue) -> Either<crate::hydra::context::InContext, i16> {
  match &*v.clone().0 {
    crate::hydra::core::IntegerValue_Variant::Int16 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("int16")), String::from(" but found ")), crate::hydra::show::core::integer(v.clone())))))))),
      context: cx.clone()})))}}

pub fn int32(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, i32> {
  crate::hydra::lib::eithers::bind(literal(cx.clone(), graph.clone(), t.clone()), |l: crate::hydra::core::Literal| crate::hydra::lib::eithers::bind(integer_literal(cx.clone(), l.clone()), |i: crate::hydra::core::IntegerValue| int32_value(cx.clone(), i.clone())))}

pub fn int32_value(cx: crate::hydra::context::Context, v: crate::hydra::core::IntegerValue) -> Either<crate::hydra::context::InContext, i32> {
  match &*v.clone().0 {
    crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("int32")), String::from(" but found ")), crate::hydra::show::core::integer(v.clone())))))))),
      context: cx.clone()})))}}

pub fn int64(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, i64> {
  crate::hydra::lib::eithers::bind(literal(cx.clone(), graph.clone(), t.clone()), |l: crate::hydra::core::Literal| crate::hydra::lib::eithers::bind(integer_literal(cx.clone(), l.clone()), |i: crate::hydra::core::IntegerValue| int64_value(cx.clone(), i.clone())))}

pub fn int64_value(cx: crate::hydra::context::Context, v: crate::hydra::core::IntegerValue) -> Either<crate::hydra::context::InContext, i64> {
  match &*v.clone().0 {
    crate::hydra::core::IntegerValue_Variant::Int64 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("int64")), String::from(" but found ")), crate::hydra::show::core::integer(v.clone())))))))),
      context: cx.clone()})))}}

pub fn int8(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, i8> {
  crate::hydra::lib::eithers::bind(literal(cx.clone(), graph.clone(), t.clone()), |l: crate::hydra::core::Literal| crate::hydra::lib::eithers::bind(integer_literal(cx.clone(), l.clone()), |i: crate::hydra::core::IntegerValue| int8_value(cx.clone(), i.clone())))}

pub fn int8_value(cx: crate::hydra::context::Context, v: crate::hydra::core::IntegerValue) -> Either<crate::hydra::context::InContext, i8> {
  match &*v.clone().0 {
    crate::hydra::core::IntegerValue_Variant::Int8 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("int8")), String::from(" but found ")), crate::hydra::show::core::integer(v.clone())))))))),
      context: cx.clone()})))}}

pub fn integer_literal(cx: crate::hydra::context::Context, lit: crate::hydra::core::Literal) -> Either<crate::hydra::context::InContext, crate::hydra::core::IntegerValue> {
  match &*lit.clone().0 {
    crate::hydra::core::Literal_Variant::Integer (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("integer value")), String::from(" but found ")), crate::hydra::show::core::literal(lit.clone())))))))),
      context: cx.clone()})))}}

pub fn integer_value(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::IntegerValue> {
  crate::hydra::lib::eithers::bind(literal(cx.clone(), graph.clone(), t.clone()), |l: crate::hydra::core::Literal| integer_literal(cx.clone(), l.clone()))}

pub fn lambda_body(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Term> {
  crate::hydra::lib::eithers::map(|v| v.0.body.clone(), lambda(cx.clone(), graph.clone(), term.clone()))}

pub fn lambda(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Lambda> {
  crate::hydra::lib::eithers::bind(crate::hydra::lexical::strip_and_dereference_term(cx.clone(), graph.clone(), term0.clone()), |term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Function_Variant::Lambda (v0_) => {
          let v0_ = v0_.clone() ;
          Right(v0_.clone())},
        _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
          object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("lambda")), String::from(" but found ")), crate::hydra::show::core::term(term.clone())))))))),
          context: cx.clone()})))}},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("lambda")), String::from(" but found ")), crate::hydra::show::core::term(term.clone())))))))),
      context: cx.clone()})))})}

pub fn let_binding(cx: crate::hydra::context::Context, n: String, graph: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Term> {
  let name = crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(n.clone()))) ;
  crate::hydra::lib::eithers::bind(let_(cx.clone(), graph.clone(), term.clone()), |let_expr: crate::hydra::core::Let| {
    let matching_bindings = crate::hydra::lib::lists::filter(|b: crate::hydra::core::Binding| crate::hydra::lib::equality::equal(b.clone().0.name.clone().0.0.clone(), name.clone().0.0.clone()), let_expr.clone().0.bindings.clone()) ;
    crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(matching_bindings.clone()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(String::from("no such binding: "), n.clone()))))))),
      context: cx.clone()}))), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(crate::hydra::lib::lists::length(matching_bindings.clone()), 1i32), Right(crate::hydra::lib::lists::head(matching_bindings.clone()).0.term.clone()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(String::from("multiple bindings named "), n.clone()))))))),
      context: cx.clone()})))))})}

pub fn let_(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Let> {
  crate::hydra::lib::eithers::bind(crate::hydra::lexical::strip_and_dereference_term(cx.clone(), graph.clone(), term0.clone()), |term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("let term")), String::from(" but found ")), crate::hydra::show::core::term(term.clone())))))))),
      context: cx.clone()})))})}

pub fn list(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, Vec<crate::hydra::core::Term>> {
  crate::hydra::lib::eithers::bind(crate::hydra::lexical::strip_and_dereference_term(cx.clone(), graph.clone(), term.clone()), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("list")), String::from(" but found ")), crate::hydra::show::core::term(stripped.clone())))))))),
      context: cx.clone()})))})}

pub fn list_head(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Term> {
  crate::hydra::lib::eithers::bind(list(cx.clone(), graph.clone(), term.clone()), |l: Vec<crate::hydra::core::Term>| crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(l.clone()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(String::from("empty list"))))))),
    context: cx.clone()}))), Right(crate::hydra::lib::lists::head(l.clone()))))}

pub fn list_of(cx: crate::hydra::context::Context, f: impl Fn(crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, T0> + Clone, graph: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, Vec<T0>> {
  crate::hydra::lib::eithers::bind(list(cx.clone(), graph.clone(), term.clone()), |els: Vec<crate::hydra::core::Term>| crate::hydra::lib::eithers::map_list(f.clone(), els.clone()))}

pub fn list_type(cx: crate::hydra::context::Context, typ: crate::hydra::core::Type) -> Either<crate::hydra::context::InContext, crate::hydra::core::Type> {
  let stripped = crate::hydra::rewriting::deannotate_type(typ.clone()) ;
  match &*stripped.clone().0 {
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("list type")), String::from(" but found ")), crate::hydra::show::core::type_(typ.clone())))))))),
      context: cx.clone()})))}}

pub fn literal(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Literal> {
  crate::hydra::lib::eithers::bind(crate::hydra::lexical::strip_and_dereference_term(cx.clone(), graph.clone(), term0.clone()), |term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("literal")), String::from(" but found ")), crate::hydra::show::core::term(term.clone())))))))),
      context: cx.clone()})))})}

pub fn map(cx: crate::hydra::context::Context, fk: impl Fn(crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, T0> + Clone, fv: impl Fn(crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, T1> + Clone, graph: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, BTreeMap<T0, T1>> {
  let pair = |kv_pair: (crate::hydra::core::Term, crate::hydra::core::Term)| {
    let kterm = crate::hydra::lib::pairs::first(kv_pair.clone()) ;
    {
      let vterm = crate::hydra::lib::pairs::second(kv_pair.clone()) ;
      crate::hydra::lib::eithers::bind(fk.clone()(kterm.clone()), |kval: T0| crate::hydra::lib::eithers::bind(fv.clone()(vterm.clone()), |vval: T1| Right((kval.clone(), vval.clone()))))}} ;
  crate::hydra::lib::eithers::bind(crate::hydra::lexical::strip_and_dereference_term(cx.clone(), graph.clone(), term0.clone()), |term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(crate::hydra::lib::maps::from_list, crate::hydra::lib::eithers::map_list(pair.clone(), crate::hydra::lib::maps::to_list(v0_.clone())))},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("map")), String::from(" but found ")), crate::hydra::show::core::term(term.clone())))))))),
      context: cx.clone()})))})}

pub fn map_type(cx: crate::hydra::context::Context, typ: crate::hydra::core::Type) -> Either<crate::hydra::context::InContext, crate::hydra::core::MapType> {
  let stripped = crate::hydra::rewriting::deannotate_type(typ.clone()) ;
  match &*stripped.clone().0 {
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("map type")), String::from(" but found ")), crate::hydra::show::core::type_(typ.clone())))))))),
      context: cx.clone()})))}}

pub fn n_args(cx: crate::hydra::context::Context, name: crate::hydra::core::Name, n: i32, args: Vec<T0>) -> Either<crate::hydra::context::InContext, ()> {
  crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(crate::hydra::lib::lists::length(args.clone()), n.clone()), Right(()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), crate::hydra::lib::strings::cat(Vec::from([
      crate::hydra::lib::literals::show_int32(n.clone()),
      String::from(" arguments to primitive "),
      crate::hydra::lib::literals::show_string(name.clone().0.0.clone())]))), String::from(" but found ")), crate::hydra::lib::literals::show_int32(crate::hydra::lib::lists::length(args.clone()))))))))),
    context: cx.clone()}))))}

pub fn maybe_term(cx: crate::hydra::context::Context, f: impl Fn(crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, T0> + Clone, graph: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, Option<T0>> {
  crate::hydra::lib::eithers::bind(crate::hydra::lexical::strip_and_dereference_term(cx.clone(), graph.clone(), term0.clone()), |term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::maybes::maybe(Right(None), |t: crate::hydra::core::Term| crate::hydra::lib::eithers::map(crate::hydra::lib::maybes::pure, f.clone()(t.clone())), v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("maybe value")), String::from(" but found ")), crate::hydra::show::core::term(term.clone())))))))),
      context: cx.clone()})))})}

pub fn maybe_type(cx: crate::hydra::context::Context, typ: crate::hydra::core::Type) -> Either<crate::hydra::context::InContext, crate::hydra::core::Type> {
  let stripped = crate::hydra::rewriting::deannotate_type(typ.clone()) ;
  match &*stripped.clone().0 {
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("maybe type")), String::from(" but found ")), crate::hydra::show::core::type_(typ.clone())))))))),
      context: cx.clone()})))}}

pub fn pair(cx: crate::hydra::context::Context, kf: impl Fn(crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, T0> + Clone, vf: impl Fn(crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, T1> + Clone, graph: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, (T0, T1)> {
  crate::hydra::lib::eithers::bind(crate::hydra::lexical::strip_and_dereference_term(cx.clone(), graph.clone(), term0.clone()), |term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(kf.clone()(crate::hydra::lib::pairs::first(v0_.clone())), |k_val: T0| crate::hydra::lib::eithers::bind(vf.clone()(crate::hydra::lib::pairs::second(v0_.clone())), |v_val: T1| Right((k_val.clone(), v_val.clone()))))},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("pair")), String::from(" but found ")), crate::hydra::show::core::term(term.clone())))))))),
      context: cx.clone()})))})}

pub fn record(cx: crate::hydra::context::Context, expected: crate::hydra::core::Name, graph: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, Vec<crate::hydra::core::Field>> {
  crate::hydra::lib::eithers::bind(term_record(cx.clone(), graph.clone(), term0.clone()), |record: crate::hydra::core::Record| crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(record.clone().0.type_name.clone(), expected.clone()), Right(record.clone().0.fields.clone()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), crate::hydra::lib::strings::cat2(String::from("record of type "), expected.clone().0.0.clone())), String::from(" but found ")), record.clone().0.type_name.clone().0.0.clone()))))))),
    context: cx.clone()})))))}

pub fn record_type(cx: crate::hydra::context::Context, ename: T0, typ: crate::hydra::core::Type) -> Either<crate::hydra::context::InContext, Vec<crate::hydra::core::FieldType>> {
  let stripped = crate::hydra::rewriting::deannotate_type(typ.clone()) ;
  match &*stripped.clone().0 {
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("record type")), String::from(" but found ")), crate::hydra::show::core::type_(typ.clone())))))))),
      context: cx.clone()})))}}

pub fn set(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, BTreeSet<crate::hydra::core::Term>> {
  crate::hydra::lib::eithers::bind(crate::hydra::lexical::strip_and_dereference_term(cx.clone(), graph.clone(), term.clone()), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("set")), String::from(" but found ")), crate::hydra::show::core::term(stripped.clone())))))))),
      context: cx.clone()})))})}

pub fn set_of(cx: crate::hydra::context::Context, f: impl Fn(crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, T0> + Clone, graph: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, BTreeSet<T0>> {
  crate::hydra::lib::eithers::bind(set(cx.clone(), graph.clone(), term.clone()), |els: BTreeSet<crate::hydra::core::Term>| crate::hydra::lib::eithers::map_set(f.clone(), els.clone()))}

pub fn set_type(cx: crate::hydra::context::Context, typ: crate::hydra::core::Type) -> Either<crate::hydra::context::InContext, crate::hydra::core::Type> {
  let stripped = crate::hydra::rewriting::deannotate_type(typ.clone()) ;
  match &*stripped.clone().0 {
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("set type")), String::from(" but found ")), crate::hydra::show::core::type_(typ.clone())))))))),
      context: cx.clone()})))}}

pub fn string(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, String> {
  crate::hydra::lib::eithers::bind(literal(cx.clone(), graph.clone(), t.clone()), |l: crate::hydra::core::Literal| string_literal(cx.clone(), l.clone()))}

pub fn string_literal(cx: crate::hydra::context::Context, v: crate::hydra::core::Literal) -> Either<crate::hydra::context::InContext, String> {
  match &*v.clone().0 {
    crate::hydra::core::Literal_Variant::String (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("string")), String::from(" but found ")), crate::hydra::show::core::literal(v.clone())))))))),
      context: cx.clone()})))}}

pub fn term_record(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Record> {
  crate::hydra::lib::eithers::bind(crate::hydra::lexical::strip_and_dereference_term(cx.clone(), graph.clone(), term0.clone()), |term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("record")), String::from(" but found ")), crate::hydra::show::core::term(term.clone())))))))),
      context: cx.clone()})))})}

pub fn uint16(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, u16> {
  crate::hydra::lib::eithers::bind(literal(cx.clone(), graph.clone(), t.clone()), |l: crate::hydra::core::Literal| crate::hydra::lib::eithers::bind(integer_literal(cx.clone(), l.clone()), |i: crate::hydra::core::IntegerValue| uint16_value(cx.clone(), i.clone())))}

pub fn uint16_value(cx: crate::hydra::context::Context, v: crate::hydra::core::IntegerValue) -> Either<crate::hydra::context::InContext, u16> {
  match &*v.clone().0 {
    crate::hydra::core::IntegerValue_Variant::Uint16 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("uint16")), String::from(" but found ")), crate::hydra::show::core::integer(v.clone())))))))),
      context: cx.clone()})))}}

pub fn uint32(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, u32> {
  crate::hydra::lib::eithers::bind(literal(cx.clone(), graph.clone(), t.clone()), |l: crate::hydra::core::Literal| crate::hydra::lib::eithers::bind(integer_literal(cx.clone(), l.clone()), |i: crate::hydra::core::IntegerValue| uint32_value(cx.clone(), i.clone())))}

pub fn uint32_value(cx: crate::hydra::context::Context, v: crate::hydra::core::IntegerValue) -> Either<crate::hydra::context::InContext, u32> {
  match &*v.clone().0 {
    crate::hydra::core::IntegerValue_Variant::Uint32 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("uint32")), String::from(" but found ")), crate::hydra::show::core::integer(v.clone())))))))),
      context: cx.clone()})))}}

pub fn uint64(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, u64> {
  crate::hydra::lib::eithers::bind(literal(cx.clone(), graph.clone(), t.clone()), |l: crate::hydra::core::Literal| crate::hydra::lib::eithers::bind(integer_literal(cx.clone(), l.clone()), |i: crate::hydra::core::IntegerValue| uint64_value(cx.clone(), i.clone())))}

pub fn uint64_value(cx: crate::hydra::context::Context, v: crate::hydra::core::IntegerValue) -> Either<crate::hydra::context::InContext, u64> {
  match &*v.clone().0 {
    crate::hydra::core::IntegerValue_Variant::Uint64 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("uint64")), String::from(" but found ")), crate::hydra::show::core::integer(v.clone())))))))),
      context: cx.clone()})))}}

pub fn uint8(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, u8> {
  crate::hydra::lib::eithers::bind(literal(cx.clone(), graph.clone(), t.clone()), |l: crate::hydra::core::Literal| crate::hydra::lib::eithers::bind(integer_literal(cx.clone(), l.clone()), |i: crate::hydra::core::IntegerValue| uint8_value(cx.clone(), i.clone())))}

pub fn uint8_value(cx: crate::hydra::context::Context, v: crate::hydra::core::IntegerValue) -> Either<crate::hydra::context::InContext, u8> {
  match &*v.clone().0 {
    crate::hydra::core::IntegerValue_Variant::Uint8 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("uint8")), String::from(" but found ")), crate::hydra::show::core::integer(v.clone())))))))),
      context: cx.clone()})))}}

pub fn union_type(cx: crate::hydra::context::Context, ename: T0, typ: crate::hydra::core::Type) -> Either<crate::hydra::context::InContext, Vec<crate::hydra::core::FieldType>> {
  let stripped = crate::hydra::rewriting::deannotate_type(typ.clone()) ;
  match &*stripped.clone().0 {
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("union type")), String::from(" but found ")), crate::hydra::show::core::type_(typ.clone())))))))),
      context: cx.clone()})))}}

pub fn unit(cx: crate::hydra::context::Context, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, ()> {
  match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      Right(())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("unit")), String::from(" but found ")), crate::hydra::show::core::term(term.clone())))))))),
      context: cx.clone()})))}}

pub fn unit_variant(cx: crate::hydra::context::Context, tname: crate::hydra::core::Name, graph: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Name> {
  crate::hydra::lib::eithers::bind(injection(cx.clone(), tname.clone(), graph.clone(), term.clone()), |field: crate::hydra::core::Field| crate::hydra::lib::eithers::bind(unit(cx.clone(), field.clone().0.term.clone()), |ignored: ()| Right(field.clone().0.name.clone())))}

pub fn wrap(cx: crate::hydra::context::Context, expected: crate::hydra::core::Name, graph: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Term> {
  crate::hydra::lib::eithers::bind(crate::hydra::lexical::strip_and_dereference_term(cx.clone(), graph.clone(), term0.clone()), |term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(v0_.clone().0.type_name.clone().0.0.clone(), expected.clone().0.0.clone()), Right(v0_.clone().0.body.clone()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
        object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), crate::hydra::lib::strings::cat2(String::from("wrapper of type "), expected.clone().0.0.clone())), String::from(" but found ")), v0_.clone().0.type_name.clone().0.0.clone()))))))),
        context: cx.clone()}))))},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("wrap("), expected.clone().0.0.clone()), String::from(")"))), String::from(" but found ")), crate::hydra::show::core::term(term.clone())))))))),
      context: cx.clone()})))})}

pub fn wrapped_type(cx: crate::hydra::context::Context, ename: T0, typ: crate::hydra::core::Type) -> Either<crate::hydra::context::InContext, crate::hydra::core::Type> {
  let stripped = crate::hydra::rewriting::deannotate_type(typ.clone()) ;
  match &*stripped.clone().0 {
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      Right(v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected "), String::from("wrapped type")), String::from(" but found ")), crate::hydra::show::core::type_(typ.clone())))))))),
      context: cx.clone()})))}}
