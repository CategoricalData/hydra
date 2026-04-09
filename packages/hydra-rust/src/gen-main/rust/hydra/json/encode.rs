#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::rewriting::*;
use crate::hydra::literals::*;
use crate::hydra::extract::core::*;
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

pub fn to_json(term: crate::hydra::core::Term) -> Either<String, crate::hydra::json::model::Value> {
  let stripped = crate::hydra::rewriting::deannotate_term(term.clone()) ;
  match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      encode_literal(v0_.clone())},
    crate::hydra::core::Term_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let results = crate::hydra::lib::eithers::map_list(|t: crate::hydra::core::Term| to_json(t.clone()), v0_.clone()) ;
        crate::hydra::lib::eithers::map(|vs: Vec<crate::hydra::json::model::Value>| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Array(vs.clone()))), results.clone())}},
    crate::hydra::core::Term_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let terms = crate::hydra::lib::sets::to_list(v0_.clone()) ;
        {
          let results = crate::hydra::lib::eithers::map_list(|t: crate::hydra::core::Term| to_json(t.clone()), terms.clone()) ;
          crate::hydra::lib::eithers::map(|vs: Vec<crate::hydra::json::model::Value>| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Array(vs.clone()))), results.clone())}}},
    crate::hydra::core::Term_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::maybes::maybe(Right(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Null))), |v: crate::hydra::core::Term| {
        let encoded_maybe = to_json(v.clone()) ;
        crate::hydra::lib::eithers::map(|encoded: crate::hydra::json::model::Value| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Array(Vec::from([
          encoded.clone()])))), encoded_maybe.clone())}, v0_.clone())},
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let encode_field = |f: crate::hydra::core::Field| {
          let fname = f.clone().0.name.clone().0.0.clone() ;
          {
            let fterm = f.clone().0.term.clone() ;
            {
              let encoded_field = to_json(fterm.clone()) ;
              crate::hydra::lib::eithers::map(|v: crate::hydra::json::model::Value| (fname.clone(), v.clone()), encoded_field.clone())}}} ;
        {
          let fields = v0_.clone().0.fields.clone() ;
          {
            let encoded_fields = crate::hydra::lib::eithers::map_list(encode_field.clone(), fields.clone()) ;
            crate::hydra::lib::eithers::map(|fs: Vec<(String, crate::hydra::json::model::Value)>| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Object(crate::hydra::lib::maps::from_list(fs.clone())))), encoded_fields.clone())}}}},
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        {
          let fname = field.clone().0.name.clone().0.0.clone() ;
          {
            let fterm = field.clone().0.term.clone() ;
            {
              let encoded_union = to_json(fterm.clone()) ;
              crate::hydra::lib::eithers::map(|v: crate::hydra::json::model::Value| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Object(crate::hydra::lib::maps::from_list(Vec::from([
                (fname.clone(), v.clone())]))))), encoded_union.clone())}}}}},
    crate::hydra::core::Term_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Object(crate::hydra::lib::maps::empty))))},
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      to_json(v0_.clone().0.body.clone())},
    crate::hydra::core::Term_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let encode_entry = |kv: (crate::hydra::core::Term, crate::hydra::core::Term)| {
          let k = crate::hydra::lib::pairs::first(kv.clone()) ;
          {
            let v = crate::hydra::lib::pairs::second(kv.clone()) ;
            {
              let encoded_k = to_json(k.clone()) ;
              {
                let encoded_v = to_json(v.clone()) ;
                crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |ek: crate::hydra::json::model::Value| crate::hydra::lib::eithers::map(|ev: crate::hydra::json::model::Value| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Object(crate::hydra::lib::maps::from_list(Vec::from([
                  (String::from("@key"), ek.clone()),
                  (String::from("@value"), ev.clone())]))))), encoded_v.clone()), encoded_k.clone())}}}} ;
        {
          let entries = crate::hydra::lib::eithers::map_list(encode_entry.clone(), crate::hydra::lib::maps::to_list(v0_.clone())) ;
          crate::hydra::lib::eithers::map(|es: Vec<crate::hydra::json::model::Value>| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Array(es.clone()))), entries.clone())}}},
    crate::hydra::core::Term_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let first = crate::hydra::lib::pairs::first(v0_.clone()) ;
        {
          let second = crate::hydra::lib::pairs::second(v0_.clone()) ;
          {
            let encoded_first = to_json(first.clone()) ;
            {
              let encoded_second = to_json(second.clone()) ;
              crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |ef: crate::hydra::json::model::Value| crate::hydra::lib::eithers::map(|es: crate::hydra::json::model::Value| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Object(crate::hydra::lib::maps::from_list(Vec::from([
                (String::from("@first"), ef.clone()),
                (String::from("@second"), es.clone())]))))), encoded_second.clone()), encoded_first.clone())}}}}},
    crate::hydra::core::Term_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::either(|l: crate::hydra::core::Term| {
        let encoded_l = to_json(l.clone()) ;
        crate::hydra::lib::eithers::map(|v: crate::hydra::json::model::Value| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Object(crate::hydra::lib::maps::from_list(Vec::from([
          (String::from("@left"), v.clone())]))))), encoded_l.clone())}, |r: crate::hydra::core::Term| {
        let encoded_r = to_json(r.clone()) ;
        crate::hydra::lib::eithers::map(|v: crate::hydra::json::model::Value| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Object(crate::hydra::lib::maps::from_list(Vec::from([
          (String::from("@right"), v.clone())]))))), encoded_r.clone())}, v0_.clone())},
    _ => Left(crate::hydra::lib::strings::cat(Vec::from([
      String::from("unsupported term variant for JSON encoding: "),
      crate::hydra::show::core::term(term.clone())])))}}

pub fn encode_literal(lit: crate::hydra::core::Literal) -> Either<T0, crate::hydra::json::model::Value> {
  match &*lit.clone().0 {
    crate::hydra::core::Literal_Variant::Binary (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::String(crate::hydra::lib::literals::binary_to_string(v0_.clone())))))},
    crate::hydra::core::Literal_Variant::Boolean (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Boolean(v0_.clone()))))},
    crate::hydra::core::Literal_Variant::Float (v0_) => {
      let v0_ = v0_.clone() ;
      encode_float(v0_.clone())},
    crate::hydra::core::Literal_Variant::Integer (v0_) => {
      let v0_ = v0_.clone() ;
      encode_integer(v0_.clone())},
    crate::hydra::core::Literal_Variant::String (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::String(v0_.clone()))))}}}

pub fn encode_float(fv: crate::hydra::core::FloatValue) -> Either<T0, crate::hydra::json::model::Value> {
  match &*fv.clone().0 {
    crate::hydra::core::FloatValue_Variant::Bigfloat (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Number(v0_.clone()))))},
    crate::hydra::core::FloatValue_Variant::Float32 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::String(crate::hydra::lib::literals::show_float32(v0_.clone())))))},
    crate::hydra::core::FloatValue_Variant::Float64 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Number(crate::hydra::lib::literals::float64_to_bigfloat(v0_.clone())))))}}}

pub fn encode_integer(iv: crate::hydra::core::IntegerValue) -> Either<T0, crate::hydra::json::model::Value> {
  match &*iv.clone().0 {
    crate::hydra::core::IntegerValue_Variant::Bigint (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::String(crate::hydra::lib::literals::show_bigint(v0_.clone())))))},
    crate::hydra::core::IntegerValue_Variant::Int64 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::String(crate::hydra::lib::literals::show_int64(v0_.clone())))))},
    crate::hydra::core::IntegerValue_Variant::Uint32 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::String(crate::hydra::lib::literals::show_uint32(v0_.clone())))))},
    crate::hydra::core::IntegerValue_Variant::Uint64 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::String(crate::hydra::lib::literals::show_uint64(v0_.clone())))))},
    crate::hydra::core::IntegerValue_Variant::Int8 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Number(crate::hydra::lib::literals::bigint_to_bigfloat(crate::hydra::lib::literals::int8_to_bigint(v0_.clone()))))))},
    crate::hydra::core::IntegerValue_Variant::Int16 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Number(crate::hydra::lib::literals::bigint_to_bigfloat(crate::hydra::lib::literals::int16_to_bigint(v0_.clone()))))))},
    crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Number(crate::hydra::lib::literals::bigint_to_bigfloat(crate::hydra::lib::literals::int32_to_bigint(v0_.clone()))))))},
    crate::hydra::core::IntegerValue_Variant::Uint8 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Number(crate::hydra::lib::literals::bigint_to_bigfloat(crate::hydra::lib::literals::uint8_to_bigint(v0_.clone()))))))},
    crate::hydra::core::IntegerValue_Variant::Uint16 (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Number(crate::hydra::lib::literals::bigint_to_bigfloat(crate::hydra::lib::literals::uint16_to_bigint(v0_.clone()))))))}}}
