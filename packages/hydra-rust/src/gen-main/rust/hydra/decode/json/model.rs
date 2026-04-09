#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::extract::helpers::*;
use crate::hydra::lexical::*;
use crate::hydra::rewriting::*;
use crate::hydra::decode::core::*;
use crate::hydra::json::model::*;
use crate::hydra::util::*;

pub fn value(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::json::model::Value> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("array")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: Vec<crate::hydra::json::model::Value>| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Array(t.clone()))), crate::hydra::extract::helpers::decode_list(value, cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("boolean")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: bool| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Boolean(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Boolean (v0_) => {
                  let v0_ = v0_.clone() ;
                  Right(v0_.clone())},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected boolean literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("null")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Null)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("number")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: OrderedFloat<f64>| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Number(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Float (v0_) => {
                  let v0_ = v0_.clone() ;
                  match &*v0_.clone().0 {
                    crate::hydra::core::FloatValue_Variant::Bigfloat (v0_) => {
                      let v0_ = v0_.clone() ;
                      Right(v0_.clone())},
                    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected bigfloat value")))))}},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected bigfloat literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("object")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: BTreeMap<String, crate::hydra::json::model::Value>| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::Object(t.clone()))), crate::hydra::extract::helpers::decode_map(|cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::String (v0_) => {
                  let v0_ = v0_.clone() ;
                  Right(v0_.clone())},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), value, cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("string")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: String| crate::hydra::json::model::Value(Rc::new(crate::hydra::json::model::Value_Variant::String(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::String (v0_) => {
                  let v0_ = v0_.clone() ;
                  Right(v0_.clone())},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone()))))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::json::model::Value>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}
