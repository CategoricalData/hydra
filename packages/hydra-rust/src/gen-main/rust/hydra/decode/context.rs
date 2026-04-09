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
use crate::hydra::context::*;
use crate::hydra::util::*;

pub fn context(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::context::Context> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("trace"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(|cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_trace: Vec<String>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("messages"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(|cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_messages: Vec<String>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("other"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_map(crate::hydra::decode::core::name, crate::hydra::decode::core::term, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_other: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term>| Right(crate::hydra::context::Context(Rc::new(crate::hydra::context::Context_Variant {
          trace: field_trace.clone(),
          messages: field_messages.clone(),
          other: field_other.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn in_context(e: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T0>> + Clone, cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::context::InContext> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("object"), e.clone(), field_map.clone(), cx.clone()), |field_object: T0| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("context"), context, field_map.clone(), cx.clone()), |field_context: crate::hydra::context::Context| Right(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
          object: field_object.clone(),
          context: field_context.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}
