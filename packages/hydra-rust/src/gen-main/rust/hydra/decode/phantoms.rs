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
use crate::hydra::phantoms::*;
use crate::hydra::util::*;

pub fn t_binding(a: T0, cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::phantoms::TBinding> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("name"), crate::hydra::decode::core::name, field_map.clone(), cx.clone()), |field_name: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("term"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| t_term(a.clone(), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_term: crate::hydra::phantoms::TTerm| Right(crate::hydra::phantoms::TBinding(Rc::new(crate::hydra::phantoms::TBinding_Variant {
          name: field_name.clone(),
          term: field_term.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn t_term(a: T0, cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::phantoms::TTerm> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: crate::hydra::core::Term| crate::hydra::phantoms::TTerm(Rc::new(crate::hydra::phantoms::TTerm_Variant(b.clone()))), crate::hydra::decode::core::term(cx.clone(), v0_.clone().0.body.clone()))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}
