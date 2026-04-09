#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::reflect::*;
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

pub fn hydra_language() -> crate::hydra::coders::Language {
  let elimination_variants = crate::hydra::lib::sets::from_list(crate::hydra::reflect::elimination_variants) ;
  let literal_variants = crate::hydra::lib::sets::from_list(crate::hydra::reflect::literal_variants) ;
  let float_types = crate::hydra::lib::sets::from_list(crate::hydra::reflect::float_types) ;
  let function_variants = crate::hydra::lib::sets::from_list(crate::hydra::reflect::function_variants) ;
  let integer_types = crate::hydra::lib::sets::from_list(crate::hydra::reflect::integer_types) ;
  let term_variants = crate::hydra::lib::sets::from_list(crate::hydra::reflect::term_variants) ;
  let type_variants = crate::hydra::lib::sets::from_list(crate::hydra::reflect::type_variants) ;
  let types = |t: crate::hydra::core::Type| match &*t.clone().0 {
    _ => true} ;
  crate::hydra::coders::Language(Rc::new(crate::hydra::coders::Language_Variant {
    name: crate::hydra::coders::LanguageName(Rc::new(crate::hydra::coders::LanguageName_Variant(String::from("hydra.core")))),
    constraints: crate::hydra::coders::LanguageConstraints(Rc::new(crate::hydra::coders::LanguageConstraints_Variant {
      elimination_variants: elimination_variants.clone(),
      literal_variants: literal_variants.clone(),
      float_types: float_types.clone(),
      function_variants: function_variants.clone(),
      integer_types: integer_types.clone(),
      term_variants: term_variants.clone(),
      type_variants: type_variants.clone(),
      types: types.clone()}))}))}
