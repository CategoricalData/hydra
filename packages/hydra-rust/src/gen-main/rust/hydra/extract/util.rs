#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::extract::core::*;
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

pub fn comparison(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::util::Comparison> {
  crate::hydra::lib::eithers::bind(crate::hydra::extract::core::unit_variant(cx.clone(), crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.util.Comparison")))), graph.clone(), term.clone()), |fname: crate::hydra::core::Name| crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(fname.clone().0.0.clone(), String::from("equalTo")), Right(crate::hydra::util::Comparison(Rc::new(crate::hydra::util::Comparison_Variant::EqualTo))), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(fname.clone().0.0.clone(), String::from("lessThan")), Right(crate::hydra::util::Comparison(Rc::new(crate::hydra::util::Comparison_Variant::LessThan))), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(fname.clone().0.0.clone(), String::from("greaterThan")), Right(crate::hydra::util::Comparison(Rc::new(crate::hydra::util::Comparison_Variant::GreaterThan))), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(String::from("expected comparison but found "), fname.clone().0.0.clone()))))))),
    context: cx.clone()})))))))}
