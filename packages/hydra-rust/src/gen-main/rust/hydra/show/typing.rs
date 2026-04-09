#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
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

pub fn type_constraint(tc: crate::hydra::typing::TypeConstraint) -> String {
  let ltyp = tc.clone().0.left.clone() ;
  let rtyp = tc.clone().0.right.clone() ;
  crate::hydra::lib::strings::cat(Vec::from([
    crate::hydra::show::core::type_(ltyp.clone()),
    String::from("\8801"),
    crate::hydra::show::core::type_(rtyp.clone())]))}

pub fn type_subst(ts: crate::hydra::typing::TypeSubst) -> String {
  let subst = ts.clone().0.0.clone() ;
  let pairs = crate::hydra::lib::maps::to_list(subst.clone()) ;
  let show_pair = |pair: (crate::hydra::core::Name, crate::hydra::core::Type)| {
    let name = crate::hydra::lib::pairs::first(pair.clone()).0.0.clone() ;
    let typ = crate::hydra::lib::pairs::second(pair.clone()) ;
    crate::hydra::lib::strings::cat(Vec::from([
      name.clone(),
      String::from("\8614"),
      crate::hydra::show::core::type_(typ.clone())]))} ;
  let pair_strs = crate::hydra::lib::lists::map(show_pair.clone(), pairs.clone()) ;
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("{"),
    crate::hydra::lib::strings::intercalate(String::from(","), pair_strs.clone()),
    String::from("}")]))}
