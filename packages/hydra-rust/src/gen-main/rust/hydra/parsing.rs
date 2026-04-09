#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::core::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ParseError_Variant {
  pub message: String,
  pub remainder: String}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ParseError (pub Rc<ParseError_Variant>);

pub type ParseResult = (ParseSuccess, ParseError) ;

pub type ParseSuccess = (A, String) ;

pub type Parser = Rc<dyn Fn(String) -> ParseResult> ;
