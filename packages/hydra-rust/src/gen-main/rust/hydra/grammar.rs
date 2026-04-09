#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::core::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Constant_Variant (pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Constant (pub Rc<Constant_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Grammar_Variant (pub Vec<Production>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Grammar (pub Rc<Grammar_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Label_Variant (pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Label (pub Rc<Label_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LabeledPattern_Variant {
  pub label: Label,
  pub pattern: Pattern}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LabeledPattern (pub Rc<LabeledPattern_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Pattern_Variant {
  Alternatives(Vec<Pattern>),
  Constant(Constant),
  Ignored(Pattern),
  Labeled(LabeledPattern),
  Nil,
  Nonterminal(Symbol),
  Option(Pattern),
  Plus(Pattern),
  Regex(Regex),
  Sequence(Vec<Pattern>),
  Star(Pattern)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pattern (pub Rc<Pattern_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Production_Variant {
  pub symbol: Symbol,
  pub pattern: Pattern}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Production (pub Rc<Production_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Regex_Variant (pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Regex (pub Rc<Regex_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol_Variant (pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol (pub Rc<Symbol_Variant>);
