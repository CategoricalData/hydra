#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::core::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ComparisonConstraint_Variant {
  Equal,
  NotEqual,
  LessThan,
  GreaterThan,
  LessThanOrEqual,
  GreaterThanOrEqual}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ComparisonConstraint (pub Rc<ComparisonConstraint_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Edge_Variant {
  pub type_: crate::hydra::core::Name,
  pub out: Option<crate::hydra::core::Name>,
  pub in_: Option<crate::hydra::core::Name>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Edge (pub Rc<Edge_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct GraphPattern_Variant {
  pub graph: crate::hydra::core::Name,
  pub patterns: Vec<Pattern>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct GraphPattern (pub Rc<GraphPattern_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Node_Variant {
  Term(crate::hydra::core::Term),
  Variable(Variable),
  Wildcard}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Node (pub Rc<Node_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Path_Variant {
  Step(Step),
  Regex(RegexSequence),
  Inverse(Path)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Path (pub Rc<Path_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PathEquation_Variant {
  pub left: Path,
  pub right: Path}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PathEquation (pub Rc<PathEquation_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Pattern_Variant {
  Triple(TriplePattern),
  Negation(Pattern),
  Conjunction(Vec<Pattern>),
  Disjunction(Vec<Pattern>),
  Graph(GraphPattern)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pattern (pub Rc<Pattern_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PatternImplication_Variant {
  pub antecedent: Pattern,
  pub consequent: Pattern}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PatternImplication (pub Rc<PatternImplication_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Query_Variant {
  pub variables: Vec<Variable>,
  pub patterns: Vec<Pattern>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Query (pub Rc<Query_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Range_Variant {
  pub min: i32,
  pub max: i32}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Range (pub Rc<Range_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum RegexQuantifier_Variant {
  One,
  ZeroOrOne,
  ZeroOrMore,
  OneOrMore,
  Exactly(i32),
  AtLeast(i32),
  Range(Range)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RegexQuantifier (pub Rc<RegexQuantifier_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RegexSequence_Variant {
  pub path: Path,
  pub quantifier: RegexQuantifier}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RegexSequence (pub Rc<RegexSequence_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Step_Variant {
  Edge(Edge),
  Project(crate::hydra::core::Projection),
  Compare(ComparisonConstraint)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Step (pub Rc<Step_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TriplePattern_Variant {
  pub subject: Node,
  pub predicate: Path,
  pub object: Node}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TriplePattern (pub Rc<TriplePattern_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Variable_Variant (pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Variable (pub Rc<Variable_Variant>);
