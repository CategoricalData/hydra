#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::graph::*;
use crate::hydra::util::*;
use crate::hydra::variants::*;
use crate::hydra::core::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AdapterContext_Variant {
  pub graph: crate::hydra::graph::Graph,
  pub language: Language,
  pub adapters: BTreeMap<crate::hydra::core::Name, crate::hydra::util::Adapter>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AdapterContext (pub Rc<AdapterContext_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum CoderDirection_Variant {
  Encode,
  Decode}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct CoderDirection (pub Rc<CoderDirection_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Language_Variant {
  pub name: LanguageName,
  pub constraints: LanguageConstraints}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Language (pub Rc<Language_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LanguageConstraints_Variant {
  pub elimination_variants: BTreeSet<crate::hydra::variants::EliminationVariant>,
  pub literal_variants: BTreeSet<crate::hydra::variants::LiteralVariant>,
  pub float_types: BTreeSet<crate::hydra::core::FloatType>,
  pub function_variants: BTreeSet<crate::hydra::variants::FunctionVariant>,
  pub integer_types: BTreeSet<crate::hydra::core::IntegerType>,
  pub term_variants: BTreeSet<crate::hydra::variants::TermVariant>,
  pub type_variants: BTreeSet<crate::hydra::variants::TypeVariant>,
  pub types: Rc<dyn Fn(crate::hydra::core::Type) -> bool>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LanguageConstraints (pub Rc<LanguageConstraints_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LanguageName_Variant (pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LanguageName (pub Rc<LanguageName_Variant>);

pub type SymmetricAdapter = crate::hydra::util::Adapter ;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TraversalOrder_Variant {
  Pre,
  Post}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TraversalOrder (pub Rc<TraversalOrder_Variant>);

pub type TypeAdapter = Rc<dyn Fn(AdapterContext) -> Rc<dyn Fn(crate::hydra::core::Type) -> Either<String, SymmetricAdapter>>> ;
