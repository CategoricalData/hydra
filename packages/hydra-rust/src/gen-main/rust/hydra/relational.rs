#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::core::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ColumnName_Variant (pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ColumnName (pub Rc<ColumnName_Variant>);

pub type ColumnSchema = (ColumnName, T) ;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ForeignKey_Variant {
  pub foreign_relation: RelationName,
  pub keys: BTreeMap<ColumnName, ColumnName>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ForeignKey (pub Rc<ForeignKey_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PrimaryKey_Variant (pub Vec<ColumnName>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PrimaryKey (pub Rc<PrimaryKey_Variant>);

pub type Relation = Vec<Row> ;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RelationName_Variant (pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RelationName (pub Rc<RelationName_Variant>);

pub type RelationSchema = (RelationName, Vec<ColumnSchema>, Vec<PrimaryKey>, Vec<ForeignKey>) ;

pub type Relationship = BTreeSet<BTreeMap<ColumnName, V>> ;

pub type Row = Vec<V> ;
