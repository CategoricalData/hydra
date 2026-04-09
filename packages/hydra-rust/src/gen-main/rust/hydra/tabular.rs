#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::core::*;
use crate::hydra::relational::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ColumnType_Variant {
  pub name: crate::hydra::relational::ColumnName,
  pub type_: crate::hydra::core::Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ColumnType (pub Rc<ColumnType_Variant>);

pub type DataRow = Vec<Option<V>> ;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct HeaderRow_Variant (pub Vec<String>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct HeaderRow (pub Rc<HeaderRow_Variant>);

pub type Table = (Option<HeaderRow>, Vec<DataRow>) ;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TableType_Variant {
  pub name: crate::hydra::relational::RelationName,
  pub columns: Vec<ColumnType>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TableType (pub Rc<TableType_Variant>);
