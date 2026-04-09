#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::encode::core::*;
use crate::hydra::encode::relational::*;
use crate::hydra::tabular::*;

pub fn column_type(x: crate::hydra::tabular::ColumnType) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.tabular.ColumnType")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
        term: crate::hydra::encode::relational::column_name(x.clone().0.name.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
        term: crate::hydra::encode::core::type_(x.clone().0.type_.clone())}))])})))))}

pub fn data_row(v: impl Fn(T0) -> crate::hydra::core::Term + Clone, x: crate::hydra::tabular::DataRow) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.tabular.DataRow")))),
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|opt: Option<T0>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(crate::hydra::lib::maybes::map(v.clone(), opt.clone())))), x.clone().0.0.clone()))))})))))}

pub fn header_row(x: crate::hydra::tabular::HeaderRow) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.tabular.HeaderRow")))),
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|x2: String| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x2.clone())))))), x.clone().0.0.clone()))))})))))}

pub fn table(v: impl Fn(T0) -> crate::hydra::core::Term + Clone, x: crate::hydra::tabular::Table) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.tabular.Table")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("header")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(crate::hydra::lib::maybes::map(header_row, x.clone().0.header.clone()))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("data")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|v1: crate::hydra::tabular::DataRow| data_row(v.clone(), v1.clone()), x.clone().0.data.clone()))))}))])})))))}

pub fn table_type(x: crate::hydra::tabular::TableType) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.tabular.TableType")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
        term: crate::hydra::encode::relational::relation_name(x.clone().0.name.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("columns")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(column_type, x.clone().0.columns.clone()))))}))])})))))}
