#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::encode::core::*;
use crate::hydra::relational::*;

pub fn column_name(x: crate::hydra::relational::ColumnName) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.relational.ColumnName")))),
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x.clone().0.0.clone()))))))})))))}

pub fn column_schema(t: impl Fn(T0) -> crate::hydra::core::Term + Clone, x: crate::hydra::relational::ColumnSchema) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.relational.ColumnSchema")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
        term: column_name(x.clone().0.name.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("domain")))),
        term: t.clone()(x.clone().0.domain.clone())}))])})))))}

pub fn foreign_key(x: crate::hydra::relational::ForeignKey) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.relational.ForeignKey")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("foreignRelation")))),
        term: relation_name(x.clone().0.foreign_relation.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("keys")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(crate::hydra::lib::maps::bimap(column_name, column_name, x.clone().0.keys.clone()))))}))])})))))}

pub fn primary_key(x: crate::hydra::relational::PrimaryKey) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.relational.PrimaryKey")))),
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(column_name, x.clone().0.0.clone()))))})))))}

pub fn relation(v: impl Fn(T0) -> crate::hydra::core::Term + Clone, x: crate::hydra::relational::Relation) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.relational.Relation")))),
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|v1: crate::hydra::relational::Row| row(v.clone(), v1.clone()), x.clone().0.0.clone()))))})))))}

pub fn relation_name(x: crate::hydra::relational::RelationName) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.relational.RelationName")))),
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x.clone().0.0.clone()))))))})))))}

pub fn relation_schema(t: impl Fn(T0) -> crate::hydra::core::Term + Clone, x: crate::hydra::relational::RelationSchema) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.relational.RelationSchema")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
        term: relation_name(x.clone().0.name.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("columns")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|v1: crate::hydra::relational::ColumnSchema| column_schema(t.clone(), v1.clone()), x.clone().0.columns.clone()))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("primaryKeys")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(primary_key, x.clone().0.primary_keys.clone()))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("foreignKeys")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(foreign_key, x.clone().0.foreign_keys.clone()))))}))])})))))}

pub fn relationship(v: impl Fn(T0) -> crate::hydra::core::Term + Clone, x: crate::hydra::relational::Relationship) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.relational.Relationship")))),
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Set(crate::hydra::lib::sets::map(|m: BTreeMap<crate::hydra::relational::ColumnName, T0>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(crate::hydra::lib::maps::bimap(column_name, v.clone(), m.clone())))), x.clone().0.0.clone()))))})))))}

pub fn row(v: impl Fn(T0) -> crate::hydra::core::Term + Clone, x: crate::hydra::relational::Row) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.relational.Row")))),
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(v.clone(), x.clone().0.0.clone()))))})))))}
