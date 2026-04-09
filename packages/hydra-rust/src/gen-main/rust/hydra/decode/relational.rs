#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::extract::helpers::*;
use crate::hydra::lexical::*;
use crate::hydra::rewriting::*;
use crate::hydra::decode::core::*;
use crate::hydra::relational::*;
use crate::hydra::util::*;

pub fn column_name(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::relational::ColumnName> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: String| crate::hydra::relational::ColumnName(Rc::new(crate::hydra::relational::ColumnName_Variant(b.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
        crate::hydra::core::Term_Variant::Literal (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::Literal_Variant::String (v0_) => {
              let v0_ = v0_.clone() ;
              Right(v0_.clone())},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
        _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), v0_.clone().0.body.clone())))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn column_schema(t: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T0>> + Clone, cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::relational::ColumnSchema> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("name"), column_name, field_map.clone(), cx.clone()), |field_name: crate::hydra::relational::ColumnName| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("domain"), t.clone(), field_map.clone(), cx.clone()), |field_domain: T0| Right(crate::hydra::relational::ColumnSchema(Rc::new(crate::hydra::relational::ColumnSchema_Variant {
          name: field_name.clone(),
          domain: field_domain.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn foreign_key(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::relational::ForeignKey> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("foreignRelation"), relation_name, field_map.clone(), cx.clone()), |field_foreign_relation: crate::hydra::relational::RelationName| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("keys"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_map(column_name, column_name, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_keys: BTreeMap<crate::hydra::relational::ColumnName, crate::hydra::relational::ColumnName>| Right(crate::hydra::relational::ForeignKey(Rc::new(crate::hydra::relational::ForeignKey_Variant {
          foreign_relation: field_foreign_relation.clone(),
          keys: field_keys.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn primary_key(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::relational::PrimaryKey> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: Vec<crate::hydra::relational::ColumnName>| crate::hydra::relational::PrimaryKey(Rc::new(crate::hydra::relational::PrimaryKey_Variant(b.clone()))), crate::hydra::extract::helpers::decode_list(column_name, cx.clone(), v0_.clone().0.body.clone()))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn relation(v: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T0>> + Clone, cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::relational::Relation> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: Vec<crate::hydra::relational::Row>| crate::hydra::relational::Relation(Rc::new(crate::hydra::relational::Relation_Variant(b.clone()))), crate::hydra::extract::helpers::decode_list(|v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| row(v.clone(), v1.clone(), v2.clone()), cx.clone(), v0_.clone().0.body.clone()))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn relation_name(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::relational::RelationName> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: String| crate::hydra::relational::RelationName(Rc::new(crate::hydra::relational::RelationName_Variant(b.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
        crate::hydra::core::Term_Variant::Literal (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::Literal_Variant::String (v0_) => {
              let v0_ = v0_.clone() ;
              Right(v0_.clone())},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
        _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), v0_.clone().0.body.clone())))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn relation_schema(t: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T0>> + Clone, cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::relational::RelationSchema> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("name"), relation_name, field_map.clone(), cx.clone()), |field_name: crate::hydra::relational::RelationName| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("columns"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(|v12: crate::hydra::graph::Graph, v22: crate::hydra::core::Term| column_schema(t.clone(), v12.clone(), v22.clone()), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_columns: Vec<crate::hydra::relational::ColumnSchema>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("primaryKeys"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(primary_key, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_primary_keys: Vec<crate::hydra::relational::PrimaryKey>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("foreignKeys"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(foreign_key, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_foreign_keys: Vec<crate::hydra::relational::ForeignKey>| Right(crate::hydra::relational::RelationSchema(Rc::new(crate::hydra::relational::RelationSchema_Variant {
          name: field_name.clone(),
          columns: field_columns.clone(),
          primary_keys: field_primary_keys.clone(),
          foreign_keys: field_foreign_keys.clone()})))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn relationship(v: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T0>> + Clone, cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::relational::Relationship> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: BTreeSet<BTreeMap<crate::hydra::relational::ColumnName, T0>>| crate::hydra::relational::Relationship(Rc::new(crate::hydra::relational::Relationship_Variant(b.clone()))), crate::hydra::extract::helpers::decode_set(|v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_map(column_name, v.clone(), v1.clone(), v2.clone()), cx.clone(), v0_.clone().0.body.clone()))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn row(v: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T0>> + Clone, cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::relational::Row> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: Vec<T0>| crate::hydra::relational::Row(Rc::new(crate::hydra::relational::Row_Variant(b.clone()))), crate::hydra::extract::helpers::decode_list(v.clone(), cx.clone(), v0_.clone().0.body.clone()))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}
