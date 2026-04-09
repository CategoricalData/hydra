#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::encode::core::*;
use crate::hydra::encode::ast::*;
use crate::hydra::encode::coders::*;
use crate::hydra::encode::graph::*;
use crate::hydra::encode::json::model::*;
use crate::hydra::encode::module::*;
use crate::hydra::encode::parsing::*;
use crate::hydra::encode::typing::*;
use crate::hydra::encode::util::*;
use crate::hydra::testing::*;

pub fn alpha_conversion_test_case(x: crate::hydra::testing::AlphaConversionTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.AlphaConversionTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("term")))),
        term: crate::hydra::encode::core::term(x.clone().0.term.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("oldVariable")))),
        term: crate::hydra::encode::core::name(x.clone().0.old_variable.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("newVariable")))),
        term: crate::hydra::encode::core::name(x.clone().0.new_variable.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("result")))),
        term: crate::hydra::encode::core::term(x.clone().0.result.clone())}))])})))))}

pub fn evaluation_style(v1: crate::hydra::testing::EvaluationStyle) -> crate::hydra::core::Term {
  match &*v1.clone().0 {
    crate::hydra::testing::EvaluationStyle_Variant::Eager (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.EvaluationStyle")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("eager")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))},
    crate::hydra::testing::EvaluationStyle_Variant::Lazy (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.EvaluationStyle")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("lazy")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))}}}

pub fn case_conversion_test_case(x: crate::hydra::testing::CaseConversionTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.CaseConversionTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("fromConvention")))),
        term: crate::hydra::encode::util::case_convention(x.clone().0.from_convention.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("toConvention")))),
        term: crate::hydra::encode::util::case_convention(x.clone().0.to_convention.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("fromString")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x.clone().0.from_string.clone()))))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("toString")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x.clone().0.to_string.clone()))))))}))])})))))}

pub fn delegated_evaluation_test_case(x: crate::hydra::testing::DelegatedEvaluationTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.DelegatedEvaluationTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::term(x.clone().0.output.clone())}))])})))))}

pub fn eta_expansion_test_case(x: crate::hydra::testing::EtaExpansionTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.EtaExpansionTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::term(x.clone().0.output.clone())}))])})))))}

pub fn deannotate_term_test_case(x: crate::hydra::testing::DeannotateTermTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.DeannotateTermTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::term(x.clone().0.output.clone())}))])})))))}

pub fn deannotate_type_test_case(x: crate::hydra::testing::DeannotateTypeTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.DeannotateTypeTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::type_(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::type_(x.clone().0.output.clone())}))])})))))}

pub fn flatten_let_terms_test_case(x: crate::hydra::testing::FlattenLetTermsTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.FlattenLetTermsTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::term(x.clone().0.output.clone())}))])})))))}

pub fn fold_operation(v1: crate::hydra::testing::FoldOperation) -> crate::hydra::core::Term {
  match &*v1.clone().0 {
    crate::hydra::testing::FoldOperation_Variant::SumInt32Literals (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.FoldOperation")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("sumInt32Literals")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))},
    crate::hydra::testing::FoldOperation_Variant::CollectListLengths (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.FoldOperation")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("collectListLengths")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))},
    crate::hydra::testing::FoldOperation_Variant::CollectLabels (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.FoldOperation")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("collectLabels")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))}}}

pub fn fold_over_term_test_case(x: crate::hydra::testing::FoldOverTermTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.FoldOverTermTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("traversalOrder")))),
        term: crate::hydra::encode::coders::traversal_order(x.clone().0.traversal_order.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("operation")))),
        term: fold_operation(x.clone().0.operation.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::term(x.clone().0.output.clone())}))])})))))}

pub fn free_variables_test_case(x: crate::hydra::testing::FreeVariablesTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.FreeVariablesTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Set(crate::hydra::lib::sets::map(crate::hydra::encode::core::name, x.clone().0.output.clone()))))}))])})))))}

pub fn hoist_predicate(v1: crate::hydra::testing::HoistPredicate) -> crate::hydra::core::Term {
  match &*v1.clone().0 {
    crate::hydra::testing::HoistPredicate_Variant::CaseStatements (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.HoistPredicate")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("caseStatements")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))},
    crate::hydra::testing::HoistPredicate_Variant::Applications (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.HoistPredicate")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("applications")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))},
    crate::hydra::testing::HoistPredicate_Variant::Lists (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.HoistPredicate")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("lists")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))},
    crate::hydra::testing::HoistPredicate_Variant::Nothing (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.HoistPredicate")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("nothing")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))}}}

pub fn hoist_let_bindings_test_case(x: crate::hydra::testing::HoistLetBindingsTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.HoistLetBindingsTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::let_(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::let_(x.clone().0.output.clone())}))])})))))}

pub fn hoist_polymorphic_let_bindings_test_case(x: crate::hydra::testing::HoistPolymorphicLetBindingsTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.HoistPolymorphicLetBindingsTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::let_(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::let_(x.clone().0.output.clone())}))])})))))}

pub fn hoist_subterms_test_case(x: crate::hydra::testing::HoistSubtermsTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.HoistSubtermsTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("predicate")))),
        term: hoist_predicate(x.clone().0.predicate.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::term(x.clone().0.output.clone())}))])})))))}

pub fn hoist_case_statements_test_case(x: crate::hydra::testing::HoistCaseStatementsTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.HoistCaseStatementsTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::term(x.clone().0.output.clone())}))])})))))}

pub fn term_rewriter(v1: crate::hydra::testing::TermRewriter) -> crate::hydra::core::Term {
  match &*v1.clone().0 {
    crate::hydra::testing::TermRewriter_Variant::ReplaceFooWithBar (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TermRewriter")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("replaceFooWithBar")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))},
    crate::hydra::testing::TermRewriter_Variant::ReplaceInt32WithInt64 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TermRewriter")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("replaceInt32WithInt64")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))}}}

pub fn rewrite_term_test_case(x: crate::hydra::testing::RewriteTermTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.RewriteTermTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("rewriter")))),
        term: term_rewriter(x.clone().0.rewriter.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::term(x.clone().0.output.clone())}))])})))))}

pub fn type_rewriter(v1: crate::hydra::testing::TypeRewriter) -> crate::hydra::core::Term {
  match &*v1.clone().0 {
    crate::hydra::testing::TypeRewriter_Variant::ReplaceStringWithInt32 (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TypeRewriter")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("replaceStringWithInt32")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))}}}

pub fn rewrite_type_test_case(x: crate::hydra::testing::RewriteTypeTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.RewriteTypeTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::type_(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("rewriter")))),
        term: type_rewriter(x.clone().0.rewriter.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::type_(x.clone().0.output.clone())}))])})))))}

pub fn evaluation_test_case(x: crate::hydra::testing::EvaluationTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.EvaluationTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("evaluationStyle")))),
        term: evaluation_style(x.clone().0.evaluation_style.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::term(x.clone().0.output.clone())}))])})))))}

pub fn inference_failure_test_case(x: crate::hydra::testing::InferenceFailureTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.InferenceFailureTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())}))])})))))}

pub fn inference_test_case(x: crate::hydra::testing::InferenceTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.InferenceTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::type_scheme(x.clone().0.output.clone())}))])})))))}

pub fn json_decode_test_case(x: crate::hydra::testing::JsonDecodeTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.JsonDecodeTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
        term: crate::hydra::encode::core::type_(x.clone().0.type_.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("json")))),
        term: crate::hydra::encode::json::model::value(x.clone().0.json.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("expected")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(crate::hydra::lib::eithers::bimap(|x2: String| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x2.clone())))))), crate::hydra::encode::core::term, x.clone().0.expected.clone()))))}))])})))))}

pub fn json_encode_test_case(x: crate::hydra::testing::JsonEncodeTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.JsonEncodeTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("term")))),
        term: crate::hydra::encode::core::term(x.clone().0.term.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("expected")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(crate::hydra::lib::eithers::bimap(|x2: String| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x2.clone())))))), crate::hydra::encode::json::model::value, x.clone().0.expected.clone()))))}))])})))))}

pub fn json_parser_test_case(v1: crate::hydra::testing::ParserTestCase) -> crate::hydra::core::Term {
  parser_test_case(crate::hydra::encode::json::model::value, v1.clone())}

pub fn json_roundtrip_test_case(x: crate::hydra::testing::JsonRoundtripTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.JsonRoundtripTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
        term: crate::hydra::encode::core::type_(x.clone().0.type_.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("term")))),
        term: crate::hydra::encode::core::term(x.clone().0.term.clone())}))])})))))}

pub fn lift_lambda_above_let_test_case(x: crate::hydra::testing::LiftLambdaAboveLetTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.LiftLambdaAboveLetTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::term(x.clone().0.output.clone())}))])})))))}

pub fn json_writer_test_case(v1: crate::hydra::testing::WriterTestCase) -> crate::hydra::core::Term {
  writer_test_case(crate::hydra::encode::json::model::value, v1.clone())}

pub fn parser_test_case(a: impl Fn(T0) -> crate::hydra::core::Term + Clone, x: crate::hydra::testing::ParserTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.ParserTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x.clone().0.input.clone()))))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::parsing::parse_result(a.clone(), x.clone().0.output.clone())}))])})))))}

pub fn tag(x: crate::hydra::testing::Tag) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.Tag")))),
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x.clone().0.0.clone()))))))})))))}

pub fn test_case(v1: crate::hydra::testing::TestCase) -> crate::hydra::core::Term {
  match &*v1.clone().0 {
    crate::hydra::testing::TestCase_Variant::AlphaConversion (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("alphaConversion")))),
          term: alpha_conversion_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::CaseConversion (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("caseConversion")))),
          term: case_conversion_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::DeannotateTerm (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("deannotateTerm")))),
          term: deannotate_term_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::DeannotateType (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("deannotateType")))),
          term: deannotate_type_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::DelegatedEvaluation (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("delegatedEvaluation")))),
          term: delegated_evaluation_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::EtaExpansion (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("etaExpansion")))),
          term: eta_expansion_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::FlattenLetTerms (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("flattenLetTerms")))),
          term: flatten_let_terms_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::FreeVariables (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("freeVariables")))),
          term: free_variables_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::Evaluation (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("evaluation")))),
          term: evaluation_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::Inference (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("inference")))),
          term: inference_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::InferenceFailure (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("inferenceFailure")))),
          term: inference_failure_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::JsonDecode (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("jsonDecode")))),
          term: json_decode_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::JsonEncode (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("jsonEncode")))),
          term: json_encode_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::JsonParser (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("jsonParser")))),
          term: json_parser_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::JsonRoundtrip (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("jsonRoundtrip")))),
          term: json_roundtrip_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::JsonWriter (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("jsonWriter")))),
          term: json_writer_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::LiftLambdaAboveLet (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("liftLambdaAboveLet")))),
          term: lift_lambda_above_let_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::Serialization (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("serialization")))),
          term: serialization_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::SimplifyTerm (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("simplifyTerm")))),
          term: simplify_term_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::TopologicalSort (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("topologicalSort")))),
          term: topological_sort_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::TopologicalSortBindings (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("topologicalSortBindings")))),
          term: topological_sort_bindings_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::TopologicalSortSCC (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("topologicalSortSCC")))),
          term: topological_sort_s_c_c_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::TypeChecking (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeChecking")))),
          term: type_checking_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::TypeCheckingFailure (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeCheckingFailure")))),
          term: type_checking_failure_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::TypeReduction (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeReduction")))),
          term: type_reduction_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::NormalizeTypeVariables (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("normalizeTypeVariables")))),
          term: normalize_type_variables_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::FoldOverTerm (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("foldOverTerm")))),
          term: fold_over_term_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::RewriteTerm (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("rewriteTerm")))),
          term: rewrite_term_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::RewriteType (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("rewriteType")))),
          term: rewrite_type_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::HoistSubterms (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hoistSubterms")))),
          term: hoist_subterms_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::HoistCaseStatements (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hoistCaseStatements")))),
          term: hoist_case_statements_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::HoistLetBindings (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hoistLetBindings")))),
          term: hoist_let_bindings_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::HoistPolymorphicLetBindings (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hoistPolymorphicLetBindings")))),
          term: hoist_polymorphic_let_bindings_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::SubstInType (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("substInType")))),
          term: subst_in_type_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::VariableOccursInType (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("variableOccursInType")))),
          term: variable_occurs_in_type_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::UnifyTypes (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unifyTypes")))),
          term: unify_types_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::JoinTypes (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("joinTypes")))),
          term: join_types_test_case(v0_.clone())}))})))))},
    crate::hydra::testing::TestCase_Variant::UnshadowVariables (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCase")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unshadowVariables")))),
          term: unshadow_variables_test_case(v0_.clone())}))})))))}}}

pub fn test_case_with_metadata(x: crate::hydra::testing::TestCaseWithMetadata) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestCaseWithMetadata")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x.clone().0.name.clone()))))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("case")))),
        term: test_case(x.clone().0.case.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(crate::hydra::lib::maybes::map(|x2: String| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x2.clone())))))), x.clone().0.description.clone()))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("tags")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(tag, x.clone().0.tags.clone()))))}))])})))))}

pub fn test_group(x: crate::hydra::testing::TestGroup) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TestGroup")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x.clone().0.name.clone()))))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(crate::hydra::lib::maybes::map(|x2: String| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x2.clone())))))), x.clone().0.description.clone()))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("subgroups")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(test_group, x.clone().0.subgroups.clone()))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cases")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(test_case_with_metadata, x.clone().0.cases.clone()))))}))])})))))}

pub fn type_checking_test_case(x: crate::hydra::testing::TypeCheckingTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TypeCheckingTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("outputTerm")))),
        term: crate::hydra::encode::core::term(x.clone().0.output_term.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("outputType")))),
        term: crate::hydra::encode::core::type_(x.clone().0.output_type.clone())}))])})))))}

pub fn type_checking_failure_test_case(x: crate::hydra::testing::TypeCheckingFailureTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TypeCheckingFailureTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())}))])})))))}

pub fn topological_sort_bindings_test_case(x: crate::hydra::testing::TopologicalSortBindingsTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TopologicalSortBindingsTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("bindings")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|p: (crate::hydra::core::Name, crate::hydra::core::Term)| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Pair(crate::hydra::lib::pairs::bimap(crate::hydra::encode::core::name, crate::hydra::encode::core::term, p.clone())))), x.clone().0.bindings.clone()))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("expected")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|xs2: Vec<(crate::hydra::core::Name, crate::hydra::core::Term)>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|p: (crate::hydra::core::Name, crate::hydra::core::Term)| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Pair(crate::hydra::lib::pairs::bimap(crate::hydra::encode::core::name, crate::hydra::encode::core::term, p.clone())))), xs2.clone())))), x.clone().0.expected.clone()))))}))])})))))}

pub fn topological_sort_test_case(x: crate::hydra::testing::TopologicalSortTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TopologicalSortTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("adjacencyList")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|p: (i32, Vec<i32>)| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Pair(crate::hydra::lib::pairs::bimap(|x2: i32| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int32(x2.clone()))))))))), |xs2: Vec<i32>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|x2: i32| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int32(x2.clone()))))))))), xs2.clone())))), p.clone())))), x.clone().0.adjacency_list.clone()))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("expected")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(crate::hydra::lib::eithers::bimap(|xs: Vec<Vec<i32>>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|xs2: Vec<i32>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|x2: i32| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int32(x2.clone()))))))))), xs2.clone())))), xs.clone())))), |xs: Vec<i32>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|x2: i32| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int32(x2.clone()))))))))), xs.clone())))), x.clone().0.expected.clone()))))}))])})))))}

pub fn topological_sort_s_c_c_test_case(x: crate::hydra::testing::TopologicalSortSCCTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TopologicalSortSCCTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("adjacencyList")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|p: (i32, Vec<i32>)| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Pair(crate::hydra::lib::pairs::bimap(|x2: i32| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int32(x2.clone()))))))))), |xs2: Vec<i32>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|x2: i32| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int32(x2.clone()))))))))), xs2.clone())))), p.clone())))), x.clone().0.adjacency_list.clone()))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("expected")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|xs2: Vec<i32>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|x2: i32| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int32(x2.clone()))))))))), xs2.clone())))), x.clone().0.expected.clone()))))}))])})))))}

pub fn serialization_test_case(x: crate::hydra::testing::SerializationTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.SerializationTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::ast::expr(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x.clone().0.output.clone()))))))}))])})))))}

pub fn simplify_term_test_case(x: crate::hydra::testing::SimplifyTermTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.SimplifyTermTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::term(x.clone().0.output.clone())}))])})))))}

pub fn normalize_type_variables_test_case(x: crate::hydra::testing::NormalizeTypeVariablesTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.NormalizeTypeVariablesTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::term(x.clone().0.output.clone())}))])})))))}

pub fn type_reduction_test_case(x: crate::hydra::testing::TypeReductionTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.TypeReductionTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::type_(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::type_(x.clone().0.output.clone())}))])})))))}

pub fn writer_test_case(a: impl Fn(T0) -> crate::hydra::core::Term + Clone, x: crate::hydra::testing::WriterTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.WriterTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: a.clone()(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x.clone().0.output.clone()))))))}))])})))))}

pub fn subst_in_type_test_case(x: crate::hydra::testing::SubstInTypeTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.SubstInTypeTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("substitution")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|p: (crate::hydra::core::Name, crate::hydra::core::Type)| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Pair(crate::hydra::lib::pairs::bimap(crate::hydra::encode::core::name, crate::hydra::encode::core::type_, p.clone())))), x.clone().0.substitution.clone()))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::type_(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::type_(x.clone().0.output.clone())}))])})))))}

pub fn variable_occurs_in_type_test_case(x: crate::hydra::testing::VariableOccursInTypeTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.VariableOccursInTypeTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("variable")))),
        term: crate::hydra::encode::core::name(x.clone().0.variable.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))),
        term: crate::hydra::encode::core::type_(x.clone().0.type_.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("expected")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Boolean(x.clone().0.expected.clone()))))))}))])})))))}

pub fn unshadow_variables_test_case(x: crate::hydra::testing::UnshadowVariablesTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.UnshadowVariablesTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
        term: crate::hydra::encode::core::term(x.clone().0.input.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("output")))),
        term: crate::hydra::encode::core::term(x.clone().0.output.clone())}))])})))))}

pub fn unify_types_test_case(x: crate::hydra::testing::UnifyTypesTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.UnifyTypesTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("schemaTypes")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(crate::hydra::encode::core::name, x.clone().0.schema_types.clone()))))})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("left")))),
        term: crate::hydra::encode::core::type_(x.clone().0.left.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("right")))),
        term: crate::hydra::encode::core::type_(x.clone().0.right.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("expected")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(crate::hydra::lib::eithers::bimap(|x2: String| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(x2.clone())))))), crate::hydra::encode::typing::type_subst, x.clone().0.expected.clone()))))}))])})))))}

pub fn join_types_test_case(x: crate::hydra::testing::JoinTypesTestCase) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.testing.JoinTypesTestCase")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("left")))),
        term: crate::hydra::encode::core::type_(x.clone().0.left.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("right")))),
        term: crate::hydra::encode::core::type_(x.clone().0.right.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("expected")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(crate::hydra::lib::eithers::bimap(|_: ()| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit)), |xs: Vec<crate::hydra::typing::TypeConstraint>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(crate::hydra::encode::typing::type_constraint, xs.clone())))), x.clone().0.expected.clone()))))}))])})))))}
