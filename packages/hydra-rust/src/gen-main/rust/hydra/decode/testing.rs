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
use crate::hydra::decode::ast::*;
use crate::hydra::decode::coders::*;
use crate::hydra::decode::graph::*;
use crate::hydra::decode::json::model::*;
use crate::hydra::decode::module::*;
use crate::hydra::decode::parsing::*;
use crate::hydra::decode::typing::*;
use crate::hydra::decode::util::*;
use crate::hydra::testing::*;
use crate::hydra::util::*;

pub fn alpha_conversion_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::AlphaConversionTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("term"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_term: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("oldVariable"), crate::hydra::decode::core::name, field_map.clone(), cx.clone()), |field_old_variable: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("newVariable"), crate::hydra::decode::core::name, field_map.clone(), cx.clone()), |field_new_variable: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("result"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_result: crate::hydra::core::Term| Right(crate::hydra::testing::AlphaConversionTestCase(Rc::new(crate::hydra::testing::AlphaConversionTestCase_Variant {
          term: field_term.clone(),
          old_variable: field_old_variable.clone(),
          new_variable: field_new_variable.clone(),
          result: field_result.clone()})))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn evaluation_style(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::EvaluationStyle> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("eager")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::testing::EvaluationStyle(Rc::new(crate::hydra::testing::EvaluationStyle_Variant::Eager)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("lazy")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::testing::EvaluationStyle(Rc::new(crate::hydra::testing::EvaluationStyle_Variant::Lazy)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::EvaluationStyle>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn case_conversion_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::CaseConversionTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("fromConvention"), crate::hydra::decode::util::case_convention, field_map.clone(), cx.clone()), |field_from_convention: crate::hydra::util::CaseConvention| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("toConvention"), crate::hydra::decode::util::case_convention, field_map.clone(), cx.clone()), |field_to_convention: crate::hydra::util::CaseConvention| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("fromString"), |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), field_map.clone(), cx.clone()), |field_from_string: String| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("toString"), |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), field_map.clone(), cx.clone()), |field_to_string: String| Right(crate::hydra::testing::CaseConversionTestCase(Rc::new(crate::hydra::testing::CaseConversionTestCase_Variant {
          from_convention: field_from_convention.clone(),
          to_convention: field_to_convention.clone(),
          from_string: field_from_string.clone(),
          to_string: field_to_string.clone()})))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn delegated_evaluation_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::DelegatedEvaluationTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Term| Right(crate::hydra::testing::DelegatedEvaluationTestCase(Rc::new(crate::hydra::testing::DelegatedEvaluationTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn eta_expansion_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::EtaExpansionTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Term| Right(crate::hydra::testing::EtaExpansionTestCase(Rc::new(crate::hydra::testing::EtaExpansionTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn deannotate_term_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::DeannotateTermTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Term| Right(crate::hydra::testing::DeannotateTermTestCase(Rc::new(crate::hydra::testing::DeannotateTermTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn deannotate_type_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::DeannotateTypeTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Type| Right(crate::hydra::testing::DeannotateTypeTestCase(Rc::new(crate::hydra::testing::DeannotateTypeTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn flatten_let_terms_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::FlattenLetTermsTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Term| Right(crate::hydra::testing::FlattenLetTermsTestCase(Rc::new(crate::hydra::testing::FlattenLetTermsTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn fold_operation(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::FoldOperation> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("sumInt32Literals")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::testing::FoldOperation(Rc::new(crate::hydra::testing::FoldOperation_Variant::SumInt32Literals)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("collectListLengths")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::testing::FoldOperation(Rc::new(crate::hydra::testing::FoldOperation_Variant::CollectListLengths)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("collectLabels")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::testing::FoldOperation(Rc::new(crate::hydra::testing::FoldOperation_Variant::CollectLabels)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::FoldOperation>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn fold_over_term_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::FoldOverTermTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("traversalOrder"), crate::hydra::decode::coders::traversal_order, field_map.clone(), cx.clone()), |field_traversal_order: crate::hydra::coders::TraversalOrder| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("operation"), fold_operation, field_map.clone(), cx.clone()), |field_operation: crate::hydra::testing::FoldOperation| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Term| Right(crate::hydra::testing::FoldOverTermTestCase(Rc::new(crate::hydra::testing::FoldOverTermTestCase_Variant {
          input: field_input.clone(),
          traversal_order: field_traversal_order.clone(),
          operation: field_operation.clone(),
          output: field_output.clone()})))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn free_variables_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::FreeVariablesTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_set(crate::hydra::decode::core::name, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_output: BTreeSet<crate::hydra::core::Name>| Right(crate::hydra::testing::FreeVariablesTestCase(Rc::new(crate::hydra::testing::FreeVariablesTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn hoist_predicate(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::HoistPredicate> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("caseStatements")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::testing::HoistPredicate(Rc::new(crate::hydra::testing::HoistPredicate_Variant::CaseStatements)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("applications")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::testing::HoistPredicate(Rc::new(crate::hydra::testing::HoistPredicate_Variant::Applications)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("lists")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::testing::HoistPredicate(Rc::new(crate::hydra::testing::HoistPredicate_Variant::Lists)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("nothing")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::testing::HoistPredicate(Rc::new(crate::hydra::testing::HoistPredicate_Variant::Nothing)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::HoistPredicate>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn hoist_let_bindings_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::HoistLetBindingsTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::let_, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Let| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::let_, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Let| Right(crate::hydra::testing::HoistLetBindingsTestCase(Rc::new(crate::hydra::testing::HoistLetBindingsTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn hoist_polymorphic_let_bindings_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::HoistPolymorphicLetBindingsTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::let_, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Let| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::let_, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Let| Right(crate::hydra::testing::HoistPolymorphicLetBindingsTestCase(Rc::new(crate::hydra::testing::HoistPolymorphicLetBindingsTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn hoist_subterms_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::HoistSubtermsTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("predicate"), hoist_predicate, field_map.clone(), cx.clone()), |field_predicate: crate::hydra::testing::HoistPredicate| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Term| Right(crate::hydra::testing::HoistSubtermsTestCase(Rc::new(crate::hydra::testing::HoistSubtermsTestCase_Variant {
          predicate: field_predicate.clone(),
          input: field_input.clone(),
          output: field_output.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn hoist_case_statements_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::HoistCaseStatementsTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Term| Right(crate::hydra::testing::HoistCaseStatementsTestCase(Rc::new(crate::hydra::testing::HoistCaseStatementsTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn term_rewriter(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::TermRewriter> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("replaceFooWithBar")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::testing::TermRewriter(Rc::new(crate::hydra::testing::TermRewriter_Variant::ReplaceFooWithBar)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("replaceInt32WithInt64")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::testing::TermRewriter(Rc::new(crate::hydra::testing::TermRewriter_Variant::ReplaceInt32WithInt64)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::TermRewriter>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn rewrite_term_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::RewriteTermTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("rewriter"), term_rewriter, field_map.clone(), cx.clone()), |field_rewriter: crate::hydra::testing::TermRewriter| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Term| Right(crate::hydra::testing::RewriteTermTestCase(Rc::new(crate::hydra::testing::RewriteTermTestCase_Variant {
          input: field_input.clone(),
          rewriter: field_rewriter.clone(),
          output: field_output.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn type_rewriter(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::TypeRewriter> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("replaceStringWithInt32")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::testing::TypeRewriter(Rc::new(crate::hydra::testing::TypeRewriter_Variant::ReplaceStringWithInt32)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::TypeRewriter>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn rewrite_type_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::RewriteTypeTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("rewriter"), type_rewriter, field_map.clone(), cx.clone()), |field_rewriter: crate::hydra::testing::TypeRewriter| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Type| Right(crate::hydra::testing::RewriteTypeTestCase(Rc::new(crate::hydra::testing::RewriteTypeTestCase_Variant {
          input: field_input.clone(),
          rewriter: field_rewriter.clone(),
          output: field_output.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn evaluation_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::EvaluationTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("evaluationStyle"), evaluation_style, field_map.clone(), cx.clone()), |field_evaluation_style: crate::hydra::testing::EvaluationStyle| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Term| Right(crate::hydra::testing::EvaluationTestCase(Rc::new(crate::hydra::testing::EvaluationTestCase_Variant {
          evaluation_style: field_evaluation_style.clone(),
          input: field_input.clone(),
          output: field_output.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn inference_failure_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::InferenceFailureTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| Right(crate::hydra::testing::InferenceFailureTestCase(Rc::new(crate::hydra::testing::InferenceFailureTestCase_Variant {
          input: field_input.clone()}))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn inference_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::InferenceTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::type_scheme, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::TypeScheme| Right(crate::hydra::testing::InferenceTestCase(Rc::new(crate::hydra::testing::InferenceTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn json_decode_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::JsonDecodeTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("type"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_type: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("json"), crate::hydra::decode::json::model::value, field_map.clone(), cx.clone()), |field_json: crate::hydra::json::model::Value| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("expected"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_either(|cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), crate::hydra::decode::core::term, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_expected: Either<String, crate::hydra::core::Term>| Right(crate::hydra::testing::JsonDecodeTestCase(Rc::new(crate::hydra::testing::JsonDecodeTestCase_Variant {
          type_: field_type.clone(),
          json: field_json.clone(),
          expected: field_expected.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn json_encode_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::JsonEncodeTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("term"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_term: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("expected"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_either(|cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), crate::hydra::decode::json::model::value, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_expected: Either<String, crate::hydra::json::model::Value>| Right(crate::hydra::testing::JsonEncodeTestCase(Rc::new(crate::hydra::testing::JsonEncodeTestCase_Variant {
          term: field_term.clone(),
          expected: field_expected.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn json_parser_test_case(v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::ParserTestCase> {
  parser_test_case(crate::hydra::decode::json::model::value, v1.clone(), v2.clone())}

pub fn json_roundtrip_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::JsonRoundtripTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("type"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_type: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("term"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_term: crate::hydra::core::Term| Right(crate::hydra::testing::JsonRoundtripTestCase(Rc::new(crate::hydra::testing::JsonRoundtripTestCase_Variant {
          type_: field_type.clone(),
          term: field_term.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn lift_lambda_above_let_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::LiftLambdaAboveLetTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Term| Right(crate::hydra::testing::LiftLambdaAboveLetTestCase(Rc::new(crate::hydra::testing::LiftLambdaAboveLetTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn json_writer_test_case(v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::WriterTestCase> {
  writer_test_case(crate::hydra::decode::json::model::value, v1.clone(), v2.clone())}

pub fn parser_test_case(a: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T0>> + Clone, cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::ParserTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), field_map.clone(), cx.clone()), |field_input: String| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::decode::parsing::parse_result(a.clone(), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_output: crate::hydra::parsing::ParseResult| Right(crate::hydra::testing::ParserTestCase(Rc::new(crate::hydra::testing::ParserTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn tag(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::Tag> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: String| crate::hydra::testing::Tag(Rc::new(crate::hydra::testing::Tag_Variant(b.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
        crate::hydra::core::Term_Variant::Literal (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::Literal_Variant::String (v0_) => {
              let v0_ = v0_.clone() ;
              Right(v0_.clone())},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
        _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), v0_.clone().0.body.clone())))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::TestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("alphaConversion")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::AlphaConversionTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::AlphaConversion(t.clone()))), alpha_conversion_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("caseConversion")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::CaseConversionTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::CaseConversion(t.clone()))), case_conversion_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("deannotateTerm")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::DeannotateTermTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::DeannotateTerm(t.clone()))), deannotate_term_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("deannotateType")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::DeannotateTypeTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::DeannotateType(t.clone()))), deannotate_type_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("delegatedEvaluation")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::DelegatedEvaluationTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::DelegatedEvaluation(t.clone()))), delegated_evaluation_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("etaExpansion")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::EtaExpansionTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::EtaExpansion(t.clone()))), eta_expansion_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("flattenLetTerms")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::FlattenLetTermsTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::FlattenLetTerms(t.clone()))), flatten_let_terms_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("freeVariables")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::FreeVariablesTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::FreeVariables(t.clone()))), free_variables_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("evaluation")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::EvaluationTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::Evaluation(t.clone()))), evaluation_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("inference")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::InferenceTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::Inference(t.clone()))), inference_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("inferenceFailure")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::InferenceFailureTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::InferenceFailure(t.clone()))), inference_failure_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("jsonDecode")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::JsonDecodeTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::JsonDecode(t.clone()))), json_decode_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("jsonEncode")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::JsonEncodeTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::JsonEncode(t.clone()))), json_encode_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("jsonParser")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::ParserTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::JsonParser(t.clone()))), json_parser_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("jsonRoundtrip")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::JsonRoundtripTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::JsonRoundtrip(t.clone()))), json_roundtrip_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("jsonWriter")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::WriterTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::JsonWriter(t.clone()))), json_writer_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("liftLambdaAboveLet")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::LiftLambdaAboveLetTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::LiftLambdaAboveLet(t.clone()))), lift_lambda_above_let_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("serialization")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::SerializationTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::Serialization(t.clone()))), serialization_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("simplifyTerm")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::SimplifyTermTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::SimplifyTerm(t.clone()))), simplify_term_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("topologicalSort")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::TopologicalSortTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::TopologicalSort(t.clone()))), topological_sort_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("topologicalSortBindings")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::TopologicalSortBindingsTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::TopologicalSortBindings(t.clone()))), topological_sort_bindings_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("topologicalSortSCC")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::TopologicalSortSCCTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::TopologicalSortSCC(t.clone()))), topological_sort_s_c_c_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeChecking")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::TypeCheckingTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::TypeChecking(t.clone()))), type_checking_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeCheckingFailure")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::TypeCheckingFailureTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::TypeCheckingFailure(t.clone()))), type_checking_failure_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeReduction")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::TypeReductionTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::TypeReduction(t.clone()))), type_reduction_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("normalizeTypeVariables")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::NormalizeTypeVariablesTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::NormalizeTypeVariables(t.clone()))), normalize_type_variables_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("foldOverTerm")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::FoldOverTermTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::FoldOverTerm(t.clone()))), fold_over_term_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("rewriteTerm")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::RewriteTermTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::RewriteTerm(t.clone()))), rewrite_term_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("rewriteType")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::RewriteTypeTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::RewriteType(t.clone()))), rewrite_type_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hoistSubterms")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::HoistSubtermsTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::HoistSubterms(t.clone()))), hoist_subterms_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hoistCaseStatements")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::HoistCaseStatementsTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::HoistCaseStatements(t.clone()))), hoist_case_statements_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hoistLetBindings")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::HoistLetBindingsTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::HoistLetBindings(t.clone()))), hoist_let_bindings_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hoistPolymorphicLetBindings")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::HoistPolymorphicLetBindingsTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::HoistPolymorphicLetBindings(t.clone()))), hoist_polymorphic_let_bindings_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("substInType")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::SubstInTypeTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::SubstInType(t.clone()))), subst_in_type_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("variableOccursInType")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::VariableOccursInTypeTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::VariableOccursInType(t.clone()))), variable_occurs_in_type_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unifyTypes")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::UnifyTypesTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::UnifyTypes(t.clone()))), unify_types_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("joinTypes")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::JoinTypesTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::JoinTypes(t.clone()))), join_types_test_case(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unshadowVariables")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::testing::UnshadowVariablesTestCase| crate::hydra::testing::TestCase(Rc::new(crate::hydra::testing::TestCase_Variant::UnshadowVariables(t.clone()))), unshadow_variables_test_case(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::TestCase>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn test_case_with_metadata(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::TestCaseWithMetadata> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("name"), |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), field_map.clone(), cx.clone()), |field_name: String| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("case"), test_case, field_map.clone(), cx.clone()), |field_case: crate::hydra::testing::TestCase| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("description"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_maybe(|cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_description: Option<String>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("tags"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(tag, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_tags: Vec<crate::hydra::testing::Tag>| Right(crate::hydra::testing::TestCaseWithMetadata(Rc::new(crate::hydra::testing::TestCaseWithMetadata_Variant {
          name: field_name.clone(),
          case: field_case.clone(),
          description: field_description.clone(),
          tags: field_tags.clone()})))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn test_group(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::TestGroup> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("name"), |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), field_map.clone(), cx.clone()), |field_name: String| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("description"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_maybe(|cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_description: Option<String>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("subgroups"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(test_group, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_subgroups: Vec<crate::hydra::testing::TestGroup>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("cases"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(test_case_with_metadata, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_cases: Vec<crate::hydra::testing::TestCaseWithMetadata>| Right(crate::hydra::testing::TestGroup(Rc::new(crate::hydra::testing::TestGroup_Variant {
          name: field_name.clone(),
          description: field_description.clone(),
          subgroups: field_subgroups.clone(),
          cases: field_cases.clone()})))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn type_checking_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::TypeCheckingTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("outputTerm"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_output_term: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("outputType"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_output_type: crate::hydra::core::Type| Right(crate::hydra::testing::TypeCheckingTestCase(Rc::new(crate::hydra::testing::TypeCheckingTestCase_Variant {
          input: field_input.clone(),
          output_term: field_output_term.clone(),
          output_type: field_output_type.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn type_checking_failure_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::TypeCheckingFailureTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| Right(crate::hydra::testing::TypeCheckingFailureTestCase(Rc::new(crate::hydra::testing::TypeCheckingFailureTestCase_Variant {
          input: field_input.clone()}))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn topological_sort_bindings_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::TopologicalSortBindingsTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("bindings"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(|v12: crate::hydra::graph::Graph, v22: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_pair(crate::hydra::decode::core::name, crate::hydra::decode::core::term, v12.clone(), v22.clone()), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_bindings: Vec<(crate::hydra::core::Name, crate::hydra::core::Term)>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("expected"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(|v12: crate::hydra::graph::Graph, v22: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(|v13: crate::hydra::graph::Graph, v23: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_pair(crate::hydra::decode::core::name, crate::hydra::decode::core::term, v13.clone(), v23.clone()), v12.clone(), v22.clone()), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_expected: Vec<Vec<(crate::hydra::core::Name, crate::hydra::core::Term)>>| Right(crate::hydra::testing::TopologicalSortBindingsTestCase(Rc::new(crate::hydra::testing::TopologicalSortBindingsTestCase_Variant {
          bindings: field_bindings.clone(),
          expected: field_expected.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn topological_sort_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::TopologicalSortTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("adjacencyList"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(|v12: crate::hydra::graph::Graph, v22: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_pair(|cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::Integer (v0_) => {
                let v0_ = v0_.clone() ;
                match &*v0_.clone().0 {
                  crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
                    let v0_ = v0_.clone() ;
                    Right(v0_.clone())},
                  _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 value")))))}},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), |v13: crate::hydra::graph::Graph, v23: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(|cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::Integer (v0_) => {
                let v0_ = v0_.clone() ;
                match &*v0_.clone().0 {
                  crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
                    let v0_ = v0_.clone() ;
                    Right(v0_.clone())},
                  _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 value")))))}},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), v13.clone(), v23.clone()), v12.clone(), v22.clone()), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_adjacency_list: Vec<(i32, Vec<i32>)>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("expected"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_either(|v12: crate::hydra::graph::Graph, v22: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(|v13: crate::hydra::graph::Graph, v23: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(|cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::Integer (v0_) => {
                let v0_ = v0_.clone() ;
                match &*v0_.clone().0 {
                  crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
                    let v0_ = v0_.clone() ;
                    Right(v0_.clone())},
                  _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 value")))))}},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), v13.clone(), v23.clone()), v12.clone(), v22.clone()), |v12: crate::hydra::graph::Graph, v22: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(|cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::Integer (v0_) => {
                let v0_ = v0_.clone() ;
                match &*v0_.clone().0 {
                  crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
                    let v0_ = v0_.clone() ;
                    Right(v0_.clone())},
                  _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 value")))))}},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), v12.clone(), v22.clone()), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_expected: Either<Vec<Vec<i32>>, Vec<i32>>| Right(crate::hydra::testing::TopologicalSortTestCase(Rc::new(crate::hydra::testing::TopologicalSortTestCase_Variant {
          adjacency_list: field_adjacency_list.clone(),
          expected: field_expected.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn topological_sort_s_c_c_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::TopologicalSortSCCTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("adjacencyList"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(|v12: crate::hydra::graph::Graph, v22: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_pair(|cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::Integer (v0_) => {
                let v0_ = v0_.clone() ;
                match &*v0_.clone().0 {
                  crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
                    let v0_ = v0_.clone() ;
                    Right(v0_.clone())},
                  _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 value")))))}},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), |v13: crate::hydra::graph::Graph, v23: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(|cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::Integer (v0_) => {
                let v0_ = v0_.clone() ;
                match &*v0_.clone().0 {
                  crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
                    let v0_ = v0_.clone() ;
                    Right(v0_.clone())},
                  _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 value")))))}},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), v13.clone(), v23.clone()), v12.clone(), v22.clone()), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_adjacency_list: Vec<(i32, Vec<i32>)>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("expected"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(|v12: crate::hydra::graph::Graph, v22: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(|cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::Integer (v0_) => {
                let v0_ = v0_.clone() ;
                match &*v0_.clone().0 {
                  crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
                    let v0_ = v0_.clone() ;
                    Right(v0_.clone())},
                  _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 value")))))}},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), v12.clone(), v22.clone()), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_expected: Vec<Vec<i32>>| Right(crate::hydra::testing::TopologicalSortSCCTestCase(Rc::new(crate::hydra::testing::TopologicalSortSCCTestCase_Variant {
          adjacency_list: field_adjacency_list.clone(),
          expected: field_expected.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn serialization_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::SerializationTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::ast::expr, field_map.clone(), cx.clone()), |field_input: crate::hydra::ast::Expr| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), field_map.clone(), cx.clone()), |field_output: String| Right(crate::hydra::testing::SerializationTestCase(Rc::new(crate::hydra::testing::SerializationTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn simplify_term_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::SimplifyTermTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Term| Right(crate::hydra::testing::SimplifyTermTestCase(Rc::new(crate::hydra::testing::SimplifyTermTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn normalize_type_variables_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::NormalizeTypeVariablesTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Term| Right(crate::hydra::testing::NormalizeTypeVariablesTestCase(Rc::new(crate::hydra::testing::NormalizeTypeVariablesTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn type_reduction_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::TypeReductionTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Type| Right(crate::hydra::testing::TypeReductionTestCase(Rc::new(crate::hydra::testing::TypeReductionTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn writer_test_case(a: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T0>> + Clone, cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::WriterTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), a.clone(), field_map.clone(), cx.clone()), |field_input: T0| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), field_map.clone(), cx.clone()), |field_output: String| Right(crate::hydra::testing::WriterTestCase(Rc::new(crate::hydra::testing::WriterTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn subst_in_type_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::SubstInTypeTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("substitution"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(|v12: crate::hydra::graph::Graph, v22: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_pair(crate::hydra::decode::core::name, crate::hydra::decode::core::type_, v12.clone(), v22.clone()), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_substitution: Vec<(crate::hydra::core::Name, crate::hydra::core::Type)>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Type| Right(crate::hydra::testing::SubstInTypeTestCase(Rc::new(crate::hydra::testing::SubstInTypeTestCase_Variant {
          substitution: field_substitution.clone(),
          input: field_input.clone(),
          output: field_output.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn variable_occurs_in_type_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::VariableOccursInTypeTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("variable"), crate::hydra::decode::core::name, field_map.clone(), cx.clone()), |field_variable: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("type"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_type: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("expected"), |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::Boolean (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected boolean literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), field_map.clone(), cx.clone()), |field_expected: bool| Right(crate::hydra::testing::VariableOccursInTypeTestCase(Rc::new(crate::hydra::testing::VariableOccursInTypeTestCase_Variant {
          variable: field_variable.clone(),
          type_: field_type.clone(),
          expected: field_expected.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn unshadow_variables_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::UnshadowVariablesTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("input"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_input: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("output"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_output: crate::hydra::core::Term| Right(crate::hydra::testing::UnshadowVariablesTestCase(Rc::new(crate::hydra::testing::UnshadowVariablesTestCase_Variant {
          input: field_input.clone(),
          output: field_output.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn unify_types_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::UnifyTypesTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("schemaTypes"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(crate::hydra::decode::core::name, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_schema_types: Vec<crate::hydra::core::Name>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("left"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_left: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("right"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_right: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("expected"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_either(|cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), crate::hydra::decode::typing::type_subst, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_expected: Either<String, crate::hydra::typing::TypeSubst>| Right(crate::hydra::testing::UnifyTypesTestCase(Rc::new(crate::hydra::testing::UnifyTypesTestCase_Variant {
          schema_types: field_schema_types.clone(),
          left: field_left.clone(),
          right: field_right.clone(),
          expected: field_expected.clone()})))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn join_types_test_case(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::testing::JoinTypesTestCase> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("left"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_left: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("right"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_right: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("expected"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_either(crate::hydra::extract::helpers::decode_unit, |v12: crate::hydra::graph::Graph, v22: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(crate::hydra::decode::typing::type_constraint, v12.clone(), v22.clone()), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_expected: Either<(), Vec<crate::hydra::typing::TypeConstraint>>| Right(crate::hydra::testing::JoinTypesTestCase(Rc::new(crate::hydra::testing::JoinTypesTestCase_Variant {
          left: field_left.clone(),
          right: field_right.clone(),
          expected: field_expected.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}
