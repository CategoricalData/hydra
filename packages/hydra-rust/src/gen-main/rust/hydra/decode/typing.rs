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
use crate::hydra::decode::context::*;
use crate::hydra::typing::*;
use crate::hydra::util::*;

pub fn function_structure(env: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, T0>> + Clone, cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::typing::FunctionStructure> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("typeParams"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(crate::hydra::decode::core::name, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_type_params: Vec<crate::hydra::core::Name>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("params"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(crate::hydra::decode::core::name, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_params: Vec<crate::hydra::core::Name>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("bindings"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(crate::hydra::decode::core::binding, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_bindings: Vec<crate::hydra::core::Binding>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("body"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_body: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("domains"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(crate::hydra::decode::core::type_, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_domains: Vec<crate::hydra::core::Type>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("codomain"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_maybe(crate::hydra::decode::core::type_, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_codomain: Option<crate::hydra::core::Type>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("environment"), env.clone(), field_map.clone(), cx.clone()), |field_environment: T0| Right(crate::hydra::typing::FunctionStructure(Rc::new(crate::hydra::typing::FunctionStructure_Variant {
          type_params: field_type_params.clone(),
          params: field_params.clone(),
          bindings: field_bindings.clone(),
          body: field_body.clone(),
          domains: field_domains.clone(),
          codomain: field_codomain.clone(),
          environment: field_environment.clone()}))))))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn inference_result(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::typing::InferenceResult> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("term"), crate::hydra::decode::core::term, field_map.clone(), cx.clone()), |field_term: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("type"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_type: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("subst"), type_subst, field_map.clone(), cx.clone()), |field_subst: crate::hydra::typing::TypeSubst| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("classConstraints"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_map(crate::hydra::decode::core::name, crate::hydra::decode::core::type_variable_metadata, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_class_constraints: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("context"), crate::hydra::decode::context::context, field_map.clone(), cx.clone()), |field_context: crate::hydra::context::Context| Right(crate::hydra::typing::InferenceResult(Rc::new(crate::hydra::typing::InferenceResult_Variant {
          term: field_term.clone(),
          type_: field_type.clone(),
          subst: field_subst.clone(),
          class_constraints: field_class_constraints.clone(),
          context: field_context.clone()}))))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn term_subst(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::typing::TermSubst> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term>| crate::hydra::typing::TermSubst(Rc::new(crate::hydra::typing::TermSubst_Variant(b.clone()))), crate::hydra::extract::helpers::decode_map(crate::hydra::decode::core::name, crate::hydra::decode::core::term, cx.clone(), v0_.clone().0.body.clone()))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn type_constraint(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::typing::TypeConstraint> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("left"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_left: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("right"), crate::hydra::decode::core::type_, field_map.clone(), cx.clone()), |field_right: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("comment"), |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), field_map.clone(), cx.clone()), |field_comment: String| Right(crate::hydra::typing::TypeConstraint(Rc::new(crate::hydra::typing::TypeConstraint_Variant {
          left: field_left.clone(),
          right: field_right.clone(),
          comment: field_comment.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn type_subst(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::typing::TypeSubst> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>| crate::hydra::typing::TypeSubst(Rc::new(crate::hydra::typing::TypeSubst_Variant(b.clone()))), crate::hydra::extract::helpers::decode_map(crate::hydra::decode::core::name, crate::hydra::decode::core::type_, cx.clone(), v0_.clone().0.body.clone()))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}
