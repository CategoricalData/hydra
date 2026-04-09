#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::show::core::*;
use crate::hydra::show::meta::*;
use crate::hydra::show::typing::*;
use crate::hydra::formatting::*;
use crate::hydra::accessors::*;
use crate::hydra::ast::*;
use crate::hydra::classes::*;
use crate::hydra::coders::*;
use crate::hydra::context::*;
use crate::hydra::core::*;
use crate::hydra::error::*;
use crate::hydra::grammar::*;
use crate::hydra::graph::*;
use crate::hydra::json::model::*;
use crate::hydra::module::*;
use crate::hydra::parsing::*;
use crate::hydra::phantoms::*;
use crate::hydra::query::*;
use crate::hydra::relational::*;
use crate::hydra::tabular::*;
use crate::hydra::testing::*;
use crate::hydra::topology::*;
use crate::hydra::typing::*;
use crate::hydra::util::*;
use crate::hydra::variants::*;

pub fn checking_error(ce: crate::hydra::error::CheckingError) -> String {
  match &*ce.clone().0 {
    crate::hydra::error::CheckingError_Variant::IncorrectUnification (v0_) => {
      let v0_ = v0_.clone() ;
      incorrect_unification_error(v0_.clone())},
    crate::hydra::error::CheckingError_Variant::NotAForallType (v0_) => {
      let v0_ = v0_.clone() ;
      not_a_forall_type_error(v0_.clone())},
    crate::hydra::error::CheckingError_Variant::NotAFunctionType (v0_) => {
      let v0_ = v0_.clone() ;
      not_a_function_type_error(v0_.clone())},
    crate::hydra::error::CheckingError_Variant::TypeArityMismatch (v0_) => {
      let v0_ = v0_.clone() ;
      type_arity_mismatch_error(v0_.clone())},
    crate::hydra::error::CheckingError_Variant::TypeMismatch (v0_) => {
      let v0_ = v0_.clone() ;
      type_mismatch_error(v0_.clone())},
    crate::hydra::error::CheckingError_Variant::UnboundTypeVariables (v0_) => {
      let v0_ = v0_.clone() ;
      unbound_type_variables_error(v0_.clone())},
    crate::hydra::error::CheckingError_Variant::UnequalTypes (v0_) => {
      let v0_ = v0_.clone() ;
      unequal_types_error(v0_.clone())},
    crate::hydra::error::CheckingError_Variant::UnsupportedTermVariant (v0_) => {
      let v0_ = v0_.clone() ;
      unsupported_term_variant_error(v0_.clone())},
    crate::hydra::error::CheckingError_Variant::UntypedLambda (v0_) => {
      let v0_ = v0_.clone() ;
      untyped_lambda_error(v0_.clone())},
    crate::hydra::error::CheckingError_Variant::UntypedLetBinding (v0_) => {
      let v0_ = v0_.clone() ;
      untyped_let_binding_error(v0_.clone())}}}

pub fn decoding_error(de: crate::hydra::error::DecodingError) -> String {
  crate::hydra::lib::strings::cat2(String::from("decoding error: "), de.clone().0.0.clone())}

pub fn duplicate_binding_error(e: crate::hydra::error::DuplicateBindingError) -> String {
  crate::hydra::lib::strings::cat2(String::from("duplicate binding: "), e.clone().0.name.clone().0.0.clone())}

pub fn duplicate_field_error(e: crate::hydra::error::DuplicateFieldError) -> String {
  crate::hydra::lib::strings::cat2(String::from("duplicate field: "), e.clone().0.name.clone().0.0.clone())}

pub fn error(e: crate::hydra::error::Error) -> String {
  match &*e.clone().0 {
    crate::hydra::error::Error_Variant::Checking (v0_) => {
      let v0_ = v0_.clone() ;
      checking_error(v0_.clone())},
    crate::hydra::error::Error_Variant::Decoding (v0_) => {
      let v0_ = v0_.clone() ;
      decoding_error(v0_.clone())},
    crate::hydra::error::Error_Variant::DuplicateBinding (v0_) => {
      let v0_ = v0_.clone() ;
      duplicate_binding_error(v0_.clone())},
    crate::hydra::error::Error_Variant::DuplicateField (v0_) => {
      let v0_ = v0_.clone() ;
      duplicate_field_error(v0_.clone())},
    crate::hydra::error::Error_Variant::Other (v0_) => {
      let v0_ = v0_.clone() ;
      other_error(v0_.clone())},
    crate::hydra::error::Error_Variant::UndefinedField (v0_) => {
      let v0_ = v0_.clone() ;
      undefined_field_error(v0_.clone())},
    crate::hydra::error::Error_Variant::UndefinedTerm (v0_) => {
      let v0_ = v0_.clone() ;
      undefined_term_error(v0_.clone())},
    crate::hydra::error::Error_Variant::UndefinedType (v0_) => {
      let v0_ = v0_.clone() ;
      undefined_type_error(v0_.clone())},
    crate::hydra::error::Error_Variant::UnexpectedTermVariant (v0_) => {
      let v0_ = v0_.clone() ;
      unexpected_term_variant_error(v0_.clone())},
    crate::hydra::error::Error_Variant::UnexpectedTypeVariant (v0_) => {
      let v0_ = v0_.clone() ;
      unexpected_type_variant_error(v0_.clone())},
    crate::hydra::error::Error_Variant::Unification (v0_) => {
      let v0_ = v0_.clone() ;
      unification_error(v0_.clone())}}}

pub fn incorrect_unification_error(e: crate::hydra::error::IncorrectUnificationError) -> String {
  let subst = e.clone().0.substitution.clone() ;
  crate::hydra::lib::strings::cat2(String::from("incorrect unification: "), crate::hydra::show::typing::type_subst(subst.clone()))}

pub fn not_a_forall_type_error(e: crate::hydra::error::NotAForallTypeError) -> String {
  let typ = e.clone().0.type_.clone() ;
  let args = e.clone().0.type_arguments.clone() ;
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("not a forall type: "),
    crate::hydra::show::core::type_(typ.clone()),
    String::from(". Trying to apply "),
    crate::hydra::lib::literals::show_int32(crate::hydra::lib::lists::length(args.clone())),
    String::from(" type argument(s): "),
    crate::hydra::formatting::show_list(crate::hydra::show::core::type_, args.clone())]))}

pub fn not_a_function_type_error(e: crate::hydra::error::NotAFunctionTypeError) -> String {
  let typ = e.clone().0.type_.clone() ;
  crate::hydra::lib::strings::cat2(String::from("not a function type: "), crate::hydra::show::core::type_(typ.clone()))}

pub fn other_error(oe: crate::hydra::error::OtherError) -> String {
  oe.clone().0.0.clone()}

pub fn type_arity_mismatch_error(e: crate::hydra::error::TypeArityMismatchError) -> String {
  let typ = e.clone().0.type_.clone() ;
  let expected = e.clone().0.expected_arity.clone() ;
  let actual = e.clone().0.actual_arity.clone() ;
  let args = e.clone().0.type_arguments.clone() ;
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("type "),
    crate::hydra::show::core::type_(typ.clone()),
    String::from(" applied to the wrong number of type arguments (expected "),
    crate::hydra::lib::literals::show_int32(expected.clone()),
    String::from(", got "),
    crate::hydra::lib::literals::show_int32(actual.clone()),
    String::from("): "),
    crate::hydra::formatting::show_list(crate::hydra::show::core::type_, args.clone())]))}

pub fn type_mismatch_error(e: crate::hydra::error::TypeMismatchError) -> String {
  let expected = e.clone().0.expected_type.clone() ;
  let actual = e.clone().0.actual_type.clone() ;
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("type mismatch: expected "),
    crate::hydra::show::core::type_(expected.clone()),
    String::from(" but found "),
    crate::hydra::show::core::type_(actual.clone())]))}

pub fn unbound_type_variables_error(e: crate::hydra::error::UnboundTypeVariablesError) -> String {
  let vars = e.clone().0.variables.clone() ;
  let typ = e.clone().0.type_.clone() ;
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("unbound type variables: {"),
    crate::hydra::lib::strings::intercalate(String::from(", "), crate::hydra::lib::lists::map(|v| v.0.0.clone(), crate::hydra::lib::sets::to_list(vars.clone()))),
    String::from("} in type "),
    crate::hydra::show::core::type_(typ.clone())]))}

pub fn undefined_field_error(e: crate::hydra::error::UndefinedFieldError) -> String {
  let fname = e.clone().0.field_name.clone() ;
  let tname = e.clone().0.type_name.clone() ;
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("no such field \""),
    fname.clone().0.0.clone(),
    String::from("\" in type \""),
    tname.clone().0.0.clone(),
    String::from("\"")]))}

pub fn undefined_term_error(e: crate::hydra::error::UndefinedTermError) -> String {
  crate::hydra::lib::strings::cat2(String::from("undefined term: "), e.clone().0.name.clone().0.0.clone())}

pub fn undefined_type_error(e: crate::hydra::error::UndefinedTypeError) -> String {
  crate::hydra::lib::strings::cat2(String::from("undefined type: "), e.clone().0.name.clone().0.0.clone())}

pub fn unequal_types_error(e: crate::hydra::error::UnequalTypesError) -> String {
  let types = e.clone().0.types.clone() ;
  let desc = e.clone().0.description.clone() ;
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("unequal types "),
    crate::hydra::formatting::show_list(crate::hydra::show::core::type_, types.clone()),
    String::from(" in "),
    desc.clone()]))}

pub fn unexpected_term_variant_error(e: crate::hydra::error::UnexpectedTermVariantError) -> String {
  let expected = e.clone().0.expected_variant.clone() ;
  let actual = e.clone().0.actual_term.clone() ;
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("expected "),
    crate::hydra::show::meta::term_variant(expected.clone()),
    String::from(" term but found "),
    crate::hydra::show::core::term(actual.clone())]))}

pub fn unexpected_type_variant_error(e: crate::hydra::error::UnexpectedTypeVariantError) -> String {
  let expected = e.clone().0.expected_variant.clone() ;
  let actual = e.clone().0.actual_type.clone() ;
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("expected "),
    crate::hydra::show::meta::type_variant(expected.clone()),
    String::from(" type but found "),
    crate::hydra::show::core::type_(actual.clone())]))}

pub fn unification_error(e: crate::hydra::error::UnificationError) -> String {
  let lt = e.clone().0.left_type.clone() ;
  let rt = e.clone().0.right_type.clone() ;
  let msg = e.clone().0.message.clone() ;
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("unification error: cannot unify "),
    crate::hydra::show::core::type_(lt.clone()),
    String::from(" with "),
    crate::hydra::show::core::type_(rt.clone()),
    String::from(": "),
    msg.clone()]))}

pub fn unsupported_term_variant_error(e: crate::hydra::error::UnsupportedTermVariantError) -> String {
  crate::hydra::lib::strings::cat2(String::from("unsupported term variant: "), crate::hydra::show::meta::term_variant(e.clone().0.term_variant.clone()))}

pub fn untyped_lambda_error(_: T0) -> String {
  String::from("untyped lambda")}

pub fn untyped_let_binding_error(e: crate::hydra::error::UntypedLetBindingError) -> String {
  let b = e.clone().0.binding.clone() ;
  crate::hydra::lib::strings::cat2(String::from("untyped let binding: "), crate::hydra::show::core::binding(b.clone()))}
