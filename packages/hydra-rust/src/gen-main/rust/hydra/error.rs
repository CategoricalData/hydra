#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::context::*;
use crate::hydra::core::*;
use crate::hydra::typing::*;
use crate::hydra::variants::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum CheckingError_Variant {
  IncorrectUnification(IncorrectUnificationError),
  NotAForallType(NotAForallTypeError),
  NotAFunctionType(NotAFunctionTypeError),
  TypeArityMismatch(TypeArityMismatchError),
  TypeMismatch(TypeMismatchError),
  UnboundTypeVariables(UnboundTypeVariablesError),
  UnequalTypes(UnequalTypesError),
  UnsupportedTermVariant(UnsupportedTermVariantError),
  UntypedLambda(UntypedLambdaError),
  UntypedLetBinding(UntypedLetBindingError)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct CheckingError (pub Rc<CheckingError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DecodingError_Variant (pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DecodingError (pub Rc<DecodingError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DuplicateBindingError_Variant {
  pub name: crate::hydra::core::Name}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DuplicateBindingError (pub Rc<DuplicateBindingError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DuplicateFieldError_Variant {
  pub name: crate::hydra::core::Name}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DuplicateFieldError (pub Rc<DuplicateFieldError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Error_Variant {
  Checking(CheckingError),
  Decoding(DecodingError),
  DuplicateBinding(DuplicateBindingError),
  DuplicateField(DuplicateFieldError),
  Other(OtherError),
  UndefinedField(UndefinedFieldError),
  UndefinedTerm(UndefinedTermError),
  UndefinedType(UndefinedTypeError),
  UnexpectedTermVariant(UnexpectedTermVariantError),
  UnexpectedTypeVariant(UnexpectedTypeVariantError),
  Unification(UnificationError)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Error (pub Rc<Error_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct IncorrectUnificationError_Variant {
  pub substitution: crate::hydra::typing::TypeSubst}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct IncorrectUnificationError (pub Rc<IncorrectUnificationError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NotAForallTypeError_Variant {
  pub type_: crate::hydra::core::Type,
  pub type_arguments: Vec<crate::hydra::core::Type>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NotAForallTypeError (pub Rc<NotAForallTypeError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NotAFunctionTypeError_Variant {
  pub type_: crate::hydra::core::Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NotAFunctionTypeError (pub Rc<NotAFunctionTypeError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct OtherError_Variant (pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct OtherError (pub Rc<OtherError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeArityMismatchError_Variant {
  pub type_: crate::hydra::core::Type,
  pub expected_arity: i32,
  pub actual_arity: i32,
  pub type_arguments: Vec<crate::hydra::core::Type>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeArityMismatchError (pub Rc<TypeArityMismatchError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeMismatchError_Variant {
  pub expected_type: crate::hydra::core::Type,
  pub actual_type: crate::hydra::core::Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeMismatchError (pub Rc<TypeMismatchError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnboundTypeVariablesError_Variant {
  pub variables: BTreeSet<crate::hydra::core::Name>,
  pub type_: crate::hydra::core::Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnboundTypeVariablesError (pub Rc<UnboundTypeVariablesError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UndefinedFieldError_Variant {
  pub field_name: crate::hydra::core::Name,
  pub type_name: crate::hydra::core::Name}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UndefinedFieldError (pub Rc<UndefinedFieldError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UndefinedTermError_Variant {
  pub name: crate::hydra::core::Name}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UndefinedTermError (pub Rc<UndefinedTermError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UndefinedTypeError_Variant {
  pub name: crate::hydra::core::Name}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UndefinedTypeError (pub Rc<UndefinedTypeError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnequalTypesError_Variant {
  pub types: Vec<crate::hydra::core::Type>,
  pub description: String}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnequalTypesError (pub Rc<UnequalTypesError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnexpectedTermVariantError_Variant {
  pub expected_variant: crate::hydra::variants::TermVariant,
  pub actual_term: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnexpectedTermVariantError (pub Rc<UnexpectedTermVariantError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnexpectedTypeVariantError_Variant {
  pub expected_variant: crate::hydra::variants::TypeVariant,
  pub actual_type: crate::hydra::core::Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnexpectedTypeVariantError (pub Rc<UnexpectedTypeVariantError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnificationError_Variant {
  pub left_type: crate::hydra::core::Type,
  pub right_type: crate::hydra::core::Type,
  pub message: String}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnificationError (pub Rc<UnificationError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnsupportedTermVariantError_Variant {
  pub term_variant: crate::hydra::variants::TermVariant}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnsupportedTermVariantError (pub Rc<UnsupportedTermVariantError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UntypedLambdaError_Variant {}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UntypedLambdaError (pub Rc<UntypedLambdaError_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UntypedLetBindingError_Variant {
  pub binding: crate::hydra::core::Binding}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UntypedLetBindingError (pub Rc<UntypedLetBindingError_Variant>);
