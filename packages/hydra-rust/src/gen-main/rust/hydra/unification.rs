#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::rewriting::*;
use crate::hydra::show::core::*;
use crate::hydra::substitution::*;
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

pub fn join_types(cx: crate::hydra::context::Context, left: crate::hydra::core::Type, right: crate::hydra::core::Type, comment: String) -> Either<crate::hydra::context::InContext, Vec<crate::hydra::typing::TypeConstraint>> {
  let sleft = crate::hydra::rewriting::deannotate_type(left.clone()) ;
  let sright = crate::hydra::rewriting::deannotate_type(right.clone()) ;
  let join_one = |l: crate::hydra::core::Type, r: crate::hydra::core::Type| crate::hydra::typing::TypeConstraint(Rc::new(crate::hydra::typing::TypeConstraint_Variant {
    left: l.clone(),
    right: r.clone(),
    comment: crate::hydra::lib::strings::cat2(String::from("join types; "), comment.clone())})) ;
  let cannot_unify = Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::UnificationError(Rc::new(crate::hydra::error::UnificationError_Variant {
      left_type: sleft.clone(),
      right_type: sright.clone(),
      message: crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("cannot unify "), crate::hydra::show::core::type_(sleft.clone())), String::from(" with ")), crate::hydra::show::core::type_(sright.clone()))})),
    context: cx.clone()}))) ;
  let assert_equal = crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(sleft.clone(), sright.clone()), Right(Vec::from([])), cannot_unify.clone()) ;
  let join_list = |lefts: Vec<crate::hydra::core::Type>, rights: Vec<crate::hydra::core::Type>| crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(crate::hydra::lib::lists::length(lefts.clone()), crate::hydra::lib::lists::length(rights.clone())), Right(crate::hydra::lib::lists::zip_with(join_one.clone(), lefts.clone(), rights.clone())), cannot_unify.clone()) ;
  let join_row_types = |left2: Vec<crate::hydra::core::FieldType>, right2: Vec<crate::hydra::core::FieldType>| crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::and(crate::hydra::lib::equality::equal(crate::hydra::lib::lists::length(crate::hydra::lib::lists::map(|v| v.0.name.clone(), left2.clone())), crate::hydra::lib::lists::length(crate::hydra::lib::lists::map(|v| v.0.name.clone(), right2.clone()))), crate::hydra::lib::lists::foldl(crate::hydra::lib::logic::and, true, crate::hydra::lib::lists::zip_with(|left3: crate::hydra::core::Name, right3: crate::hydra::core::Name| crate::hydra::lib::equality::equal(left3.clone().0.0.clone(), right3.clone().0.0.clone()), crate::hydra::lib::lists::map(|v| v.0.name.clone(), left2.clone()), crate::hydra::lib::lists::map(|v| v.0.name.clone(), right2.clone())))), join_list.clone()(crate::hydra::lib::lists::map(|v| v.0.type_.clone(), left2.clone()), crate::hydra::lib::lists::map(|v| v.0.type_.clone(), right2.clone())), cannot_unify.clone()) ;
  match &*sleft.clone().0 {
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      match &*sright.clone().0 {
        crate::hydra::core::Type_Variant::Application (v0_) => {
          let v0_ = v0_.clone() ;
          Right(Vec::from([
            join_one.clone()(v0_.clone().0.function.clone(), v0_.clone().0.function.clone()),
            join_one.clone()(v0_.clone().0.argument.clone(), v0_.clone().0.argument.clone())]))},
        _ => cannot_unify.clone()}},
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      match &*sright.clone().0 {
        crate::hydra::core::Type_Variant::Either (v0_) => {
          let v0_ = v0_.clone() ;
          Right(Vec::from([
            join_one.clone()(v0_.clone().0.left.clone(), v0_.clone().0.left.clone()),
            join_one.clone()(v0_.clone().0.right.clone(), v0_.clone().0.right.clone())]))},
        _ => cannot_unify.clone()}},
    crate::hydra::core::Type_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      match &*sright.clone().0 {
        crate::hydra::core::Type_Variant::Function (v0_) => {
          let v0_ = v0_.clone() ;
          Right(Vec::from([
            join_one.clone()(v0_.clone().0.domain.clone(), v0_.clone().0.domain.clone()),
            join_one.clone()(v0_.clone().0.codomain.clone(), v0_.clone().0.codomain.clone())]))},
        _ => cannot_unify.clone()}},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      match &*sright.clone().0 {
        crate::hydra::core::Type_Variant::List (v0_) => {
          let v0_ = v0_.clone() ;
          Right(Vec::from([
            join_one.clone()(v0_.clone(), v0_.clone())]))},
        _ => cannot_unify.clone()}},
    crate::hydra::core::Type_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      assert_equal.clone()},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      match &*sright.clone().0 {
        crate::hydra::core::Type_Variant::Map (v0_) => {
          let v0_ = v0_.clone() ;
          Right(Vec::from([
            join_one.clone()(v0_.clone().0.keys.clone(), v0_.clone().0.keys.clone()),
            join_one.clone()(v0_.clone().0.values.clone(), v0_.clone().0.values.clone())]))},
        _ => cannot_unify.clone()}},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      match &*sright.clone().0 {
        crate::hydra::core::Type_Variant::Maybe (v0_) => {
          let v0_ = v0_.clone() ;
          Right(Vec::from([
            join_one.clone()(v0_.clone(), v0_.clone())]))},
        _ => cannot_unify.clone()}},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      match &*sright.clone().0 {
        crate::hydra::core::Type_Variant::Pair (v0_) => {
          let v0_ = v0_.clone() ;
          Right(Vec::from([
            join_one.clone()(v0_.clone().0.first.clone(), v0_.clone().0.first.clone()),
            join_one.clone()(v0_.clone().0.second.clone(), v0_.clone().0.second.clone())]))},
        _ => cannot_unify.clone()}},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      match &*sright.clone().0 {
        crate::hydra::core::Type_Variant::Record (v0_) => {
          let v0_ = v0_.clone() ;
          join_row_types.clone()(v0_.clone(), v0_.clone())},
        _ => cannot_unify.clone()}},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      match &*sright.clone().0 {
        crate::hydra::core::Type_Variant::Set (v0_) => {
          let v0_ = v0_.clone() ;
          Right(Vec::from([
            join_one.clone()(v0_.clone(), v0_.clone())]))},
        _ => cannot_unify.clone()}},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      match &*sright.clone().0 {
        crate::hydra::core::Type_Variant::Union (v0_) => {
          let v0_ = v0_.clone() ;
          join_row_types.clone()(v0_.clone(), v0_.clone())},
        _ => cannot_unify.clone()}},
    crate::hydra::core::Type_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      match &*sright.clone().0 {
        crate::hydra::core::Type_Variant::Unit (v0_) => {
          let v0_ = v0_.clone() ;
          Right(Vec::from([]))},
        _ => cannot_unify.clone()}},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      match &*sright.clone().0 {
        crate::hydra::core::Type_Variant::Wrap (v0_) => {
          let v0_ = v0_.clone() ;
          Right(Vec::from([
            join_one.clone()(v0_.clone(), v0_.clone())]))},
        _ => cannot_unify.clone()}},
    _ => cannot_unify.clone()}}

pub fn unify_type_constraints(cx: crate::hydra::context::Context, schema_types: BTreeMap<crate::hydra::core::Name, T0>, constraints: Vec<crate::hydra::typing::TypeConstraint>) -> Either<crate::hydra::context::InContext, crate::hydra::typing::TypeSubst> {
  let with_constraint = |c: crate::hydra::typing::TypeConstraint, rest: Vec<crate::hydra::typing::TypeConstraint>| {
    let sleft = crate::hydra::rewriting::deannotate_type(c.clone().0.left.clone()) ;
    {
      let sright = crate::hydra::rewriting::deannotate_type(c.clone().0.right.clone()) ;
      {
        let comment = c.clone().0.comment.clone() ;
        {
          let bind = |v: crate::hydra::core::Name, t: crate::hydra::core::Type| {
            let subst = crate::hydra::substitution::singleton_type_subst(v.clone(), t.clone()) ;
            {
              let with_result = |s: crate::hydra::typing::TypeSubst| crate::hydra::substitution::compose_type_subst(subst.clone(), s.clone()) ;
              crate::hydra::lib::eithers::map(with_result.clone(), unify_type_constraints(cx.clone(), schema_types.clone(), crate::hydra::substitution::substitute_in_constraints(subst.clone(), rest.clone())))}} ;
          {
            let try_binding = |v: crate::hydra::core::Name, t: crate::hydra::core::Type| crate::hydra::lib::logic::if_else(variable_occurs_in_type(v.clone(), t.clone()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
              object: crate::hydra::error::UnificationError(Rc::new(crate::hydra::error::UnificationError_Variant {
                left_type: sleft.clone(),
                right_type: sright.clone(),
                message: crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("Variable "), v.clone().0.0.clone()), String::from(" appears free in type ")), crate::hydra::show::core::type_(t.clone())), String::from(" (")), comment.clone()), String::from(")"))})),
              context: cx.clone()}))), bind.clone()(v.clone(), t.clone())) ;
            {
              let no_vars = {
                let with_constraints = |constraints2: Vec<crate::hydra::typing::TypeConstraint>| unify_type_constraints(cx.clone(), schema_types.clone(), crate::hydra::lib::lists::concat2(constraints2.clone(), rest.clone())) ;
                crate::hydra::lib::eithers::bind(join_types(cx.clone(), sleft.clone(), sright.clone(), comment.clone()), with_constraints.clone())} ;
              {
                let dflt = match &*sright.clone().0 {
                  crate::hydra::core::Type_Variant::Variable (v0_) => {
                    let v0_ = v0_.clone() ;
                    try_binding.clone()(v0_.clone(), sleft.clone())},
                  _ => no_vars.clone()} ;
                match &*sleft.clone().0 {
                  crate::hydra::core::Type_Variant::Variable (v0_) => {
                    let v0_ = v0_.clone() ;
                    match &*sright.clone().0 {
                      crate::hydra::core::Type_Variant::Variable (v0_) => {
                        let v0_ = v0_.clone() ;
                        crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(v0_.clone().0.0.clone(), v0_.clone().0.0.clone()), unify_type_constraints(cx.clone(), schema_types.clone(), rest.clone()), crate::hydra::lib::logic::if_else(crate::hydra::lib::maybes::is_just(crate::hydra::lib::maps::lookup(v0_.clone(), schema_types.clone())), crate::hydra::lib::logic::if_else(crate::hydra::lib::maybes::is_just(crate::hydra::lib::maps::lookup(v0_.clone(), schema_types.clone())), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
                          object: crate::hydra::error::UnificationError(Rc::new(crate::hydra::error::UnificationError_Variant {
                            left_type: sleft.clone(),
                            right_type: sright.clone(),
                            message: crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("Attempted to unify schema names "), v0_.clone().0.0.clone()), String::from(" and ")), v0_.clone().0.0.clone()), String::from(" (")), comment.clone()), String::from(")"))})),
                          context: cx.clone()}))), bind.clone()(v0_.clone(), sleft.clone())), bind.clone()(v0_.clone(), sright.clone())))},
                      _ => try_binding.clone()(v0_.clone(), sright.clone())}},
                  _ => dflt.clone()}}}}}}}} ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(constraints.clone()), Right(crate::hydra::substitution::id_type_subst), with_constraint.clone()(crate::hydra::lib::lists::head(constraints.clone()), crate::hydra::lib::lists::tail(constraints.clone())))}

pub fn unify_type_lists(cx: crate::hydra::context::Context, schema_types: BTreeMap<crate::hydra::core::Name, T0>, l: Vec<crate::hydra::core::Type>, r: Vec<crate::hydra::core::Type>, comment: String) -> Either<crate::hydra::context::InContext, crate::hydra::typing::TypeSubst> {
  let to_constraint = |l2: crate::hydra::core::Type, r2: crate::hydra::core::Type| crate::hydra::typing::TypeConstraint(Rc::new(crate::hydra::typing::TypeConstraint_Variant {
    left: l2.clone(),
    right: r2.clone(),
    comment: comment.clone()})) ;
  unify_type_constraints(cx.clone(), schema_types.clone(), crate::hydra::lib::lists::zip_with(to_constraint.clone(), l.clone(), r.clone()))}

pub fn unify_types(cx: crate::hydra::context::Context, schema_types: BTreeMap<crate::hydra::core::Name, T0>, l: crate::hydra::core::Type, r: crate::hydra::core::Type, comment: String) -> Either<crate::hydra::context::InContext, crate::hydra::typing::TypeSubst> {
  unify_type_constraints(cx.clone(), schema_types.clone(), Vec::from([
    crate::hydra::typing::TypeConstraint(Rc::new(crate::hydra::typing::TypeConstraint_Variant {
      left: l.clone(),
      right: r.clone(),
      comment: comment.clone()}))]))}

pub fn variable_occurs_in_type(var: crate::hydra::core::Name, typ0: crate::hydra::core::Type) -> bool {
  let try_type = |b: bool, typ: crate::hydra::core::Type| match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::or(b.clone(), crate::hydra::lib::equality::equal(v0_.clone().0.0.clone(), var.clone().0.0.clone()))},
    _ => b.clone()} ;
  crate::hydra::rewriting::fold_over_type(crate::hydra::coders::TraversalOrder(Rc::new(crate::hydra::coders::TraversalOrder_Variant::Pre)), try_type.clone(), false, typ0.clone())}
