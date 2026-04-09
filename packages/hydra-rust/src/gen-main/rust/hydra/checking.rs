#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::constants::*;
use crate::hydra::extract::core::*;
use crate::hydra::formatting::*;
use crate::hydra::lexical::*;
use crate::hydra::reflect::*;
use crate::hydra::rewriting::*;
use crate::hydra::schemas::*;
use crate::hydra::show::core::*;
use crate::hydra::show::error::*;
use crate::hydra::show::meta::*;
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

pub fn all_equal(els: Vec<T0>) -> bool {
  crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(els.clone()), true, crate::hydra::lib::lists::foldl(|b: bool, t: T0| crate::hydra::lib::logic::and(b.clone(), crate::hydra::lib::equality::equal(t.clone(), crate::hydra::lib::lists::head(els.clone()))), true, crate::hydra::lib::lists::tail(els.clone())))}

pub fn apply_type_arguments_to_type(cx: crate::hydra::context::Context, tx: T0, type_args: Vec<crate::hydra::core::Type>, t: crate::hydra::core::Type) -> Either<crate::hydra::context::InContext, crate::hydra::core::Type> {
  let nonnull = match &*t.clone().0 {
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let v = v0_.clone().0.parameter.clone() ;
        {
          let tbody = v0_.clone().0.body.clone() ;
          apply_type_arguments_to_type(cx.clone(), tx.clone(), crate::hydra::lib::lists::tail(type_args.clone()), crate::hydra::substitution::subst_in_type(crate::hydra::typing::TypeSubst(Rc::new(crate::hydra::typing::TypeSubst_Variant(crate::hydra::lib::maps::singleton(v.clone(), crate::hydra::lib::lists::head(type_args.clone()))))), tbody.clone()))}}},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Checking(crate::hydra::error::CheckingError(Rc::new(crate::hydra::error::CheckingError_Variant::NotAForallType(crate::hydra::error::NotAForallTypeError(Rc::new(crate::hydra::error::NotAForallTypeError_Variant {
        type_: t.clone(),
        type_arguments: type_args.clone()})))))))),
      context: cx.clone()})))} ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(type_args.clone()), Right(t.clone()), nonnull.clone())}

pub fn check_for_unbound_type_variables(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, ()> {
  let svars = crate::hydra::lib::sets::from_list(crate::hydra::lib::maps::keys(tx.clone().0.schema_types.clone())) ;
  let check_recursive = |vars: BTreeSet<crate::hydra::core::Name>, trace: Vec<String>, lbinding: Option<crate::hydra::core::Binding>, term: crate::hydra::core::Term| {
    let recurse = |v1: crate::hydra::core::Term| check_recursive.clone()(vars.clone(), trace.clone(), lbinding.clone(), v1.clone()) ;
    {
      let dflt = crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(recurse.clone(), crate::hydra::rewriting::subterms(term.clone())), |_: Vec<()>| Right(())) ;
      {
        let check = |typ: crate::hydra::core::Type| {
          let freevars = crate::hydra::rewriting::free_variables_in_type(typ.clone()) ;
          {
            let badvars = crate::hydra::lib::sets::difference(crate::hydra::lib::sets::difference(freevars.clone(), vars.clone()), svars.clone()) ;
            crate::hydra::lib::logic::if_else(crate::hydra::lib::sets::null(badvars.clone()), Right(()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
              object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Checking(crate::hydra::error::CheckingError(Rc::new(crate::hydra::error::CheckingError_Variant::UnboundTypeVariables(crate::hydra::error::UnboundTypeVariablesError(Rc::new(crate::hydra::error::UnboundTypeVariablesError_Variant {
                variables: badvars.clone(),
                type_: typ.clone()})))))))),
              context: cx.clone()}))))}} ;
        {
          let check_optional = |m: Option<crate::hydra::core::Type>| crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_maybe(check.clone(), m.clone()), |_: Option<()>| Right(())) ;
          match &*term.clone().0 {
            crate::hydra::core::Term_Variant::Function (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Function_Variant::Elimination (v0_) => {
                  let v0_ = v0_.clone() ;
                  dflt.clone()},
                crate::hydra::core::Function_Variant::Lambda (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::lib::eithers::bind(check_optional.clone()(v0_.clone().0.domain.clone()), |_: ()| recurse.clone()(v0_.clone().0.body.clone()))},
                _ => dflt.clone()}},
            crate::hydra::core::Term_Variant::Let (v0_) => {
              let v0_ = v0_.clone() ;
              {
                let for_binding = |b: crate::hydra::core::Binding| {
                  let bterm = b.clone().0.term.clone() ;
                  {
                    let new_vars = crate::hydra::lib::maybes::maybe(vars.clone(), |ts: crate::hydra::core::TypeScheme| crate::hydra::lib::sets::union_(vars.clone(), crate::hydra::lib::sets::from_list(ts.clone().0.variables.clone())), b.clone().0.type_.clone()) ;
                    {
                      let new_trace = crate::hydra::lib::lists::cons(b.clone().0.name.clone().0.0.clone(), trace.clone()) ;
                      check_recursive.clone()(new_vars.clone(), new_trace.clone(), Some(b.clone()), bterm.clone())}}} ;
                crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(for_binding.clone(), v0_.clone().0.bindings.clone()), |_: Vec<()>| recurse.clone()(v0_.clone().0.body.clone()))}},
            crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
              let v0_ = v0_.clone() ;
              crate::hydra::lib::eithers::bind(check.clone()(v0_.clone().0.type_.clone()), |_: ()| recurse.clone()(v0_.clone().0.body.clone()))},
            crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
              let v0_ = v0_.clone() ;
              crate::hydra::lib::eithers::bind(check.clone()(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v0_.clone().0.parameter.clone())))), |_: ()| recurse.clone()(v0_.clone().0.body.clone()))},
            _ => dflt.clone()}}}}} ;
  check_recursive.clone()(crate::hydra::lib::sets::empty, Vec::from([
    String::from("top level")]), None, term0.clone())}

pub fn check_nominal_application(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, tname: crate::hydra::core::Name, type_args: Vec<crate::hydra::core::Type>) -> Either<crate::hydra::context::InContext, ((), crate::hydra::context::Context)> {
  crate::hydra::lib::eithers::bind(crate::hydra::schemas::require_schema_type(cx.clone(), tx.clone().0.schema_types.clone(), tname.clone()), |result: (crate::hydra::core::TypeScheme, crate::hydra::context::Context)| {
    let schema_type = crate::hydra::lib::pairs::first(result.clone()) ;
    {
      let cx2 = crate::hydra::lib::pairs::second(result.clone()) ;
      {
        let vars = schema_type.clone().0.variables.clone() ;
        {
          let varslen = crate::hydra::lib::lists::length(vars.clone()) ;
          {
            let argslen = crate::hydra::lib::lists::length(type_args.clone()) ;
            crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(varslen.clone(), argslen.clone()), Right(((), cx2.clone())), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
              object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Checking(crate::hydra::error::CheckingError(Rc::new(crate::hydra::error::CheckingError_Variant::TypeArityMismatch(crate::hydra::error::TypeArityMismatchError(Rc::new(crate::hydra::error::TypeArityMismatchError_Variant {
                type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(tname.clone()))),
                expected_arity: varslen.clone(),
                actual_arity: argslen.clone(),
                type_arguments: type_args.clone()})))))))),
              context: cx2.clone()}))))}}}}})}

pub fn check_same_type(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, desc: String, types: Vec<crate::hydra::core::Type>) -> Either<crate::hydra::context::InContext, crate::hydra::core::Type> {
  crate::hydra::lib::logic::if_else(types_all_effectively_equal(tx.clone(), types.clone()), Right(crate::hydra::lib::lists::head(types.clone())), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Checking(crate::hydra::error::CheckingError(Rc::new(crate::hydra::error::CheckingError_Variant::UnequalTypes(crate::hydra::error::UnequalTypesError(Rc::new(crate::hydra::error::UnequalTypesError_Variant {
      types: types.clone(),
      description: desc.clone()})))))))),
    context: cx.clone()}))))}

pub fn check_type(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, term: crate::hydra::core::Term, typ: crate::hydra::core::Type) -> Either<crate::hydra::context::InContext, ()> {
  let vars = tx.clone().0.type_variables.clone() ;
  crate::hydra::lib::logic::if_else(crate::hydra::constants::debug_inference, crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map(|_p: (crate::hydra::core::Type, crate::hydra::context::Context)| crate::hydra::lib::pairs::first(_p.clone()), type_of(cx.clone(), tx.clone(), Vec::from([]), term.clone())), |t0: crate::hydra::core::Type| crate::hydra::lib::logic::if_else(types_effectively_equal(tx.clone(), t0.clone(), typ.clone()), Right(()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Checking(crate::hydra::error::CheckingError(Rc::new(crate::hydra::error::CheckingError_Variant::TypeMismatch(crate::hydra::error::TypeMismatchError(Rc::new(crate::hydra::error::TypeMismatchError_Variant {
      expected_type: typ.clone(),
      actual_type: t0.clone()})))))))),
    context: cx.clone()}))))), Right(()))}

pub fn check_type_subst(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, subst: crate::hydra::typing::TypeSubst) -> Either<crate::hydra::context::InContext, crate::hydra::typing::TypeSubst> {
  let s = subst.clone().0.0.clone() ;
  let vars = crate::hydra::lib::sets::from_list(crate::hydra::lib::maps::keys(s.clone())) ;
  let suspect_vars = crate::hydra::lib::sets::intersection(vars.clone(), crate::hydra::lib::sets::from_list(crate::hydra::lib::maps::keys(tx.clone().0.schema_types.clone()))) ;
  let is_nominal = |ts: crate::hydra::core::TypeScheme| match &*crate::hydra::rewriting::deannotate_type(ts.clone().0.type_.clone()).0 {
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    _ => false} ;
  let bad_vars = crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::filter(|v: crate::hydra::core::Name| crate::hydra::lib::maybes::maybe(false, is_nominal.clone(), crate::hydra::lexical::dereference_schema_type(v.clone(), tx.clone().0.schema_types.clone())), crate::hydra::lib::sets::to_list(suspect_vars.clone()))) ;
  let bad_pairs = crate::hydra::lib::lists::filter(|p: (crate::hydra::core::Name, crate::hydra::core::Type)| crate::hydra::lib::sets::member(crate::hydra::lib::pairs::first(p.clone()), bad_vars.clone()), crate::hydra::lib::maps::to_list(s.clone())) ;
  let print_pair = |p: (crate::hydra::core::Name, crate::hydra::core::Type)| crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::pairs::first(p.clone()).0.0.clone(), String::from(" --> ")), crate::hydra::show::core::type_(crate::hydra::lib::pairs::second(p.clone()))) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::sets::null(bad_vars.clone()), Right(subst.clone()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Checking(crate::hydra::error::CheckingError(Rc::new(crate::hydra::error::CheckingError_Variant::IncorrectUnification(crate::hydra::error::IncorrectUnificationError(Rc::new(crate::hydra::error::IncorrectUnificationError_Variant {
      substitution: subst.clone()})))))))),
    context: cx.clone()}))))}

pub fn check_type_variables(_tx: T0, _typ: T1) -> () {
  ()}

pub fn contains_in_scope_type_vars(tx: crate::hydra::graph::Graph, t: crate::hydra::core::Type) -> bool {
  let vars = tx.clone().0.type_variables.clone() ;
  let free_vars = crate::hydra::rewriting::free_variables_in_type_simple(t.clone()) ;
  crate::hydra::lib::logic::not(crate::hydra::lib::sets::null(crate::hydra::lib::sets::intersection(vars.clone(), free_vars.clone())))}

pub fn normalize_type_free_vars(typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  let collect_vars = |acc: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Name>, t: crate::hydra::core::Type| match &*t.clone().0 {
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(crate::hydra::lib::maps::member(v0_.clone(), acc.clone()), acc.clone(), crate::hydra::lib::maps::insert(v0_.clone(), crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat2(String::from("_tv"), crate::hydra::lib::literals::show_int32(crate::hydra::lib::maps::size(acc.clone())))))), acc.clone()))},
    _ => acc.clone()} ;
  let subst = crate::hydra::rewriting::fold_over_type(crate::hydra::coders::TraversalOrder(Rc::new(crate::hydra::coders::TraversalOrder_Variant::Pre)), collect_vars.clone(), crate::hydra::lib::maps::empty, typ.clone()) ;
  crate::hydra::rewriting::substitute_type_variables(subst.clone(), typ.clone())}

pub fn to_f_context(cx: crate::hydra::graph::Graph) -> BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type> {
  crate::hydra::lib::maps::map(crate::hydra::rewriting::type_scheme_to_f_type, cx.clone().0.bound_types.clone())}

pub fn type_lists_effectively_equal(tx: crate::hydra::graph::Graph, tlist1: Vec<crate::hydra::core::Type>, tlist2: Vec<crate::hydra::core::Type>) -> bool {
  crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(crate::hydra::lib::lists::length(tlist1.clone()), crate::hydra::lib::lists::length(tlist2.clone())), crate::hydra::lib::lists::foldl(crate::hydra::lib::logic::and, true, crate::hydra::lib::lists::zip_with(|v1: crate::hydra::core::Type, v2: crate::hydra::core::Type| types_effectively_equal(tx.clone(), v1.clone(), v2.clone()), tlist1.clone(), tlist2.clone())), false)}

pub fn type_of(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  let cx1 = crate::hydra::context::Context(Rc::new(crate::hydra::context::Context_Variant {
    trace: crate::hydra::lib::lists::cons(String::from("typeOf"), cx.clone().0.trace.clone()),
    messages: cx.clone().0.messages.clone(),
    other: cx.clone().0.other.clone()})) ;
  match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      type_of_annotated_term(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      type_of_application(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      type_of_either(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Function_Variant::Elimination (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::Elimination_Variant::Record (v0_) => {
              let v0_ = v0_.clone() ;
              type_of_projection(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
            crate::hydra::core::Elimination_Variant::Union (v0_) => {
              let v0_ = v0_.clone() ;
              type_of_case_statement(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
            crate::hydra::core::Elimination_Variant::Wrap (v0_) => {
              let v0_ = v0_.clone() ;
              type_of_unwrap(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())}}},
        crate::hydra::core::Function_Variant::Lambda (v0_) => {
          let v0_ = v0_.clone() ;
          type_of_lambda(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
        crate::hydra::core::Function_Variant::Primitive (v0_) => {
          let v0_ = v0_.clone() ;
          type_of_primitive(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())}}},
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      type_of_let(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      type_of_list(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      type_of_literal(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      type_of_map(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      type_of_maybe(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      type_of_pair(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      type_of_record(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      type_of_set(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
      let v0_ = v0_.clone() ;
      type_of_type_application(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
      let v0_ = v0_.clone() ;
      type_of_type_lambda(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      type_of_injection(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      type_of_unit(cx1.clone(), tx.clone(), type_args.clone())},
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      type_of_variable(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      type_of_wrapped_term(cx1.clone(), tx.clone(), type_args.clone(), v0_.clone())},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Checking(crate::hydra::error::CheckingError(Rc::new(crate::hydra::error::CheckingError_Variant::UnsupportedTermVariant(crate::hydra::error::UnsupportedTermVariantError(Rc::new(crate::hydra::error::UnsupportedTermVariantError_Variant {
        term_variant: crate::hydra::reflect::term_variant(term.clone())})))))))),
      context: cx1.clone()})))}}

pub fn type_of_annotated_term(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, at: crate::hydra::core::AnnotatedTerm) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  type_of(cx.clone(), tx.clone(), type_args.clone(), at.clone().0.body.clone())}

pub fn type_of_application(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, app: crate::hydra::core::Application) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  let fun = app.clone().0.function.clone() ;
  let arg = app.clone().0.argument.clone() ;
  let try_type = |cx0: crate::hydra::context::Context, tfun: crate::hydra::core::Type, targ: crate::hydra::core::Type| match &*tfun.clone().0 {
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      try_type.clone()(cx0.clone(), v0_.clone().0.body.clone(), targ.clone())},
    crate::hydra::core::Type_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let dom = v0_.clone().0.domain.clone() ;
        {
          let cod = v0_.clone().0.codomain.clone() ;
          crate::hydra::lib::logic::if_else(types_effectively_equal(tx.clone(), dom.clone(), targ.clone()), Right((cod.clone(), cx0.clone())), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
            object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Checking(crate::hydra::error::CheckingError(Rc::new(crate::hydra::error::CheckingError_Variant::TypeMismatch(crate::hydra::error::TypeMismatchError(Rc::new(crate::hydra::error::TypeMismatchError_Variant {
              expected_type: dom.clone(),
              actual_type: targ.clone()})))))))),
            context: cx0.clone()}))))}}},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let name_result = crate::hydra::schemas::fresh_name(cx0.clone()) ;
        {
          let fresh_n = crate::hydra::lib::pairs::first(name_result.clone()) ;
          {
            let cx1 = crate::hydra::lib::pairs::second(name_result.clone()) ;
            Right((crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(fresh_n.clone()))), cx1.clone()))}}}},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Checking(crate::hydra::error::CheckingError(Rc::new(crate::hydra::error::CheckingError_Variant::NotAFunctionType(crate::hydra::error::NotAFunctionTypeError(Rc::new(crate::hydra::error::NotAFunctionTypeError_Variant {
        type_: tfun.clone()})))))))),
      context: cx0.clone()})))} ;
  crate::hydra::lib::eithers::bind(type_of(cx.clone(), tx.clone(), Vec::from([]), fun.clone()), |result1: (crate::hydra::core::Type, crate::hydra::context::Context)| {
    let tfun = crate::hydra::lib::pairs::first(result1.clone()) ;
    {
      let cx2 = crate::hydra::lib::pairs::second(result1.clone()) ;
      crate::hydra::lib::eithers::bind(type_of(cx2.clone(), tx.clone(), Vec::from([]), arg.clone()), |result2: (crate::hydra::core::Type, crate::hydra::context::Context)| {
        let targ = crate::hydra::lib::pairs::first(result2.clone()) ;
        {
          let cx3 = crate::hydra::lib::pairs::second(result2.clone()) ;
          crate::hydra::lib::eithers::bind(try_type.clone()(cx3.clone(), tfun.clone(), targ.clone()), |result3: (crate::hydra::core::Type, crate::hydra::context::Context)| {
            let t = crate::hydra::lib::pairs::first(result3.clone()) ;
            {
              let cx4 = crate::hydra::lib::pairs::second(result3.clone()) ;
              crate::hydra::lib::eithers::bind(apply_type_arguments_to_type(cx4.clone(), tx.clone(), type_args.clone(), t.clone()), |applied: crate::hydra::core::Type| Right((applied.clone(), cx4.clone())))}})}})}})}

pub fn type_of_case_statement(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, cs: crate::hydra::core::CaseStatement) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  let tname = cs.clone().0.type_name.clone() ;
  let dflt = cs.clone().0.default_.clone() ;
  let cases = cs.clone().0.cases.clone() ;
  let cterms = crate::hydra::lib::lists::map(|v| v.0.term.clone(), cases.clone()) ;
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_maybe(|e: crate::hydra::core::Term| type_of(cx.clone(), tx.clone(), Vec::from([]), e.clone()), dflt.clone()), |dflt_result: Option<(crate::hydra::core::Type, crate::hydra::context::Context)>| {
    let tdflt = crate::hydra::lib::maybes::map(crate::hydra::lib::pairs::first, dflt_result.clone()) ;
    {
      let cx2 = crate::hydra::lib::maybes::maybe(cx.clone(), crate::hydra::lib::pairs::second, dflt_result.clone()) ;
      {
        let fold_result = crate::hydra::lib::lists::foldl(|acc: Either<crate::hydra::context::InContext, (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)>, term: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(acc.clone(), |acc_r: (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)| {
          let types = crate::hydra::lib::pairs::first(acc_r.clone()) ;
          {
            let cx_a = crate::hydra::lib::pairs::second(acc_r.clone()) ;
            crate::hydra::lib::eithers::bind(type_of(cx_a.clone(), tx.clone(), Vec::from([]), term.clone()), |t_result: (crate::hydra::core::Type, crate::hydra::context::Context)| {
              let t = crate::hydra::lib::pairs::first(t_result.clone()) ;
              {
                let cx_b = crate::hydra::lib::pairs::second(t_result.clone()) ;
                Right((crate::hydra::lib::lists::concat2(types.clone(), crate::hydra::lib::lists::pure(t.clone())), cx_b.clone()))}})}}), Right((Vec::from([]), cx2.clone())), cterms.clone()) ;
        crate::hydra::lib::eithers::bind(fold_result.clone(), |fold_r: (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)| {
          let tcterms = crate::hydra::lib::pairs::first(fold_r.clone()) ;
          {
            let cx3 = crate::hydra::lib::pairs::second(fold_r.clone()) ;
            {
              let fcods_result = crate::hydra::lib::lists::foldl(|acc: Either<crate::hydra::context::InContext, (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)>, t: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(acc.clone(), |acc_r: (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)| {
                let cods = crate::hydra::lib::pairs::first(acc_r.clone()) ;
                crate::hydra::lib::eithers::bind(crate::hydra::extract::core::function_type(cx3.clone(), t.clone()), |ft: crate::hydra::core::FunctionType| Right((crate::hydra::lib::lists::concat2(cods.clone(), crate::hydra::lib::lists::pure(ft.clone().0.codomain.clone())), cx3.clone())))}), Right((Vec::from([]), cx3.clone())), tcterms.clone()) ;
              crate::hydra::lib::eithers::bind(fcods_result.clone(), |fcods_r: (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)| {
                let fcods = crate::hydra::lib::pairs::first(fcods_r.clone()) ;
                {
                  let cods = crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::cons(tdflt.clone(), crate::hydra::lib::lists::map(crate::hydra::lib::maybes::pure, fcods.clone()))) ;
                  crate::hydra::lib::eithers::bind(check_same_type(cx3.clone(), tx.clone(), String::from("case branches"), cods.clone()), |cod: crate::hydra::core::Type| Right((crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                    domain: crate::hydra::schemas::nominal_application(tname.clone(), type_args.clone()),
                    codomain: cod.clone()}))))), cx3.clone())))}})}}})}}})}

pub fn type_of_either(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, et: Either<crate::hydra::core::Term, crate::hydra::core::Term>) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  let n = crate::hydra::lib::lists::length(type_args.clone()) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(n.clone(), 2i32), crate::hydra::lib::eithers::either(|left_term: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(type_of(cx.clone(), tx.clone(), Vec::from([]), left_term.clone()), |result: (crate::hydra::core::Type, crate::hydra::context::Context)| {
    let left_type = crate::hydra::lib::pairs::first(result.clone()) ;
    {
      let cx2 = crate::hydra::lib::pairs::second(result.clone()) ;
      Right((crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
        left: left_type.clone(),
        right: crate::hydra::lib::lists::at(1i32, type_args.clone())}))))), cx2.clone()))}}), |right_term: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(type_of(cx.clone(), tx.clone(), Vec::from([]), right_term.clone()), |result: (crate::hydra::core::Type, crate::hydra::context::Context)| {
    let right_type = crate::hydra::lib::pairs::first(result.clone()) ;
    {
      let cx2 = crate::hydra::lib::pairs::second(result.clone()) ;
      Right((crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
        left: crate::hydra::lib::lists::at(0i32, type_args.clone()),
        right: right_type.clone()}))))), cx2.clone()))}}), et.clone()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Checking(crate::hydra::error::CheckingError(Rc::new(crate::hydra::error::CheckingError_Variant::TypeArityMismatch(crate::hydra::error::TypeArityMismatchError(Rc::new(crate::hydra::error::TypeArityMismatchError_Variant {
      type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
        left: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
        right: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))}))))),
      expected_arity: 2i32,
      actual_arity: n.clone(),
      type_arguments: type_args.clone()})))))))),
    context: cx.clone()}))))}

pub fn type_of_injection(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, injection: crate::hydra::core::Injection) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  let tname = injection.clone().0.type_name.clone() ;
  let field = injection.clone().0.field.clone() ;
  let fname = field.clone().0.name.clone() ;
  let fterm = field.clone().0.term.clone() ;
  crate::hydra::lib::eithers::bind(crate::hydra::schemas::require_schema_type(cx.clone(), tx.clone().0.schema_types.clone(), tname.clone()), |schema_result: (crate::hydra::core::TypeScheme, crate::hydra::context::Context)| {
    let schema_type = crate::hydra::lib::pairs::first(schema_result.clone()) ;
    {
      let cx2 = crate::hydra::lib::pairs::second(schema_result.clone()) ;
      {
        let svars = schema_type.clone().0.variables.clone() ;
        {
          let sbody = schema_type.clone().0.type_.clone() ;
          crate::hydra::lib::eithers::bind(crate::hydra::extract::core::union_type(cx2.clone(), tname.clone(), sbody.clone()), |sfields: Vec<crate::hydra::core::FieldType>| crate::hydra::lib::eithers::bind(crate::hydra::schemas::find_field_type(cx2.clone(), fname.clone(), sfields.clone()), |ftyp: crate::hydra::core::Type| Right((crate::hydra::schemas::nominal_application(tname.clone(), type_args.clone()), cx2.clone()))))}}}})}

pub fn type_of_lambda(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, l: crate::hydra::core::Lambda) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  let v = l.clone().0.parameter.clone() ;
  let mdom = l.clone().0.domain.clone() ;
  let body = l.clone().0.body.clone() ;
  crate::hydra::lib::eithers::bind(crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Checking(crate::hydra::error::CheckingError(Rc::new(crate::hydra::error::CheckingError_Variant::UntypedLambda))))),
    context: cx.clone()}))), |dom: crate::hydra::core::Type| {
    let types2 = crate::hydra::lib::maps::insert(v.clone(), crate::hydra::rewriting::f_type_to_type_scheme(dom.clone()), tx.clone().0.bound_types.clone()) ;
    crate::hydra::lib::eithers::bind(type_of(cx.clone(), crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
      bound_terms: tx.clone().0.bound_terms.clone(),
      bound_types: types2.clone(),
      class_constraints: tx.clone().0.class_constraints.clone(),
      lambda_variables: tx.clone().0.lambda_variables.clone(),
      metadata: tx.clone().0.metadata.clone(),
      primitives: tx.clone().0.primitives.clone(),
      schema_types: tx.clone().0.schema_types.clone(),
      type_variables: tx.clone().0.type_variables.clone()})), Vec::from([]), body.clone()), |cod_result: (crate::hydra::core::Type, crate::hydra::context::Context)| {
      let cod = crate::hydra::lib::pairs::first(cod_result.clone()) ;
      {
        let cx2 = crate::hydra::lib::pairs::second(cod_result.clone()) ;
        Right((crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
          domain: dom.clone(),
          codomain: cod.clone()}))))), cx2.clone()))}})}, mdom.clone()), |tbody_result: (crate::hydra::core::Type, crate::hydra::context::Context)| {
    let tbody = crate::hydra::lib::pairs::first(tbody_result.clone()) ;
    {
      let cx3 = crate::hydra::lib::pairs::second(tbody_result.clone()) ;
      crate::hydra::lib::eithers::bind(apply_type_arguments_to_type(cx3.clone(), tx.clone(), type_args.clone(), tbody.clone()), |applied: crate::hydra::core::Type| Right((applied.clone(), cx3.clone())))}})}

pub fn type_of_let(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, let_term: crate::hydra::core::Let) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  let bs = let_term.clone().0.bindings.clone() ;
  let body = let_term.clone().0.body.clone() ;
  let bnames = crate::hydra::lib::lists::map(|v| v.0.name.clone(), bs.clone()) ;
  let binding_type = |b: crate::hydra::core::Binding| crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Checking(crate::hydra::error::CheckingError(Rc::new(crate::hydra::error::CheckingError_Variant::UntypedLetBinding(crate::hydra::error::UntypedLetBindingError(Rc::new(crate::hydra::error::UntypedLetBindingError_Variant {
      binding: b.clone()})))))))),
    context: cx.clone()}))), |ts: crate::hydra::core::TypeScheme| Right(crate::hydra::rewriting::type_scheme_to_f_type(ts.clone())), b.clone().0.type_.clone()) ;
  let btypes_result = crate::hydra::lib::lists::foldl(|acc: Either<crate::hydra::context::InContext, (Vec<crate::hydra::core::Type>, ())>, b: crate::hydra::core::Binding| crate::hydra::lib::eithers::bind(acc.clone(), |acc_r: (Vec<crate::hydra::core::Type>, ())| {
    let types = crate::hydra::lib::pairs::first(acc_r.clone()) ;
    crate::hydra::lib::eithers::bind(binding_type.clone()(b.clone()), |btype: crate::hydra::core::Type| Right((crate::hydra::lib::lists::concat2(types.clone(), crate::hydra::lib::lists::pure(btype.clone())), ())))}), Right((Vec::from([]), ())), bs.clone()) ;
  crate::hydra::lib::eithers::bind(btypes_result.clone(), |btypes_r: (Vec<crate::hydra::core::Type>, ())| {
    let btypes = crate::hydra::lib::pairs::first(btypes_r.clone()) ;
    {
      let tx2 = crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
        bound_terms: tx.clone().0.bound_terms.clone(),
        bound_types: crate::hydra::lib::maps::union_(crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::zip(bnames.clone(), crate::hydra::lib::lists::map(crate::hydra::rewriting::f_type_to_type_scheme, btypes.clone()))), tx.clone().0.bound_types.clone()),
        class_constraints: tx.clone().0.class_constraints.clone(),
        lambda_variables: tx.clone().0.lambda_variables.clone(),
        metadata: tx.clone().0.metadata.clone(),
        primitives: tx.clone().0.primitives.clone(),
        schema_types: tx.clone().0.schema_types.clone(),
        type_variables: tx.clone().0.type_variables.clone()})) ;
      crate::hydra::lib::eithers::bind(type_of(cx.clone(), tx2.clone(), Vec::from([]), body.clone()), |t_result: (crate::hydra::core::Type, crate::hydra::context::Context)| {
        let t = crate::hydra::lib::pairs::first(t_result.clone()) ;
        {
          let cx2 = crate::hydra::lib::pairs::second(t_result.clone()) ;
          crate::hydra::lib::eithers::bind(apply_type_arguments_to_type(cx2.clone(), tx.clone(), type_args.clone(), t.clone()), |applied: crate::hydra::core::Type| Right((applied.clone(), cx2.clone())))}})}})}

pub fn type_of_list(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, els: Vec<crate::hydra::core::Term>) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(els.clone()), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(crate::hydra::lib::lists::length(type_args.clone()), 1i32), Right((crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::lib::lists::head(type_args.clone())))), cx.clone())), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Checking(crate::hydra::error::CheckingError(Rc::new(crate::hydra::error::CheckingError_Variant::TypeArityMismatch(crate::hydra::error::TypeArityMismatchError(Rc::new(crate::hydra::error::TypeArityMismatchError_Variant {
      type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))))),
      expected_arity: 1i32,
      actual_arity: crate::hydra::lib::lists::length(type_args.clone()),
      type_arguments: type_args.clone()})))))))),
    context: cx.clone()})))), {
    let fold_result = crate::hydra::lib::lists::foldl(|acc: Either<crate::hydra::context::InContext, (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)>, term: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(acc.clone(), |acc_r: (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)| {
      let types = crate::hydra::lib::pairs::first(acc_r.clone()) ;
      {
        let cx_a = crate::hydra::lib::pairs::second(acc_r.clone()) ;
        crate::hydra::lib::eithers::bind(type_of(cx_a.clone(), tx.clone(), Vec::from([]), term.clone()), |t_result: (crate::hydra::core::Type, crate::hydra::context::Context)| {
          let t = crate::hydra::lib::pairs::first(t_result.clone()) ;
          {
            let cx_b = crate::hydra::lib::pairs::second(t_result.clone()) ;
            Right((crate::hydra::lib::lists::concat2(types.clone(), crate::hydra::lib::lists::pure(t.clone())), cx_b.clone()))}})}}), Right((Vec::from([]), cx.clone())), els.clone()) ;
    crate::hydra::lib::eithers::bind(fold_result.clone(), |fold_r: (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)| {
      let eltypes = crate::hydra::lib::pairs::first(fold_r.clone()) ;
      {
        let cx2 = crate::hydra::lib::pairs::second(fold_r.clone()) ;
        crate::hydra::lib::eithers::bind(check_same_type(cx2.clone(), tx.clone(), String::from("list elements"), eltypes.clone()), |unified_type: crate::hydra::core::Type| Right((crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(unified_type.clone()))), cx2.clone())))}})})}

pub fn type_of_literal(cx: crate::hydra::context::Context, tx: T0, type_args: Vec<crate::hydra::core::Type>, lit: crate::hydra::core::Literal) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  let t = crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::reflect::literal_type(lit.clone())))) ;
  crate::hydra::lib::eithers::bind(apply_type_arguments_to_type(cx.clone(), tx.clone(), type_args.clone(), t.clone()), |applied: crate::hydra::core::Type| Right((applied.clone(), cx.clone())))}

pub fn type_of_map(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, m: BTreeMap<crate::hydra::core::Term, crate::hydra::core::Term>) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  crate::hydra::lib::logic::if_else(crate::hydra::lib::maps::null(m.clone()), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(crate::hydra::lib::lists::length(type_args.clone()), 2i32), Right((crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
    keys: crate::hydra::lib::lists::at(0i32, type_args.clone()),
    values: crate::hydra::lib::lists::at(1i32, type_args.clone())}))))), cx.clone())), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Checking(crate::hydra::error::CheckingError(Rc::new(crate::hydra::error::CheckingError_Variant::TypeArityMismatch(crate::hydra::error::TypeArityMismatchError(Rc::new(crate::hydra::error::TypeArityMismatchError_Variant {
      type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
        keys: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
        values: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))}))))),
      expected_arity: 2i32,
      actual_arity: crate::hydra::lib::lists::length(type_args.clone()),
      type_arguments: type_args.clone()})))))))),
    context: cx.clone()})))), {
    let pairs = crate::hydra::lib::maps::to_list(m.clone()) ;
    {
      let key_fold_result = crate::hydra::lib::lists::foldl(|acc: Either<crate::hydra::context::InContext, (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)>, p: (crate::hydra::core::Term, crate::hydra::core::Term)| crate::hydra::lib::eithers::bind(acc.clone(), |acc_r: (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)| {
        let types = crate::hydra::lib::pairs::first(acc_r.clone()) ;
        {
          let cx_a = crate::hydra::lib::pairs::second(acc_r.clone()) ;
          crate::hydra::lib::eithers::bind(type_of(cx_a.clone(), tx.clone(), Vec::from([]), crate::hydra::lib::pairs::first(p.clone())), |t_result: (crate::hydra::core::Type, crate::hydra::context::Context)| {
            let t = crate::hydra::lib::pairs::first(t_result.clone()) ;
            {
              let cx_b = crate::hydra::lib::pairs::second(t_result.clone()) ;
              Right((crate::hydra::lib::lists::concat2(types.clone(), crate::hydra::lib::lists::pure(t.clone())), cx_b.clone()))}})}}), Right((Vec::from([]), cx.clone())), pairs.clone()) ;
      crate::hydra::lib::eithers::bind(key_fold_result.clone(), |key_fold_r: (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)| {
        let key_types = crate::hydra::lib::pairs::first(key_fold_r.clone()) ;
        {
          let cx2 = crate::hydra::lib::pairs::second(key_fold_r.clone()) ;
          crate::hydra::lib::eithers::bind(check_same_type(cx2.clone(), tx.clone(), String::from("map keys"), key_types.clone()), |kt: crate::hydra::core::Type| {
            let val_fold_result = crate::hydra::lib::lists::foldl(|acc: Either<crate::hydra::context::InContext, (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)>, p: (crate::hydra::core::Term, crate::hydra::core::Term)| crate::hydra::lib::eithers::bind(acc.clone(), |acc_r: (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)| {
              let types = crate::hydra::lib::pairs::first(acc_r.clone()) ;
              {
                let cx_a = crate::hydra::lib::pairs::second(acc_r.clone()) ;
                crate::hydra::lib::eithers::bind(type_of(cx_a.clone(), tx.clone(), Vec::from([]), crate::hydra::lib::pairs::second(p.clone())), |t_result: (crate::hydra::core::Type, crate::hydra::context::Context)| {
                  let t = crate::hydra::lib::pairs::first(t_result.clone()) ;
                  {
                    let cx_b = crate::hydra::lib::pairs::second(t_result.clone()) ;
                    Right((crate::hydra::lib::lists::concat2(types.clone(), crate::hydra::lib::lists::pure(t.clone())), cx_b.clone()))}})}}), Right((Vec::from([]), cx2.clone())), pairs.clone()) ;
            crate::hydra::lib::eithers::bind(val_fold_result.clone(), |val_fold_r: (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)| {
              let val_types = crate::hydra::lib::pairs::first(val_fold_r.clone()) ;
              {
                let cx3 = crate::hydra::lib::pairs::second(val_fold_r.clone()) ;
                crate::hydra::lib::eithers::bind(check_same_type(cx3.clone(), tx.clone(), String::from("map values"), val_types.clone()), |vt: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(apply_type_arguments_to_type(cx3.clone(), tx.clone(), type_args.clone(), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
                  keys: kt.clone(),
                  values: vt.clone()})))))), |applied: crate::hydra::core::Type| Right((applied.clone(), cx3.clone()))))}})})}})}})}

pub fn type_of_maybe(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, mt: Option<crate::hydra::core::Term>) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  let for_nothing = {
    let n = crate::hydra::lib::lists::length(type_args.clone()) ;
    crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(n.clone(), 1i32), Right((crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(crate::hydra::lib::lists::head(type_args.clone())))), cx.clone())), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Checking(crate::hydra::error::CheckingError(Rc::new(crate::hydra::error::CheckingError_Variant::TypeArityMismatch(crate::hydra::error::TypeArityMismatchError(Rc::new(crate::hydra::error::TypeArityMismatchError_Variant {
        type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))))),
        expected_arity: 1i32,
        actual_arity: n.clone(),
        type_arguments: type_args.clone()})))))))),
      context: cx.clone()}))))} ;
  let for_just = |term: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(type_of(cx.clone(), tx.clone(), Vec::from([]), term.clone()), |t_result: (crate::hydra::core::Type, crate::hydra::context::Context)| {
    let term_type = crate::hydra::lib::pairs::first(t_result.clone()) ;
    {
      let cx2 = crate::hydra::lib::pairs::second(t_result.clone()) ;
      {
        let t = crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(term_type.clone()))) ;
        crate::hydra::lib::eithers::bind(apply_type_arguments_to_type(cx2.clone(), tx.clone(), type_args.clone(), t.clone()), |applied: crate::hydra::core::Type| Right((applied.clone(), cx2.clone())))}}}) ;
  crate::hydra::lib::maybes::maybe(for_nothing.clone(), for_just.clone(), mt.clone())}

pub fn type_of_pair(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, p: (crate::hydra::core::Term, crate::hydra::core::Term)) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  let n = crate::hydra::lib::lists::length(type_args.clone()) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(n.clone(), 2i32), {
    let pair_fst = crate::hydra::lib::pairs::first(p.clone()) ;
    {
      let pair_snd = crate::hydra::lib::pairs::second(p.clone()) ;
      crate::hydra::lib::eithers::bind(type_of(cx.clone(), tx.clone(), Vec::from([]), pair_fst.clone()), |result1: (crate::hydra::core::Type, crate::hydra::context::Context)| {
        let first_type = crate::hydra::lib::pairs::first(result1.clone()) ;
        {
          let cx2 = crate::hydra::lib::pairs::second(result1.clone()) ;
          crate::hydra::lib::eithers::bind(type_of(cx2.clone(), tx.clone(), Vec::from([]), pair_snd.clone()), |result2: (crate::hydra::core::Type, crate::hydra::context::Context)| {
            let second_type = crate::hydra::lib::pairs::first(result2.clone()) ;
            {
              let cx3 = crate::hydra::lib::pairs::second(result2.clone()) ;
              Right((crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Pair(crate::hydra::core::PairType(Rc::new(crate::hydra::core::PairType_Variant {
                first: first_type.clone(),
                second: second_type.clone()}))))), cx3.clone()))}})}})}}, Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Checking(crate::hydra::error::CheckingError(Rc::new(crate::hydra::error::CheckingError_Variant::TypeArityMismatch(crate::hydra::error::TypeArityMismatchError(Rc::new(crate::hydra::error::TypeArityMismatchError_Variant {
      type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Pair(crate::hydra::core::PairType(Rc::new(crate::hydra::core::PairType_Variant {
        first: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
        second: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))}))))),
      expected_arity: 2i32,
      actual_arity: n.clone(),
      type_arguments: type_args.clone()})))))))),
    context: cx.clone()}))))}

pub fn type_of_primitive(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, name: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  let raw_ts = crate::hydra::lib::maps::lookup(name.clone(), crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|_gpt_p: crate::hydra::graph::Primitive| (_gpt_p.clone().0.name.clone(), _gpt_p.clone().0.type_.clone()), crate::hydra::lib::maps::elems(tx.clone().0.primitives.clone())))) ;
  crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::UndefinedTerm(crate::hydra::error::UndefinedTermError(Rc::new(crate::hydra::error::UndefinedTermError_Variant {
      name: name.clone()}))))),
    context: cx.clone()}))), |ts_raw: crate::hydra::core::TypeScheme| {
    let inst_result = crate::hydra::schemas::instantiate_type_scheme(cx.clone(), ts_raw.clone()) ;
    {
      let ts = crate::hydra::lib::pairs::first(inst_result.clone()) ;
      {
        let cx2 = crate::hydra::lib::pairs::second(inst_result.clone()) ;
        {
          let t = crate::hydra::rewriting::type_scheme_to_f_type(ts.clone()) ;
          crate::hydra::lib::eithers::bind(apply_type_arguments_to_type(cx2.clone(), tx.clone(), type_args.clone(), t.clone()), |applied: crate::hydra::core::Type| Right((applied.clone(), cx2.clone())))}}}}, raw_ts.clone())}

pub fn type_of_projection(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, p: crate::hydra::core::Projection) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  let tname = p.clone().0.type_name.clone() ;
  let fname = p.clone().0.field.clone() ;
  crate::hydra::lib::eithers::bind(crate::hydra::schemas::require_schema_type(cx.clone(), tx.clone().0.schema_types.clone(), tname.clone()), |schema_result: (crate::hydra::core::TypeScheme, crate::hydra::context::Context)| {
    let schema_type = crate::hydra::lib::pairs::first(schema_result.clone()) ;
    {
      let cx2 = crate::hydra::lib::pairs::second(schema_result.clone()) ;
      {
        let svars = schema_type.clone().0.variables.clone() ;
        {
          let sbody = schema_type.clone().0.type_.clone() ;
          crate::hydra::lib::eithers::bind(crate::hydra::extract::core::record_type(cx2.clone(), tname.clone(), sbody.clone()), |sfields: Vec<crate::hydra::core::FieldType>| crate::hydra::lib::eithers::bind(crate::hydra::schemas::find_field_type(cx2.clone(), fname.clone(), sfields.clone()), |ftyp: crate::hydra::core::Type| {
            let subst = crate::hydra::typing::TypeSubst(Rc::new(crate::hydra::typing::TypeSubst_Variant(crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::zip(svars.clone(), type_args.clone()))))) ;
            {
              let sftyp = crate::hydra::substitution::subst_in_type(subst.clone(), ftyp.clone()) ;
              Right((crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                domain: crate::hydra::schemas::nominal_application(tname.clone(), type_args.clone()),
                codomain: sftyp.clone()}))))), cx2.clone()))}}))}}}})}

pub fn type_of_record(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, record: crate::hydra::core::Record) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  let tname = record.clone().0.type_name.clone() ;
  let fields = record.clone().0.fields.clone() ;
  let fold_result = crate::hydra::lib::lists::foldl(|acc: Either<crate::hydra::context::InContext, (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)>, term: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(acc.clone(), |acc_r: (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)| {
    let types = crate::hydra::lib::pairs::first(acc_r.clone()) ;
    {
      let cx_a = crate::hydra::lib::pairs::second(acc_r.clone()) ;
      crate::hydra::lib::eithers::bind(type_of(cx_a.clone(), tx.clone(), Vec::from([]), term.clone()), |t_result: (crate::hydra::core::Type, crate::hydra::context::Context)| {
        let t = crate::hydra::lib::pairs::first(t_result.clone()) ;
        {
          let cx_b = crate::hydra::lib::pairs::second(t_result.clone()) ;
          Right((crate::hydra::lib::lists::concat2(types.clone(), crate::hydra::lib::lists::pure(t.clone())), cx_b.clone()))}})}}), Right((Vec::from([]), cx.clone())), crate::hydra::lib::lists::map(|v| v.0.term.clone(), fields.clone())) ;
  crate::hydra::lib::eithers::bind(fold_result.clone(), |fold_r: (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)| {
    let cx2 = crate::hydra::lib::pairs::second(fold_r.clone()) ;
    Right((crate::hydra::schemas::nominal_application(tname.clone(), type_args.clone()), cx2.clone()))})}

pub fn type_of_set(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, els: BTreeSet<crate::hydra::core::Term>) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  crate::hydra::lib::logic::if_else(crate::hydra::lib::sets::null(els.clone()), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(crate::hydra::lib::lists::length(type_args.clone()), 1i32), Right((crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(crate::hydra::lib::lists::head(type_args.clone())))), cx.clone())), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Checking(crate::hydra::error::CheckingError(Rc::new(crate::hydra::error::CheckingError_Variant::TypeArityMismatch(crate::hydra::error::TypeArityMismatchError(Rc::new(crate::hydra::error::TypeArityMismatchError_Variant {
      type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))))),
      expected_arity: 1i32,
      actual_arity: crate::hydra::lib::lists::length(type_args.clone()),
      type_arguments: type_args.clone()})))))))),
    context: cx.clone()})))), {
    let fold_result = crate::hydra::lib::lists::foldl(|acc: Either<crate::hydra::context::InContext, (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)>, term: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(acc.clone(), |acc_r: (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)| {
      let types = crate::hydra::lib::pairs::first(acc_r.clone()) ;
      {
        let cx_a = crate::hydra::lib::pairs::second(acc_r.clone()) ;
        crate::hydra::lib::eithers::bind(type_of(cx_a.clone(), tx.clone(), Vec::from([]), term.clone()), |t_result: (crate::hydra::core::Type, crate::hydra::context::Context)| {
          let t = crate::hydra::lib::pairs::first(t_result.clone()) ;
          {
            let cx_b = crate::hydra::lib::pairs::second(t_result.clone()) ;
            Right((crate::hydra::lib::lists::concat2(types.clone(), crate::hydra::lib::lists::pure(t.clone())), cx_b.clone()))}})}}), Right((Vec::from([]), cx.clone())), crate::hydra::lib::sets::to_list(els.clone())) ;
    crate::hydra::lib::eithers::bind(fold_result.clone(), |fold_r: (Vec<crate::hydra::core::Type>, crate::hydra::context::Context)| {
      let eltypes = crate::hydra::lib::pairs::first(fold_r.clone()) ;
      {
        let cx2 = crate::hydra::lib::pairs::second(fold_r.clone()) ;
        crate::hydra::lib::eithers::bind(check_same_type(cx2.clone(), tx.clone(), String::from("set elements"), eltypes.clone()), |unified_type: crate::hydra::core::Type| Right((crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(unified_type.clone()))), cx2.clone())))}})})}

pub fn type_of_type_application(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, tyapp: crate::hydra::core::TypeApplicationTerm) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  let body = tyapp.clone().0.body.clone() ;
  let t = tyapp.clone().0.type_.clone() ;
  type_of(cx.clone(), tx.clone(), crate::hydra::lib::lists::cons(t.clone(), type_args.clone()), body.clone())}

pub fn type_of_type_lambda(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, tl: crate::hydra::core::TypeLambda) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  let v = tl.clone().0.parameter.clone() ;
  let body = tl.clone().0.body.clone() ;
  let vars = tx.clone().0.type_variables.clone() ;
  let tx2 = crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
    bound_terms: tx.clone().0.bound_terms.clone(),
    bound_types: tx.clone().0.bound_types.clone(),
    class_constraints: tx.clone().0.class_constraints.clone(),
    lambda_variables: tx.clone().0.lambda_variables.clone(),
    metadata: tx.clone().0.metadata.clone(),
    primitives: tx.clone().0.primitives.clone(),
    schema_types: tx.clone().0.schema_types.clone(),
    type_variables: crate::hydra::lib::sets::insert(v.clone(), vars.clone())})) ;
  crate::hydra::lib::eithers::bind(type_of(cx.clone(), tx2.clone(), Vec::from([]), body.clone()), |result1: (crate::hydra::core::Type, crate::hydra::context::Context)| {
    let t1 = crate::hydra::lib::pairs::first(result1.clone()) ;
    {
      let cx2 = crate::hydra::lib::pairs::second(result1.clone()) ;
      crate::hydra::lib::eithers::bind(apply_type_arguments_to_type(cx2.clone(), tx.clone(), type_args.clone(), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
        parameter: v.clone(),
        body: t1.clone()})))))), |applied: crate::hydra::core::Type| Right((applied.clone(), cx2.clone())))}})}

pub fn type_of_unit(cx: crate::hydra::context::Context, tx: T0, type_args: Vec<crate::hydra::core::Type>) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  crate::hydra::lib::eithers::bind(apply_type_arguments_to_type(cx.clone(), tx.clone(), type_args.clone(), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))), |applied: crate::hydra::core::Type| Right((applied.clone(), cx.clone())))}

pub fn type_of_unwrap(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, tname: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  crate::hydra::lib::eithers::bind(crate::hydra::schemas::require_schema_type(cx.clone(), tx.clone().0.schema_types.clone(), tname.clone()), |schema_result: (crate::hydra::core::TypeScheme, crate::hydra::context::Context)| {
    let schema_type = crate::hydra::lib::pairs::first(schema_result.clone()) ;
    {
      let cx2 = crate::hydra::lib::pairs::second(schema_result.clone()) ;
      {
        let svars = schema_type.clone().0.variables.clone() ;
        {
          let sbody = schema_type.clone().0.type_.clone() ;
          crate::hydra::lib::eithers::bind(crate::hydra::extract::core::wrapped_type(cx2.clone(), tname.clone(), sbody.clone()), |wrapped: crate::hydra::core::Type| {
            let subst = crate::hydra::typing::TypeSubst(Rc::new(crate::hydra::typing::TypeSubst_Variant(crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::zip(svars.clone(), type_args.clone()))))) ;
            {
              let swrapped = crate::hydra::substitution::subst_in_type(subst.clone(), wrapped.clone()) ;
              Right((crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                domain: crate::hydra::schemas::nominal_application(tname.clone(), type_args.clone()),
                codomain: swrapped.clone()}))))), cx2.clone()))}})}}}})}

pub fn type_of_variable(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, name: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  let raw_type_scheme = crate::hydra::lib::maps::lookup(name.clone(), tx.clone().0.bound_types.clone()) ;
  crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::UndefinedType(crate::hydra::error::UndefinedTypeError(Rc::new(crate::hydra::error::UndefinedTypeError_Variant {
      name: name.clone()}))))),
    context: cx.clone()}))), |ts: crate::hydra::core::TypeScheme| {
    let t_result = crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(type_args.clone()), crate::hydra::schemas::instantiate_type(cx.clone(), crate::hydra::rewriting::type_scheme_to_f_type(ts.clone())), (crate::hydra::rewriting::type_scheme_to_f_type(ts.clone()), cx.clone())) ;
    {
      let t = crate::hydra::lib::pairs::first(t_result.clone()) ;
      {
        let cx2 = crate::hydra::lib::pairs::second(t_result.clone()) ;
        crate::hydra::lib::eithers::bind(apply_type_arguments_to_type(cx2.clone(), tx.clone(), type_args.clone(), t.clone()), |applied: crate::hydra::core::Type| Right((applied.clone(), cx2.clone())))}}}, raw_type_scheme.clone())}

pub fn type_of_wrapped_term(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, type_args: Vec<crate::hydra::core::Type>, wt: crate::hydra::core::WrappedTerm) -> Either<crate::hydra::context::InContext, (crate::hydra::core::Type, crate::hydra::context::Context)> {
  let tname = wt.clone().0.type_name.clone() ;
  let body = wt.clone().0.body.clone() ;
  crate::hydra::lib::eithers::bind(type_of(cx.clone(), tx.clone(), Vec::from([]), body.clone()), |result: (crate::hydra::core::Type, crate::hydra::context::Context)| {
    let cx2 = crate::hydra::lib::pairs::second(result.clone()) ;
    Right((crate::hydra::schemas::nominal_application(tname.clone(), type_args.clone()), cx2.clone()))})}

pub fn types_all_effectively_equal(tx: crate::hydra::graph::Graph, tlist: Vec<crate::hydra::core::Type>) -> bool {
  let types = tx.clone().0.schema_types.clone() ;
  let contains_free_var = |t: crate::hydra::core::Type| {
    let all_vars = crate::hydra::rewriting::free_variables_in_type_simple(t.clone()) ;
    {
      let schema_names = crate::hydra::lib::sets::from_list(crate::hydra::lib::maps::keys(types.clone())) ;
      crate::hydra::lib::logic::not(crate::hydra::lib::sets::null(crate::hydra::lib::sets::difference(all_vars.clone(), schema_names.clone())))}} ;
  let any_contains_free_var = crate::hydra::lib::lists::foldl(|acc: bool, t: crate::hydra::core::Type| crate::hydra::lib::logic::or(acc.clone(), contains_free_var.clone()(t.clone())), false, tlist.clone()) ;
  crate::hydra::lib::logic::if_else(any_contains_free_var.clone(), true, crate::hydra::lib::logic::if_else(all_equal(crate::hydra::lib::lists::map(|t: crate::hydra::core::Type| normalize_type_free_vars(t.clone()), tlist.clone())), true, all_equal(crate::hydra::lib::lists::map(|t: crate::hydra::core::Type| normalize_type_free_vars(crate::hydra::rewriting::deannotate_type_recursive(crate::hydra::rewriting::replace_typedefs(types.clone(), t.clone()))), tlist.clone()))))}

pub fn types_effectively_equal(tx: crate::hydra::graph::Graph, t1: crate::hydra::core::Type, t2: crate::hydra::core::Type) -> bool {
  crate::hydra::lib::logic::or(contains_in_scope_type_vars(tx.clone(), t1.clone()), crate::hydra::lib::logic::or(contains_in_scope_type_vars(tx.clone(), t2.clone()), types_all_effectively_equal(tx.clone(), Vec::from([
    crate::hydra::schemas::fully_strip_and_normalize_type(t1.clone()),
    crate::hydra::schemas::fully_strip_and_normalize_type(t2.clone())]))))}
