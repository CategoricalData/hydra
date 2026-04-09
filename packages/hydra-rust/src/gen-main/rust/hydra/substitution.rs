#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::rewriting::*;
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

pub fn compose_type_subst(s1: crate::hydra::typing::TypeSubst, s2: crate::hydra::typing::TypeSubst) -> crate::hydra::typing::TypeSubst {
  crate::hydra::lib::logic::if_else(crate::hydra::lib::maps::null(s1.clone().0.0.clone()), s2.clone(), crate::hydra::lib::logic::if_else(crate::hydra::lib::maps::null(s2.clone().0.0.clone()), s1.clone(), compose_type_subst_non_empty(s1.clone(), s2.clone())))}

pub fn compose_type_subst_non_empty(s1: crate::hydra::typing::TypeSubst, s2: crate::hydra::typing::TypeSubst) -> crate::hydra::typing::TypeSubst {
  let is_extra = |k: crate::hydra::core::Name, v: T0| crate::hydra::lib::maybes::is_nothing(crate::hydra::lib::maps::lookup(k.clone(), s1.clone().0.0.clone())) ;
  let with_extra = crate::hydra::lib::maps::filter_with_key(is_extra.clone(), s2.clone().0.0.clone()) ;
  crate::hydra::typing::TypeSubst(Rc::new(crate::hydra::typing::TypeSubst_Variant(crate::hydra::lib::maps::union_(with_extra.clone(), crate::hydra::lib::maps::map(|v1: crate::hydra::core::Type| subst_in_type(s2.clone(), v1.clone()), s1.clone().0.0.clone())))))}

pub fn compose_type_subst_list(v1: Vec<X>) -> crate::hydra::typing::TypeSubst {
  crate::hydra::lib::lists::foldl(compose_type_subst, id_type_subst, v1.clone())}

pub fn id_type_subst() -> crate::hydra::typing::TypeSubst {
  crate::hydra::typing::TypeSubst(Rc::new(crate::hydra::typing::TypeSubst_Variant(crate::hydra::lib::maps::empty)))}

pub fn singleton_type_subst(v: crate::hydra::core::Name, t: crate::hydra::core::Type) -> crate::hydra::typing::TypeSubst {
  crate::hydra::typing::TypeSubst(Rc::new(crate::hydra::typing::TypeSubst_Variant(crate::hydra::lib::maps::singleton(v.clone(), t.clone()))))}

pub fn substitute_in_binding(subst: crate::hydra::typing::TermSubst, b: crate::hydra::core::Binding) -> crate::hydra::core::Binding {
  crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
    name: b.clone().0.name.clone(),
    term: substitute_in_term(subst.clone(), b.clone().0.term.clone()),
    type_: b.clone().0.type_.clone()}))}

pub fn substitute_in_constraint(subst: crate::hydra::typing::TypeSubst, c: crate::hydra::typing::TypeConstraint) -> crate::hydra::typing::TypeConstraint {
  crate::hydra::typing::TypeConstraint(Rc::new(crate::hydra::typing::TypeConstraint_Variant {
    left: subst_in_type(subst.clone(), c.clone().0.left.clone()),
    right: subst_in_type(subst.clone(), c.clone().0.right.clone()),
    comment: c.clone().0.comment.clone()}))}

pub fn substitute_in_constraints(subst: crate::hydra::typing::TypeSubst, cs: Vec<crate::hydra::typing::TypeConstraint>) -> Vec<crate::hydra::typing::TypeConstraint> {
  crate::hydra::lib::lists::map(|v1: crate::hydra::typing::TypeConstraint| substitute_in_constraint(subst.clone(), v1.clone()), cs.clone())}

pub fn subst_in_class_constraints(subst: crate::hydra::typing::TypeSubst, constraints: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>) -> BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata> {
  let subst_map = subst.clone().0.0.clone() ;
  let insert_or_merge = |var_name: T0, metadata: crate::hydra::core::TypeVariableMetadata, acc: BTreeMap<T0, crate::hydra::core::TypeVariableMetadata>| crate::hydra::lib::maybes::maybe(crate::hydra::lib::maps::insert(var_name.clone(), metadata.clone(), acc.clone()), |existing: crate::hydra::core::TypeVariableMetadata| {
    let merged = crate::hydra::core::TypeVariableMetadata(Rc::new(crate::hydra::core::TypeVariableMetadata_Variant {
      classes: crate::hydra::lib::sets::union_(existing.clone().0.classes.clone(), metadata.clone().0.classes.clone())})) ;
    crate::hydra::lib::maps::insert(var_name.clone(), merged.clone(), acc.clone())}, crate::hydra::lib::maps::lookup(var_name.clone(), acc.clone())) ;
  crate::hydra::lib::lists::foldl(|acc: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>, pair: (crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata)| {
    let var_name = crate::hydra::lib::pairs::first(pair.clone()) ;
    {
      let metadata = crate::hydra::lib::pairs::second(pair.clone()) ;
      crate::hydra::lib::maybes::maybe(insert_or_merge.clone()(var_name.clone(), metadata.clone(), acc.clone()), |target_type: crate::hydra::core::Type| {
        let free_vars = crate::hydra::lib::sets::to_list(crate::hydra::rewriting::free_variables_in_type(target_type.clone())) ;
        crate::hydra::lib::lists::foldl(|acc2: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>, free_var: crate::hydra::core::Name| insert_or_merge.clone()(free_var.clone(), metadata.clone(), acc2.clone()), acc.clone(), free_vars.clone())}, crate::hydra::lib::maps::lookup(var_name.clone(), subst_map.clone()))}}, crate::hydra::lib::maps::empty, crate::hydra::lib::maps::to_list(constraints.clone()))}

pub fn subst_in_context(subst: crate::hydra::typing::TypeSubst, cx: crate::hydra::graph::Graph) -> crate::hydra::graph::Graph {
  let new_bound_types = crate::hydra::lib::maps::map(|v1: crate::hydra::core::TypeScheme| subst_in_type_scheme(subst.clone(), v1.clone()), cx.clone().0.bound_types.clone()) ;
  let new_class_constraints = subst_in_class_constraints(subst.clone(), cx.clone().0.class_constraints.clone()) ;
  let cx2 = crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
    bound_terms: cx.clone().0.bound_terms.clone(),
    bound_types: new_bound_types.clone(),
    class_constraints: cx.clone().0.class_constraints.clone(),
    lambda_variables: cx.clone().0.lambda_variables.clone(),
    metadata: cx.clone().0.metadata.clone(),
    primitives: cx.clone().0.primitives.clone(),
    schema_types: cx.clone().0.schema_types.clone(),
    type_variables: cx.clone().0.type_variables.clone()})) ;
  crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
    bound_terms: cx2.clone().0.bound_terms.clone(),
    bound_types: cx2.clone().0.bound_types.clone(),
    class_constraints: new_class_constraints.clone(),
    lambda_variables: cx2.clone().0.lambda_variables.clone(),
    metadata: cx2.clone().0.metadata.clone(),
    primitives: cx2.clone().0.primitives.clone(),
    schema_types: cx2.clone().0.schema_types.clone(),
    type_variables: cx2.clone().0.type_variables.clone()}))}

pub fn substitute_in_term(subst: crate::hydra::typing::TermSubst, term0: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let s = subst.clone().0.0.clone() ;
  let rewrite = |recurse: Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>, term: crate::hydra::core::Term| {
    let with_lambda = |l: crate::hydra::core::Lambda| {
      let v = l.clone().0.parameter.clone() ;
      let subst2 = crate::hydra::typing::TermSubst(Rc::new(crate::hydra::typing::TermSubst_Variant(crate::hydra::lib::maps::delete(v.clone(), s.clone())))) ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
        parameter: v.clone(),
        domain: l.clone().0.domain.clone(),
        body: substitute_in_term(subst2.clone(), l.clone().0.body.clone())}))))))))} ;
    let with_let = |lt: crate::hydra::core::Let| {
      let bindings = lt.clone().0.bindings.clone() ;
      let names = crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::map(|v| v.0.name.clone(), bindings.clone())) ;
      let subst2 = crate::hydra::typing::TermSubst(Rc::new(crate::hydra::typing::TermSubst_Variant(crate::hydra::lib::maps::filter_with_key(|k: crate::hydra::core::Name, v: crate::hydra::core::Term| crate::hydra::lib::logic::not(crate::hydra::lib::sets::member(k.clone(), names.clone())), s.clone())))) ;
      let rewrite_binding = |b: crate::hydra::core::Binding| crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
        name: b.clone().0.name.clone(),
        term: substitute_in_term(subst2.clone(), b.clone().0.term.clone()),
        type_: b.clone().0.type_.clone()})) ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
        bindings: crate::hydra::lib::lists::map(rewrite_binding.clone(), bindings.clone()),
        body: substitute_in_term(subst2.clone(), lt.clone().0.body.clone())})))))} ;
    match &*term.clone().0 {
      crate::hydra::core::Term_Variant::Function (v0_) => {
        let v0_ = v0_.clone() ;
        match &*v0_.clone().0 {
          crate::hydra::core::Function_Variant::Lambda (v0_) => {
            let v0_ = v0_.clone() ;
            with_lambda.clone()(v0_.clone())},
          _ => recurse.clone()(term.clone())}},
      crate::hydra::core::Term_Variant::Let (v0_) => {
        let v0_ = v0_.clone() ;
        with_let.clone()(v0_.clone())},
      crate::hydra::core::Term_Variant::Variable (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::lib::maybes::maybe(recurse.clone()(term.clone()), |sterm: crate::hydra::core::Term| sterm.clone(), crate::hydra::lib::maps::lookup(v0_.clone(), s.clone()))},
      _ => recurse.clone()(term.clone())}} ;
  crate::hydra::rewriting::rewrite_term(rewrite.clone(), term0.clone())}

pub fn subst_in_type(subst: crate::hydra::typing::TypeSubst, typ0: crate::hydra::core::Type) -> crate::hydra::core::Type {
  crate::hydra::lib::logic::if_else(crate::hydra::lib::maps::null(subst.clone().0.0.clone()), typ0.clone(), subst_in_type_non_empty(subst.clone(), typ0.clone()))}

pub fn subst_in_type_non_empty(subst: crate::hydra::typing::TypeSubst, typ0: crate::hydra::core::Type) -> crate::hydra::core::Type {
  let rewrite = |recurse: Rc<dyn Fn(crate::hydra::core::Type) -> crate::hydra::core::Type>, typ: crate::hydra::core::Type| match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::maybes::maybe(recurse.clone()(typ.clone()), |styp: crate::hydra::core::Type| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
        parameter: v0_.clone().0.parameter.clone(),
        body: subst_in_type(remove_var.clone()(v0_.clone().0.parameter.clone()), v0_.clone().0.body.clone())}))))), crate::hydra::lib::maps::lookup(v0_.clone().0.parameter.clone(), subst.clone().0.0.clone()))},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::maybes::maybe(typ.clone(), |styp: crate::hydra::core::Type| styp.clone(), crate::hydra::lib::maps::lookup(v0_.clone(), subst.clone().0.0.clone()))},
    _ => recurse.clone()(typ.clone())} ;
  let remove_var = |v: crate::hydra::core::Name| crate::hydra::typing::TypeSubst(Rc::new(crate::hydra::typing::TypeSubst_Variant(crate::hydra::lib::maps::delete(v.clone(), subst.clone().0.0.clone())))) ;
  crate::hydra::rewriting::rewrite_type(rewrite.clone(), typ0.clone())}

pub fn subst_in_type_scheme(subst: crate::hydra::typing::TypeSubst, ts: crate::hydra::core::TypeScheme) -> crate::hydra::core::TypeScheme {
  crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
    variables: ts.clone().0.variables.clone(),
    type_: subst_in_type(subst.clone(), ts.clone().0.type_.clone()),
    constraints: crate::hydra::lib::maybes::map(|v1: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>| subst_in_class_constraints(subst.clone(), v1.clone()), ts.clone().0.constraints.clone())}))}

pub fn subst_types_in_term(subst: crate::hydra::typing::TypeSubst, term0: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let rewrite = |recurse: Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>, term: crate::hydra::core::Term| {
    let dflt = recurse.clone()(term.clone()) ;
    let for_function = |f: crate::hydra::core::Function| match &*f.clone().0 {
      crate::hydra::core::Function_Variant::Elimination (v0_) => {
        let v0_ = v0_.clone() ;
        dflt.clone()},
      crate::hydra::core::Function_Variant::Lambda (v0_) => {
        let v0_ = v0_.clone() ;
        for_lambda.clone()(v0_.clone())},
      _ => dflt.clone()} ;
    let for_lambda = |l: crate::hydra::core::Lambda| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
      parameter: l.clone().0.parameter.clone(),
      domain: crate::hydra::lib::maybes::map(|v1: crate::hydra::core::Type| subst_in_type(subst.clone(), v1.clone()), l.clone().0.domain.clone()),
      body: subst_types_in_term(subst.clone(), l.clone().0.body.clone())})))))))) ;
    let for_let = |l: crate::hydra::core::Let| {
      let rewrite_binding = |b: crate::hydra::core::Binding| crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
        name: b.clone().0.name.clone(),
        term: subst_types_in_term(subst.clone(), b.clone().0.term.clone()),
        type_: crate::hydra::lib::maybes::map(|v1: crate::hydra::core::TypeScheme| subst_in_type_scheme(subst.clone(), v1.clone()), b.clone().0.type_.clone())})) ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
        bindings: crate::hydra::lib::lists::map(rewrite_binding.clone(), l.clone().0.bindings.clone()),
        body: subst_types_in_term(subst.clone(), l.clone().0.body.clone())})))))} ;
    let for_type_application = |tt: crate::hydra::core::TypeApplicationTerm| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
      body: subst_types_in_term(subst.clone(), tt.clone().0.body.clone()),
      type_: subst_in_type(subst.clone(), tt.clone().0.type_.clone())}))))) ;
    let for_type_lambda = |ta: crate::hydra::core::TypeLambda| {
      let param = ta.clone().0.parameter.clone() ;
      let subst2 = crate::hydra::typing::TypeSubst(Rc::new(crate::hydra::typing::TypeSubst_Variant(crate::hydra::lib::maps::delete(param.clone(), subst.clone().0.0.clone())))) ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeLambda(crate::hydra::core::TypeLambda(Rc::new(crate::hydra::core::TypeLambda_Variant {
        parameter: param.clone(),
        body: subst_types_in_term(subst2.clone(), ta.clone().0.body.clone())})))))} ;
    match &*term.clone().0 {
      crate::hydra::core::Term_Variant::Function (v0_) => {
        let v0_ = v0_.clone() ;
        for_function.clone()(v0_.clone())},
      crate::hydra::core::Term_Variant::Let (v0_) => {
        let v0_ = v0_.clone() ;
        for_let.clone()(v0_.clone())},
      crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
        let v0_ = v0_.clone() ;
        for_type_application.clone()(v0_.clone())},
      crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
        let v0_ = v0_.clone() ;
        for_type_lambda.clone()(v0_.clone())},
      _ => dflt.clone()}} ;
  crate::hydra::rewriting::rewrite_term(rewrite.clone(), term0.clone())}
