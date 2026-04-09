#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::annotations::*;
use crate::hydra::constants::*;
use crate::hydra::decode::core::*;
use crate::hydra::encode::core::*;
use crate::hydra::lexical::*;
use crate::hydra::names::*;
use crate::hydra::reflect::*;
use crate::hydra::rewriting::*;
use crate::hydra::show::core::*;
use crate::hydra::show::error::*;
use crate::hydra::sorting::*;
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

pub fn add_names_to_namespaces(encode_namespace: impl Fn(crate::hydra::module::Namespace) -> T0 + Clone, names: BTreeSet<crate::hydra::core::Name>, ns0: crate::hydra::module::Namespaces) -> crate::hydra::module::Namespaces {
  let nss = crate::hydra::lib::sets::from_list(crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(crate::hydra::names::namespace_of, crate::hydra::lib::sets::to_list(names.clone())))) ;
  let to_pair = |ns: crate::hydra::module::Namespace| (ns.clone(), encode_namespace.clone()(ns.clone())) ;
  crate::hydra::module::Namespaces(Rc::new(crate::hydra::module::Namespaces_Variant {
    focus: ns0.clone().0.focus.clone(),
    mapping: crate::hydra::lib::maps::union_(ns0.clone().0.mapping.clone(), crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(to_pair.clone(), crate::hydra::lib::sets::to_list(nss.clone()))))}))}

pub fn definition_dependency_namespaces(defs: Vec<crate::hydra::module::Definition>) -> BTreeSet<crate::hydra::module::Namespace> {
  let def_names = |def: crate::hydra::module::Definition| match &*def.clone().0 {
    crate::hydra::module::Definition_Variant::Type (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::rewriting::type_dependency_names(true, v0_.clone().0.type_.clone())},
    crate::hydra::module::Definition_Variant::Term (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::rewriting::term_dependency_names(true, true, true, v0_.clone().0.term.clone())}} ;
  let all_names = crate::hydra::lib::sets::unions(crate::hydra::lib::lists::map(def_names.clone(), defs.clone())) ;
  crate::hydra::lib::sets::from_list(crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(crate::hydra::names::namespace_of, crate::hydra::lib::sets::to_list(all_names.clone()))))}

pub fn dependency_namespaces(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, binds: bool, with_prims: bool, with_noms: bool, with_schema: bool, els: Vec<crate::hydra::core::Binding>) -> Either<crate::hydra::context::InContext, BTreeSet<crate::hydra::module::Namespace>> {
  let dep_names = |el: crate::hydra::core::Binding| {
    let term = el.clone().0.term.clone() ;
    {
      let deannotated_term = crate::hydra::rewriting::deannotate_term(term.clone()) ;
      {
        let data_names = crate::hydra::rewriting::term_dependency_names(binds.clone(), with_prims.clone(), with_noms.clone(), term.clone()) ;
        {
          let schema_names = crate::hydra::lib::logic::if_else(with_schema.clone(), crate::hydra::lib::maybes::maybe(crate::hydra::lib::sets::empty, |ts: crate::hydra::core::TypeScheme| crate::hydra::rewriting::type_dependency_names(true, ts.clone().0.type_.clone()), el.clone().0.type_.clone()), crate::hydra::lib::sets::empty) ;
          crate::hydra::lib::logic::if_else(is_encoded_type(deannotated_term.clone()), crate::hydra::lib::eithers::map(|typ: crate::hydra::core::Type| crate::hydra::lib::sets::unions(Vec::from([
            data_names.clone(),
            schema_names.clone(),
            crate::hydra::rewriting::type_dependency_names(true, typ.clone())])), crate::hydra::lib::eithers::bimap(|_wc_e: crate::hydra::error::Error| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
            object: _wc_e.clone(),
            context: crate::hydra::context::Context(Rc::new(crate::hydra::context::Context_Variant {
              trace: crate::hydra::lib::lists::cons(String::from("dependency namespace (type)"), cx.clone().0.trace.clone()),
              messages: cx.clone().0.messages.clone(),
              other: cx.clone().0.other.clone()}))})), |_wc_a: crate::hydra::core::Type| _wc_a.clone(), crate::hydra::lib::eithers::bimap(|_e: crate::hydra::error::DecodingError| crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(_e.clone().0.0.clone())))))), |_a: crate::hydra::core::Type| _a.clone(), crate::hydra::decode::core::type_(graph.clone(), term.clone())))), crate::hydra::lib::logic::if_else(is_encoded_term(deannotated_term.clone()), crate::hydra::lib::eithers::map(|decoded_term: crate::hydra::core::Term| crate::hydra::lib::sets::unions(Vec::from([
            data_names.clone(),
            schema_names.clone(),
            crate::hydra::rewriting::term_dependency_names(binds.clone(), with_prims.clone(), with_noms.clone(), decoded_term.clone())])), crate::hydra::lib::eithers::bimap(|_wc_e: crate::hydra::error::Error| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
            object: _wc_e.clone(),
            context: crate::hydra::context::Context(Rc::new(crate::hydra::context::Context_Variant {
              trace: crate::hydra::lib::lists::cons(String::from("dependency namespace (term)"), cx.clone().0.trace.clone()),
              messages: cx.clone().0.messages.clone(),
              other: cx.clone().0.other.clone()}))})), |_wc_a: crate::hydra::core::Term| _wc_a.clone(), crate::hydra::lib::eithers::bimap(|_e: crate::hydra::error::DecodingError| crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(_e.clone().0.0.clone())))))), |_a: crate::hydra::core::Term| _a.clone(), crate::hydra::decode::core::term(graph.clone(), term.clone())))), Right(crate::hydra::lib::sets::unions(Vec::from([
            data_names.clone(),
            schema_names.clone()])))))}}}} ;
  crate::hydra::lib::eithers::map(|names_list: Vec<BTreeSet<crate::hydra::core::Name>>| crate::hydra::lib::sets::from_list(crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(crate::hydra::names::namespace_of, crate::hydra::lib::sets::to_list(crate::hydra::lib::sets::unions(names_list.clone()))))), crate::hydra::lib::eithers::map_list(dep_names.clone(), els.clone()))}

pub fn dereference_type(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, Option<crate::hydra::core::Type>> {
  let mel = crate::hydra::lexical::dereference_element(graph.clone(), name.clone()) ;
  crate::hydra::lib::maybes::maybe(Right(None), |el: crate::hydra::core::Binding| crate::hydra::lib::eithers::map(crate::hydra::lib::maybes::pure, crate::hydra::lib::eithers::bimap(|_wc_e: crate::hydra::error::Error| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: _wc_e.clone(),
    context: cx.clone()})), |_wc_a: crate::hydra::core::Type| _wc_a.clone(), crate::hydra::lib::eithers::bimap(|_e: crate::hydra::error::DecodingError| crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(_e.clone().0.0.clone())))))), |_a: crate::hydra::core::Type| _a.clone(), crate::hydra::decode::core::type_(graph.clone(), el.clone().0.term.clone())))), mel.clone())}

pub fn element_as_type_application_term(cx: crate::hydra::context::Context, el: crate::hydra::core::Binding) -> Either<crate::hydra::context::InContext, crate::hydra::core::TypeApplicationTerm> {
  crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(String::from("missing element type"))))))),
    context: cx.clone()}))), |ts: crate::hydra::core::TypeScheme| Right(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
    body: el.clone().0.term.clone(),
    type_: ts.clone().0.type_.clone()}))), el.clone().0.type_.clone())}

pub fn elements_with_dependencies(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, original: Vec<crate::hydra::core::Binding>) -> Either<crate::hydra::context::InContext, Vec<crate::hydra::core::Binding>> {
  let dep_names = |el: crate::hydra::core::Binding| crate::hydra::lib::sets::to_list(crate::hydra::rewriting::term_dependency_names(true, false, false, el.clone().0.term.clone())) ;
  let all_dep_names = crate::hydra::lib::lists::nub(crate::hydra::lib::lists::concat2(crate::hydra::lib::lists::map(|v| v.0.name.clone(), original.clone()), crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(dep_names.clone(), original.clone())))) ;
  crate::hydra::lib::eithers::map_list(|name: crate::hydra::core::Name| crate::hydra::lexical::require_element(cx.clone(), graph.clone(), name.clone()), all_dep_names.clone())}

pub fn extend_graph_for_lambda(g: crate::hydra::graph::Graph, lam: crate::hydra::core::Lambda) -> crate::hydra::graph::Graph {
  let var = lam.clone().0.parameter.clone() ;
  crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
    bound_terms: g.clone().0.bound_terms.clone(),
    bound_types: crate::hydra::lib::maybes::maybe(g.clone().0.bound_types.clone(), |dom: crate::hydra::core::Type| crate::hydra::lib::maps::insert(var.clone(), crate::hydra::rewriting::f_type_to_type_scheme(dom.clone()), g.clone().0.bound_types.clone()), lam.clone().0.domain.clone()),
    class_constraints: g.clone().0.class_constraints.clone(),
    lambda_variables: crate::hydra::lib::sets::insert(var.clone(), g.clone().0.lambda_variables.clone()),
    metadata: crate::hydra::lib::maps::delete(var.clone(), g.clone().0.metadata.clone()),
    primitives: g.clone().0.primitives.clone(),
    schema_types: g.clone().0.schema_types.clone(),
    type_variables: g.clone().0.type_variables.clone()}))}

pub fn extend_graph_for_let(for_binding: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Binding) -> Option<crate::hydra::core::Term>> + Clone, g: crate::hydra::graph::Graph, letrec: crate::hydra::core::Let) -> crate::hydra::graph::Graph {
  let bindings = letrec.clone().0.bindings.clone() ;
  let g2 = crate::hydra::lexical::extend_graph_with_bindings(bindings.clone(), g.clone()) ;
  crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
    bound_terms: crate::hydra::lib::maps::union_(crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|b: crate::hydra::core::Binding| (b.clone().0.name.clone(), b.clone().0.term.clone()), bindings.clone())), g.clone().0.bound_terms.clone()),
    bound_types: crate::hydra::lib::maps::union_(crate::hydra::lib::maps::from_list(crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(|b: crate::hydra::core::Binding| crate::hydra::lib::maybes::map(|ts: crate::hydra::core::TypeScheme| (b.clone().0.name.clone(), ts.clone()), b.clone().0.type_.clone()), bindings.clone()))), g.clone().0.bound_types.clone()),
    class_constraints: g.clone().0.class_constraints.clone(),
    lambda_variables: crate::hydra::lib::lists::foldl(|s: BTreeSet<crate::hydra::core::Name>, b: crate::hydra::core::Binding| crate::hydra::lib::sets::delete(b.clone().0.name.clone(), s.clone()), g.clone().0.lambda_variables.clone(), bindings.clone()),
    metadata: crate::hydra::lib::lists::foldl(|g_acc: crate::hydra::graph::Graph, b: crate::hydra::core::Binding| {
      let m = g_acc.clone().0.metadata.clone() ;
      {
        let new_meta = crate::hydra::lib::maybes::maybe(crate::hydra::lib::maps::delete(b.clone().0.name.clone(), m.clone()), |t: crate::hydra::core::Term| crate::hydra::lib::maps::insert(b.clone().0.name.clone(), t.clone(), m.clone()), for_binding.clone()(g_acc.clone(), b.clone())) ;
        crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
          bound_terms: g_acc.clone().0.bound_terms.clone(),
          bound_types: g_acc.clone().0.bound_types.clone(),
          class_constraints: g_acc.clone().0.class_constraints.clone(),
          lambda_variables: g_acc.clone().0.lambda_variables.clone(),
          metadata: new_meta.clone(),
          primitives: g_acc.clone().0.primitives.clone(),
          schema_types: g_acc.clone().0.schema_types.clone(),
          type_variables: g_acc.clone().0.type_variables.clone()}))}}, g2.clone(), bindings.clone()).0.metadata.clone(),
    primitives: g.clone().0.primitives.clone(),
    schema_types: g.clone().0.schema_types.clone(),
    type_variables: g.clone().0.type_variables.clone()}))}

pub fn extend_graph_for_type_lambda(g: crate::hydra::graph::Graph, tlam: crate::hydra::core::TypeLambda) -> crate::hydra::graph::Graph {
  let name = tlam.clone().0.parameter.clone() ;
  crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
    bound_terms: g.clone().0.bound_terms.clone(),
    bound_types: g.clone().0.bound_types.clone(),
    class_constraints: g.clone().0.class_constraints.clone(),
    lambda_variables: g.clone().0.lambda_variables.clone(),
    metadata: g.clone().0.metadata.clone(),
    primitives: g.clone().0.primitives.clone(),
    schema_types: g.clone().0.schema_types.clone(),
    type_variables: crate::hydra::lib::sets::insert(name.clone(), g.clone().0.type_variables.clone())}))}

pub fn field_map(fields: Vec<crate::hydra::core::Field>) -> BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term> {
  let to_pair = |f: crate::hydra::core::Field| (f.clone().0.name.clone(), f.clone().0.term.clone()) ;
  crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(to_pair.clone(), fields.clone()))}

pub fn field_type_map(fields: Vec<crate::hydra::core::FieldType>) -> BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type> {
  let to_pair = |f: crate::hydra::core::FieldType| (f.clone().0.name.clone(), f.clone().0.type_.clone()) ;
  crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(to_pair.clone(), fields.clone()))}

pub fn field_types(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, t: crate::hydra::core::Type) -> Either<crate::hydra::context::InContext, BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>> {
  let to_map = |fields: Vec<crate::hydra::core::FieldType>| crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|ft: crate::hydra::core::FieldType| (ft.clone().0.name.clone(), ft.clone().0.type_.clone()), fields.clone())) ;
  match &*crate::hydra::rewriting::deannotate_type(t.clone()).0 {
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      field_types(cx.clone(), graph.clone(), v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      Right(to_map.clone()(v0_.clone()))},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      Right(to_map.clone()(v0_.clone()))},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(crate::hydra::lexical::require_element(cx.clone(), graph.clone(), v0_.clone()), |el: crate::hydra::core::Binding| crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|_wc_e: crate::hydra::error::Error| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
        object: _wc_e.clone(),
        context: cx.clone()})), |_wc_a: crate::hydra::core::Type| _wc_a.clone(), crate::hydra::lib::eithers::bimap(|_e: crate::hydra::error::DecodingError| crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(_e.clone().0.0.clone())))))), |_a: crate::hydra::core::Type| _a.clone(), crate::hydra::decode::core::type_(graph.clone(), el.clone().0.term.clone()))), |decoded_type: crate::hydra::core::Type| field_types(cx.clone(), graph.clone(), decoded_type.clone())))},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat(Vec::from([
        String::from("expected record or union type but found "),
        crate::hydra::show::core::type_(t.clone())])))))))),
      context: cx.clone()})))}}

pub fn find_field_type(cx: crate::hydra::context::Context, fname: crate::hydra::core::Name, fields: Vec<crate::hydra::core::FieldType>) -> Either<crate::hydra::context::InContext, crate::hydra::core::Type> {
  let matching_fields = crate::hydra::lib::lists::filter(|ft: crate::hydra::core::FieldType| crate::hydra::lib::equality::equal(ft.clone().0.name.clone().0.0.clone(), fname.clone().0.0.clone()), fields.clone()) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(matching_fields.clone()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(String::from("No such field: "), fname.clone().0.0.clone()))))))),
    context: cx.clone()}))), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(crate::hydra::lib::lists::length(matching_fields.clone()), 1i32), Right(crate::hydra::lib::lists::head(matching_fields.clone()).0.type_.clone()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(String::from("Multiple fields named "), fname.clone().0.0.clone()))))))),
    context: cx.clone()})))))}

pub fn fresh_name(cx: crate::hydra::context::Context) -> (crate::hydra::core::Name, crate::hydra::context::Context) {
  let count = crate::hydra::annotations::get_count(crate::hydra::constants::key_fresh_type_variable_count, cx.clone()) ;
  (normal_type_variable(count.clone()), crate::hydra::annotations::put_count(crate::hydra::constants::key_fresh_type_variable_count, crate::hydra::lib::math::add(count.clone(), 1i32), cx.clone()))}

pub fn fresh_names(n: i32, cx: crate::hydra::context::Context) -> (Vec<crate::hydra::core::Name>, crate::hydra::context::Context) {
  let go = |acc: (Vec<crate::hydra::core::Name>, crate::hydra::context::Context), _: T0| {
    let names = crate::hydra::lib::pairs::first(acc.clone()) ;
    {
      let cx0 = crate::hydra::lib::pairs::second(acc.clone()) ;
      {
        let result = fresh_name(cx0.clone()) ;
        {
          let name = crate::hydra::lib::pairs::first(result.clone()) ;
          {
            let cx1 = crate::hydra::lib::pairs::second(result.clone()) ;
            (crate::hydra::lib::lists::concat2(names.clone(), crate::hydra::lib::lists::pure(name.clone())), cx1.clone())}}}}} ;
  crate::hydra::lib::lists::foldl(go.clone(), (Vec::from([]), cx.clone()), crate::hydra::lib::lists::replicate(n.clone(), ()))}

pub fn f_type_is_polymorphic(typ: crate::hydra::core::Type) -> bool {
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      f_type_is_polymorphic(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    _ => false}}

pub fn fully_strip_and_normalize_type(typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  let go = |depth: i32, subst: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Name>, t: crate::hydra::core::Type| match &*crate::hydra::rewriting::deannotate_type(t.clone()).0 {
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let old_var = v0_.clone().0.parameter.clone() ;
        {
          let new_var = crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat2(String::from("_"), crate::hydra::lib::literals::show_int32(depth.clone()))))) ;
          go.clone()(crate::hydra::lib::math::add(depth.clone(), 1i32), crate::hydra::lib::maps::insert(old_var.clone(), new_var.clone(), subst.clone()), v0_.clone().0.body.clone())}}},
    _ => (subst.clone(), t.clone())} ;
  let result = go.clone()(0i32, crate::hydra::lib::maps::empty, typ.clone()) ;
  let subst = crate::hydra::lib::pairs::first(result.clone()) ;
  let body = crate::hydra::lib::pairs::second(result.clone()) ;
  crate::hydra::rewriting::substitute_type_variables(subst.clone(), body.clone())}

pub fn fully_strip_type(typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  match &*crate::hydra::rewriting::deannotate_type(typ.clone()).0 {
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      fully_strip_type(v0_.clone().0.body.clone())},
    _ => typ.clone()}}

pub fn graph_as_let(bindings: Vec<crate::hydra::core::Binding>, body: crate::hydra::core::Term) -> crate::hydra::core::Let {
  crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
    bindings: bindings.clone(),
    body: body.clone()}))}

pub fn graph_as_term(bindings: Vec<crate::hydra::core::Binding>, body: crate::hydra::core::Term) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(graph_as_let(bindings.clone(), body.clone()))))}

pub fn graph_as_types(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, els: Vec<crate::hydra::core::Binding>) -> Either<crate::hydra::context::InContext, BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>> {
  let to_pair = |el: crate::hydra::core::Binding| crate::hydra::lib::eithers::map(|typ: crate::hydra::core::Type| (el.clone().0.name.clone(), typ.clone()), crate::hydra::lib::eithers::bimap(|_wc_e: crate::hydra::error::DecodingError| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: _wc_e.clone(),
    context: cx.clone()})), |_wc_a: crate::hydra::core::Type| _wc_a.clone(), crate::hydra::decode::core::type_(graph.clone(), el.clone().0.term.clone()))) ;
  crate::hydra::lib::eithers::map(crate::hydra::lib::maps::from_list, crate::hydra::lib::eithers::map_list(to_pair.clone(), els.clone()))}

pub fn instantiate_type(cx: crate::hydra::context::Context, typ: crate::hydra::core::Type) -> (crate::hydra::core::Type, crate::hydra::context::Context) {
  let result = instantiate_type_scheme(cx.clone(), type_to_type_scheme(typ.clone())) ;
  (crate::hydra::rewriting::type_scheme_to_f_type(crate::hydra::lib::pairs::first(result.clone())), crate::hydra::lib::pairs::second(result.clone()))}

pub fn instantiate_type_scheme(cx: crate::hydra::context::Context, scheme: crate::hydra::core::TypeScheme) -> (crate::hydra::core::TypeScheme, crate::hydra::context::Context) {
  let old_vars = scheme.clone().0.variables.clone() ;
  let result = fresh_names(crate::hydra::lib::lists::length(old_vars.clone()), cx.clone()) ;
  let new_vars = crate::hydra::lib::pairs::first(result.clone()) ;
  let cx2 = crate::hydra::lib::pairs::second(result.clone()) ;
  let subst = crate::hydra::typing::TypeSubst(Rc::new(crate::hydra::typing::TypeSubst_Variant(crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::zip(old_vars.clone(), crate::hydra::lib::lists::map(|x: crate::hydra::core::Name| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(x.clone()))), new_vars.clone())))))) ;
  let name_subst = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::zip(old_vars.clone(), new_vars.clone())) ;
  let renamed_constraints = crate::hydra::lib::maybes::map(|old_constraints: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>| crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|kv: (crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata)| (crate::hydra::lib::maybes::from_maybe(crate::hydra::lib::pairs::first(kv.clone()), crate::hydra::lib::maps::lookup(crate::hydra::lib::pairs::first(kv.clone()), name_subst.clone())), crate::hydra::lib::pairs::second(kv.clone())), crate::hydra::lib::maps::to_list(old_constraints.clone()))), scheme.clone().0.constraints.clone()) ;
  (crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
    variables: new_vars.clone(),
    type_: crate::hydra::substitution::subst_in_type(subst.clone(), scheme.clone().0.type_.clone()),
    constraints: renamed_constraints.clone()})), cx2.clone())}

pub fn is_encoded_term(t: crate::hydra::core::Term) -> bool {
  match &*crate::hydra::rewriting::deannotate_term(t.clone()).0 {
    crate::hydra::core::Term_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      is_encoded_term(v0_.clone().0.function.clone())},
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::equality::equal(String::from("hydra.core.Term"), v0_.clone().0.type_name.clone().0.0.clone())},
    _ => false}}

pub fn is_encoded_type(t: crate::hydra::core::Term) -> bool {
  match &*crate::hydra::rewriting::deannotate_term(t.clone()).0 {
    crate::hydra::core::Term_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      is_encoded_type(v0_.clone().0.function.clone())},
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::equality::equal(String::from("hydra.core.Type"), v0_.clone().0.type_name.clone().0.0.clone())},
    _ => false}}

pub fn is_enum_row_type(rt: Vec<crate::hydra::core::FieldType>) -> bool {
  crate::hydra::lib::lists::foldl(crate::hydra::lib::logic::and, true, crate::hydra::lib::lists::map(|f: crate::hydra::core::FieldType| is_unit_type(crate::hydra::rewriting::deannotate_type(f.clone().0.type_.clone())), rt.clone()))}

pub fn is_enum_type(typ: crate::hydra::core::Type) -> bool {
  match &*crate::hydra::rewriting::deannotate_type(typ.clone()).0 {
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      is_enum_row_type(v0_.clone())},
    _ => false}}

pub fn is_serializable(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, el: crate::hydra::core::Binding) -> Either<crate::hydra::context::InContext, bool> {
  let variants = |typ: crate::hydra::core::Type| crate::hydra::lib::lists::map(crate::hydra::reflect::type_variant, crate::hydra::rewriting::fold_over_type(crate::hydra::coders::TraversalOrder(Rc::new(crate::hydra::coders::TraversalOrder_Variant::Pre)), |m: Vec<crate::hydra::core::Type>, t: crate::hydra::core::Type| crate::hydra::lib::lists::cons(t.clone(), m.clone()), Vec::from([]), typ.clone())) ;
  crate::hydra::lib::eithers::map(|deps: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>| {
    let all_variants = crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(variants.clone(), crate::hydra::lib::maps::elems(deps.clone())))) ;
    crate::hydra::lib::logic::not(crate::hydra::lib::sets::member(crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Function)), all_variants.clone()))}, type_dependencies(cx.clone(), graph.clone(), false, crate::hydra::lib::equality::identity, el.clone().0.name.clone()))}

pub fn is_serializable_type(typ: crate::hydra::core::Type) -> bool {
  let all_variants = crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::map(crate::hydra::reflect::type_variant, crate::hydra::rewriting::fold_over_type(crate::hydra::coders::TraversalOrder(Rc::new(crate::hydra::coders::TraversalOrder_Variant::Pre)), |m: Vec<crate::hydra::core::Type>, t: crate::hydra::core::Type| crate::hydra::lib::lists::cons(t.clone(), m.clone()), Vec::from([]), typ.clone()))) ;
  crate::hydra::lib::logic::not(crate::hydra::lib::sets::member(crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Function)), all_variants.clone()))}

pub fn is_serializable_by_name(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, bool> {
  let variants = |typ: crate::hydra::core::Type| crate::hydra::lib::lists::map(crate::hydra::reflect::type_variant, crate::hydra::rewriting::fold_over_type(crate::hydra::coders::TraversalOrder(Rc::new(crate::hydra::coders::TraversalOrder_Variant::Pre)), |m: Vec<crate::hydra::core::Type>, t: crate::hydra::core::Type| crate::hydra::lib::lists::cons(t.clone(), m.clone()), Vec::from([]), typ.clone())) ;
  crate::hydra::lib::eithers::map(|deps: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>| {
    let all_variants = crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(variants.clone(), crate::hydra::lib::maps::elems(deps.clone())))) ;
    crate::hydra::lib::logic::not(crate::hydra::lib::sets::member(crate::hydra::variants::TypeVariant(Rc::new(crate::hydra::variants::TypeVariant_Variant::Function)), all_variants.clone()))}, type_dependencies(cx.clone(), graph.clone(), false, crate::hydra::lib::equality::identity, name.clone()))}

pub fn is_type(t: crate::hydra::core::Type) -> bool {
  match &*crate::hydra::rewriting::deannotate_type(t.clone()).0 {
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      is_type(v0_.clone().0.function.clone())},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      is_type(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      false},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::equality::equal(v0_.clone(), crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type")))))},
    _ => false}}

pub fn is_unit_term(v1: crate::hydra::core::Term) -> bool {
  match &*v1.clone().0 {
    crate::hydra::core::Term_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    _ => false}}

pub fn is_unit_type(v1: crate::hydra::core::Type) -> bool {
  match &*v1.clone().0 {
    crate::hydra::core::Type_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    _ => false}}

pub fn module_contains_binary_literals(mod_: crate::hydra::module::Module) -> bool {
  let check_term = |found: bool, term: crate::hydra::core::Term| crate::hydra::lib::logic::or(found.clone(), match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Literal_Variant::Binary (v0_) => {
          let v0_ = v0_.clone() ;
          true},
        _ => false}},
    _ => false}) ;
  let term_contains_binary = |term: crate::hydra::core::Term| crate::hydra::rewriting::fold_over_term(crate::hydra::coders::TraversalOrder(Rc::new(crate::hydra::coders::TraversalOrder_Variant::Pre)), check_term.clone(), false, term.clone()) ;
  crate::hydra::lib::lists::foldl(|acc: bool, el: crate::hydra::core::Binding| crate::hydra::lib::logic::or(acc.clone(), term_contains_binary.clone()(el.clone().0.term.clone())), false, mod_.clone().0.elements.clone())}

pub fn module_dependency_namespaces(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, binds: bool, with_prims: bool, with_noms: bool, with_schema: bool, mod_: crate::hydra::module::Module) -> Either<crate::hydra::context::InContext, BTreeSet<crate::hydra::module::Namespace>> {
  crate::hydra::lib::eithers::map(|deps: BTreeSet<crate::hydra::module::Namespace>| crate::hydra::lib::sets::delete(mod_.clone().0.namespace.clone(), deps.clone()), dependency_namespaces(cx.clone(), graph.clone(), binds.clone(), with_prims.clone(), with_noms.clone(), with_schema.clone(), mod_.clone().0.elements.clone()))}

pub fn namespaces_for_definitions(encode_namespace: impl Fn(crate::hydra::module::Namespace) -> T0 + Clone, focus_ns: crate::hydra::module::Namespace, defs: Vec<crate::hydra::module::Definition>) -> crate::hydra::module::Namespaces {
  let nss = crate::hydra::lib::sets::delete(focus_ns.clone(), definition_dependency_namespaces(defs.clone())) ;
  let to_pair = |ns: crate::hydra::module::Namespace| (ns.clone(), encode_namespace.clone()(ns.clone())) ;
  crate::hydra::module::Namespaces(Rc::new(crate::hydra::module::Namespaces_Variant {
    focus: to_pair.clone()(focus_ns.clone()),
    mapping: crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(to_pair.clone(), crate::hydra::lib::sets::to_list(nss.clone())))}))}

pub fn nominal_application(tname: crate::hydra::core::Name, args: Vec<crate::hydra::core::Type>) -> crate::hydra::core::Type {
  crate::hydra::lib::lists::foldl(|t: crate::hydra::core::Type, a: crate::hydra::core::Type| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
    function: t.clone(),
    argument: a.clone()}))))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(tname.clone()))), args.clone())}

pub fn normal_type_variable(i: i32) -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat2(String::from("t"), crate::hydra::lib::literals::show_int32(i.clone())))))}

pub fn partition_definitions(defs: Vec<crate::hydra::module::Definition>) -> (Vec<crate::hydra::module::TypeDefinition>, Vec<crate::hydra::module::TermDefinition>) {
  let get_type = |def: crate::hydra::module::Definition| match &*def.clone().0 {
    crate::hydra::module::Definition_Variant::Type (v0_) => {
      let v0_ = v0_.clone() ;
      Some(v0_.clone())},
    crate::hydra::module::Definition_Variant::Term (v0_) => {
      let v0_ = v0_.clone() ;
      None}} ;
  let get_term = |def: crate::hydra::module::Definition| match &*def.clone().0 {
    crate::hydra::module::Definition_Variant::Type (v0_) => {
      let v0_ = v0_.clone() ;
      None},
    crate::hydra::module::Definition_Variant::Term (v0_) => {
      let v0_ = v0_.clone() ;
      Some(v0_.clone())}} ;
  (crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(get_type.clone(), defs.clone())), crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(get_term.clone(), defs.clone())))}

pub fn require_record_type(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, Vec<crate::hydra::core::FieldType>> {
  let to_record = |t: crate::hydra::core::Type| match &*t.clone().0 {
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      Some(v0_.clone())},
    _ => None} ;
  require_row_type(cx.clone(), String::from("record type"), to_record.clone(), graph.clone(), name.clone())}

pub fn require_row_type(cx: crate::hydra::context::Context, label: String, getter: impl Fn(crate::hydra::core::Type) -> Option<T0> + Clone, graph: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, T0> {
  let raw_type = |t: crate::hydra::core::Type| match &*t.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      raw_type.clone()(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      raw_type.clone()(v0_.clone().0.body.clone())},
    _ => t.clone()} ;
  crate::hydra::lib::eithers::bind(require_type(cx.clone(), graph.clone(), name.clone()), |t: crate::hydra::core::Type| crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat(Vec::from([
      name.clone().0.0.clone(),
      String::from(" does not resolve to a "),
      label.clone(),
      String::from(" type: "),
      crate::hydra::show::core::type_(t.clone())])))))))),
    context: cx.clone()}))), |x: T0| Right(x.clone()), getter.clone()(raw_type.clone()(t.clone()))))}

pub fn require_schema_type(cx: crate::hydra::context::Context, types: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeScheme>, tname: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, (crate::hydra::core::TypeScheme, crate::hydra::context::Context)> {
  crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat(Vec::from([
      String::from("No such schema type: "),
      tname.clone().0.0.clone(),
      String::from(". Available types are: "),
      crate::hydra::lib::strings::intercalate(String::from(", "), crate::hydra::lib::lists::map(|v| v.0.0.clone(), crate::hydra::lib::maps::keys(types.clone())))])))))))),
    context: cx.clone()}))), |ts: crate::hydra::core::TypeScheme| Right(instantiate_type_scheme(cx.clone(), crate::hydra::rewriting::deannotate_type_scheme_recursive(ts.clone()))), crate::hydra::lib::maps::lookup(tname.clone(), types.clone()))}

pub fn require_type(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, crate::hydra::core::Type> {
  crate::hydra::lib::maybes::maybe(crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(String::from("no such type: "), name.clone().0.0.clone()))))))),
    context: cx.clone()}))), |ts: crate::hydra::core::TypeScheme| Right(crate::hydra::rewriting::type_scheme_to_f_type(ts.clone())), crate::hydra::lib::maps::lookup(name.clone(), graph.clone().0.bound_types.clone())), |ts: crate::hydra::core::TypeScheme| Right(crate::hydra::rewriting::type_scheme_to_f_type(ts.clone())), crate::hydra::lib::maps::lookup(name.clone(), graph.clone().0.schema_types.clone()))}

pub fn require_union_field(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, tname: crate::hydra::core::Name, fname: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, crate::hydra::core::Type> {
  let with_row_type = |rt: Vec<crate::hydra::core::FieldType>| {
    let matches = crate::hydra::lib::lists::filter(|ft: crate::hydra::core::FieldType| crate::hydra::lib::equality::equal(ft.clone().0.name.clone(), fname.clone()), rt.clone()) ;
    crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(matches.clone()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat(Vec::from([
        String::from("no field \""),
        fname.clone().0.0.clone(),
        String::from("\" in union type \""),
        tname.clone().0.0.clone()])))))))),
      context: cx.clone()}))), Right(crate::hydra::lib::lists::head(matches.clone()).0.type_.clone()))} ;
  crate::hydra::lib::eithers::bind(require_union_type(cx.clone(), graph.clone(), tname.clone()), with_row_type.clone())}

pub fn require_union_type(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, Vec<crate::hydra::core::FieldType>> {
  let to_union = |t: crate::hydra::core::Type| match &*t.clone().0 {
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      Some(v0_.clone())},
    _ => None} ;
  require_row_type(cx.clone(), String::from("union"), to_union.clone(), graph.clone(), name.clone())}

pub fn resolve_type(graph: crate::hydra::graph::Graph, typ: crate::hydra::core::Type) -> Option<crate::hydra::core::Type> {
  match &*crate::hydra::rewriting::deannotate_type(typ.clone()).0 {
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::maybes::maybe(crate::hydra::lib::maybes::map(|ts: crate::hydra::core::TypeScheme| crate::hydra::rewriting::type_scheme_to_f_type(ts.clone()), crate::hydra::lib::maps::lookup(v0_.clone(), graph.clone().0.bound_types.clone())), |ts: crate::hydra::core::TypeScheme| Some(crate::hydra::rewriting::type_scheme_to_f_type(ts.clone())), crate::hydra::lib::maps::lookup(v0_.clone(), graph.clone().0.schema_types.clone()))},
    _ => Some(typ.clone())}}

pub fn schema_graph_to_typing_environment(cx: crate::hydra::context::Context, g: crate::hydra::graph::Graph) -> Either<crate::hydra::context::InContext, BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeScheme>> {
  let to_type_scheme = |vars: Vec<crate::hydra::core::Name>, typ: crate::hydra::core::Type| match &*crate::hydra::rewriting::deannotate_type(typ.clone()).0 {
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      to_type_scheme.clone()(crate::hydra::lib::lists::cons(v0_.clone().0.parameter.clone(), vars.clone()), v0_.clone().0.body.clone())},
    _ => crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
      variables: crate::hydra::lib::lists::reverse(vars.clone()),
      type_: typ.clone(),
      constraints: None}))} ;
  let decode_type = |term: crate::hydra::core::Term| crate::hydra::lib::eithers::bimap(|_wc_e: crate::hydra::error::Error| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: _wc_e.clone(),
    context: cx.clone()})), |_wc_a: crate::hydra::core::Type| _wc_a.clone(), crate::hydra::lib::eithers::bimap(|_e: crate::hydra::error::DecodingError| crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(_e.clone().0.0.clone())))))), |_a: crate::hydra::core::Type| _a.clone(), crate::hydra::decode::core::type_(g.clone(), term.clone()))) ;
  let decode_type_scheme = |term: crate::hydra::core::Term| crate::hydra::lib::eithers::bimap(|_wc_e: crate::hydra::error::Error| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: _wc_e.clone(),
    context: cx.clone()})), |_wc_a: crate::hydra::core::TypeScheme| _wc_a.clone(), crate::hydra::lib::eithers::bimap(|_e: crate::hydra::error::DecodingError| crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(_e.clone().0.0.clone())))))), |_a: crate::hydra::core::TypeScheme| _a.clone(), crate::hydra::decode::core::type_scheme(g.clone(), term.clone()))) ;
  let to_pair = |el: crate::hydra::core::Binding| {
    let for_term = |term: crate::hydra::core::Term| match &*term.clone().0 {
      crate::hydra::core::Term_Variant::Record (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(v0_.clone().0.type_name.clone(), crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.TypeScheme"))))), crate::hydra::lib::eithers::map(crate::hydra::lib::maybes::pure, decode_type_scheme.clone()(el.clone().0.term.clone())), Right(None))},
      crate::hydra::core::Term_Variant::Union (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(v0_.clone().0.type_name.clone(), crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))), crate::hydra::lib::eithers::map(|decoded: crate::hydra::core::Type| Some(to_type_scheme.clone()(Vec::from([]), decoded.clone())), decode_type.clone()(el.clone().0.term.clone())), Right(None))},
      _ => Right(None)} ;
    crate::hydra::lib::eithers::bind(crate::hydra::lib::maybes::maybe(crate::hydra::lib::eithers::map(|typ: crate::hydra::core::Type| Some(crate::hydra::rewriting::f_type_to_type_scheme(typ.clone())), decode_type.clone()(el.clone().0.term.clone())), |ts: crate::hydra::core::TypeScheme| crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(ts.clone(), crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
      variables: Vec::from([]),
      type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.TypeScheme"))))))),
      constraints: None}))), crate::hydra::lib::eithers::map(crate::hydra::lib::maybes::pure, decode_type_scheme.clone()(el.clone().0.term.clone())), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(ts.clone(), crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
      variables: Vec::from([]),
      type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
      constraints: None}))), crate::hydra::lib::eithers::map(|decoded: crate::hydra::core::Type| Some(to_type_scheme.clone()(Vec::from([]), decoded.clone())), decode_type.clone()(el.clone().0.term.clone())), for_term.clone()(crate::hydra::rewriting::deannotate_term(el.clone().0.term.clone())))), el.clone().0.type_.clone()), |mts: Option<crate::hydra::core::TypeScheme>| Right(crate::hydra::lib::maybes::map(|ts: crate::hydra::core::TypeScheme| (el.clone().0.name.clone(), ts.clone()), mts.clone())))} ;
  crate::hydra::lib::eithers::map(|mpairs: Vec<Option<(crate::hydra::core::Name, crate::hydra::core::TypeScheme)>>| crate::hydra::lib::maps::from_list(crate::hydra::lib::maybes::cat(mpairs.clone())), crate::hydra::lib::eithers::map_list(to_pair.clone(), crate::hydra::lexical::graph_to_bindings(g.clone())))}

pub fn term_as_bindings(term: crate::hydra::core::Term) -> Vec<crate::hydra::core::Binding> {
  match &*crate::hydra::rewriting::deannotate_term(term.clone()).0 {
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      v0_.clone().0.bindings.clone()},
    _ => Vec::from([])}}

pub fn topological_sort_type_definitions(defs: Vec<crate::hydra::module::TypeDefinition>) -> Vec<Vec<crate::hydra::module::TypeDefinition>> {
  let to_pair = |def: crate::hydra::module::TypeDefinition| (def.clone().0.name.clone(), crate::hydra::lib::sets::to_list(crate::hydra::rewriting::type_dependency_names(false, def.clone().0.type_.clone()))) ;
  let name_to_def = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|d: crate::hydra::module::TypeDefinition| (d.clone().0.name.clone(), d.clone()), defs.clone())) ;
  let sorted = crate::hydra::sorting::topological_sort_components(crate::hydra::lib::lists::map(to_pair.clone(), defs.clone())) ;
  crate::hydra::lib::lists::map(|names: Vec<crate::hydra::core::Name>| crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(|n: crate::hydra::core::Name| crate::hydra::lib::maps::lookup(n.clone(), name_to_def.clone()), names.clone())), sorted.clone())}

pub fn type_dependencies(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, with_schema: bool, transform: impl Fn(crate::hydra::core::Type) -> crate::hydra::core::Type + Clone, name: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>> {
  let require_type = |name2: crate::hydra::core::Name| {
    let cx1 = crate::hydra::context::Context(Rc::new(crate::hydra::context::Context_Variant {
      trace: crate::hydra::lib::lists::cons(crate::hydra::lib::strings::cat2(String::from("type dependencies of "), name2.clone().0.0.clone()), cx.clone().0.trace.clone()),
      messages: cx.clone().0.messages.clone(),
      other: cx.clone().0.other.clone()})) ;
    crate::hydra::lib::eithers::bind(crate::hydra::lexical::require_element(cx1.clone(), graph.clone(), name2.clone()), |el: crate::hydra::core::Binding| crate::hydra::lib::eithers::bimap(|_wc_e: crate::hydra::error::Error| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: _wc_e.clone(),
      context: cx1.clone()})), |_wc_a: crate::hydra::core::Type| _wc_a.clone(), crate::hydra::lib::eithers::bimap(|_e: crate::hydra::error::DecodingError| crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(_e.clone().0.0.clone())))))), |_a: crate::hydra::core::Type| _a.clone(), crate::hydra::decode::core::type_(graph.clone(), el.clone().0.term.clone()))))} ;
  let to_pair = |name2: crate::hydra::core::Name| crate::hydra::lib::eithers::map(|typ: crate::hydra::core::Type| (name2.clone(), transform.clone()(typ.clone())), require_type.clone()(name2.clone())) ;
  let deps = |seeds: BTreeSet<crate::hydra::core::Name>, names: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>| crate::hydra::lib::logic::if_else(crate::hydra::lib::sets::null(seeds.clone()), Right(names.clone()), crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(to_pair.clone(), crate::hydra::lib::sets::to_list(seeds.clone())), |pairs: Vec<(crate::hydra::core::Name, crate::hydra::core::Type)>| {
    let new_names = crate::hydra::lib::maps::union_(names.clone(), crate::hydra::lib::maps::from_list(pairs.clone())) ;
    {
      let refs = crate::hydra::lib::lists::foldl(crate::hydra::lib::sets::union_, crate::hydra::lib::sets::empty, crate::hydra::lib::lists::map(|pair: (crate::hydra::core::Name, crate::hydra::core::Type)| crate::hydra::rewriting::type_dependency_names(with_schema.clone(), crate::hydra::lib::pairs::second(pair.clone())), pairs.clone())) ;
      {
        let visited = crate::hydra::lib::sets::from_list(crate::hydra::lib::maps::keys(names.clone())) ;
        {
          let new_seeds = crate::hydra::lib::sets::difference(refs.clone(), visited.clone()) ;
          deps.clone()(new_seeds.clone(), new_names.clone())}}}})) ;
  deps.clone()(crate::hydra::lib::sets::singleton(name.clone()), crate::hydra::lib::maps::empty)}

pub fn type_to_type_scheme(t0: crate::hydra::core::Type) -> crate::hydra::core::TypeScheme {
  let helper = |vars: Vec<crate::hydra::core::Name>, t: crate::hydra::core::Type| match &*crate::hydra::rewriting::deannotate_type(t.clone()).0 {
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      helper.clone()(crate::hydra::lib::lists::cons(v0_.clone().0.parameter.clone(), vars.clone()), v0_.clone().0.body.clone())},
    _ => crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
      variables: crate::hydra::lib::lists::reverse(vars.clone()),
      type_: t.clone(),
      constraints: None}))} ;
  helper.clone()(Vec::from([]), t0.clone())}

pub fn types_to_elements(type_map: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>) -> Vec<crate::hydra::core::Binding> {
  let to_element = |pair: (crate::hydra::core::Name, crate::hydra::core::Type)| {
    let name = crate::hydra::lib::pairs::first(pair.clone()) ;
    crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
      name: name.clone(),
      term: crate::hydra::encode::core::type_(crate::hydra::lib::pairs::second(pair.clone())),
      type_: None}))} ;
  crate::hydra::lib::lists::map(to_element.clone(), crate::hydra::lib::maps::to_list(type_map.clone()))}

pub fn with_lambda_context(get_context: impl Fn(T0) -> crate::hydra::graph::Graph + Clone, set_context: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(T0) -> T1> + Clone, env: T0, lam: crate::hydra::core::Lambda, body: impl Fn(T1) -> T2 + Clone) -> T2 {
  let new_context = extend_graph_for_lambda(get_context.clone()(env.clone()), lam.clone()) ;
  body.clone()(set_context.clone()(new_context.clone(), env.clone()))}

pub fn with_let_context(get_context: impl Fn(T0) -> crate::hydra::graph::Graph + Clone, set_context: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(T0) -> T1> + Clone, for_binding: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Binding) -> Option<crate::hydra::core::Term>> + Clone, env: T0, letrec: crate::hydra::core::Let, body: impl Fn(T1) -> T2 + Clone) -> T2 {
  let new_context = extend_graph_for_let(for_binding.clone(), get_context.clone()(env.clone()), letrec.clone()) ;
  body.clone()(set_context.clone()(new_context.clone(), env.clone()))}

pub fn with_type_lambda_context(get_context: impl Fn(T0) -> crate::hydra::graph::Graph + Clone, set_context: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(T0) -> T1> + Clone, env: T0, tlam: crate::hydra::core::TypeLambda, body: impl Fn(T1) -> T2 + Clone) -> T2 {
  let new_context = extend_graph_for_type_lambda(get_context.clone()(env.clone()), tlam.clone()) ;
  body.clone()(set_context.clone()(new_context.clone(), env.clone()))}
