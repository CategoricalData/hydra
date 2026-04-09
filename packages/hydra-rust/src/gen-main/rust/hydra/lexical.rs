#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::rewriting::*;
use crate::hydra::show::core::*;
use crate::hydra::show::error::*;
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

pub fn build_graph(elements: Vec<crate::hydra::core::Binding>, environment: BTreeMap<crate::hydra::core::Name, Option<crate::hydra::core::Term>>, primitives: BTreeMap<crate::hydra::core::Name, crate::hydra::graph::Primitive>) -> crate::hydra::graph::Graph {
  let element_terms = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|b: crate::hydra::core::Binding| (b.clone().0.name.clone(), b.clone().0.term.clone()), elements.clone())) ;
  let let_terms = crate::hydra::lib::maps::map(|mt: Option<crate::hydra::core::Term>| crate::hydra::lib::maybes::from_just(mt.clone()), crate::hydra::lib::maps::filter(|mt: Option<crate::hydra::core::Term>| crate::hydra::lib::maybes::is_just(mt.clone()), environment.clone())) ;
  let element_types = crate::hydra::lib::maps::from_list(crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(|b: crate::hydra::core::Binding| crate::hydra::lib::maybes::map(|ts: crate::hydra::core::TypeScheme| (b.clone().0.name.clone(), ts.clone()), b.clone().0.type_.clone()), elements.clone()))) ;
  crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
    bound_terms: crate::hydra::lib::maps::union_(element_terms.clone(), let_terms.clone()),
    bound_types: element_types.clone(),
    class_constraints: crate::hydra::lib::maps::empty,
    lambda_variables: crate::hydra::lib::sets::from_list(crate::hydra::lib::maps::keys(crate::hydra::lib::maps::filter(|mt: Option<crate::hydra::core::Term>| crate::hydra::lib::maybes::is_nothing(mt.clone()), environment.clone()))),
    metadata: crate::hydra::lib::maps::empty,
    primitives: primitives.clone(),
    schema_types: crate::hydra::lib::maps::empty,
    type_variables: crate::hydra::lib::sets::empty}))}

pub fn choose_unique_name(reserved: BTreeSet<crate::hydra::core::Name>, name: crate::hydra::core::Name) -> crate::hydra::core::Name {
  let try_name = |index: i32| {
    let candidate = crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(index.clone(), 1i32), name.clone(), crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat2(name.clone().0.0.clone(), crate::hydra::lib::literals::show_int32(index.clone())))))) ;
    crate::hydra::lib::logic::if_else(crate::hydra::lib::sets::member(candidate.clone(), reserved.clone()), try_name.clone()(crate::hydra::lib::math::add(index.clone(), 1i32)), candidate.clone())} ;
  try_name.clone()(1i32)}

pub fn dereference_element(graph: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Option<crate::hydra::core::Binding> {
  lookup_element(graph.clone(), name.clone())}

pub fn dereference_schema_type(name: crate::hydra::core::Name, types: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeScheme>) -> Option<crate::hydra::core::TypeScheme> {
  let for_type = |t: crate::hydra::core::Type| match &*t.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      for_type.clone()(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::maybes::map(|ts: crate::hydra::core::TypeScheme| crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
        variables: crate::hydra::lib::lists::cons(v0_.clone().0.parameter.clone(), ts.clone().0.variables.clone()),
        type_: ts.clone().0.type_.clone(),
        constraints: ts.clone().0.constraints.clone()})), for_type.clone()(v0_.clone().0.body.clone()))},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      dereference_schema_type(v0_.clone(), types.clone())},
    _ => Some(crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
      variables: Vec::from([]),
      type_: t.clone(),
      constraints: None})))} ;
  crate::hydra::lib::maybes::bind(crate::hydra::lib::maps::lookup(name.clone(), types.clone()), |ts: crate::hydra::core::TypeScheme| crate::hydra::lib::maybes::map(|ts2: crate::hydra::core::TypeScheme| crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
    variables: crate::hydra::lib::lists::concat2(ts.clone().0.variables.clone(), ts2.clone().0.variables.clone()),
    type_: ts2.clone().0.type_.clone(),
    constraints: ts2.clone().0.constraints.clone()})), for_type.clone()(ts.clone().0.type_.clone())))}

pub fn dereference_variable(graph: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Either<String, crate::hydra::core::Binding> {
  crate::hydra::lib::maybes::maybe(Left(crate::hydra::lib::strings::cat2(String::from("no such element: "), name.clone().0.0.clone())), |right_: crate::hydra::core::Binding| Right(right_.clone()), lookup_element(graph.clone(), name.clone()))}

pub fn elements_to_graph(parent: crate::hydra::graph::Graph, schema_types: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeScheme>, elements: Vec<crate::hydra::core::Binding>) -> crate::hydra::graph::Graph {
  let prims = parent.clone().0.primitives.clone() ;
  let g = build_graph(elements.clone(), crate::hydra::lib::maps::empty, prims.clone()) ;
  crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
    bound_terms: g.clone().0.bound_terms.clone(),
    bound_types: g.clone().0.bound_types.clone(),
    class_constraints: g.clone().0.class_constraints.clone(),
    lambda_variables: g.clone().0.lambda_variables.clone(),
    metadata: g.clone().0.metadata.clone(),
    primitives: g.clone().0.primitives.clone(),
    schema_types: schema_types.clone(),
    type_variables: g.clone().0.type_variables.clone()}))}

pub fn empty_context() -> crate::hydra::context::Context {
  crate::hydra::context::Context(Rc::new(crate::hydra::context::Context_Variant {
    trace: Vec::from([]),
    messages: Vec::from([]),
    other: crate::hydra::lib::maps::empty}))}

pub fn empty_graph() -> crate::hydra::graph::Graph {
  crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
    bound_terms: crate::hydra::lib::maps::empty,
    bound_types: crate::hydra::lib::maps::empty,
    class_constraints: crate::hydra::lib::maps::empty,
    lambda_variables: crate::hydra::lib::sets::empty,
    metadata: crate::hydra::lib::maps::empty,
    primitives: crate::hydra::lib::maps::empty,
    schema_types: crate::hydra::lib::maps::empty,
    type_variables: crate::hydra::lib::sets::empty}))}

pub fn extend_graph_with_bindings(bindings: Vec<crate::hydra::core::Binding>, g: crate::hydra::graph::Graph) -> crate::hydra::graph::Graph {
  let new_terms = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|b: crate::hydra::core::Binding| (b.clone().0.name.clone(), b.clone().0.term.clone()), bindings.clone())) ;
  let new_types = crate::hydra::lib::maps::from_list(crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(|b: crate::hydra::core::Binding| crate::hydra::lib::maybes::map(|ts: crate::hydra::core::TypeScheme| (b.clone().0.name.clone(), ts.clone()), b.clone().0.type_.clone()), bindings.clone()))) ;
  crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
    bound_terms: crate::hydra::lib::maps::union_(new_terms.clone(), g.clone().0.bound_terms.clone()),
    bound_types: crate::hydra::lib::maps::union_(new_types.clone(), g.clone().0.bound_types.clone()),
    class_constraints: g.clone().0.class_constraints.clone(),
    lambda_variables: g.clone().0.lambda_variables.clone(),
    metadata: g.clone().0.metadata.clone(),
    primitives: g.clone().0.primitives.clone(),
    schema_types: g.clone().0.schema_types.clone(),
    type_variables: g.clone().0.type_variables.clone()}))}

pub fn graph_to_bindings(g: crate::hydra::graph::Graph) -> Vec<crate::hydra::core::Binding> {
  crate::hydra::lib::lists::map(|p: (crate::hydra::core::Name, crate::hydra::core::Term)| {
    let name = crate::hydra::lib::pairs::first(p.clone()) ;
    {
      let term = crate::hydra::lib::pairs::second(p.clone()) ;
      crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
        name: name.clone(),
        term: term.clone(),
        type_: crate::hydra::lib::maps::lookup(name.clone(), g.clone().0.bound_types.clone())}))}}, crate::hydra::lib::maps::to_list(g.clone().0.bound_terms.clone()))}

pub fn fields_of(t: crate::hydra::core::Type) -> Vec<crate::hydra::core::FieldType> {
  let stripped = crate::hydra::rewriting::deannotate_type(t.clone()) ;
  match &*stripped.clone().0 {
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      fields_of(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      v0_.clone()},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      v0_.clone()},
    _ => Vec::from([])}}

pub fn get_field(cx: crate::hydra::context::Context, m: BTreeMap<crate::hydra::core::Name, T0>, fname: crate::hydra::core::Name, decode: impl Fn(T0) -> Either<crate::hydra::context::InContext, T1> + Clone) -> Either<crate::hydra::context::InContext, T0> {
  crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected field "), fname.clone().0.0.clone()), String::from(" not found")))))))),
    context: cx.clone()}))), decode.clone(), crate::hydra::lib::maps::lookup(fname.clone(), m.clone()))}

pub fn lookup_element(graph: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Option<crate::hydra::core::Binding> {
  crate::hydra::lib::maybes::map(|term: crate::hydra::core::Term| crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
    name: name.clone(),
    term: term.clone(),
    type_: crate::hydra::lib::maps::lookup(name.clone(), graph.clone().0.bound_types.clone())})), crate::hydra::lib::maps::lookup(name.clone(), graph.clone().0.bound_terms.clone()))}

pub fn lookup_primitive(graph: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Option<crate::hydra::graph::Primitive> {
  crate::hydra::lib::maps::lookup(name.clone(), graph.clone().0.primitives.clone())}

pub fn lookup_term(graph: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Option<crate::hydra::core::Term> {
  crate::hydra::lib::maps::lookup(name.clone(), graph.clone().0.bound_terms.clone())}

pub fn match_enum(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, tname: crate::hydra::core::Name, pairs: Vec<(crate::hydra::core::Name, T0)>, v1: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, T0> {
  match_union(cx.clone(), graph.clone(), tname.clone(), crate::hydra::lib::lists::map(|pair: (crate::hydra::core::Name, T0)| match_unit_field(crate::hydra::lib::pairs::first(pair.clone()), crate::hydra::lib::pairs::second(pair.clone())), pairs.clone()), v1.clone())}

pub fn match_record(cx: crate::hydra::context::Context, graph: T0, decode: impl Fn(BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term>) -> Either<crate::hydra::context::InContext, T1> + Clone, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, T1> {
  let stripped = crate::hydra::rewriting::deannotate_and_detype_term(term.clone()) ;
  match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      decode.clone()(crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|field: crate::hydra::core::Field| (field.clone().0.name.clone(), field.clone().0.term.clone()), v0_.clone().0.fields.clone())))},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(String::from("expected a record, got "), crate::hydra::show::core::term(term.clone())))))))),
      context: cx.clone()})))}}

pub fn match_union(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, tname: crate::hydra::core::Name, pairs: Vec<(crate::hydra::core::Name, Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, T0>>)>, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, T0> {
  let stripped = crate::hydra::rewriting::deannotate_and_detype_term(term.clone()) ;
  let mapping = crate::hydra::lib::maps::from_list(pairs.clone()) ;
  match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(require_element(cx.clone(), graph.clone(), v0_.clone()), |el: crate::hydra::core::Binding| match_union(cx.clone(), graph.clone(), tname.clone(), pairs.clone(), el.clone().0.term.clone()))},
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let exp = {
          let fname = v0_.clone().0.field.clone().0.name.clone() ;
          {
            let val = v0_.clone().0.field.clone().0.term.clone() ;
            crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
              object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("no matching case for field \""), fname.clone().0.0.clone()), String::from("\" in union type ")), tname.clone().0.0.clone()))))))),
              context: cx.clone()}))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, T0>>| f.clone()(val.clone()), crate::hydra::lib::maps::lookup(fname.clone(), mapping.clone()))}} ;
        crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(v0_.clone().0.type_name.clone().0.0.clone(), tname.clone().0.0.clone()), exp.clone(), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
          object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("expected injection for type "), tname.clone().0.0.clone()), String::from(", got ")), crate::hydra::show::core::term(term.clone())))))))),
          context: cx.clone()}))))}},
    _ => Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat(Vec::from([
        String::from("expected inject("),
        tname.clone().0.0.clone(),
        String::from(") with one of {"),
        crate::hydra::lib::strings::intercalate(String::from(", "), crate::hydra::lib::lists::map(|pair: (crate::hydra::core::Name, Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, T0>>)| crate::hydra::lib::pairs::first(pair.clone()).0.0.clone(), pairs.clone())),
        String::from("}, got "),
        crate::hydra::show::core::term(stripped.clone())])))))))),
      context: cx.clone()})))}}

pub fn match_unit_field(fname: T0, x: T1) -> (T0, Rc<dyn Fn(T2) -> Either<T3, T1>>) {
  (fname.clone(), |ignored: T2| Right(x.clone()))}

pub fn require_element(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, crate::hydra::core::Binding> {
  let show_all = false ;
  let ellipsis = |strings: Vec<String>| crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::and(crate::hydra::lib::equality::gt(crate::hydra::lib::lists::length(strings.clone()), 3i32), crate::hydra::lib::logic::not(show_all.clone())), crate::hydra::lib::lists::concat2(crate::hydra::lib::lists::take(3i32, strings.clone()), Vec::from([
    String::from("...")])), strings.clone()) ;
  let err_msg = crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("no such element: "), name.clone().0.0.clone()), String::from(". Available elements: {")), crate::hydra::lib::strings::intercalate(String::from(", "), ellipsis.clone()(crate::hydra::lib::lists::map(|v| v.0.0.clone(), crate::hydra::lib::maps::keys(graph.clone().0.bound_terms.clone()))))), String::from("}")) ;
  crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(err_msg.clone())))))),
    context: cx.clone()}))), |x: crate::hydra::core::Binding| Right(x.clone()), dereference_element(graph.clone(), name.clone()))}

pub fn require_primitive(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, crate::hydra::graph::Primitive> {
  crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(String::from("no such primitive function: "), name.clone().0.0.clone()))))))),
    context: cx.clone()}))), |x: crate::hydra::graph::Primitive| Right(x.clone()), lookup_primitive(graph.clone(), name.clone()))}

pub fn require_primitive_type(cx: crate::hydra::context::Context, tx: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, crate::hydra::core::TypeScheme> {
  let mts = crate::hydra::lib::maps::lookup(name.clone(), crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|_gpt_p: crate::hydra::graph::Primitive| (_gpt_p.clone().0.name.clone(), _gpt_p.clone().0.type_.clone()), crate::hydra::lib::maps::elems(tx.clone().0.primitives.clone())))) ;
  crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(String::from("no such primitive function: "), name.clone().0.0.clone()))))))),
    context: cx.clone()}))), |ts: crate::hydra::core::TypeScheme| Right(ts.clone()), mts.clone())}

pub fn require_term(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, crate::hydra::core::Term> {
  crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(String::from("no such element: "), name.clone().0.0.clone()))))))),
    context: cx.clone()}))), |x: crate::hydra::core::Term| Right(x.clone()), resolve_term(graph.clone(), name.clone()))}

pub fn resolve_term(graph: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Option<crate::hydra::core::Term> {
  let recurse = |term: crate::hydra::core::Term| {
    let stripped = crate::hydra::rewriting::deannotate_term(term.clone()) ;
    match &*stripped.clone().0 {
      crate::hydra::core::Term_Variant::Variable (v0_) => {
        let v0_ = v0_.clone() ;
        resolve_term(graph.clone(), v0_.clone())},
      _ => Some(term.clone())}} ;
  crate::hydra::lib::maybes::maybe(None, recurse.clone(), lookup_term(graph.clone(), name.clone()))}

pub fn strip_and_dereference_term(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Term> {
  let stripped = crate::hydra::rewriting::deannotate_and_detype_term(term.clone()) ;
  match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(require_term(cx.clone(), graph.clone(), v0_.clone()), |t: crate::hydra::core::Term| strip_and_dereference_term(cx.clone(), graph.clone(), t.clone()))},
    _ => Right(stripped.clone())}}

pub fn strip_and_dereference_term_either(graph: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<String, crate::hydra::core::Term> {
  let stripped = crate::hydra::rewriting::deannotate_and_detype_term(term.clone()) ;
  match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::either(|left_: String| Left(left_.clone()), |binding: crate::hydra::core::Binding| strip_and_dereference_term_either(graph.clone(), binding.clone().0.term.clone()), dereference_variable(graph.clone(), v0_.clone()))},
    _ => Right(stripped.clone())}}
