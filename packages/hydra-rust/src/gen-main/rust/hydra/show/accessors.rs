#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::names::*;
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

pub fn term_accessor(accessor: crate::hydra::accessors::TermAccessor) -> Option<String> {
  let idx = |i: T0| None ;
  let idx_suff = |suffix: String, i: T0| crate::hydra::lib::maybes::map(|s: String| crate::hydra::lib::strings::cat2(s.clone(), suffix.clone()), idx.clone()(i.clone())) ;
  match &*accessor.clone().0 {
    crate::hydra::accessors::TermAccessor_Variant::AnnotatedBody (v0_) => {
      let v0_ = v0_.clone() ;
      None},
    crate::hydra::accessors::TermAccessor_Variant::ApplicationFunction (v0_) => {
      let v0_ = v0_.clone() ;
      Some(String::from("fun"))},
    crate::hydra::accessors::TermAccessor_Variant::ApplicationArgument (v0_) => {
      let v0_ = v0_.clone() ;
      Some(String::from("arg"))},
    crate::hydra::accessors::TermAccessor_Variant::LambdaBody (v0_) => {
      let v0_ = v0_.clone() ;
      Some(String::from("body"))},
    crate::hydra::accessors::TermAccessor_Variant::UnionCasesDefault (v0_) => {
      let v0_ = v0_.clone() ;
      Some(String::from("default"))},
    crate::hydra::accessors::TermAccessor_Variant::UnionCasesBranch (v0_) => {
      let v0_ = v0_.clone() ;
      Some(crate::hydra::lib::strings::cat2(String::from("."), v0_.clone().0.0.clone()))},
    crate::hydra::accessors::TermAccessor_Variant::LetBody (v0_) => {
      let v0_ = v0_.clone() ;
      Some(String::from("in"))},
    crate::hydra::accessors::TermAccessor_Variant::LetBinding (v0_) => {
      let v0_ = v0_.clone() ;
      Some(crate::hydra::lib::strings::cat2(v0_.clone().0.0.clone(), String::from("=")))},
    crate::hydra::accessors::TermAccessor_Variant::ListElement (v0_) => {
      let v0_ = v0_.clone() ;
      idx.clone()(v0_.clone())},
    crate::hydra::accessors::TermAccessor_Variant::MapKey (v0_) => {
      let v0_ = v0_.clone() ;
      idx_suff.clone()(String::from(".key"), v0_.clone())},
    crate::hydra::accessors::TermAccessor_Variant::MapValue (v0_) => {
      let v0_ = v0_.clone() ;
      idx_suff.clone()(String::from(".value"), v0_.clone())},
    crate::hydra::accessors::TermAccessor_Variant::MaybeTerm (v0_) => {
      let v0_ = v0_.clone() ;
      Some(String::from("just"))},
    crate::hydra::accessors::TermAccessor_Variant::ProductTerm (v0_) => {
      let v0_ = v0_.clone() ;
      idx.clone()(v0_.clone())},
    crate::hydra::accessors::TermAccessor_Variant::RecordField (v0_) => {
      let v0_ = v0_.clone() ;
      Some(crate::hydra::lib::strings::cat2(String::from("."), v0_.clone().0.0.clone()))},
    crate::hydra::accessors::TermAccessor_Variant::SetElement (v0_) => {
      let v0_ = v0_.clone() ;
      idx.clone()(v0_.clone())},
    crate::hydra::accessors::TermAccessor_Variant::SumTerm (v0_) => {
      let v0_ = v0_.clone() ;
      None},
    crate::hydra::accessors::TermAccessor_Variant::TypeLambdaBody (v0_) => {
      let v0_ = v0_.clone() ;
      None},
    crate::hydra::accessors::TermAccessor_Variant::TypeApplicationTerm (v0_) => {
      let v0_ = v0_.clone() ;
      None},
    crate::hydra::accessors::TermAccessor_Variant::InjectionTerm (v0_) => {
      let v0_ = v0_.clone() ;
      None},
    crate::hydra::accessors::TermAccessor_Variant::WrappedTerm (v0_) => {
      let v0_ = v0_.clone() ;
      None}}}

pub fn term_to_accessor_graph(namespaces: BTreeMap<crate::hydra::module::Namespace, String>, term: crate::hydra::core::Term) -> crate::hydra::accessors::AccessorGraph {
  let dont_care_accessor = crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::AnnotatedBody)) ;
  let helper = |ids: BTreeMap<crate::hydra::core::Name, crate::hydra::accessors::AccessorNode>, mroot: Option<crate::hydra::accessors::AccessorNode>, path: Vec<crate::hydra::accessors::TermAccessor>, state: ((Vec<crate::hydra::accessors::AccessorNode>, Vec<crate::hydra::accessors::AccessorEdge>), BTreeSet<String>), accessor_term: (crate::hydra::accessors::TermAccessor, crate::hydra::core::Term)| {
    let accessor = crate::hydra::lib::pairs::first(accessor_term.clone()) ;
    let current_term = crate::hydra::lib::pairs::second(accessor_term.clone()) ;
    let nodes_edges = crate::hydra::lib::pairs::first(state.clone()) ;
    let visited = crate::hydra::lib::pairs::second(state.clone()) ;
    let nodes = crate::hydra::lib::pairs::first(nodes_edges.clone()) ;
    let edges = crate::hydra::lib::pairs::second(nodes_edges.clone()) ;
    let next_path = crate::hydra::lib::lists::cons(accessor.clone(), path.clone()) ;
    match &*current_term.clone().0 {
      crate::hydra::core::Term_Variant::Let (v0_) => {
        let v0_ = v0_.clone() ;
        {
          let bindings = v0_.clone().0.bindings.clone() ;
          let env = v0_.clone().0.body.clone() ;
          let binding_names = crate::hydra::lib::lists::map(|v| v.0.name.clone(), bindings.clone()) ;
          let add_binding_name = |nodes_visited_ids: ((Vec<crate::hydra::accessors::AccessorNode>, BTreeSet<String>), BTreeMap<crate::hydra::core::Name, crate::hydra::accessors::AccessorNode>), name: crate::hydra::core::Name| {
            let current_nodes_visited = crate::hydra::lib::pairs::first(nodes_visited_ids.clone()) ;
            let current_ids = crate::hydra::lib::pairs::second(nodes_visited_ids.clone()) ;
            let current_nodes = crate::hydra::lib::pairs::first(current_nodes_visited.clone()) ;
            let current_visited = crate::hydra::lib::pairs::second(current_nodes_visited.clone()) ;
            let raw_label = crate::hydra::names::compact_name(namespaces.clone(), name.clone()) ;
            let unique_label = crate::hydra::names::unique_label(current_visited.clone(), raw_label.clone()) ;
            let node = crate::hydra::accessors::AccessorNode(Rc::new(crate::hydra::accessors::AccessorNode_Variant {
              name: name.clone(),
              label: raw_label.clone(),
              id: unique_label.clone()})) ;
            let new_visited = crate::hydra::lib::sets::insert(unique_label.clone(), current_visited.clone()) ;
            let new_nodes = crate::hydra::lib::lists::cons(node.clone(), current_nodes.clone()) ;
            let new_ids = crate::hydra::lib::maps::insert(name.clone(), node.clone(), current_ids.clone()) ;
            ((new_nodes.clone(), new_visited.clone()), new_ids.clone())} ;
          let nodes_visited_ids1 = crate::hydra::lib::lists::foldl(add_binding_name.clone(), ((Vec::from([]), visited.clone()), ids.clone()), binding_names.clone()) ;
          let nodes1 = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::first(nodes_visited_ids1.clone())) ;
          let visited1 = crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::first(nodes_visited_ids1.clone())) ;
          let ids1 = crate::hydra::lib::pairs::second(nodes_visited_ids1.clone()) ;
          let add_binding_term = |current_state: ((Vec<crate::hydra::accessors::AccessorNode>, Vec<crate::hydra::accessors::AccessorEdge>), BTreeSet<String>), node_binding: (crate::hydra::accessors::AccessorNode, crate::hydra::core::Binding)| {
            let root = crate::hydra::lib::pairs::first(node_binding.clone()) ;
            let binding = crate::hydra::lib::pairs::second(node_binding.clone()) ;
            let term1 = binding.clone().0.term.clone() ;
            helper.clone()(ids1.clone(), Some(root.clone()), Vec::from([]), current_state.clone(), (dont_care_accessor.clone(), term1.clone()))} ;
          let node_binding_pairs = crate::hydra::lib::lists::zip(nodes1.clone(), bindings.clone()) ;
          let state_after_bindings = crate::hydra::lib::lists::foldl(add_binding_term.clone(), ((crate::hydra::lib::lists::concat2(nodes1.clone(), nodes.clone()), edges.clone()), visited1.clone()), node_binding_pairs.clone()) ;
          helper.clone()(ids1.clone(), mroot.clone(), next_path.clone(), state_after_bindings.clone(), (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::LetBody)), env.clone()))}},
      crate::hydra::core::Term_Variant::Variable (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::lib::maybes::maybe(state.clone(), |root: crate::hydra::accessors::AccessorNode| crate::hydra::lib::maybes::maybe(state.clone(), |node: crate::hydra::accessors::AccessorNode| {
          let edge = crate::hydra::accessors::AccessorEdge(Rc::new(crate::hydra::accessors::AccessorEdge_Variant {
            source: root.clone(),
            path: crate::hydra::accessors::AccessorPath(Rc::new(crate::hydra::accessors::AccessorPath_Variant(crate::hydra::lib::lists::reverse(next_path.clone())))),
            target: node.clone()})) ;
          let new_edges = crate::hydra::lib::lists::cons(edge.clone(), edges.clone()) ;
          ((nodes.clone(), new_edges.clone()), visited.clone())}, crate::hydra::lib::maps::lookup(v0_.clone(), ids.clone())), mroot.clone())},
      _ => crate::hydra::lib::lists::foldl(|v1: ((Vec<crate::hydra::accessors::AccessorNode>, Vec<crate::hydra::accessors::AccessorEdge>), BTreeSet<String>), v2: (crate::hydra::accessors::TermAccessor, crate::hydra::core::Term)| helper.clone()(ids.clone(), mroot.clone(), next_path.clone(), v1.clone(), v2.clone()), state.clone(), crate::hydra::rewriting::subterms_with_accessors(current_term.clone()))}} ;
  let initial_state = ((Vec::from([]), Vec::from([])), crate::hydra::lib::sets::empty) ;
  let result = helper.clone()(crate::hydra::lib::maps::empty, None, Vec::from([]), initial_state.clone(), (dont_care_accessor.clone(), term.clone())) ;
  let final_nodes_edges = crate::hydra::lib::pairs::first(result.clone()) ;
  let final_nodes = crate::hydra::lib::pairs::first(final_nodes_edges.clone()) ;
  let final_edges = crate::hydra::lib::pairs::second(final_nodes_edges.clone()) ;
  crate::hydra::accessors::AccessorGraph(Rc::new(crate::hydra::accessors::AccessorGraph_Variant {
    nodes: final_nodes.clone(),
    edges: final_edges.clone()}))}
