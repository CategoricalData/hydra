#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::tarjan::*;
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

pub fn adjacency_list_to_map(pairs: Vec<(T0, Vec<T1>)>) -> BTreeMap<T0, Vec<(T0, Vec<T1>)>> {
  crate::hydra::lib::lists::foldl(|mp: BTreeMap<T0, Vec<T1>>, p: (T0, Vec<T1>)| {
    let k = crate::hydra::lib::pairs::first(p.clone()) ;
    {
      let vs = crate::hydra::lib::pairs::second(p.clone()) ;
      {
        let existing = crate::hydra::lib::maybes::maybe(Vec::from([]), crate::hydra::lib::equality::identity, crate::hydra::lib::maps::lookup(k.clone(), mp.clone())) ;
        crate::hydra::lib::maps::insert(k.clone(), crate::hydra::lib::lists::concat2(existing.clone(), vs.clone()), mp.clone())}}}, crate::hydra::lib::maps::empty, pairs.clone())}

pub fn create_ordering_isomorphism(source_ord: Vec<T0>, target_ord: Vec<T0>) -> crate::hydra::topology::OrderingIsomorphism {
  let source_to_target_mapping = |els: Vec<T2>| {
    let mp = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::zip(source_ord.clone(), els.clone())) ;
    crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(|n: T0| crate::hydra::lib::maps::lookup(n.clone(), mp.clone()), target_ord.clone()))} ;
  let target_to_source_mapping = |els: Vec<T2>| {
    let mp = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::zip(target_ord.clone(), els.clone())) ;
    crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(|n: T0| crate::hydra::lib::maps::lookup(n.clone(), mp.clone()), source_ord.clone()))} ;
  crate::hydra::topology::OrderingIsomorphism(Rc::new(crate::hydra::topology::OrderingIsomorphism_Variant {
    encode: source_to_target_mapping.clone(),
    decode: target_to_source_mapping.clone()}))}

pub fn find_reachable_nodes(adj: impl Fn(T0) -> BTreeSet<T0> + Clone, root: T0) -> BTreeSet<T0> {
  let visit = |visited: BTreeSet<T0>, node: T0| {
    let to_visit = crate::hydra::lib::sets::difference(adj.clone()(node.clone()), visited.clone()) ;
    crate::hydra::lib::logic::if_else(crate::hydra::lib::sets::null(to_visit.clone()), visited.clone(), crate::hydra::lib::lists::foldl(|v: BTreeSet<T0>, n: T0| visit.clone()(crate::hydra::lib::sets::insert(n.clone(), v.clone()), n.clone()), visited.clone(), crate::hydra::lib::sets::to_list(to_visit.clone())))} ;
  visit.clone()(crate::hydra::lib::sets::singleton(root.clone()), root.clone())}

pub fn propagate_tags(edges: Vec<(T0, Vec<T0>)>, node_tags: Vec<(T0, Vec<T1>)>) -> Vec<(T0, BTreeSet<T1>)> {
  let adj_map = adjacency_list_to_map(edges.clone()) ;
  let tag_map = crate::hydra::lib::maps::map(crate::hydra::lib::sets::from_list, adjacency_list_to_map(node_tags.clone())) ;
  let all_nodes = crate::hydra::lib::sets::to_list(crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::concat2(crate::hydra::lib::lists::map(crate::hydra::lib::pairs::first, edges.clone()), crate::hydra::lib::lists::map(crate::hydra::lib::pairs::first, node_tags.clone())))) ;
  let get_tags_for_node = |node: T0| {
    let reachable = find_reachable_nodes(|n: T0| crate::hydra::lib::sets::from_list(crate::hydra::lib::maybes::maybe(Vec::from([]), crate::hydra::lib::equality::identity, crate::hydra::lib::maps::lookup(n.clone(), adj_map.clone()))), node.clone()) ;
    crate::hydra::lib::sets::unions(crate::hydra::lib::lists::map(|n: T0| crate::hydra::lib::maybes::maybe(crate::hydra::lib::sets::empty, crate::hydra::lib::equality::identity, crate::hydra::lib::maps::lookup(n.clone(), tag_map.clone())), crate::hydra::lib::sets::to_list(reachable.clone())))} ;
  crate::hydra::lib::lists::map(|n: T0| (n.clone(), get_tags_for_node.clone()(n.clone())), all_nodes.clone())}

pub fn topological_sort(pairs: Vec<(T0, Vec<T0>)>) -> Either<Vec<Vec<T0>>, Vec<T0>> {
  let sccs = topological_sort_components(pairs.clone()) ;
  let is_cycle = |scc: Vec<T1>| crate::hydra::lib::logic::not(crate::hydra::lib::lists::null(crate::hydra::lib::lists::tail(scc.clone()))) ;
  let with_cycles = crate::hydra::lib::lists::filter(is_cycle.clone(), sccs.clone()) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(with_cycles.clone()), Right(crate::hydra::lib::lists::concat(sccs.clone())), Left(with_cycles.clone()))}

pub fn topological_sort_components(pairs: Vec<(T0, Vec<T0>)>) -> Vec<Vec<T0>> {
  let graph_result = crate::hydra::tarjan::adjacency_lists_to_graph(pairs.clone()) ;
  let g = crate::hydra::lib::pairs::first(graph_result.clone()) ;
  crate::hydra::lib::lists::map(|comp: Vec<i32>| crate::hydra::lib::lists::map(crate::hydra::lib::pairs::second(graph_result.clone()), comp.clone()), crate::hydra::tarjan::strongly_connected_components(g.clone()))}

pub fn topological_sort_nodes(get_key: impl Fn(T0) -> T1 + Clone, get_adj: impl Fn(T0) -> Vec<T1> + Clone, nodes: Vec<T0>) -> Vec<Vec<T0>> {
  let nodes_by_key = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|n: T0| (get_key.clone()(n.clone()), n.clone()), nodes.clone())) ;
  let pairs = crate::hydra::lib::lists::map(|n: T0| (get_key.clone()(n.clone()), get_adj.clone()(n.clone())), nodes.clone()) ;
  let comps = topological_sort_components(pairs.clone()) ;
  crate::hydra::lib::lists::map(|c: Vec<T1>| crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(|k: T1| crate::hydra::lib::maps::lookup(k.clone(), nodes_by_key.clone()), c.clone())), comps.clone())}
