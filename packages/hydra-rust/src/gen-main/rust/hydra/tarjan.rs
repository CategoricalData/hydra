#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::constants::*;
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

pub fn adjacency_lists_to_graph(edges0: Vec<(T0, Vec<T0>)>) -> (BTreeMap<i32, Vec<i32>>, Rc<dyn Fn(i32) -> T0>) {
  let sorted_edges = crate::hydra::lib::lists::sort_on(crate::hydra::lib::pairs::first, edges0.clone()) ;
  let indexed_edges = crate::hydra::lib::lists::zip(crate::hydra::lib::math::range(0i32, crate::hydra::lib::lists::length(sorted_edges.clone())), sorted_edges.clone()) ;
  let key_to_vertex = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|vk_neighbors: (i32, (T0, Vec<T0>))| {
    let v = crate::hydra::lib::pairs::first(vk_neighbors.clone()) ;
    {
      let k_neighbors = crate::hydra::lib::pairs::second(vk_neighbors.clone()) ;
      {
        let k = crate::hydra::lib::pairs::first(k_neighbors.clone()) ;
        (k.clone(), v.clone())}}}, indexed_edges.clone())) ;
  let vertex_map = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|vk_neighbors: (i32, (T0, Vec<T0>))| {
    let v = crate::hydra::lib::pairs::first(vk_neighbors.clone()) ;
    {
      let k_neighbors = crate::hydra::lib::pairs::second(vk_neighbors.clone()) ;
      {
        let k = crate::hydra::lib::pairs::first(k_neighbors.clone()) ;
        (v.clone(), k.clone())}}}, indexed_edges.clone())) ;
  let graph = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|vk_neighbors: (i32, (T0, Vec<T0>))| {
    let v = crate::hydra::lib::pairs::first(vk_neighbors.clone()) ;
    {
      let k_neighbors = crate::hydra::lib::pairs::second(vk_neighbors.clone()) ;
      {
        let neighbors = crate::hydra::lib::pairs::second(k_neighbors.clone()) ;
        (v.clone(), crate::hydra::lib::maybes::map_maybe(|k: T0| crate::hydra::lib::maps::lookup(k.clone(), key_to_vertex.clone()), neighbors.clone()))}}}, indexed_edges.clone())) ;
  let vertex_to_key = |v: i32| crate::hydra::lib::maybes::from_just(crate::hydra::lib::maps::lookup(v.clone(), vertex_map.clone())) ;
  (graph.clone(), vertex_to_key.clone())}

pub fn strongly_connected_components(graph: BTreeMap<i32, Vec<i32>>) -> Vec<Vec<i32>> {
  let verts = crate::hydra::lib::maps::keys(graph.clone()) ;
  let final_state = crate::hydra::lib::lists::foldl(|st: crate::hydra::topology::TarjanState, v: i32| crate::hydra::lib::logic::if_else(crate::hydra::lib::maps::member(v.clone(), st.clone().0.indices.clone()), st.clone(), strong_connect(graph.clone(), v.clone(), st.clone())), initial_state, verts.clone()) ;
  crate::hydra::lib::lists::reverse(crate::hydra::lib::lists::map(crate::hydra::lib::lists::sort, final_state.clone().0.sccs.clone()))}

pub fn initial_state() -> crate::hydra::topology::TarjanState {
  crate::hydra::topology::TarjanState(Rc::new(crate::hydra::topology::TarjanState_Variant {
    counter: 0i32,
    indices: crate::hydra::lib::maps::empty,
    low_links: crate::hydra::lib::maps::empty,
    stack: Vec::from([]),
    on_stack: crate::hydra::lib::sets::empty,
    sccs: Vec::from([])}))}

pub fn pop_stack_until(v: i32, st0: crate::hydra::topology::TarjanState) -> (Vec<i32>, crate::hydra::topology::TarjanState) {
  let go = |acc: Vec<i32>, st: crate::hydra::topology::TarjanState| {
    let x = crate::hydra::lib::lists::head(st.clone().0.stack.clone()) ;
    {
      let xs = crate::hydra::lib::lists::tail(st.clone().0.stack.clone()) ;
      {
        let new_st = crate::hydra::topology::TarjanState(Rc::new(crate::hydra::topology::TarjanState_Variant {
          counter: st.clone().0.counter.clone(),
          indices: st.clone().0.indices.clone(),
          low_links: st.clone().0.low_links.clone(),
          stack: xs.clone(),
          on_stack: st.clone().0.on_stack.clone(),
          sccs: st.clone().0.sccs.clone()})) ;
        {
          let new_st2 = crate::hydra::topology::TarjanState(Rc::new(crate::hydra::topology::TarjanState_Variant {
            counter: new_st.clone().0.counter.clone(),
            indices: new_st.clone().0.indices.clone(),
            low_links: new_st.clone().0.low_links.clone(),
            stack: new_st.clone().0.stack.clone(),
            on_stack: crate::hydra::lib::sets::delete(x.clone(), st.clone().0.on_stack.clone()),
            sccs: new_st.clone().0.sccs.clone()})) ;
          {
            let acc_ = crate::hydra::lib::lists::cons(x.clone(), acc.clone()) ;
            crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(x.clone(), v.clone()), (crate::hydra::lib::lists::reverse(acc_.clone()), new_st2.clone()), go.clone()(acc_.clone(), new_st2.clone()))}}}}} ;
  go.clone()(Vec::from([]), st0.clone())}

pub fn strong_connect(graph: BTreeMap<i32, Vec<i32>>, v: i32, st: crate::hydra::topology::TarjanState) -> crate::hydra::topology::TarjanState {
  let i = st.clone().0.counter.clone() ;
  let new_st = crate::hydra::topology::TarjanState(Rc::new(crate::hydra::topology::TarjanState_Variant {
    counter: crate::hydra::lib::math::add(i.clone(), 1i32),
    indices: crate::hydra::lib::maps::insert(v.clone(), i.clone(), st.clone().0.indices.clone()),
    low_links: crate::hydra::lib::maps::insert(v.clone(), i.clone(), st.clone().0.low_links.clone()),
    stack: crate::hydra::lib::lists::cons(v.clone(), st.clone().0.stack.clone()),
    on_stack: crate::hydra::lib::sets::insert(v.clone(), st.clone().0.on_stack.clone()),
    sccs: st.clone().0.sccs.clone()})) ;
  let neighbors = crate::hydra::lib::maps::find_with_default(Vec::from([]), v.clone(), graph.clone()) ;
  let process_neighbor = |st_: crate::hydra::topology::TarjanState, w: i32| {
    let low_link = |s: crate::hydra::topology::TarjanState| {
      let low_v1 = crate::hydra::lib::maps::find_with_default(crate::hydra::constants::max_int32, v.clone(), s.clone().0.low_links.clone()) ;
      {
        let idx_w = crate::hydra::lib::maps::find_with_default(crate::hydra::constants::max_int32, w.clone(), s.clone().0.indices.clone()) ;
        crate::hydra::topology::TarjanState(Rc::new(crate::hydra::topology::TarjanState_Variant {
          counter: s.clone().0.counter.clone(),
          indices: s.clone().0.indices.clone(),
          low_links: crate::hydra::lib::maps::insert(v.clone(), crate::hydra::lib::equality::min(low_v1.clone(), idx_w.clone()), s.clone().0.low_links.clone()),
          stack: s.clone().0.stack.clone(),
          on_stack: s.clone().0.on_stack.clone(),
          sccs: s.clone().0.sccs.clone()}))}} ;
    crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::not(crate::hydra::lib::maps::member(w.clone(), st_.clone().0.indices.clone())), {
      let st_after = strong_connect(graph.clone(), w.clone(), st_.clone()) ;
      {
        let low_v2 = crate::hydra::lib::maps::find_with_default(crate::hydra::constants::max_int32, v.clone(), st_after.clone().0.low_links.clone()) ;
        {
          let low_w = crate::hydra::lib::maps::find_with_default(crate::hydra::constants::max_int32, w.clone(), st_after.clone().0.low_links.clone()) ;
          crate::hydra::topology::TarjanState(Rc::new(crate::hydra::topology::TarjanState_Variant {
            counter: st_after.clone().0.counter.clone(),
            indices: st_after.clone().0.indices.clone(),
            low_links: crate::hydra::lib::maps::insert(v.clone(), crate::hydra::lib::equality::min(low_v2.clone(), low_w.clone()), st_after.clone().0.low_links.clone()),
            stack: st_after.clone().0.stack.clone(),
            on_stack: st_after.clone().0.on_stack.clone(),
            sccs: st_after.clone().0.sccs.clone()}))}}}, crate::hydra::lib::logic::if_else(crate::hydra::lib::sets::member(w.clone(), st_.clone().0.on_stack.clone()), low_link.clone()(st_.clone()), st_.clone()))} ;
  let st_after_neighbors = crate::hydra::lib::lists::foldl(process_neighbor.clone(), new_st.clone(), neighbors.clone()) ;
  let low_v = crate::hydra::lib::maps::find_with_default(crate::hydra::constants::max_int32, v.clone(), st_after_neighbors.clone().0.low_links.clone()) ;
  let idx_v = crate::hydra::lib::maps::find_with_default(crate::hydra::constants::max_int32, v.clone(), st_after_neighbors.clone().0.indices.clone()) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(low_v.clone(), idx_v.clone()), {
    let comp_result = pop_stack_until(v.clone(), st_after_neighbors.clone()) ;
    {
      let comp = crate::hydra::lib::pairs::first(comp_result.clone()) ;
      {
        let st_popped = crate::hydra::lib::pairs::second(comp_result.clone()) ;
        crate::hydra::topology::TarjanState(Rc::new(crate::hydra::topology::TarjanState_Variant {
          counter: st_popped.clone().0.counter.clone(),
          indices: st_popped.clone().0.indices.clone(),
          low_links: st_popped.clone().0.low_links.clone(),
          stack: st_popped.clone().0.stack.clone(),
          on_stack: st_popped.clone().0.on_stack.clone(),
          sccs: crate::hydra::lib::lists::cons(comp.clone(), st_popped.clone().0.sccs.clone())}))}}}, st_after_neighbors.clone())}
