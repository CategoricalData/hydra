#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::core::*;

pub type Graph = BTreeMap<Vertex, Vec<Vertex>> ;

pub type OrderingIsomorphism = (Rc<dyn Fn(Vec<A>) -> Vec<A>>, Rc<dyn Fn(Vec<A>) -> Vec<A>>) ;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TarjanState_Variant {
  pub counter: i32,
  pub indices: BTreeMap<Vertex, i32>,
  pub low_links: BTreeMap<Vertex, i32>,
  pub stack: Vec<Vertex>,
  pub on_stack: BTreeSet<Vertex>,
  pub sccs: Vec<Vec<Vertex>>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TarjanState (pub Rc<TarjanState_Variant>);

pub type Vertex = i32 ;
