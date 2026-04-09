#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::core::*;

pub type TBinding = (crate::hydra::core::Name, TTerm) ;

pub type TTerm = crate::hydra::core::Term ;
