#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::formatting::*;
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

pub fn compact_name(namespaces: BTreeMap<crate::hydra::module::Namespace, String>, name: crate::hydra::core::Name) -> String {
  let qual_name = qualify_name(name.clone()) ;
  let mns = qual_name.clone().0.namespace.clone() ;
  let local = qual_name.clone().0.local.clone() ;
  crate::hydra::lib::maybes::maybe(name.clone().0.0.clone(), |ns: crate::hydra::module::Namespace| crate::hydra::lib::maybes::maybe(local.clone(), |pre: String| crate::hydra::lib::strings::cat(Vec::from([
    pre.clone(),
    String::from(":"),
    local.clone()])), crate::hydra::lib::maps::lookup(ns.clone(), namespaces.clone())), mns.clone())}

pub fn local_name_of(arg_: crate::hydra::core::Name) -> String {
  qualify_name(arg_.clone()).0.local.clone()}

pub fn namespace_of(arg_: crate::hydra::core::Name) -> Option<crate::hydra::module::Namespace> {
  qualify_name(arg_.clone()).0.namespace.clone()}

pub fn namespace_to_file_path(case_conv: crate::hydra::util::CaseConvention, ext: crate::hydra::module::FileExtension, ns: crate::hydra::module::Namespace) -> String {
  let parts = crate::hydra::lib::lists::map(|v1: String| crate::hydra::formatting::convert_case(crate::hydra::util::CaseConvention(Rc::new(crate::hydra::util::CaseConvention_Variant::Camel)), case_conv.clone(), v1.clone()), crate::hydra::lib::strings::split_on(String::from("."), ns.clone().0.0.clone())) ;
  crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::intercalate(String::from("/"), parts.clone()), String::from(".")), ext.clone().0.0.clone())}

pub fn qname(ns: crate::hydra::module::Namespace, name: String) -> crate::hydra::core::Name {
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat(Vec::from([
    ns.clone().0.0.clone(),
    String::from("."),
    name.clone()])))))}

pub fn qualify_name(name: crate::hydra::core::Name) -> crate::hydra::module::QualifiedName {
  let parts = crate::hydra::lib::lists::reverse(crate::hydra::lib::strings::split_on(String::from("."), name.clone().0.0.clone())) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(1i32, crate::hydra::lib::lists::length(parts.clone())), crate::hydra::module::QualifiedName(Rc::new(crate::hydra::module::QualifiedName_Variant {
    namespace: None,
    local: name.clone().0.0.clone()})), crate::hydra::module::QualifiedName(Rc::new(crate::hydra::module::QualifiedName_Variant {
    namespace: Some(crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(crate::hydra::lib::strings::intercalate(String::from("."), crate::hydra::lib::lists::reverse(crate::hydra::lib::lists::tail(parts.clone()))))))),
    local: crate::hydra::lib::lists::head(parts.clone())})))}

pub fn unique_label(visited: BTreeSet<String>, l: String) -> String {
  crate::hydra::lib::logic::if_else(crate::hydra::lib::sets::member(l.clone(), visited.clone()), unique_label(visited.clone(), crate::hydra::lib::strings::cat2(l.clone(), String::from("'"))), l.clone())}

pub fn unqualify_name(qname: crate::hydra::module::QualifiedName) -> crate::hydra::core::Name {
  let prefix = crate::hydra::lib::maybes::maybe(String::from(""), |n: crate::hydra::module::Namespace| crate::hydra::lib::strings::cat2(n.clone().0.0.clone(), String::from(".")), qname.clone().0.namespace.clone()) ;
  crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat2(prefix.clone(), qname.clone().0.local.clone()))))}
