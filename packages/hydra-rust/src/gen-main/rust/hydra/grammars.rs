#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::annotations::*;
use crate::hydra::constants::*;
use crate::hydra::formatting::*;
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

pub fn child_name(lname: String, n: String) -> String {
  crate::hydra::lib::strings::cat(Vec::from([
    lname.clone(),
    String::from("_"),
    crate::hydra::formatting::capitalize(n.clone())]))}

pub fn find_names(pats: Vec<crate::hydra::grammar::Pattern>) -> Vec<String> {
  let next_name = |acc: (Vec<String>, BTreeMap<String, i32>), pat: crate::hydra::grammar::Pattern| {
    let names = crate::hydra::lib::pairs::first(acc.clone()) ;
    {
      let name_map = crate::hydra::lib::pairs::second(acc.clone()) ;
      {
        let rn = raw_name(pat.clone()) ;
        {
          let name_and_index = crate::hydra::lib::maybes::maybe((rn.clone(), 1i32), |i: i32| (crate::hydra::lib::strings::cat2(rn.clone(), crate::hydra::lib::literals::show_int32(crate::hydra::lib::math::add(i.clone(), 1i32))), crate::hydra::lib::math::add(i.clone(), 1i32)), crate::hydra::lib::maps::lookup(rn.clone(), name_map.clone())) ;
          {
            let nn = crate::hydra::lib::pairs::first(name_and_index.clone()) ;
            {
              let ni = crate::hydra::lib::pairs::second(name_and_index.clone()) ;
              (crate::hydra::lib::lists::cons(nn.clone(), names.clone()), crate::hydra::lib::maps::insert(rn.clone(), ni.clone(), name_map.clone()))}}}}}} ;
  crate::hydra::lib::lists::reverse(crate::hydra::lib::pairs::first(crate::hydra::lib::lists::foldl(next_name.clone(), (Vec::from([]), crate::hydra::lib::maps::empty), pats.clone())))}

pub fn grammar_to_module(ns: crate::hydra::module::Namespace, grammar: crate::hydra::grammar::Grammar, desc: Option<String>) -> crate::hydra::module::Module {
  let prod_pairs = crate::hydra::lib::lists::map(|prod: crate::hydra::grammar::Production| (prod.clone().0.symbol.clone().0.0.clone(), prod.clone().0.pattern.clone()), grammar.clone().0.0.clone()) ;
  let capitalized_names = crate::hydra::lib::lists::map(|pair: (String, crate::hydra::grammar::Pattern)| crate::hydra::formatting::capitalize(crate::hydra::lib::pairs::first(pair.clone())), prod_pairs.clone()) ;
  let patterns = crate::hydra::lib::lists::map(|pair: (String, crate::hydra::grammar::Pattern)| crate::hydra::lib::pairs::second(pair.clone()), prod_pairs.clone()) ;
  let element_pairs = crate::hydra::lib::lists::concat(crate::hydra::lib::lists::zip_with(|v1: String, v2: crate::hydra::grammar::Pattern| make_elements(false, ns.clone(), v1.clone(), v2.clone()), capitalized_names.clone(), patterns.clone())) ;
  let elements = crate::hydra::lib::lists::map(|pair: (String, crate::hydra::core::Type)| {
    let lname = crate::hydra::lib::pairs::first(pair.clone()) ;
    {
      let el_name = to_name(ns.clone(), lname.clone()) ;
      {
        let typ = replace_placeholders(el_name.clone(), wrap_type(crate::hydra::lib::pairs::second(pair.clone()))) ;
        crate::hydra::annotations::type_element(el_name.clone(), typ.clone())}}}, element_pairs.clone()) ;
  crate::hydra::module::Module(Rc::new(crate::hydra::module::Module_Variant {
    namespace: ns.clone(),
    elements: elements.clone(),
    term_dependencies: Vec::from([]),
    type_dependencies: Vec::from([]),
    description: desc.clone()}))}

pub fn is_complex(pat: crate::hydra::grammar::Pattern) -> bool {
  match &*pat.clone().0 {
    crate::hydra::grammar::Pattern_Variant::Labeled (v0_) => {
      let v0_ = v0_.clone() ;
      is_complex(v0_.clone().0.pattern.clone())},
    crate::hydra::grammar::Pattern_Variant::Sequence (v0_) => {
      let v0_ = v0_.clone() ;
      is_nontrivial(true, v0_.clone())},
    crate::hydra::grammar::Pattern_Variant::Alternatives (v0_) => {
      let v0_ = v0_.clone() ;
      is_nontrivial(false, v0_.clone())},
    _ => false}}

pub fn is_nontrivial(is_record: bool, pats: Vec<crate::hydra::grammar::Pattern>) -> bool {
  let min_pats = simplify(is_record.clone(), pats.clone()) ;
  let is_labeled = |p: crate::hydra::grammar::Pattern| match &*p.clone().0 {
    crate::hydra::grammar::Pattern_Variant::Labeled (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    _ => false} ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(crate::hydra::lib::lists::length(min_pats.clone()), 1i32), is_labeled.clone()(crate::hydra::lib::lists::head(min_pats.clone())), true)}

pub fn make_elements(omit_trivial: bool, ns: crate::hydra::module::Namespace, lname: String, pat: crate::hydra::grammar::Pattern) -> Vec<(String, crate::hydra::core::Type)> {
  let trivial = crate::hydra::lib::logic::if_else(omit_trivial.clone(), Vec::from([]), Vec::from([
    (lname.clone(), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)))])) ;
  let descend = |n: String, f: Rc<dyn Fn(Vec<(String, crate::hydra::core::Type)>) -> T0>, p: crate::hydra::grammar::Pattern| {
    let cpairs = make_elements(false, ns.clone(), child_name(lname.clone(), n.clone()), p.clone()) ;
    f.clone()(crate::hydra::lib::logic::if_else(is_complex(p.clone()), crate::hydra::lib::lists::cons((lname.clone(), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(to_name(ns.clone(), crate::hydra::lib::pairs::first(crate::hydra::lib::lists::head(cpairs.clone()))))))), cpairs.clone()), crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(cpairs.clone()), Vec::from([
      (lname.clone(), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)))]), crate::hydra::lib::lists::cons((lname.clone(), crate::hydra::lib::pairs::second(crate::hydra::lib::lists::head(cpairs.clone()))), crate::hydra::lib::lists::tail(cpairs.clone())))))} ;
  let mod_ = |n: String, f: Rc<dyn Fn(crate::hydra::core::Type) -> crate::hydra::core::Type>, p: crate::hydra::grammar::Pattern| descend.clone()(n.clone(), |pairs: Vec<(String, crate::hydra::core::Type)>| crate::hydra::lib::lists::cons((lname.clone(), f.clone()(crate::hydra::lib::pairs::second(crate::hydra::lib::lists::head(pairs.clone())))), crate::hydra::lib::lists::tail(pairs.clone())), p.clone()) ;
  let for_pat = |pat2: crate::hydra::grammar::Pattern| match &*pat2.clone().0 {
    crate::hydra::grammar::Pattern_Variant::Alternatives (v0_) => {
      let v0_ = v0_.clone() ;
      for_record_or_union.clone()(false, |fields: Vec<crate::hydra::core::FieldType>| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(fields.clone()))), v0_.clone())},
    crate::hydra::grammar::Pattern_Variant::Constant (v0_) => {
      let v0_ = v0_.clone() ;
      trivial.clone()},
    crate::hydra::grammar::Pattern_Variant::Ignored (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([])},
    crate::hydra::grammar::Pattern_Variant::Labeled (v0_) => {
      let v0_ = v0_.clone() ;
      for_pat.clone()(v0_.clone().0.pattern.clone())},
    crate::hydra::grammar::Pattern_Variant::Nil (v0_) => {
      let v0_ = v0_.clone() ;
      trivial.clone()},
    crate::hydra::grammar::Pattern_Variant::Nonterminal (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        (lname.clone(), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(to_name(ns.clone(), v0_.clone().0.0.clone())))))])},
    crate::hydra::grammar::Pattern_Variant::Option (v0_) => {
      let v0_ = v0_.clone() ;
      mod_.clone()(String::from("Option"), |x: crate::hydra::core::Type| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(x.clone()))), v0_.clone())},
    crate::hydra::grammar::Pattern_Variant::Plus (v0_) => {
      let v0_ = v0_.clone() ;
      mod_.clone()(String::from("Elmt"), |x: crate::hydra::core::Type| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(x.clone()))), v0_.clone())},
    crate::hydra::grammar::Pattern_Variant::Regex (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        (lname.clone(), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String))))))])},
    crate::hydra::grammar::Pattern_Variant::Sequence (v0_) => {
      let v0_ = v0_.clone() ;
      for_record_or_union.clone()(true, |fields: Vec<crate::hydra::core::FieldType>| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(fields.clone()))), v0_.clone())},
    crate::hydra::grammar::Pattern_Variant::Star (v0_) => {
      let v0_ = v0_.clone() ;
      mod_.clone()(String::from("Elmt"), |x: crate::hydra::core::Type| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(x.clone()))), v0_.clone())}} ;
  let for_record_or_union = |is_record: bool, construct: Rc<dyn Fn(Vec<crate::hydra::core::FieldType>) -> crate::hydra::core::Type>, pats: Vec<crate::hydra::grammar::Pattern>| {
    let min_pats = simplify(is_record.clone(), pats.clone()) ;
    {
      let field_names = find_names(min_pats.clone()) ;
      {
        let to_field = |n: String, p: crate::hydra::grammar::Pattern| descend.clone()(n.clone(), |pairs: Vec<(String, crate::hydra::core::Type)>| (crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(n.clone()))),
          type_: crate::hydra::lib::pairs::second(crate::hydra::lib::lists::head(pairs.clone()))})), crate::hydra::lib::lists::tail(pairs.clone())), p.clone()) ;
        {
          let field_pairs = crate::hydra::lib::lists::zip_with(to_field.clone(), field_names.clone(), min_pats.clone()) ;
          {
            let fields = crate::hydra::lib::lists::map(crate::hydra::lib::pairs::first, field_pairs.clone()) ;
            {
              let els = crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(crate::hydra::lib::pairs::second, field_pairs.clone())) ;
              crate::hydra::lib::logic::if_else(is_nontrivial(is_record.clone(), pats.clone()), crate::hydra::lib::lists::cons((lname.clone(), construct.clone()(fields.clone())), els.clone()), for_pat.clone()(crate::hydra::lib::lists::head(min_pats.clone())))}}}}}} ;
  for_pat.clone()(pat.clone())}

pub fn raw_name(pat: crate::hydra::grammar::Pattern) -> String {
  match &*pat.clone().0 {
    crate::hydra::grammar::Pattern_Variant::Alternatives (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("alts")},
    crate::hydra::grammar::Pattern_Variant::Constant (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::formatting::capitalize(crate::hydra::formatting::with_character_aliases(v0_.clone().0.0.clone()))},
    crate::hydra::grammar::Pattern_Variant::Ignored (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("ignored")},
    crate::hydra::grammar::Pattern_Variant::Labeled (v0_) => {
      let v0_ = v0_.clone() ;
      v0_.clone().0.label.clone().0.0.clone()},
    crate::hydra::grammar::Pattern_Variant::Nil (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("none")},
    crate::hydra::grammar::Pattern_Variant::Nonterminal (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::formatting::capitalize(v0_.clone().0.0.clone())},
    crate::hydra::grammar::Pattern_Variant::Option (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::formatting::capitalize(raw_name(v0_.clone()))},
    crate::hydra::grammar::Pattern_Variant::Plus (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat2(String::from("listOf"), crate::hydra::formatting::capitalize(raw_name(v0_.clone())))},
    crate::hydra::grammar::Pattern_Variant::Regex (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("regex")},
    crate::hydra::grammar::Pattern_Variant::Sequence (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("sequence")},
    crate::hydra::grammar::Pattern_Variant::Star (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::strings::cat2(String::from("listOf"), crate::hydra::formatting::capitalize(raw_name(v0_.clone())))}}}

pub fn replace_placeholders(el_name: T0, typ: T1) -> T1 {
  typ.clone()}

pub fn simplify(is_record: bool, pats: Vec<crate::hydra::grammar::Pattern>) -> Vec<crate::hydra::grammar::Pattern> {
  let is_constant = |p: crate::hydra::grammar::Pattern| match &*p.clone().0 {
    crate::hydra::grammar::Pattern_Variant::Constant (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    _ => false} ;
  crate::hydra::lib::logic::if_else(is_record.clone(), crate::hydra::lib::lists::filter(|p: crate::hydra::grammar::Pattern| crate::hydra::lib::logic::not(is_constant.clone()(p.clone())), pats.clone()), pats.clone())}

pub fn to_name(ns: crate::hydra::module::Namespace, local: String) -> crate::hydra::core::Name {
  crate::hydra::names::unqualify_name(crate::hydra::module::QualifiedName(Rc::new(crate::hydra::module::QualifiedName_Variant {
    namespace: Some(ns.clone()),
    local: local.clone()})))}

pub fn wrap_type(t: crate::hydra::core::Type) -> crate::hydra::core::Type {
  match &*t.clone().0 {
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      t.clone()},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      t.clone()},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      t.clone()},
    _ => crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Wrap(t.clone())))}}
