#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::annotations::*;
use crate::hydra::decode::core::*;
use crate::hydra::formatting::*;
use crate::hydra::names::*;
use crate::hydra::rewriting::*;
use crate::hydra::schemas::*;
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

pub fn encode_binding(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, b: crate::hydra::core::Binding) -> Either<crate::hydra::context::InContext, crate::hydra::core::Binding> {
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|_wc_e: crate::hydra::error::DecodingError| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: _wc_e.clone(),
    context: cx.clone()})), |_wc_a: crate::hydra::core::Type| _wc_a.clone(), crate::hydra::decode::core::type_(graph.clone(), b.clone().0.term.clone())), |typ: crate::hydra::core::Type| Right(crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
    name: encode_binding_name(b.clone().0.name.clone()),
    term: encode_type_named(b.clone().0.name.clone(), typ.clone()),
    type_: Some(encoder_type_scheme_named(b.clone().0.name.clone(), typ.clone()))}))))}

pub fn encode_binding_name(n: crate::hydra::core::Name) -> crate::hydra::core::Name {
  crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::not(crate::hydra::lib::lists::null(crate::hydra::lib::lists::tail(crate::hydra::lib::strings::split_on(String::from("."), n.clone().0.0.clone())))), crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::intercalate(String::from("."), crate::hydra::lib::lists::concat2(Vec::from([
    String::from("hydra"),
    String::from("encode")]), crate::hydra::lib::lists::concat2(crate::hydra::lib::lists::tail(crate::hydra::lib::lists::init(crate::hydra::lib::strings::split_on(String::from("."), n.clone().0.0.clone()))), Vec::from([
    crate::hydra::formatting::decapitalize(crate::hydra::names::local_name_of(n.clone()))]))))))), crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::formatting::decapitalize(crate::hydra::names::local_name_of(n.clone()))))))}

pub fn encoder_collect_forall_variables(typ: crate::hydra::core::Type) -> Vec<crate::hydra::core::Name> {
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      encoder_collect_forall_variables(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::cons(v0_.clone().0.parameter.clone(), encoder_collect_forall_variables(v0_.clone().0.body.clone()))},
    _ => Vec::from([])}}

pub fn encoder_collect_ord_vars(typ: crate::hydra::core::Type) -> Vec<crate::hydra::core::Name> {
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      encoder_collect_ord_vars(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat2(encoder_collect_ord_vars(v0_.clone().0.function.clone()), encoder_collect_ord_vars(v0_.clone().0.argument.clone()))},
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat2(encoder_collect_ord_vars(v0_.clone().0.left.clone()), encoder_collect_ord_vars(v0_.clone().0.right.clone()))},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      encoder_collect_ord_vars(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      encoder_collect_ord_vars(v0_.clone())},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat(Vec::from([
        encoder_collect_type_vars_from_type(v0_.clone().0.keys.clone()),
        encoder_collect_ord_vars(v0_.clone().0.keys.clone()),
        encoder_collect_ord_vars(v0_.clone().0.values.clone())]))},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      encoder_collect_ord_vars(v0_.clone())},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat2(encoder_collect_ord_vars(v0_.clone().0.first.clone()), encoder_collect_ord_vars(v0_.clone().0.second.clone()))},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|ft: crate::hydra::core::FieldType| encoder_collect_ord_vars(ft.clone().0.type_.clone()), v0_.clone()))},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat2(encoder_collect_type_vars_from_type(v0_.clone()), encoder_collect_ord_vars(v0_.clone()))},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|ft: crate::hydra::core::FieldType| encoder_collect_ord_vars(ft.clone().0.type_.clone()), v0_.clone()))},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      encoder_collect_ord_vars(v0_.clone())},
    _ => Vec::from([])}}

pub fn encoder_collect_type_vars_from_type(typ: crate::hydra::core::Type) -> Vec<crate::hydra::core::Name> {
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      encoder_collect_type_vars_from_type(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat2(encoder_collect_type_vars_from_type(v0_.clone().0.function.clone()), encoder_collect_type_vars_from_type(v0_.clone().0.argument.clone()))},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      encoder_collect_type_vars_from_type(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      encoder_collect_type_vars_from_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat2(encoder_collect_type_vars_from_type(v0_.clone().0.keys.clone()), encoder_collect_type_vars_from_type(v0_.clone().0.values.clone()))},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      encoder_collect_type_vars_from_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat2(encoder_collect_type_vars_from_type(v0_.clone().0.first.clone()), encoder_collect_type_vars_from_type(v0_.clone().0.second.clone()))},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|ft: crate::hydra::core::FieldType| encoder_collect_type_vars_from_type(ft.clone().0.type_.clone()), v0_.clone()))},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      encoder_collect_type_vars_from_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|ft: crate::hydra::core::FieldType| encoder_collect_type_vars_from_type(ft.clone().0.type_.clone()), v0_.clone()))},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone()])},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      encoder_collect_type_vars_from_type(v0_.clone())},
    _ => Vec::from([])}}

pub fn encoder_full_result_type(typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      encoder_full_result_type(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
        function: encoder_full_result_type(v0_.clone().0.function.clone()),
        argument: v0_.clone().0.argument.clone()})))))},
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
        left: encoder_full_result_type(v0_.clone().0.left.clone()),
        right: encoder_full_result_type(v0_.clone().0.right.clone())})))))},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
        function: encoder_full_result_type(v0_.clone().0.body.clone()),
        argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v0_.clone().0.parameter.clone())))})))))},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(encoder_full_result_type(v0_.clone()))))},
    crate::hydra::core::Type_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))))))},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
        keys: encoder_full_result_type(v0_.clone().0.keys.clone()),
        values: encoder_full_result_type(v0_.clone().0.values.clone())})))))},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(encoder_full_result_type(v0_.clone()))))},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Pair(crate::hydra::core::PairType(Rc::new(crate::hydra::core::PairType_Variant {
        first: encoder_full_result_type(v0_.clone().0.first.clone()),
        second: encoder_full_result_type(v0_.clone().0.second.clone())})))))},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(encoder_full_result_type(v0_.clone()))))},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))},
    crate::hydra::core::Type_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v0_.clone())))},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))},
    _ => crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))}}

pub fn encoder_full_result_type_named(ename: crate::hydra::core::Name, typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      encoder_full_result_type_named(ename.clone(), v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
        function: encoder_full_result_type(v0_.clone().0.function.clone()),
        argument: v0_.clone().0.argument.clone()})))))},
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
        left: encoder_full_result_type(v0_.clone().0.left.clone()),
        right: encoder_full_result_type(v0_.clone().0.right.clone())})))))},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
        function: encoder_full_result_type_named(ename.clone(), v0_.clone().0.body.clone()),
        argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v0_.clone().0.parameter.clone())))})))))},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(encoder_full_result_type(v0_.clone()))))},
    crate::hydra::core::Type_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))))))},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
        keys: encoder_full_result_type(v0_.clone().0.keys.clone()),
        values: encoder_full_result_type(v0_.clone().0.values.clone())})))))},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(encoder_full_result_type(v0_.clone()))))},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Pair(crate::hydra::core::PairType(Rc::new(crate::hydra::core::PairType_Variant {
        first: encoder_full_result_type(v0_.clone().0.first.clone()),
        second: encoder_full_result_type(v0_.clone().0.second.clone())})))))},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(ename.clone())))},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(encoder_full_result_type(v0_.clone()))))},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(ename.clone())))},
    crate::hydra::core::Type_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v0_.clone())))},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(ename.clone())))},
    _ => crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))}}

pub fn encoder_type(typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  let result_type = encoder_full_result_type(typ.clone()) ;
  let base_type = crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
    domain: result_type.clone(),
    codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))}))))) ;
  prepend_forall_encoders(base_type.clone(), typ.clone())}

pub fn encoder_type_named(ename: crate::hydra::core::Name, typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  let result_type = encoder_full_result_type_named(ename.clone(), typ.clone()) ;
  let base_type = crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
    domain: result_type.clone(),
    codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))}))))) ;
  prepend_forall_encoders(base_type.clone(), typ.clone())}

pub fn encoder_type_scheme(typ: crate::hydra::core::Type) -> crate::hydra::core::TypeScheme {
  let type_vars = encoder_collect_forall_variables(typ.clone()) ;
  let encoder_fun_type = encoder_type(typ.clone()) ;
  let all_ord_vars = encoder_collect_ord_vars(typ.clone()) ;
  let ord_vars = crate::hydra::lib::lists::filter(|v: crate::hydra::core::Name| crate::hydra::lib::lists::elem(v.clone(), type_vars.clone()), all_ord_vars.clone()) ;
  let constraints = crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(ord_vars.clone()), None, Some(crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|v: crate::hydra::core::Name| (v.clone(), crate::hydra::core::TypeVariableMetadata(Rc::new(crate::hydra::core::TypeVariableMetadata_Variant {
    classes: crate::hydra::lib::sets::singleton(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("ordering")))))}))), ord_vars.clone())))) ;
  crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
    variables: type_vars.clone(),
    type_: encoder_fun_type.clone(),
    constraints: constraints.clone()}))}

pub fn encoder_type_scheme_named(ename: crate::hydra::core::Name, typ: crate::hydra::core::Type) -> crate::hydra::core::TypeScheme {
  let type_vars = encoder_collect_forall_variables(typ.clone()) ;
  let encoder_fun_type = encoder_type_named(ename.clone(), typ.clone()) ;
  let all_ord_vars = encoder_collect_ord_vars(typ.clone()) ;
  let ord_vars = crate::hydra::lib::lists::filter(|v: crate::hydra::core::Name| crate::hydra::lib::lists::elem(v.clone(), type_vars.clone()), all_ord_vars.clone()) ;
  let constraints = crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(ord_vars.clone()), None, Some(crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|v: crate::hydra::core::Name| (v.clone(), crate::hydra::core::TypeVariableMetadata(Rc::new(crate::hydra::core::TypeVariableMetadata_Variant {
    classes: crate::hydra::lib::sets::singleton(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("ordering")))))}))), ord_vars.clone())))) ;
  crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
    variables: type_vars.clone(),
    type_: encoder_fun_type.clone(),
    constraints: constraints.clone()}))}

pub fn prepend_forall_encoders(base_type: crate::hydra::core::Type, typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      prepend_forall_encoders(base_type.clone(), v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
        domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
          domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v0_.clone().0.parameter.clone()))),
          codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))}))))),
        codomain: prepend_forall_encoders(base_type.clone(), v0_.clone().0.body.clone())})))))},
    _ => base_type.clone()}}

pub fn encode_field_value(type_name: crate::hydra::core::Name, field_name: crate::hydra::core::Name, field_type: crate::hydra::core::Type) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("y")))),
    domain: None,
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
      type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
      field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("union")))),
        term: encode_injection(type_name.clone(), field_name.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: encode_type(field_type.clone()),
          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("y")))))))}))))))}))})))))}))))))))}

pub fn encode_float_value(float_type: crate::hydra::core::FloatType, val_term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.FloatValue")))),
    field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
      name: match &*float_type.clone().0 {
        crate::hydra::core::FloatType_Variant::Bigfloat (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("bigfloat"))))},
        crate::hydra::core::FloatType_Variant::Float32 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float32"))))},
        crate::hydra::core::FloatType_Variant::Float64 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float64"))))}},
      term: val_term.clone()}))})))))}

pub fn encode_injection(type_name: crate::hydra::core::Name, field_name: crate::hydra::core::Name, field_term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Injection")))),
    fields: Vec::from([
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeName")))),
        term: encode_name(type_name.clone())})),
      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("field")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Field")))),
          fields: Vec::from([
            crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
              name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
              term: encode_name(field_name.clone())})),
            crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
              name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("term")))),
              term: field_term.clone()}))])})))))}))])})))))}

pub fn encode_integer_value(int_type: crate::hydra::core::IntegerType, val_term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.IntegerValue")))),
    field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
      name: match &*int_type.clone().0 {
        crate::hydra::core::IntegerType_Variant::Bigint (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("bigint"))))},
        crate::hydra::core::IntegerType_Variant::Int8 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int8"))))},
        crate::hydra::core::IntegerType_Variant::Int16 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int16"))))},
        crate::hydra::core::IntegerType_Variant::Int32 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int32"))))},
        crate::hydra::core::IntegerType_Variant::Int64 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int64"))))},
        crate::hydra::core::IntegerType_Variant::Uint8 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint8"))))},
        crate::hydra::core::IntegerType_Variant::Uint16 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint16"))))},
        crate::hydra::core::IntegerType_Variant::Uint32 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint32"))))},
        crate::hydra::core::IntegerType_Variant::Uint64 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint64"))))}},
      term: val_term.clone()}))})))))}

pub fn encode_list_type(elem_type: crate::hydra::core::Type) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("xs")))),
    domain: None,
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
      type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
      field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("list")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.lists.map")))))))))),
            argument: encode_type(elem_type.clone())}))))),
          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("xs")))))))})))))}))})))))}))))))))}

pub fn encode_literal_type(v1: crate::hydra::core::LiteralType) -> crate::hydra::core::Term {
  match &*v1.clone().0 {
    crate::hydra::core::LiteralType_Variant::Binary (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))),
        domain: None,
        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
          field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
              type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
              field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("binary")))),
                term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))))))}))})))))}))})))))}))))))))},
    crate::hydra::core::LiteralType_Variant::Boolean (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))),
        domain: None,
        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
          field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
              type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
              field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("boolean")))),
                term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))))))}))})))))}))})))))}))))))))},
    crate::hydra::core::LiteralType_Variant::String (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))),
        domain: None,
        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
          field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
              type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
              field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("string")))),
                term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))))))}))})))))}))})))))}))))))))},
    crate::hydra::core::LiteralType_Variant::Integer (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))),
        domain: None,
        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
          field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
              type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
              field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("integer")))),
                term: encode_integer_value(v0_.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x"))))))))}))})))))}))})))))}))))))))},
    crate::hydra::core::LiteralType_Variant::Float (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))),
        domain: None,
        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
          field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
              type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
              field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float")))),
                term: encode_float_value(v0_.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x"))))))))}))})))))}))})))))}))))))))},
    _ => crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))),
      domain: None,
      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))))))}))))))))}}

pub fn encode_either_type(et: crate::hydra::core::EitherType) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("e")))),
    domain: None,
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
      type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
      field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("either")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.bimap")))))))))),
              argument: encode_type(et.clone().0.left.clone())}))))),
            argument: encode_type(et.clone().0.right.clone())}))))),
          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("e")))))))})))))}))})))))}))))))))}

pub fn encode_forall_type(ft: crate::hydra::core::ForallType) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
    parameter: encode_binding_name(ft.clone().0.parameter.clone()),
    domain: None,
    body: encode_type(ft.clone().0.body.clone())}))))))))}

pub fn encode_map_type(mt: crate::hydra::core::MapType) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("m")))),
    domain: None,
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
      type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
      field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("map")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.bimap")))))))))),
              argument: encode_type(mt.clone().0.keys.clone())}))))),
            argument: encode_type(mt.clone().0.values.clone())}))))),
          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("m")))))))})))))}))})))))}))))))))}

pub fn encode_optional_type(elem_type: crate::hydra::core::Type) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("opt")))),
    domain: None,
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
      type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
      field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("maybe")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maybes.map")))))))))),
            argument: encode_type(elem_type.clone())}))))),
          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("opt")))))))})))))}))})))))}))))))))}

pub fn encode_pair_type(pt: crate::hydra::core::PairType) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("p")))),
    domain: None,
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
      type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
      field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("pair")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.pairs.bimap")))))))))),
              argument: encode_type(pt.clone().0.first.clone())}))))),
            argument: encode_type(pt.clone().0.second.clone())}))))),
          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("p")))))))})))))}))})))))}))))))))}

pub fn encode_module(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, mod_: crate::hydra::module::Module) -> Either<crate::hydra::context::InContext, Option<crate::hydra::module::Module>> {
  crate::hydra::lib::eithers::bind(filter_type_bindings(cx.clone(), graph.clone(), mod_.clone().0.elements.clone()), |type_bindings: Vec<crate::hydra::core::Binding>| crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(type_bindings.clone()), Right(None), crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(|b: crate::hydra::core::Binding| crate::hydra::lib::eithers::bimap(|ic: crate::hydra::context::InContext| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(ic.clone().0.object.clone().0.0.clone())))))),
    context: ic.clone().0.context.clone()})), |x: crate::hydra::core::Binding| x.clone(), encode_binding(cx.clone(), graph.clone(), b.clone())), type_bindings.clone()), |encoded_bindings: Vec<crate::hydra::core::Binding>| Right(Some(crate::hydra::module::Module(Rc::new(crate::hydra::module::Module_Variant {
    namespace: encode_namespace(mod_.clone().0.namespace.clone()),
    elements: encoded_bindings.clone(),
    term_dependencies: crate::hydra::lib::lists::nub(crate::hydra::lib::lists::concat2(crate::hydra::lib::lists::map(encode_namespace, mod_.clone().0.type_dependencies.clone()), crate::hydra::lib::lists::map(encode_namespace, mod_.clone().0.term_dependencies.clone()))),
    type_dependencies: Vec::from([
      mod_.clone().0.namespace.clone()]),
    description: Some(crate::hydra::lib::strings::cat(Vec::from([
      String::from("Term encoders for "),
      mod_.clone().0.namespace.clone().0.0.clone()])))})))))))}

pub fn encode_name(n: crate::hydra::core::Name) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name")))),
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(n.clone().0.0.clone()))))))})))))}

pub fn encode_namespace(ns: crate::hydra::module::Namespace) -> crate::hydra::module::Namespace {
  crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(crate::hydra::lib::strings::cat(Vec::from([
    String::from("hydra.encode."),
    crate::hydra::lib::strings::intercalate(String::from("."), crate::hydra::lib::lists::tail(crate::hydra::lib::strings::split_on(String::from("."), ns.clone().0.0.clone())))])))))}

pub fn encode_record_type(rt: Vec<crate::hydra::core::FieldType>) -> crate::hydra::core::Term {
  encode_record_type_named(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unknown")))), rt.clone())}

pub fn encode_record_type_named(ename: crate::hydra::core::Name, rt: Vec<crate::hydra::core::FieldType>) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))),
    domain: None,
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
      type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
      field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("record")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Record")))),
          fields: Vec::from([
            crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
              name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeName")))),
              term: encode_name(ename.clone())})),
            crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
              name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("fields")))),
              term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|ft: crate::hydra::core::FieldType| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
                type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Field")))),
                fields: Vec::from([
                  crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                    name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name")))),
                    term: encode_name(ft.clone().0.name.clone())})),
                  crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                    name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("term")))),
                    term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                      function: encode_type(ft.clone().0.type_.clone()),
                      argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                        function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Record(crate::hydra::core::Projection(Rc::new(crate::hydra::core::Projection_Variant {
                          type_name: ename.clone(),
                          field: ft.clone().0.name.clone()}))))))))))),
                        argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))))))})))))})))))}))])}))))), rt.clone()))))}))])})))))}))})))))}))))))))}

pub fn encode_set_type(elem_type: crate::hydra::core::Type) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("s")))),
    domain: None,
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
      type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
      field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("set")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.sets.map")))))))))),
            argument: encode_type(elem_type.clone())}))))),
          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("s")))))))})))))}))})))))}))))))))}

pub fn encode_type(v1: crate::hydra::core::Type) -> crate::hydra::core::Term {
  match &*v1.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      encode_type(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
        function: encode_type(v0_.clone().0.function.clone()),
        argument: encode_type(v0_.clone().0.argument.clone())})))))},
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      encode_either_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      encode_forall_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))),
        domain: None,
        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))))))}))))))))},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      encode_list_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      encode_literal_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      encode_map_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      encode_optional_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      encode_pair_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      encode_record_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      encode_set_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      encode_union_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      encode_wrapped_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("_")))),
        domain: None,
        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
          field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unit")))),
            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))}))))))))},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(encode_binding_name(v0_.clone()))))},
    _ => crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))),
      domain: None,
      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))))))}))))))))}}

pub fn encode_type_named(ename: crate::hydra::core::Name, typ: crate::hydra::core::Type) -> crate::hydra::core::Term {
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      encode_type_named(ename.clone(), v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
        function: encode_type(v0_.clone().0.function.clone()),
        argument: encode_type(v0_.clone().0.argument.clone())})))))},
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      encode_either_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
        parameter: encode_binding_name(v0_.clone().0.parameter.clone()),
        domain: None,
        body: encode_type_named(ename.clone(), v0_.clone().0.body.clone())}))))))))},
    crate::hydra::core::Type_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))),
        domain: None,
        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))))))}))))))))},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      encode_list_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      encode_literal_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      encode_map_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      encode_optional_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      encode_pair_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      encode_record_type_named(ename.clone(), v0_.clone())},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      encode_set_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      encode_union_type_named(ename.clone(), v0_.clone())},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      encode_wrapped_type_named(ename.clone(), v0_.clone())},
    crate::hydra::core::Type_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("_")))),
        domain: None,
        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
          field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unit")))),
            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))}))))))))},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(encode_binding_name(v0_.clone()))))},
    _ => crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))),
      domain: None,
      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))))))}))))))))}}

pub fn encode_union_type(rt: Vec<crate::hydra::core::FieldType>) -> crate::hydra::core::Term {
  encode_union_type_named(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unknown")))), rt.clone())}

pub fn encode_union_type_named(ename: crate::hydra::core::Name, rt: Vec<crate::hydra::core::FieldType>) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
    type_name: ename.clone(),
    default_: None,
    cases: crate::hydra::lib::lists::map(|ft: crate::hydra::core::FieldType| crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
      name: ft.clone().0.name.clone(),
      term: encode_field_value(ename.clone(), ft.clone().0.name.clone(), ft.clone().0.type_.clone())})), rt.clone())})))))))))))}

pub fn encode_wrapped_type(wt: crate::hydra::core::Type) -> crate::hydra::core::Term {
  encode_wrapped_type_named(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unknown")))), wt.clone())}

pub fn encode_wrapped_type_named(ename: crate::hydra::core::Name, wt: crate::hydra::core::Type) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))),
    domain: None,
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
      type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
      field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("wrap")))),
        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.WrappedTerm")))),
          fields: Vec::from([
            crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
              name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeName")))),
              term: encode_name(ename.clone())})),
            crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
              name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("body")))),
              term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: encode_type(wt.clone()),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Wrap(ename.clone()))))))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("x")))))))})))))})))))}))])})))))}))})))))}))))))))}

pub fn filter_type_bindings(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, bindings: Vec<crate::hydra::core::Binding>) -> Either<crate::hydra::context::InContext, Vec<crate::hydra::core::Binding>> {
  crate::hydra::lib::eithers::map(crate::hydra::lib::maybes::cat, crate::hydra::lib::eithers::map_list(|v1: crate::hydra::core::Binding| is_encodable_binding(cx.clone(), graph.clone(), v1.clone()), crate::hydra::lib::lists::filter(crate::hydra::annotations::is_native_type, bindings.clone())))}

pub fn is_encodable_binding(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, b: crate::hydra::core::Binding) -> Either<crate::hydra::context::InContext, Option<crate::hydra::core::Binding>> {
  crate::hydra::lib::eithers::bind(crate::hydra::schemas::is_serializable_by_name(cx.clone(), graph.clone(), b.clone().0.name.clone()), |serializable: bool| Right(crate::hydra::lib::logic::if_else(serializable.clone(), Some(b.clone()), None)))}

pub fn is_unit_type(v1: crate::hydra::core::Type) -> bool {
  match &*v1.clone().0 {
    crate::hydra::core::Type_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    _ => false}}
