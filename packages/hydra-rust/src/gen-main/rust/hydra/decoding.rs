#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::annotations::*;
use crate::hydra::formatting::*;
use crate::hydra::extract::helpers::*;
use crate::hydra::lexical::*;
use crate::hydra::names::*;
use crate::hydra::rewriting::*;
use crate::hydra::schemas::*;
use crate::hydra::show::core::*;
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

pub fn collect_forall_variables(typ: crate::hydra::core::Type) -> Vec<crate::hydra::core::Name> {
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      collect_forall_variables(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::cons(v0_.clone().0.parameter.clone(), collect_forall_variables(v0_.clone().0.body.clone()))},
    _ => Vec::from([])}}

pub fn collect_ord_constrained_variables(typ: crate::hydra::core::Type) -> Vec<crate::hydra::core::Name> {
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      collect_ord_constrained_variables(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat2(collect_ord_constrained_variables(v0_.clone().0.function.clone()), collect_ord_constrained_variables(v0_.clone().0.argument.clone()))},
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat2(collect_ord_constrained_variables(v0_.clone().0.left.clone()), collect_ord_constrained_variables(v0_.clone().0.right.clone()))},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      collect_ord_constrained_variables(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      collect_ord_constrained_variables(v0_.clone())},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat(Vec::from([
        collect_type_variables_from_type(v0_.clone().0.keys.clone()),
        collect_ord_constrained_variables(v0_.clone().0.keys.clone()),
        collect_ord_constrained_variables(v0_.clone().0.values.clone())]))},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      collect_ord_constrained_variables(v0_.clone())},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat2(collect_ord_constrained_variables(v0_.clone().0.first.clone()), collect_ord_constrained_variables(v0_.clone().0.second.clone()))},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|ft: crate::hydra::core::FieldType| collect_ord_constrained_variables(ft.clone().0.type_.clone()), v0_.clone()))},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat2(collect_type_variables_from_type(v0_.clone()), collect_ord_constrained_variables(v0_.clone()))},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|ft: crate::hydra::core::FieldType| collect_ord_constrained_variables(ft.clone().0.type_.clone()), v0_.clone()))},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      collect_ord_constrained_variables(v0_.clone())},
    _ => Vec::from([])}}

pub fn collect_type_variables(typ: crate::hydra::core::Type) -> Vec<crate::hydra::core::Name> {
  collect_forall_variables(typ.clone())}

pub fn collect_type_variables_from_type(typ: crate::hydra::core::Type) -> Vec<crate::hydra::core::Name> {
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      collect_type_variables_from_type(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat2(collect_type_variables_from_type(v0_.clone().0.function.clone()), collect_type_variables_from_type(v0_.clone().0.argument.clone()))},
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat2(collect_type_variables_from_type(v0_.clone().0.left.clone()), collect_type_variables_from_type(v0_.clone().0.right.clone()))},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      collect_type_variables_from_type(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      collect_type_variables_from_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat2(collect_type_variables_from_type(v0_.clone().0.keys.clone()), collect_type_variables_from_type(v0_.clone().0.values.clone()))},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      collect_type_variables_from_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat2(collect_type_variables_from_type(v0_.clone().0.first.clone()), collect_type_variables_from_type(v0_.clone().0.second.clone()))},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|ft: crate::hydra::core::FieldType| collect_type_variables_from_type(ft.clone().0.type_.clone()), v0_.clone()))},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      collect_type_variables_from_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|ft: crate::hydra::core::FieldType| collect_type_variables_from_type(ft.clone().0.type_.clone()), v0_.clone()))},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone()])},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      collect_type_variables_from_type(v0_.clone())},
    _ => Vec::from([])}}

pub fn decode_binding(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, b: crate::hydra::core::Binding) -> Either<crate::hydra::context::InContext, crate::hydra::core::Binding> {
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|_wc_e: crate::hydra::error::DecodingError| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: _wc_e.clone(),
    context: cx.clone()})), |_wc_a: crate::hydra::core::Type| _wc_a.clone(), crate::hydra::decode::core::type_(graph.clone(), b.clone().0.term.clone())), |typ: crate::hydra::core::Type| Right(crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
    name: decode_binding_name(b.clone().0.name.clone()),
    term: decode_type_named(b.clone().0.name.clone(), typ.clone()),
    type_: Some(decoder_type_scheme_named(b.clone().0.name.clone(), typ.clone()))}))))}

pub fn decode_binding_name(n: crate::hydra::core::Name) -> crate::hydra::core::Name {
  crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::not(crate::hydra::lib::lists::null(crate::hydra::lib::lists::tail(crate::hydra::lib::strings::split_on(String::from("."), n.clone().0.0.clone())))), crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::intercalate(String::from("."), crate::hydra::lib::lists::concat2(Vec::from([
    String::from("hydra"),
    String::from("decode")]), crate::hydra::lib::lists::concat2(crate::hydra::lib::lists::tail(crate::hydra::lib::lists::init(crate::hydra::lib::strings::split_on(String::from("."), n.clone().0.0.clone()))), Vec::from([
    crate::hydra::formatting::decapitalize(crate::hydra::names::local_name_of(n.clone()))]))))))), crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::formatting::decapitalize(crate::hydra::names::local_name_of(n.clone()))))))}

pub fn decode_either_type(et: crate::hydra::core::EitherType) -> crate::hydra::core::Term {
  let left_decoder = decode_type(et.clone().0.left.clone()) ;
  let right_decoder = decode_type(et.clone().0.right.clone()) ;
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.extract.helpers.decodeEither"))))))),
      argument: left_decoder.clone()}))))),
    argument: right_decoder.clone()})))))}

pub fn decode_forall_type(ft: crate::hydra::core::ForallType) -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
    parameter: decode_binding_name(ft.clone().0.parameter.clone()),
    domain: None,
    body: decode_type(ft.clone().0.body.clone())}))))))))}

pub fn decode_list_type(elem_type: crate::hydra::core::Type) -> crate::hydra::core::Term {
  let elem_decoder = decode_type(elem_type.clone()) ;
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.extract.helpers.decodeList"))))))),
    argument: elem_decoder.clone()})))))}

pub fn decode_literal_type(lt: crate::hydra::core::LiteralType) -> crate::hydra::core::Term {
  match &*lt.clone().0 {
    crate::hydra::core::LiteralType_Variant::Binary (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
        domain: None,
        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
          parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
          domain: None,
          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                  parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
                  domain: None,
                  body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
              argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
                domain: None,
                body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                    default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                      type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected literal")))))))})))))))))),
                    cases: Vec::from([
                      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
                        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                          parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))),
                          domain: None,
                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                              type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
                              default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected binary literal")))))))})))))))))),
                              cases: Vec::from([
                                crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                  name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("binary")))),
                                  term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("b")))),
                                    domain: None,
                                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("b")))))))))))}))))))))}))])}))))))))))),
                            argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))))))})))))}))))))))}))])}))))))))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
            argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
              argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))},
    crate::hydra::core::LiteralType_Variant::Boolean (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
        domain: None,
        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
          parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
          domain: None,
          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                  parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
                  domain: None,
                  body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
              argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
                domain: None,
                body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                    default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                      type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected literal")))))))})))))))))),
                    cases: Vec::from([
                      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
                        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                          parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))),
                          domain: None,
                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                              type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
                              default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected boolean literal")))))))})))))))))),
                              cases: Vec::from([
                                crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                  name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("boolean")))),
                                  term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("b")))),
                                    domain: None,
                                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("b")))))))))))}))))))))}))])}))))))))))),
                            argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))))))})))))}))))))))}))])}))))))))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
            argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
              argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))},
    crate::hydra::core::LiteralType_Variant::Float (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::FloatType_Variant::Bigfloat (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
            domain: None,
            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
              domain: None,
              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
                      domain: None,
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
                    domain: None,
                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected literal")))))))})))))))))),
                        cases: Vec::from([
                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))),
                              domain: None,
                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                  type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
                                  default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                      String::from("expected "),
                                      String::from("bigfloat"),
                                      String::from(" literal")]))))))))})))))))))),
                                  cases: Vec::from([
                                    crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                      name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float")))),
                                      term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.FloatValue")))),
                                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                            String::from("expected "),
                                            String::from("bigfloat"),
                                            String::from(" value")]))))))))})))))))))),
                                        cases: Vec::from([
                                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("bigfloat")))),
                                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("f")))),
                                              domain: None,
                                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("f")))))))))))}))))))))}))])})))))))))))}))])}))))))))))),
                                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))))))})))))}))))))))}))])}))))))))))),
                      argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))},
        crate::hydra::core::FloatType_Variant::Float32 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
            domain: None,
            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
              domain: None,
              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
                      domain: None,
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
                    domain: None,
                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected literal")))))))})))))))))),
                        cases: Vec::from([
                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))),
                              domain: None,
                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                  type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
                                  default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                      String::from("expected "),
                                      String::from("float32"),
                                      String::from(" literal")]))))))))})))))))))),
                                  cases: Vec::from([
                                    crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                      name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float")))),
                                      term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.FloatValue")))),
                                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                            String::from("expected "),
                                            String::from("float32"),
                                            String::from(" value")]))))))))})))))))))),
                                        cases: Vec::from([
                                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float32")))),
                                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("f")))),
                                              domain: None,
                                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("f")))))))))))}))))))))}))])})))))))))))}))])}))))))))))),
                                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))))))})))))}))))))))}))])}))))))))))),
                      argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))},
        crate::hydra::core::FloatType_Variant::Float64 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
            domain: None,
            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
              domain: None,
              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
                      domain: None,
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
                    domain: None,
                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected literal")))))))})))))))))),
                        cases: Vec::from([
                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))),
                              domain: None,
                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                  type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
                                  default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                      String::from("expected "),
                                      String::from("float64"),
                                      String::from(" literal")]))))))))})))))))))),
                                  cases: Vec::from([
                                    crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                      name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float")))),
                                      term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.FloatValue")))),
                                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                            String::from("expected "),
                                            String::from("float64"),
                                            String::from(" value")]))))))))})))))))))),
                                        cases: Vec::from([
                                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float64")))),
                                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("f")))),
                                              domain: None,
                                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("f")))))))))))}))))))))}))])})))))))))))}))])}))))))))))),
                                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))))))})))))}))))))))}))])}))))))))))),
                      argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))}}},
    crate::hydra::core::LiteralType_Variant::Integer (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::IntegerType_Variant::Bigint (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
            domain: None,
            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
              domain: None,
              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
                      domain: None,
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
                    domain: None,
                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected literal")))))))})))))))))),
                        cases: Vec::from([
                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))),
                              domain: None,
                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                  type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
                                  default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                      String::from("expected "),
                                      String::from("bigint"),
                                      String::from(" literal")]))))))))})))))))))),
                                  cases: Vec::from([
                                    crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                      name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("integer")))),
                                      term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.IntegerValue")))),
                                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                            String::from("expected "),
                                            String::from("bigint"),
                                            String::from(" value")]))))))))})))))))))),
                                        cases: Vec::from([
                                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("bigint")))),
                                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))),
                                              domain: None,
                                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))))))))))}))))))))}))])})))))))))))}))])}))))))))))),
                                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))))))})))))}))))))))}))])}))))))))))),
                      argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))},
        crate::hydra::core::IntegerType_Variant::Int8 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
            domain: None,
            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
              domain: None,
              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
                      domain: None,
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
                    domain: None,
                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected literal")))))))})))))))))),
                        cases: Vec::from([
                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))),
                              domain: None,
                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                  type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
                                  default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                      String::from("expected "),
                                      String::from("int8"),
                                      String::from(" literal")]))))))))})))))))))),
                                  cases: Vec::from([
                                    crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                      name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("integer")))),
                                      term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.IntegerValue")))),
                                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                            String::from("expected "),
                                            String::from("int8"),
                                            String::from(" value")]))))))))})))))))))),
                                        cases: Vec::from([
                                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int8")))),
                                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))),
                                              domain: None,
                                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))))))))))}))))))))}))])})))))))))))}))])}))))))))))),
                                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))))))})))))}))))))))}))])}))))))))))),
                      argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))},
        crate::hydra::core::IntegerType_Variant::Int16 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
            domain: None,
            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
              domain: None,
              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
                      domain: None,
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
                    domain: None,
                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected literal")))))))})))))))))),
                        cases: Vec::from([
                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))),
                              domain: None,
                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                  type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
                                  default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                      String::from("expected "),
                                      String::from("int16"),
                                      String::from(" literal")]))))))))})))))))))),
                                  cases: Vec::from([
                                    crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                      name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("integer")))),
                                      term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.IntegerValue")))),
                                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                            String::from("expected "),
                                            String::from("int16"),
                                            String::from(" value")]))))))))})))))))))),
                                        cases: Vec::from([
                                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int16")))),
                                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))),
                                              domain: None,
                                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))))))))))}))))))))}))])})))))))))))}))])}))))))))))),
                                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))))))})))))}))))))))}))])}))))))))))),
                      argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))},
        crate::hydra::core::IntegerType_Variant::Int32 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
            domain: None,
            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
              domain: None,
              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
                      domain: None,
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
                    domain: None,
                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected literal")))))))})))))))))),
                        cases: Vec::from([
                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))),
                              domain: None,
                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                  type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
                                  default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                      String::from("expected "),
                                      String::from("int32"),
                                      String::from(" literal")]))))))))})))))))))),
                                  cases: Vec::from([
                                    crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                      name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("integer")))),
                                      term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.IntegerValue")))),
                                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                            String::from("expected "),
                                            String::from("int32"),
                                            String::from(" value")]))))))))})))))))))),
                                        cases: Vec::from([
                                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int32")))),
                                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))),
                                              domain: None,
                                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))))))))))}))))))))}))])})))))))))))}))])}))))))))))),
                                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))))))})))))}))))))))}))])}))))))))))),
                      argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))},
        crate::hydra::core::IntegerType_Variant::Int64 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
            domain: None,
            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
              domain: None,
              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
                      domain: None,
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
                    domain: None,
                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected literal")))))))})))))))))),
                        cases: Vec::from([
                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))),
                              domain: None,
                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                  type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
                                  default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                      String::from("expected "),
                                      String::from("int64"),
                                      String::from(" literal")]))))))))})))))))))),
                                  cases: Vec::from([
                                    crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                      name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("integer")))),
                                      term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.IntegerValue")))),
                                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                            String::from("expected "),
                                            String::from("int64"),
                                            String::from(" value")]))))))))})))))))))),
                                        cases: Vec::from([
                                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int64")))),
                                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))),
                                              domain: None,
                                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))))))))))}))))))))}))])})))))))))))}))])}))))))))))),
                                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))))))})))))}))))))))}))])}))))))))))),
                      argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))},
        crate::hydra::core::IntegerType_Variant::Uint8 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
            domain: None,
            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
              domain: None,
              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
                      domain: None,
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
                    domain: None,
                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected literal")))))))})))))))))),
                        cases: Vec::from([
                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))),
                              domain: None,
                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                  type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
                                  default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                      String::from("expected "),
                                      String::from("uint8"),
                                      String::from(" literal")]))))))))})))))))))),
                                  cases: Vec::from([
                                    crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                      name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("integer")))),
                                      term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.IntegerValue")))),
                                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                            String::from("expected "),
                                            String::from("uint8"),
                                            String::from(" value")]))))))))})))))))))),
                                        cases: Vec::from([
                                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint8")))),
                                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))),
                                              domain: None,
                                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))))))))))}))))))))}))])})))))))))))}))])}))))))))))),
                                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))))))})))))}))))))))}))])}))))))))))),
                      argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))},
        crate::hydra::core::IntegerType_Variant::Uint16 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
            domain: None,
            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
              domain: None,
              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
                      domain: None,
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
                    domain: None,
                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected literal")))))))})))))))))),
                        cases: Vec::from([
                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))),
                              domain: None,
                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                  type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
                                  default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                      String::from("expected "),
                                      String::from("uint16"),
                                      String::from(" literal")]))))))))})))))))))),
                                  cases: Vec::from([
                                    crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                      name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("integer")))),
                                      term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.IntegerValue")))),
                                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                            String::from("expected "),
                                            String::from("uint16"),
                                            String::from(" value")]))))))))})))))))))),
                                        cases: Vec::from([
                                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint16")))),
                                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))),
                                              domain: None,
                                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))))))))))}))))))))}))])})))))))))))}))])}))))))))))),
                                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))))))})))))}))))))))}))])}))))))))))),
                      argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))},
        crate::hydra::core::IntegerType_Variant::Uint32 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
            domain: None,
            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
              domain: None,
              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
                      domain: None,
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
                    domain: None,
                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected literal")))))))})))))))))),
                        cases: Vec::from([
                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))),
                              domain: None,
                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                  type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
                                  default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                      String::from("expected "),
                                      String::from("uint32"),
                                      String::from(" literal")]))))))))})))))))))),
                                  cases: Vec::from([
                                    crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                      name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("integer")))),
                                      term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.IntegerValue")))),
                                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                            String::from("expected "),
                                            String::from("uint32"),
                                            String::from(" value")]))))))))})))))))))),
                                        cases: Vec::from([
                                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint32")))),
                                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))),
                                              domain: None,
                                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))))))))))}))))))))}))])})))))))))))}))])}))))))))))),
                                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))))))})))))}))))))))}))])}))))))))))),
                      argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))},
        crate::hydra::core::IntegerType_Variant::Uint64 (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
            domain: None,
            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
              domain: None,
              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
                      domain: None,
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
                    domain: None,
                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected literal")))))))})))))))))),
                        cases: Vec::from([
                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))),
                              domain: None,
                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                  type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
                                  default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                      String::from("expected "),
                                      String::from("uint64"),
                                      String::from(" literal")]))))))))})))))))))),
                                  cases: Vec::from([
                                    crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                      name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("integer")))),
                                      term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.IntegerValue")))),
                                        default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::strings::cat(Vec::from([
                                            String::from("expected "),
                                            String::from("uint64"),
                                            String::from(" value")]))))))))})))))))))),
                                        cases: Vec::from([
                                          crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint64")))),
                                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))),
                                              domain: None,
                                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("i")))))))))))}))))))))}))])})))))))))))}))])}))))))))))),
                                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))))))})))))}))))))))}))])}))))))))))),
                      argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
                    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))}}},
    crate::hydra::core::LiteralType_Variant::String (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
        domain: None,
        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
          parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
          domain: None,
          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                  parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
                  domain: None,
                  body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
              argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
                domain: None,
                body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                    default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                      type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected literal")))))))})))))))))),
                    cases: Vec::from([
                      crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))),
                        term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                          parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))),
                          domain: None,
                          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                              type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))),
                              default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected string literal")))))))})))))))))),
                              cases: Vec::from([
                                crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                  name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("string")))),
                                  term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("s")))),
                                    domain: None,
                                    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("s")))))))))))}))))))))}))])}))))))))))),
                            argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("v")))))))})))))}))))))))}))])}))))))))))),
                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
            argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
              argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))}}}

pub fn decode_map_type(mt: crate::hydra::core::MapType) -> crate::hydra::core::Term {
  let key_decoder = decode_type(mt.clone().0.keys.clone()) ;
  let val_decoder = decode_type(mt.clone().0.values.clone()) ;
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.extract.helpers.decodeMap"))))))),
      argument: key_decoder.clone()}))))),
    argument: val_decoder.clone()})))))}

pub fn decode_maybe_type(elem_type: crate::hydra::core::Type) -> crate::hydra::core::Term {
  let elem_decoder = decode_type(elem_type.clone()) ;
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.extract.helpers.decodeMaybe"))))))),
    argument: elem_decoder.clone()})))))}

pub fn decode_module(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, mod_: crate::hydra::module::Module) -> Either<crate::hydra::context::InContext, Option<crate::hydra::module::Module>> {
  crate::hydra::lib::eithers::bind(filter_type_bindings(cx.clone(), graph.clone(), mod_.clone().0.elements.clone()), |type_bindings: Vec<crate::hydra::core::Binding>| crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(type_bindings.clone()), Right(None), crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(|b: crate::hydra::core::Binding| crate::hydra::lib::eithers::bimap(|ic: crate::hydra::context::InContext| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(ic.clone().0.object.clone().0.0.clone())))))),
    context: ic.clone().0.context.clone()})), |x: crate::hydra::core::Binding| x.clone(), decode_binding(cx.clone(), graph.clone(), b.clone())), type_bindings.clone()), |decoded_bindings: Vec<crate::hydra::core::Binding>| {
    let decoded_type_deps = crate::hydra::lib::lists::map(decode_namespace, mod_.clone().0.type_dependencies.clone()) ;
    {
      let decoded_term_deps = crate::hydra::lib::lists::map(decode_namespace, mod_.clone().0.term_dependencies.clone()) ;
      {
        let all_decoded_deps = crate::hydra::lib::lists::nub(crate::hydra::lib::lists::concat2(decoded_type_deps.clone(), decoded_term_deps.clone())) ;
        Right(Some(crate::hydra::module::Module(Rc::new(crate::hydra::module::Module_Variant {
          namespace: decode_namespace(mod_.clone().0.namespace.clone()),
          elements: decoded_bindings.clone(),
          term_dependencies: crate::hydra::lib::lists::concat2(Vec::from([
            crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.extract.helpers")))),
            crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.lexical")))),
            crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.rewriting"))))]), all_decoded_deps.clone()),
          type_dependencies: Vec::from([
            mod_.clone().0.namespace.clone(),
            crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.util"))))]),
          description: Some(crate::hydra::lib::strings::cat(Vec::from([
            String::from("Term decoders for "),
            mod_.clone().0.namespace.clone().0.0.clone()])))}))))}}})))}

pub fn decode_namespace(ns: crate::hydra::module::Namespace) -> crate::hydra::module::Namespace {
  crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(crate::hydra::lib::strings::cat(Vec::from([
    String::from("hydra.decode."),
    crate::hydra::lib::strings::intercalate(String::from("."), crate::hydra::lib::lists::tail(crate::hydra::lib::strings::split_on(String::from("."), ns.clone().0.0.clone())))])))))}

pub fn decode_pair_type(pt: crate::hydra::core::PairType) -> crate::hydra::core::Term {
  let first_decoder = decode_type(pt.clone().0.first.clone()) ;
  let second_decoder = decode_type(pt.clone().0.second.clone()) ;
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.extract.helpers.decodePair"))))))),
      argument: first_decoder.clone()}))))),
    argument: second_decoder.clone()})))))}

pub fn decode_record_type(rt: Vec<crate::hydra::core::FieldType>) -> crate::hydra::core::Term {
  decode_record_type_impl(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unknown")))), rt.clone())}

pub fn decode_record_type_impl(tname: crate::hydra::core::Name, rt: Vec<crate::hydra::core::FieldType>) -> crate::hydra::core::Term {
  let decode_field_term = |ft: crate::hydra::core::FieldType| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
        function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.extract.helpers.requireField"))))))),
          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(ft.clone().0.name.clone().0.0.clone()))))))}))))),
        argument: decode_type(ft.clone().0.type_.clone())}))))),
      argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("fieldMap")))))))}))))),
    argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))) ;
  let local_var_name = |ft: crate::hydra::core::FieldType| crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat(Vec::from([
    String::from("field_"),
    ft.clone().0.name.clone().0.0.clone()]))))) ;
  let to_field_lambda = |ft: crate::hydra::core::FieldType, body: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
    parameter: local_var_name.clone()(ft.clone()),
    domain: None,
    body: body.clone()})))))))) ;
  let decode_body = crate::hydra::lib::lists::foldl(|acc: crate::hydra::core::Term, ft: crate::hydra::core::FieldType| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.bind")))))))))),
      argument: decode_field_term.clone()(ft.clone())}))))),
    argument: to_field_lambda.clone()(ft.clone(), acc.clone())}))))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
    type_name: tname.clone(),
    fields: crate::hydra::lib::lists::map(|ft: crate::hydra::core::FieldType| crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
      name: ft.clone().0.name.clone(),
      term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(local_var_name.clone()(ft.clone()))))})), rt.clone())}))))))))), crate::hydra::lib::lists::reverse(rt.clone())) ;
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
    domain: None,
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
      domain: None,
      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
        function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
            argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
              domain: None,
              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
            domain: None,
            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                  type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                  body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected record")))))))})))))))))),
                cases: Vec::from([
                  crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                    name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("record")))),
                    term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("record")))),
                      domain: None,
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
                        bindings: Vec::from([
                          crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("fieldMap")))),
                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.extract.helpers.toFieldMap"))))))),
                              argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("record")))))))}))))),
                            type_: None}))]),
                        body: decode_body.clone()})))))}))))))))}))])}))))))))))),
              argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
        argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
            argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))}

pub fn decode_record_type_named(ename: crate::hydra::core::Name, rt: Vec<crate::hydra::core::FieldType>) -> crate::hydra::core::Term {
  decode_record_type_impl(ename.clone(), rt.clone())}

pub fn decode_set_type(elem_type: crate::hydra::core::Type) -> crate::hydra::core::Term {
  let elem_decoder = decode_type(elem_type.clone()) ;
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
    function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.extract.helpers.decodeSet"))))))),
    argument: elem_decoder.clone()})))))}

pub fn decode_type(typ: crate::hydra::core::Type) -> crate::hydra::core::Term {
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      decode_type(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
        function: decode_type(v0_.clone().0.function.clone()),
        argument: decode_type(v0_.clone().0.argument.clone())})))))},
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      decode_either_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      decode_forall_type(v0_.clone())},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      decode_list_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      decode_literal_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      decode_map_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      decode_maybe_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      decode_pair_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      decode_record_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      decode_set_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      decode_union_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      decode_unit_type},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      decode_wrapped_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(decode_binding_name(v0_.clone()))))},
    _ => crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
      domain: None,
      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t")))),
        domain: None,
        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("unsupported type variant")))))))})))))))))}))))))))}))))))))}}

pub fn decode_type_named(ename: crate::hydra::core::Name, typ: crate::hydra::core::Type) -> crate::hydra::core::Term {
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      decode_type_named(ename.clone(), v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
        function: decode_type(v0_.clone().0.function.clone()),
        argument: decode_type(v0_.clone().0.argument.clone())})))))},
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      decode_either_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
        parameter: decode_binding_name(v0_.clone().0.parameter.clone()),
        domain: None,
        body: decode_type_named(ename.clone(), v0_.clone().0.body.clone())}))))))))},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      decode_list_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      decode_literal_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      decode_map_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      decode_maybe_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      decode_pair_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      decode_record_type_named(ename.clone(), v0_.clone())},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      decode_set_type(v0_.clone())},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      decode_union_type_named(ename.clone(), v0_.clone())},
    crate::hydra::core::Type_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      decode_unit_type},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      decode_wrapped_type_named(ename.clone(), v0_.clone())},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(decode_binding_name(v0_.clone()))))},
    _ => crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
      domain: None,
      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
        parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t")))),
        domain: None,
        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
          type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("unsupported type variant")))))))})))))))))}))))))))}))))))))}}

pub fn decode_unit_type() -> crate::hydra::core::Term {
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.extract.helpers.decodeUnit")))))))}

pub fn decode_union_type(rt: Vec<crate::hydra::core::FieldType>) -> crate::hydra::core::Term {
  decode_union_type_named(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unknown")))), rt.clone())}

pub fn decode_union_type_named(ename: crate::hydra::core::Name, rt: Vec<crate::hydra::core::FieldType>) -> crate::hydra::core::Term {
  let to_variant_pair = |ft: crate::hydra::core::FieldType| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Pair((crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
    type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name")))),
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(ft.clone().0.name.clone().0.0.clone()))))))}))))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))),
    domain: None,
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
        function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.map")))))))))),
        argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
          parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t")))),
          domain: None,
          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
            type_name: ename.clone(),
            field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
              name: ft.clone().0.name.clone(),
              term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("t")))))))}))})))))}))))))))}))))),
      argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
        function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: decode_type(ft.clone().0.type_.clone()),
          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
        argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("input")))))))})))))})))))})))))))))))) ;
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
    domain: None,
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
      domain: None,
      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
        function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
            argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
              domain: None,
              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
            domain: None,
            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                  type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                  body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected union")))))))})))))))))),
                cases: Vec::from([
                  crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                    name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("union")))),
                    term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("inj")))),
                      domain: None,
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
                        bindings: Vec::from([
                          crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("field")))),
                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Record(crate::hydra::core::Projection(Rc::new(crate::hydra::core::Projection_Variant {
                                type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Injection")))),
                                field: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("field"))))}))))))))))),
                              argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("inj")))))))}))))),
                            type_: None})),
                          crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("fname")))),
                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Record(crate::hydra::core::Projection(Rc::new(crate::hydra::core::Projection_Variant {
                                type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Field")))),
                                field: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("name"))))}))))))))))),
                              argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("field")))))))}))))),
                            type_: None})),
                          crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("fterm")))),
                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Record(crate::hydra::core::Projection(Rc::new(crate::hydra::core::Projection_Variant {
                                type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Field")))),
                                field: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("term"))))}))))))))))),
                              argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("field")))))))}))))),
                            type_: None})),
                          crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                            name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("variantMap")))),
                            term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.fromList")))))))))),
                              argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(to_variant_pair.clone(), rt.clone()))))}))))),
                            type_: None}))]),
                        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                          function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maybes.maybe")))))))))),
                              argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                                type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                                body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                  function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.strings.cat")))))))))),
                                  argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(Vec::from([
                                    crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("no such field "))))))),
                                    crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                      function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Wrap(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Name"))))))))))))),
                                      argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("fname")))))))}))))),
                                    crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from(" in union")))))))]))))})))))})))))))))}))))),
                            argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("f")))),
                              domain: None,
                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("f"))))))),
                                argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("fterm")))))))})))))}))))))))}))))),
                          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.maps.lookup")))))))))),
                              argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("fname")))))))}))))),
                            argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("variantMap")))))))})))))})))))})))))}))))))))}))])}))))))))))),
              argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
        argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
            argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))}

pub fn decode_wrapped_type(wt: crate::hydra::core::Type) -> crate::hydra::core::Term {
  decode_wrapped_type_named(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unknown")))), wt.clone())}

pub fn decode_wrapped_type_named(ename: crate::hydra::core::Name, wt: crate::hydra::core::Type) -> crate::hydra::core::Term {
  let body_decoder = decode_type(wt.clone()) ;
  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
    parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))),
    domain: None,
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))),
      domain: None,
      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
        function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.either")))))))))),
            argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
              parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))),
              domain: None,
              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("err")))))))})))))))))}))))))))}))))),
          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))),
            domain: None,
            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
              function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))),
                default_: Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                  type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError")))),
                  body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("expected wrapped type")))))))})))))))))),
                cases: Vec::from([
                  crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                    name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("wrap")))),
                    term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                      parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("wrappedTerm")))),
                      domain: None,
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                        function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                          function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lib.eithers.map")))))))))),
                          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                            parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("b")))),
                            domain: None,
                            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                              type_name: ename.clone(),
                              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("b")))))))})))))}))))))))}))))),
                        argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                          function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                            function: body_decoder.clone(),
                            argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
                          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Record(crate::hydra::core::Projection(Rc::new(crate::hydra::core::Projection_Variant {
                              type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.WrappedTerm")))),
                              field: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("body"))))}))))))))))),
                            argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("wrappedTerm")))))))})))))})))))})))))}))))))))}))])}))))))))))),
              argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("stripped")))))))})))))}))))))))}))))),
        argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
            function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.lexical.stripAndDereferenceTermEither"))))))),
            argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("cx")))))))}))))),
          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("raw")))))))})))))})))))}))))))))}))))))))}

pub fn decoder_full_result_type(typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      decoder_full_result_type(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
        function: decoder_full_result_type(v0_.clone().0.function.clone()),
        argument: v0_.clone().0.argument.clone()})))))},
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
        left: decoder_full_result_type(v0_.clone().0.left.clone()),
        right: decoder_full_result_type(v0_.clone().0.right.clone())})))))},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
        function: decoder_full_result_type(v0_.clone().0.body.clone()),
        argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v0_.clone().0.parameter.clone())))})))))},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(decoder_full_result_type(v0_.clone()))))},
    crate::hydra::core::Type_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))))))},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
        keys: decoder_full_result_type(v0_.clone().0.keys.clone()),
        values: decoder_full_result_type(v0_.clone().0.values.clone())})))))},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(decoder_full_result_type(v0_.clone()))))},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Pair(crate::hydra::core::PairType(Rc::new(crate::hydra::core::PairType_Variant {
        first: decoder_full_result_type(v0_.clone().0.first.clone()),
        second: decoder_full_result_type(v0_.clone().0.second.clone())})))))},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(decoder_full_result_type(v0_.clone()))))},
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

pub fn decoder_full_result_type_named(ename: crate::hydra::core::Name, typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      decoder_full_result_type_named(ename.clone(), v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
        function: decoder_full_result_type_named(ename.clone(), v0_.clone().0.body.clone()),
        argument: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v0_.clone().0.parameter.clone())))})))))},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(ename.clone())))},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(ename.clone())))},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(ename.clone())))},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
        function: decoder_full_result_type(v0_.clone().0.function.clone()),
        argument: v0_.clone().0.argument.clone()})))))},
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
        left: decoder_full_result_type(v0_.clone().0.left.clone()),
        right: decoder_full_result_type(v0_.clone().0.right.clone())})))))},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(decoder_full_result_type(v0_.clone()))))},
    crate::hydra::core::Type_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal")))))))},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
        keys: decoder_full_result_type(v0_.clone().0.keys.clone()),
        values: decoder_full_result_type(v0_.clone().0.values.clone())})))))},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(decoder_full_result_type(v0_.clone()))))},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Pair(crate::hydra::core::PairType(Rc::new(crate::hydra::core::PairType_Variant {
        first: decoder_full_result_type(v0_.clone().0.first.clone()),
        second: decoder_full_result_type(v0_.clone().0.second.clone())})))))},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(decoder_full_result_type(v0_.clone()))))},
    crate::hydra::core::Type_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v0_.clone())))},
    _ => crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term")))))))}}

pub fn decoder_result_type(typ: crate::hydra::core::Type) -> crate::hydra::core::Name {
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      decoder_result_type(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      decoder_result_type(v0_.clone().0.function.clone())},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      decoder_result_type(v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Literal"))))},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))},
    _ => crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))}}

pub fn decoder_type(typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  let result_type = decoder_full_result_type(typ.clone()) ;
  let base_type = crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
    domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.graph.Graph"))))))),
    codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
      domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
      codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
        left: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError"))))))),
        right: result_type.clone()})))))})))))}))))) ;
  prepend_forall_decoders(base_type.clone(), typ.clone())}

pub fn decoder_type_named(ename: crate::hydra::core::Name, typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  let result_type = decoder_full_result_type_named(ename.clone(), typ.clone()) ;
  let base_type = crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
    domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.graph.Graph"))))))),
    codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
      domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
      codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
        left: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError"))))))),
        right: result_type.clone()})))))})))))}))))) ;
  prepend_forall_decoders(base_type.clone(), typ.clone())}

pub fn decoder_type_scheme(typ: crate::hydra::core::Type) -> crate::hydra::core::TypeScheme {
  let type_vars = collect_type_variables(typ.clone()) ;
  let all_ord_vars = collect_ord_constrained_variables(typ.clone()) ;
  let ord_vars = crate::hydra::lib::lists::filter(|v: crate::hydra::core::Name| crate::hydra::lib::lists::elem(v.clone(), type_vars.clone()), all_ord_vars.clone()) ;
  let constraints = crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(ord_vars.clone()), None, Some(crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|v: crate::hydra::core::Name| (v.clone(), crate::hydra::core::TypeVariableMetadata(Rc::new(crate::hydra::core::TypeVariableMetadata_Variant {
    classes: crate::hydra::lib::sets::singleton(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("ordering")))))}))), ord_vars.clone())))) ;
  crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
    variables: type_vars.clone(),
    type_: decoder_type(typ.clone()),
    constraints: constraints.clone()}))}

pub fn decoder_type_scheme_named(ename: crate::hydra::core::Name, typ: crate::hydra::core::Type) -> crate::hydra::core::TypeScheme {
  let type_vars = collect_type_variables(typ.clone()) ;
  let all_ord_vars = collect_ord_constrained_variables(typ.clone()) ;
  let ord_vars = crate::hydra::lib::lists::filter(|v: crate::hydra::core::Name| crate::hydra::lib::lists::elem(v.clone(), type_vars.clone()), all_ord_vars.clone()) ;
  let constraints = crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(ord_vars.clone()), None, Some(crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|v: crate::hydra::core::Name| (v.clone(), crate::hydra::core::TypeVariableMetadata(Rc::new(crate::hydra::core::TypeVariableMetadata_Variant {
    classes: crate::hydra::lib::sets::singleton(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("ordering")))))}))), ord_vars.clone())))) ;
  crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
    variables: type_vars.clone(),
    type_: decoder_type_named(ename.clone(), typ.clone()),
    constraints: constraints.clone()}))}

pub fn filter_type_bindings(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, bindings: Vec<crate::hydra::core::Binding>) -> Either<crate::hydra::context::InContext, Vec<crate::hydra::core::Binding>> {
  crate::hydra::lib::eithers::map(crate::hydra::lib::maybes::cat, crate::hydra::lib::eithers::map_list(|v1: crate::hydra::core::Binding| is_decodable_binding(cx.clone(), graph.clone(), v1.clone()), crate::hydra::lib::lists::filter(crate::hydra::annotations::is_native_type, bindings.clone())))}

pub fn is_decodable_binding(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, b: crate::hydra::core::Binding) -> Either<crate::hydra::context::InContext, Option<crate::hydra::core::Binding>> {
  crate::hydra::lib::eithers::bind(crate::hydra::schemas::is_serializable_by_name(cx.clone(), graph.clone(), b.clone().0.name.clone()), |serializable: bool| Right(crate::hydra::lib::logic::if_else(serializable.clone(), Some(b.clone()), None)))}

pub fn prepend_forall_decoders(base_type: crate::hydra::core::Type, typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      prepend_forall_decoders(base_type.clone(), v0_.clone().0.body.clone())},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
        domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
          domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.graph.Graph"))))))),
          codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
            domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Term"))))))),
            codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
              left: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.error.DecodingError"))))))),
              right: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v0_.clone().0.parameter.clone())))})))))})))))}))))),
        codomain: prepend_forall_decoders(base_type.clone(), v0_.clone().0.body.clone())})))))},
    _ => base_type.clone()}}
