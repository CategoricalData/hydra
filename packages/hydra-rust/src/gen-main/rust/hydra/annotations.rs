#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::constants::*;
use crate::hydra::decode::core::*;
use crate::hydra::encode::core::*;
use crate::hydra::extract::core::*;
use crate::hydra::lexical::*;
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

pub fn aggregate_annotations(get_value: impl Fn(T0) -> Option<T1> + Clone, get_x: impl Fn(T1) -> T0 + Clone, get_anns: impl Fn(T1) -> BTreeMap<T2, T3> + Clone, t: T0) -> BTreeMap<T2, T3> {
  let to_pairs = |rest: Vec<Vec<(T2, T3)>>, t2: T0| crate::hydra::lib::maybes::maybe(rest.clone(), |yy: T1| to_pairs.clone()(crate::hydra::lib::lists::cons(crate::hydra::lib::maps::to_list(get_anns.clone()(yy.clone())), rest.clone()), get_x.clone()(yy.clone())), get_value.clone()(t2.clone())) ;
  crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::concat(to_pairs.clone()(Vec::from([]), t.clone())))}

pub fn debug_if(cx: crate::hydra::context::Context, debug_id: String, message: String) -> Either<crate::hydra::context::InContext, ()> {
  crate::hydra::lib::eithers::bind(get_debug_id(cx.clone()), |mid: Option<String>| crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(mid.clone(), Some(debug_id.clone())), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(message.clone())))))),
    context: cx.clone()}))), Right(())))}

pub fn fail_on_flag(cx: crate::hydra::context::Context, flag: crate::hydra::core::Name, msg: String) -> Either<crate::hydra::context::InContext, ()> {
  crate::hydra::lib::eithers::bind(has_flag(cx.clone(), flag.clone()), |val: bool| crate::hydra::lib::logic::if_else(val.clone(), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(msg.clone())))))),
    context: cx.clone()}))), Right(())))}

pub fn get_debug_id(cx: crate::hydra::context::Context) -> Either<crate::hydra::context::InContext, Option<String>> {
  crate::hydra::lib::maybes::maybe(Right(None), |term: crate::hydra::core::Term| crate::hydra::lib::eithers::map(crate::hydra::lib::maybes::pure, crate::hydra::extract::core::string(cx.clone(), crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
    bound_terms: crate::hydra::lib::maps::empty,
    bound_types: crate::hydra::lib::maps::empty,
    class_constraints: crate::hydra::lib::maps::empty,
    lambda_variables: crate::hydra::lib::sets::empty,
    metadata: crate::hydra::lib::maps::empty,
    primitives: crate::hydra::lib::maps::empty,
    schema_types: crate::hydra::lib::maps::empty,
    type_variables: crate::hydra::lib::sets::empty})), term.clone())), get_attr(crate::hydra::constants::key_debug_id, cx.clone()))}

pub fn get_attr(key: crate::hydra::core::Name, cx: crate::hydra::context::Context) -> Option<crate::hydra::core::Term> {
  crate::hydra::lib::maps::lookup(key.clone(), cx.clone().0.other.clone())}

pub fn get_attr_with_default(key: crate::hydra::core::Name, def: crate::hydra::core::Term, cx: crate::hydra::context::Context) -> crate::hydra::core::Term {
  crate::hydra::lib::maybes::from_maybe(def.clone(), get_attr(key.clone(), cx.clone()))}

pub fn get_count(key: crate::hydra::core::Name, cx: crate::hydra::context::Context) -> i32 {
  crate::hydra::lib::maybes::maybe(0i32, |term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Literal_Variant::Integer (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
              let v0_ = v0_.clone() ;
              v0_.clone()},
            _ => 0i32}},
        _ => 0i32}},
    _ => 0i32}, crate::hydra::lib::maps::lookup(key.clone(), cx.clone().0.other.clone()))}

pub fn get_description(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, anns: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term>) -> Either<crate::hydra::context::InContext, Option<String>> {
  crate::hydra::lib::maybes::maybe(Right(None), |term: crate::hydra::core::Term| crate::hydra::lib::eithers::map(crate::hydra::lib::maybes::pure, crate::hydra::extract::core::string(cx.clone(), graph.clone(), term.clone())), crate::hydra::lib::maps::lookup(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("description")))), anns.clone()))}

pub fn get_term_annotation(key: crate::hydra::core::Name, term: crate::hydra::core::Term) -> Option<crate::hydra::core::Term> {
  crate::hydra::lib::maps::lookup(key.clone(), term_annotation_internal(term.clone()))}

pub fn get_term_description(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, Option<String>> {
  let peel = |t: crate::hydra::core::Term| match &*t.clone().0 {
    crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
      let v0_ = v0_.clone() ;
      peel.clone()(v0_.clone().0.body.clone())},
    crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
      let v0_ = v0_.clone() ;
      peel.clone()(v0_.clone().0.body.clone())},
    _ => t.clone()} ;
  get_description(cx.clone(), graph.clone(), term_annotation_internal(peel.clone()(term.clone())))}

pub fn get_type(graph: crate::hydra::graph::Graph, anns: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term>) -> Either<crate::hydra::error::DecodingError, Option<crate::hydra::core::Type>> {
  crate::hydra::lib::maybes::maybe(Right(None), |dat: crate::hydra::core::Term| crate::hydra::lib::eithers::map(crate::hydra::lib::maybes::pure, crate::hydra::decode::core::type_(graph.clone(), dat.clone())), crate::hydra::lib::maps::lookup(crate::hydra::constants::key_type, anns.clone()))}

pub fn get_type_annotation(key: crate::hydra::core::Name, typ: crate::hydra::core::Type) -> Option<crate::hydra::core::Term> {
  crate::hydra::lib::maps::lookup(key.clone(), type_annotation_internal(typ.clone()))}

pub fn get_type_classes(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, BTreeMap<crate::hydra::core::Name, BTreeSet<crate::hydra::classes::TypeClass>>> {
  let decode_class = |term2: crate::hydra::core::Term| {
    let by_name = crate::hydra::lib::maps::from_list(Vec::from([
      (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("equality")))), crate::hydra::classes::TypeClass(Rc::new(crate::hydra::classes::TypeClass_Variant::Equality))),
      (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("ordering")))), crate::hydra::classes::TypeClass(Rc::new(crate::hydra::classes::TypeClass_Variant::Ordering)))])) ;
    crate::hydra::lib::eithers::bind(crate::hydra::extract::core::unit_variant(cx.clone(), crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.classes.TypeClass")))), graph.clone(), term2.clone()), |fn_: crate::hydra::core::Name| crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(String::from("unexpected: expected type class, got "), crate::hydra::show::core::term(term2.clone())))))))),
      context: cx.clone()}))), |x: crate::hydra::classes::TypeClass| Right(x.clone()), crate::hydra::lib::maps::lookup(fn_.clone(), by_name.clone())))} ;
  crate::hydra::lib::maybes::maybe(Right(crate::hydra::lib::maps::empty), |term2: crate::hydra::core::Term| crate::hydra::extract::core::map(cx.clone(), |t: crate::hydra::core::Term| crate::hydra::lib::eithers::bimap(|de: crate::hydra::error::DecodingError| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(de.clone().0.0.clone())))))),
    context: cx.clone()})), |x: crate::hydra::core::Name| x.clone(), crate::hydra::decode::core::name(graph.clone(), t.clone())), |v1: crate::hydra::core::Term| crate::hydra::extract::core::set_of(cx.clone(), decode_class.clone(), graph.clone(), v1.clone()), graph.clone(), term2.clone()), get_term_annotation(crate::hydra::constants::key_classes, term.clone()))}

pub fn get_type_description(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, typ: crate::hydra::core::Type) -> Either<crate::hydra::context::InContext, Option<String>> {
  get_description(cx.clone(), graph.clone(), type_annotation_internal(typ.clone()))}

pub fn is_native_type(el: crate::hydra::core::Binding) -> bool {
  let is_flagged_as_first_class_type = crate::hydra::lib::maybes::from_maybe(false, crate::hydra::lib::maybes::map(|_: crate::hydra::core::Term| true, get_term_annotation(crate::hydra::constants::key_first_class_type, el.clone().0.term.clone()))) ;
  crate::hydra::lib::maybes::maybe(false, |ts: crate::hydra::core::TypeScheme| crate::hydra::lib::logic::and(crate::hydra::lib::equality::equal(ts.clone(), crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
    variables: Vec::from([]),
    type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
    constraints: None}))), crate::hydra::lib::logic::not(is_flagged_as_first_class_type.clone())), el.clone().0.type_.clone())}

pub fn has_description(anns: BTreeMap<crate::hydra::core::Name, T0>) -> bool {
  crate::hydra::lib::maybes::is_just(crate::hydra::lib::maps::lookup(crate::hydra::constants::key_description, anns.clone()))}

pub fn has_flag(cx: crate::hydra::context::Context, flag: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, bool> {
  let term = get_attr_with_default(flag.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Boolean(false)))))), cx.clone()) ;
  crate::hydra::extract::core::boolean(cx.clone(), crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
    bound_terms: crate::hydra::lib::maps::empty,
    bound_types: crate::hydra::lib::maps::empty,
    class_constraints: crate::hydra::lib::maps::empty,
    lambda_variables: crate::hydra::lib::sets::empty,
    metadata: crate::hydra::lib::maps::empty,
    primitives: crate::hydra::lib::maps::empty,
    schema_types: crate::hydra::lib::maps::empty,
    type_variables: crate::hydra::lib::sets::empty})), term.clone())}

pub fn has_type_description(typ: crate::hydra::core::Type) -> bool {
  has_description(type_annotation_internal(typ.clone()))}

pub fn next_count(key: crate::hydra::core::Name, cx: crate::hydra::context::Context) -> (i32, crate::hydra::context::Context) {
  let count = get_count(key.clone(), cx.clone()) ;
  (count.clone(), put_count(key.clone(), crate::hydra::lib::math::add(count.clone(), 1i32), cx.clone()))}

pub fn normalize_term_annotations(term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let anns = term_annotation_internal(term.clone()) ;
  let stripped = crate::hydra::rewriting::deannotate_term(term.clone()) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::maps::null(anns.clone()), stripped.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
    body: stripped.clone(),
    annotation: anns.clone()}))))))}

pub fn normalize_type_annotations(typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  let anns = type_annotation_internal(typ.clone()) ;
  let stripped = crate::hydra::rewriting::deannotate_type(typ.clone()) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::maps::null(anns.clone()), stripped.clone(), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
    body: stripped.clone(),
    annotation: anns.clone()}))))))}

pub fn put_attr(key: crate::hydra::core::Name, val: crate::hydra::core::Term, cx: crate::hydra::context::Context) -> crate::hydra::context::Context {
  crate::hydra::context::Context(Rc::new(crate::hydra::context::Context_Variant {
    trace: cx.clone().0.trace.clone(),
    messages: cx.clone().0.messages.clone(),
    other: crate::hydra::lib::maps::insert(key.clone(), val.clone(), cx.clone().0.other.clone())}))}

pub fn put_count(key: crate::hydra::core::Name, count: i32, cx: crate::hydra::context::Context) -> crate::hydra::context::Context {
  put_attr(key.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int32(count.clone()))))))))), cx.clone())}

pub fn reset_count(key: crate::hydra::core::Name, cx: crate::hydra::context::Context) -> crate::hydra::context::Context {
  put_attr(key.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int32(0i32))))))))), cx.clone())}

pub fn set_annotation(key: T0, val: Option<T1>, m: BTreeMap<T0, T1>) -> BTreeMap<T0, T0> {
  crate::hydra::lib::maps::alter(|_: Option<T1>| val.clone(), key.clone(), m.clone())}

pub fn set_description(d: Option<String>, v1: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term>) -> BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term> {
  set_annotation(crate::hydra::constants::key_description, crate::hydra::lib::maybes::map(|arg_: String| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(arg_.clone())))))), d.clone()), v1.clone())}

pub fn set_term_annotation(key: crate::hydra::core::Name, val: Option<crate::hydra::core::Term>, term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let term_ = crate::hydra::rewriting::deannotate_term(term.clone()) ;
  let anns = set_annotation(key.clone(), val.clone(), term_annotation_internal(term.clone())) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::maps::null(anns.clone()), term_.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
    body: term_.clone(),
    annotation: anns.clone()}))))))}

pub fn set_term_description(d: Option<String>, v1: crate::hydra::core::Term) -> crate::hydra::core::Term {
  set_term_annotation(crate::hydra::constants::key_description, crate::hydra::lib::maybes::map(|s: String| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(s.clone())))))), d.clone()), v1.clone())}

pub fn set_type(mt: Option<crate::hydra::core::Type>, v1: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term>) -> BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term> {
  set_annotation(crate::hydra::constants::key_type, crate::hydra::lib::maybes::map(crate::hydra::encode::core::type_, mt.clone()), v1.clone())}

pub fn set_type_annotation(key: crate::hydra::core::Name, val: Option<crate::hydra::core::Term>, typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  let typ_ = crate::hydra::rewriting::deannotate_type(typ.clone()) ;
  let anns = set_annotation(key.clone(), val.clone(), type_annotation_internal(typ.clone())) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::maps::null(anns.clone()), typ_.clone(), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
    body: typ_.clone(),
    annotation: anns.clone()}))))))}

pub fn set_type_classes(m: BTreeMap<crate::hydra::core::Name, BTreeSet<crate::hydra::classes::TypeClass>>, term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let encode_class = |tc: crate::hydra::classes::TypeClass| match &*tc.clone().0 {
    crate::hydra::classes::TypeClass_Variant::Equality (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.classes.TypeClass")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("equality")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))},
    crate::hydra::classes::TypeClass_Variant::Ordering (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
        type_name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.classes.TypeClass")))),
        field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("ordering")))),
          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))})))))}} ;
  let encode_pair = |name_classes: (crate::hydra::core::Name, BTreeSet<crate::hydra::classes::TypeClass>)| {
    let name = crate::hydra::lib::pairs::first(name_classes.clone()) ;
    {
      let classes = crate::hydra::lib::pairs::second(name_classes.clone()) ;
      (crate::hydra::encode::core::name(name.clone()), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Set(crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::map(encode_class.clone(), crate::hydra::lib::sets::to_list(classes.clone())))))))}} ;
  let encoded = crate::hydra::lib::logic::if_else(crate::hydra::lib::maps::null(m.clone()), None, Some(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(encode_pair.clone(), crate::hydra::lib::maps::to_list(m.clone())))))))) ;
  set_term_annotation(crate::hydra::constants::key_classes, encoded.clone(), term.clone())}

pub fn set_type_description(d: Option<String>, v1: crate::hydra::core::Type) -> crate::hydra::core::Type {
  set_type_annotation(crate::hydra::constants::key_description, crate::hydra::lib::maybes::map(|arg_: String| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(arg_.clone())))))), d.clone()), v1.clone())}

pub fn term_annotation_internal(term: crate::hydra::core::Term) -> BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term> {
  let get_ann = |t: crate::hydra::core::Term| match &*t.clone().0 {
    crate::hydra::core::Term_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      Some(v0_.clone())},
    _ => None} ;
  aggregate_annotations(get_ann.clone(), |at: crate::hydra::core::AnnotatedTerm| at.clone().0.body.clone(), |at: crate::hydra::core::AnnotatedTerm| at.clone().0.annotation.clone(), term.clone())}

pub fn type_annotation_internal(typ: crate::hydra::core::Type) -> BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term> {
  let get_ann = |t: crate::hydra::core::Type| match &*t.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      Some(v0_.clone())},
    _ => None} ;
  aggregate_annotations(get_ann.clone(), |at: crate::hydra::core::AnnotatedType| at.clone().0.body.clone(), |at: crate::hydra::core::AnnotatedType| at.clone().0.annotation.clone(), typ.clone())}

pub fn type_element(name: crate::hydra::core::Name, typ: crate::hydra::core::Type) -> crate::hydra::core::Binding {
  let schema_term = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))) ;
  let data_term = normalize_term_annotations(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
    body: crate::hydra::encode::core::type_(typ.clone()),
    annotation: crate::hydra::lib::maps::from_list(Vec::from([
      (crate::hydra::constants::key_type, schema_term.clone())]))})))))) ;
  crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
    name: name.clone(),
    term: data_term.clone(),
    type_: Some(crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
      variables: Vec::from([]),
      type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.core.Type"))))))),
      constraints: None})))}))}

pub fn when_flag(cx: crate::hydra::context::Context, flag: crate::hydra::core::Name, ethen: Either<crate::hydra::context::InContext, T0>, eelse: Either<crate::hydra::context::InContext, T0>) -> Either<crate::hydra::context::InContext, T0> {
  crate::hydra::lib::eithers::bind(has_flag(cx.clone(), flag.clone()), |b: bool| crate::hydra::lib::logic::if_else(b.clone(), ethen.clone(), eelse.clone()))}
