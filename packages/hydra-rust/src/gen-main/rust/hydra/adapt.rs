#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::hoisting::*;
use crate::hydra::inference::*;
use crate::hydra::lexical::*;
use crate::hydra::literals::*;
use crate::hydra::names::*;
use crate::hydra::reduction::*;
use crate::hydra::reflect::*;
use crate::hydra::rewriting::*;
use crate::hydra::schemas::*;
use crate::hydra::show::core::*;
use crate::hydra::show::error::*;
use crate::hydra::show::graph::*;
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

pub fn adapt_float_type(constraints: crate::hydra::coders::LanguageConstraints, ft: crate::hydra::core::FloatType) -> Option<crate::hydra::core::FloatType> {
  let supported = crate::hydra::lib::sets::member(ft.clone(), constraints.clone().0.float_types.clone()) ;
  let alt = |v1: crate::hydra::core::FloatType| adapt_float_type(constraints.clone(), v1.clone()) ;
  let for_unsupported = |ft2: crate::hydra::core::FloatType| match &*ft2.clone().0 {
    crate::hydra::core::FloatType_Variant::Bigfloat (v0_) => {
      let v0_ = v0_.clone() ;
      alt.clone()(crate::hydra::core::FloatType(Rc::new(crate::hydra::core::FloatType_Variant::Float64)))},
    crate::hydra::core::FloatType_Variant::Float32 (v0_) => {
      let v0_ = v0_.clone() ;
      alt.clone()(crate::hydra::core::FloatType(Rc::new(crate::hydra::core::FloatType_Variant::Float64)))},
    crate::hydra::core::FloatType_Variant::Float64 (v0_) => {
      let v0_ = v0_.clone() ;
      alt.clone()(crate::hydra::core::FloatType(Rc::new(crate::hydra::core::FloatType_Variant::Bigfloat)))}} ;
  crate::hydra::lib::logic::if_else(supported.clone(), Some(ft.clone()), for_unsupported.clone()(ft.clone()))}

pub fn adapt_data_graph(constraints: crate::hydra::coders::LanguageConstraints, do_expand: bool, els0: Vec<crate::hydra::core::Binding>, cx: crate::hydra::context::Context, graph0: crate::hydra::graph::Graph) -> Either<String, (crate::hydra::graph::Graph, Vec<crate::hydra::core::Binding>)> {
  let transform = |g: crate::hydra::graph::Graph, gterm: crate::hydra::core::Term| {
    let tx = g.clone() ;
    {
      let gterm1 = crate::hydra::rewriting::unshadow_variables(push_type_apps_inward(gterm.clone())) ;
      {
        let gterm2 = crate::hydra::rewriting::unshadow_variables(crate::hydra::lib::logic::if_else(do_expand.clone(), push_type_apps_inward(crate::hydra::reduction::eta_expand_term_new(tx.clone(), gterm1.clone())), gterm1.clone())) ;
        crate::hydra::rewriting::lift_lambda_above_let(gterm2.clone())}}} ;
  let litmap = adapt_literal_types_map(constraints.clone()) ;
  let prims0 = graph0.clone().0.primitives.clone() ;
  let schema_types0 = graph0.clone().0.schema_types.clone() ;
  let schema_bindings = crate::hydra::schemas::types_to_elements(crate::hydra::lib::maps::map(|ts: crate::hydra::core::TypeScheme| crate::hydra::rewriting::type_scheme_to_f_type(ts.clone()), schema_types0.clone())) ;
  crate::hydra::lib::eithers::bind(crate::hydra::lib::logic::if_else(crate::hydra::lib::maps::null(schema_types0.clone()), Right(crate::hydra::lib::maps::empty), crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|ic: crate::hydra::context::InContext| ic.clone().0.object.clone().0.0.clone(), |x: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>| x.clone(), crate::hydra::schemas::graph_as_types(cx.clone(), graph0.clone(), schema_bindings.clone())), |tmap0: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>| crate::hydra::lib::eithers::bind(adapt_graph_schema(constraints.clone(), litmap.clone(), tmap0.clone()), |tmap1: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>| Right(crate::hydra::lib::maps::map(|t: crate::hydra::core::Type| crate::hydra::schemas::type_to_type_scheme(t.clone()), tmap1.clone()))))), |schema_result: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeScheme>| {
    let adapted_schema_types = schema_result.clone() ;
    {
      let gterm0 = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
        bindings: els0.clone(),
        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))))) ;
      {
        let gterm1 = crate::hydra::lib::logic::if_else(do_expand.clone(), transform.clone()(graph0.clone(), gterm0.clone()), gterm0.clone()) ;
        crate::hydra::lib::eithers::bind(adapt_term(constraints.clone(), litmap.clone(), cx.clone(), graph0.clone(), gterm1.clone()), |gterm2: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::rewriting::rewrite_term_m(|v1: Rc<dyn Fn(crate::hydra::core::Term) -> Either<String, crate::hydra::core::Term>>, v2: crate::hydra::core::Term| adapt_lambda_domains(constraints.clone(), litmap.clone(), v1.clone(), v2.clone()), gterm2.clone()), |gterm3: crate::hydra::core::Term| {
          let els1_raw = crate::hydra::schemas::term_as_bindings(gterm3.clone()) ;
          {
            let process_binding = |el: crate::hydra::core::Binding| crate::hydra::lib::eithers::bind(crate::hydra::rewriting::rewrite_term_m(|v1: Rc<dyn Fn(crate::hydra::core::Term) -> Either<String, crate::hydra::core::Term>>, v2: crate::hydra::core::Term| adapt_nested_types(constraints.clone(), litmap.clone(), v1.clone(), v2.clone()), el.clone().0.term.clone()), |new_term: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::lib::maybes::maybe(Right(None), |ts: crate::hydra::core::TypeScheme| crate::hydra::lib::eithers::bind(adapt_type_scheme(constraints.clone(), litmap.clone(), ts.clone()), |ts1: crate::hydra::core::TypeScheme| Right(Some(ts1.clone()))), el.clone().0.type_.clone()), |adapted_type: Option<crate::hydra::core::TypeScheme>| Right(crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
              name: el.clone().0.name.clone(),
              term: new_term.clone(),
              type_: adapted_type.clone()}))))) ;
            crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(process_binding.clone(), els1_raw.clone()), |els1: Vec<crate::hydra::core::Binding>| crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(|kv: (crate::hydra::core::Name, crate::hydra::graph::Primitive)| crate::hydra::lib::eithers::bind(adapt_primitive(constraints.clone(), litmap.clone(), crate::hydra::lib::pairs::second(kv.clone())), |prim1: crate::hydra::graph::Primitive| Right((crate::hydra::lib::pairs::first(kv.clone()), prim1.clone()))), crate::hydra::lib::maps::to_list(prims0.clone())), |prim_pairs: Vec<(crate::hydra::core::Name, crate::hydra::graph::Primitive)>| {
              let prims1 = crate::hydra::lib::maps::from_list(prim_pairs.clone()) ;
              {
                let adapted_graph = crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
                  bound_terms: crate::hydra::lexical::build_graph(els1.clone(), crate::hydra::lib::maps::empty, prims1.clone()).0.bound_terms.clone(),
                  bound_types: crate::hydra::lexical::build_graph(els1.clone(), crate::hydra::lib::maps::empty, prims1.clone()).0.bound_types.clone(),
                  class_constraints: crate::hydra::lexical::build_graph(els1.clone(), crate::hydra::lib::maps::empty, prims1.clone()).0.class_constraints.clone(),
                  lambda_variables: crate::hydra::lexical::build_graph(els1.clone(), crate::hydra::lib::maps::empty, prims1.clone()).0.lambda_variables.clone(),
                  metadata: crate::hydra::lexical::build_graph(els1.clone(), crate::hydra::lib::maps::empty, prims1.clone()).0.metadata.clone(),
                  primitives: crate::hydra::lexical::build_graph(els1.clone(), crate::hydra::lib::maps::empty, prims1.clone()).0.primitives.clone(),
                  schema_types: adapted_schema_types.clone(),
                  type_variables: crate::hydra::lexical::build_graph(els1.clone(), crate::hydra::lib::maps::empty, prims1.clone()).0.type_variables.clone()})) ;
                Right((adapted_graph.clone(), els1.clone()))}}))}}))}}})}

pub fn adapt_graph_schema(constraints: crate::hydra::coders::LanguageConstraints, litmap: BTreeMap<crate::hydra::core::LiteralType, crate::hydra::core::LiteralType>, types0: BTreeMap<T0, crate::hydra::core::Type>) -> Either<String, BTreeMap<T0, crate::hydra::core::Type>> {
  let map_pair = |pair: (T1, crate::hydra::core::Type)| {
    let name = crate::hydra::lib::pairs::first(pair.clone()) ;
    {
      let typ = crate::hydra::lib::pairs::second(pair.clone()) ;
      crate::hydra::lib::eithers::bind(adapt_type(constraints.clone(), litmap.clone(), typ.clone()), |typ1: crate::hydra::core::Type| Right((name.clone(), typ1.clone())))}} ;
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(map_pair.clone(), crate::hydra::lib::maps::to_list(types0.clone())), |pairs: Vec<(T0, crate::hydra::core::Type)>| Right(crate::hydra::lib::maps::from_list(pairs.clone())))}

pub fn adapt_integer_type(constraints: crate::hydra::coders::LanguageConstraints, it: crate::hydra::core::IntegerType) -> Option<crate::hydra::core::IntegerType> {
  let supported = crate::hydra::lib::sets::member(it.clone(), constraints.clone().0.integer_types.clone()) ;
  let alt = |v1: crate::hydra::core::IntegerType| adapt_integer_type(constraints.clone(), v1.clone()) ;
  let for_unsupported = |it2: crate::hydra::core::IntegerType| match &*it2.clone().0 {
    crate::hydra::core::IntegerType_Variant::Bigint (v0_) => {
      let v0_ = v0_.clone() ;
      None},
    crate::hydra::core::IntegerType_Variant::Int8 (v0_) => {
      let v0_ = v0_.clone() ;
      alt.clone()(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint16)))},
    crate::hydra::core::IntegerType_Variant::Int16 (v0_) => {
      let v0_ = v0_.clone() ;
      alt.clone()(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint32)))},
    crate::hydra::core::IntegerType_Variant::Int32 (v0_) => {
      let v0_ = v0_.clone() ;
      alt.clone()(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint64)))},
    crate::hydra::core::IntegerType_Variant::Int64 (v0_) => {
      let v0_ = v0_.clone() ;
      alt.clone()(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Bigint)))},
    crate::hydra::core::IntegerType_Variant::Uint8 (v0_) => {
      let v0_ = v0_.clone() ;
      alt.clone()(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int16)))},
    crate::hydra::core::IntegerType_Variant::Uint16 (v0_) => {
      let v0_ = v0_.clone() ;
      alt.clone()(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int32)))},
    crate::hydra::core::IntegerType_Variant::Uint32 (v0_) => {
      let v0_ = v0_.clone() ;
      alt.clone()(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int64)))},
    crate::hydra::core::IntegerType_Variant::Uint64 (v0_) => {
      let v0_ = v0_.clone() ;
      alt.clone()(crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Bigint)))}} ;
  crate::hydra::lib::logic::if_else(supported.clone(), Some(it.clone()), for_unsupported.clone()(it.clone()))}

pub fn adapt_lambda_domains(constraints: crate::hydra::coders::LanguageConstraints, litmap: BTreeMap<crate::hydra::core::LiteralType, crate::hydra::core::LiteralType>, recurse: impl Fn(T0) -> Either<String, crate::hydra::core::Term> + Clone, term: T0) -> Either<String, crate::hydra::core::Term> {
  crate::hydra::lib::eithers::bind(recurse.clone()(term.clone()), |rewritten: crate::hydra::core::Term| match &*rewritten.clone().0 {
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Function_Variant::Lambda (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::lib::eithers::bind(crate::hydra::lib::maybes::maybe(Right(None), |dom: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(adapt_type(constraints.clone(), litmap.clone(), dom.clone()), |dom1: crate::hydra::core::Type| Right(Some(dom1.clone()))), v0_.clone().0.domain.clone()), |adapted_domain: Option<crate::hydra::core::Type>| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: v0_.clone().0.parameter.clone(),
            domain: adapted_domain.clone(),
            body: v0_.clone().0.body.clone()}))))))))))},
        _ => Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(v0_.clone()))))}},
    _ => Right(rewritten.clone())})}

pub fn adapt_literal(lt: crate::hydra::core::LiteralType, l: crate::hydra::core::Literal) -> crate::hydra::core::Literal {
  match &*l.clone().0 {
    crate::hydra::core::Literal_Variant::Binary (v0_) => {
      let v0_ = v0_.clone() ;
      match &*lt.clone().0 {
        crate::hydra::core::LiteralType_Variant::String (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::lib::literals::binary_to_string(v0_.clone()))))}}},
    crate::hydra::core::Literal_Variant::Boolean (v0_) => {
      let v0_ = v0_.clone() ;
      match &*lt.clone().0 {
        crate::hydra::core::LiteralType_Variant::Integer (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::literals::bigint_to_integer_value(v0_.clone(), crate::hydra::lib::logic::if_else(v0_.clone(), String::from("1:bigint"), String::from("0:bigint"))))))}}},
    crate::hydra::core::Literal_Variant::Float (v0_) => {
      let v0_ = v0_.clone() ;
      match &*lt.clone().0 {
        crate::hydra::core::LiteralType_Variant::Float (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Float(crate::hydra::literals::bigfloat_to_float_value(v0_.clone(), crate::hydra::literals::float_value_to_bigfloat(v0_.clone())))))}}},
    crate::hydra::core::Literal_Variant::Integer (v0_) => {
      let v0_ = v0_.clone() ;
      match &*lt.clone().0 {
        crate::hydra::core::LiteralType_Variant::Integer (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(crate::hydra::literals::bigint_to_integer_value(v0_.clone(), crate::hydra::literals::integer_value_to_bigint(v0_.clone())))))}}}}}

pub fn adapt_literal_type(constraints: crate::hydra::coders::LanguageConstraints, lt: crate::hydra::core::LiteralType) -> Option<crate::hydra::core::LiteralType> {
  let for_unsupported = |lt2: crate::hydra::core::LiteralType| match &*lt2.clone().0 {
    crate::hydra::core::LiteralType_Variant::Binary (v0_) => {
      let v0_ = v0_.clone() ;
      Some(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String)))},
    crate::hydra::core::LiteralType_Variant::Boolean (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::maybes::map(|x: crate::hydra::core::IntegerType| crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Integer(x.clone()))), adapt_integer_type(constraints.clone(), crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int8))))},
    crate::hydra::core::LiteralType_Variant::Float (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::maybes::map(|x: crate::hydra::core::FloatType| crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Float(x.clone()))), adapt_float_type(constraints.clone(), v0_.clone()))},
    crate::hydra::core::LiteralType_Variant::Integer (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::maybes::map(|x: crate::hydra::core::IntegerType| crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Integer(x.clone()))), adapt_integer_type(constraints.clone(), v0_.clone()))},
    _ => None} ;
  crate::hydra::lib::logic::if_else(literal_type_supported(constraints.clone(), lt.clone()), None, for_unsupported.clone()(lt.clone()))}

pub fn adapt_literal_types_map(constraints: crate::hydra::coders::LanguageConstraints) -> BTreeMap<crate::hydra::core::LiteralType, crate::hydra::core::LiteralType> {
  let try_type = |lt: crate::hydra::core::LiteralType| crate::hydra::lib::maybes::maybe(None, |lt2: crate::hydra::core::LiteralType| Some((lt.clone(), lt2.clone())), adapt_literal_type(constraints.clone(), lt.clone())) ;
  crate::hydra::lib::maps::from_list(crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(try_type.clone(), crate::hydra::reflect::literal_types)))}

pub fn adapt_literal_value(litmap: BTreeMap<T0, crate::hydra::core::LiteralType>, lt: T0, l: crate::hydra::core::Literal) -> crate::hydra::core::Literal {
  crate::hydra::lib::maybes::maybe(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(crate::hydra::show::core::literal(l.clone())))), |lt2: crate::hydra::core::LiteralType| adapt_literal(lt2.clone(), l.clone()), crate::hydra::lib::maps::lookup(lt.clone(), litmap.clone()))}

pub fn adapt_nested_types(constraints: crate::hydra::coders::LanguageConstraints, litmap: BTreeMap<crate::hydra::core::LiteralType, crate::hydra::core::LiteralType>, recurse: impl Fn(T0) -> Either<String, crate::hydra::core::Term> + Clone, term: T0) -> Either<String, crate::hydra::core::Term> {
  crate::hydra::lib::eithers::bind(recurse.clone()(term.clone()), |rewritten: crate::hydra::core::Term| match &*rewritten.clone().0 {
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let adapt_b = |b: crate::hydra::core::Binding| crate::hydra::lib::eithers::bind(crate::hydra::lib::maybes::maybe(Right(None), |ts: crate::hydra::core::TypeScheme| crate::hydra::lib::eithers::bind(adapt_type_scheme(constraints.clone(), litmap.clone(), ts.clone()), |ts1: crate::hydra::core::TypeScheme| Right(Some(ts1.clone()))), b.clone().0.type_.clone()), |adapted_b_type: Option<crate::hydra::core::TypeScheme>| Right(crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
          name: b.clone().0.name.clone(),
          term: b.clone().0.term.clone(),
          type_: adapted_b_type.clone()})))) ;
        crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(adapt_b.clone(), v0_.clone().0.bindings.clone()), |adapted_bindings: Vec<crate::hydra::core::Binding>| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
          bindings: adapted_bindings.clone(),
          body: v0_.clone().0.body.clone()})))))))}},
    _ => Right(rewritten.clone())})}

pub fn adapt_primitive(constraints: crate::hydra::coders::LanguageConstraints, litmap: BTreeMap<crate::hydra::core::LiteralType, crate::hydra::core::LiteralType>, prim0: crate::hydra::graph::Primitive) -> Either<String, crate::hydra::graph::Primitive> {
  let ts0 = prim0.clone().0.type_.clone() ;
  crate::hydra::lib::eithers::bind(adapt_type_scheme(constraints.clone(), litmap.clone(), ts0.clone()), |ts1: crate::hydra::core::TypeScheme| Right(crate::hydra::graph::Primitive(Rc::new(crate::hydra::graph::Primitive_Variant {
    name: prim0.clone().0.name.clone(),
    type_: ts1.clone(),
    implementation: prim0.clone().0.implementation.clone()}))))}

pub fn adapt_term(constraints: crate::hydra::coders::LanguageConstraints, litmap: BTreeMap<crate::hydra::core::LiteralType, crate::hydra::core::LiteralType>, cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> Either<String, crate::hydra::core::Term> {
  let rewrite = |recurse: Rc<dyn Fn(T0) -> Either<String, crate::hydra::core::Term>>, term02: T0| {
    let for_supported = |term: crate::hydra::core::Term| match &*term.clone().0 {
      crate::hydra::core::Term_Variant::Literal (v0_) => {
        let v0_ = v0_.clone() ;
        {
          let lt = crate::hydra::reflect::literal_type(v0_.clone()) ;
          Right(Some(crate::hydra::lib::logic::if_else(literal_type_supported(constraints.clone(), lt.clone()), term.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(adapt_literal_value(litmap.clone(), lt.clone(), v0_.clone())))))))}},
      _ => Right(Some(term.clone()))} ;
    let for_unsupported = |term: crate::hydra::core::Term| {
      let for_non_null = |alts: Vec<crate::hydra::core::Term>| crate::hydra::lib::eithers::bind(try_term.clone()(crate::hydra::lib::lists::head(alts.clone())), |mterm: Option<crate::hydra::core::Term>| crate::hydra::lib::maybes::maybe(try_alts.clone()(crate::hydra::lib::lists::tail(alts.clone())), |t: crate::hydra::core::Term| Right(Some(t.clone())), mterm.clone())) ;
      let try_alts = |alts: Vec<crate::hydra::core::Term>| crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(alts.clone()), Right(None), for_non_null.clone()(alts.clone())) ;
      crate::hydra::lib::eithers::bind(term_alternatives(cx.clone(), graph.clone(), term.clone()), |alts0: Vec<crate::hydra::core::Term>| try_alts.clone()(alts0.clone()))} ;
    let try_term = |term: crate::hydra::core::Term| {
      let supported_variant = crate::hydra::lib::sets::member(crate::hydra::reflect::term_variant(term.clone()), constraints.clone().0.term_variants.clone()) ;
      crate::hydra::lib::logic::if_else(supported_variant.clone(), for_supported.clone()(term.clone()), for_unsupported.clone()(term.clone()))} ;
    crate::hydra::lib::eithers::bind(recurse.clone()(term02.clone()), |term1: crate::hydra::core::Term| match &*term1.clone().0 {
      crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::lib::eithers::bind(adapt_type(constraints.clone(), litmap.clone(), v0_.clone().0.type_.clone()), |atyp: crate::hydra::core::Type| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
          body: v0_.clone().0.body.clone(),
          type_: atyp.clone()})))))))},
      crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
        let v0_ = v0_.clone() ;
        Right(term1.clone())},
      _ => crate::hydra::lib::eithers::bind(try_term.clone()(term1.clone()), |mterm: Option<crate::hydra::core::Term>| crate::hydra::lib::maybes::maybe(Left(crate::hydra::lib::strings::cat2(String::from("no alternatives for term: "), crate::hydra::show::core::term(term1.clone()))), |term2: crate::hydra::core::Term| Right(term2.clone()), mterm.clone()))})} ;
  crate::hydra::rewriting::rewrite_term_m(rewrite.clone(), term0.clone())}

pub fn adapt_term_for_language(lang: crate::hydra::coders::Language, cx: crate::hydra::context::Context, g: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<String, crate::hydra::core::Term> {
  let constraints = lang.clone().0.constraints.clone() ;
  let litmap = adapt_literal_types_map(constraints.clone()) ;
  adapt_term(constraints.clone(), litmap.clone(), cx.clone(), g.clone(), term.clone())}

pub fn adapt_type(constraints: crate::hydra::coders::LanguageConstraints, litmap: BTreeMap<crate::hydra::core::LiteralType, crate::hydra::core::LiteralType>, type0: crate::hydra::core::Type) -> Either<String, crate::hydra::core::Type> {
  let for_supported = |typ: crate::hydra::core::Type| match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(literal_type_supported(constraints.clone(), v0_.clone()), Some(typ.clone()), crate::hydra::lib::maybes::maybe(Some(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String)))))), |lt2: crate::hydra::core::LiteralType| Some(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(lt2.clone())))), crate::hydra::lib::maps::lookup(v0_.clone(), litmap.clone())))},
    _ => Some(typ.clone())} ;
  let for_unsupported = |typ: crate::hydra::core::Type| {
    let try_alts = |alts: Vec<crate::hydra::core::Type>| crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(alts.clone()), None, crate::hydra::lib::maybes::maybe(try_alts.clone()(crate::hydra::lib::lists::tail(alts.clone())), |t: crate::hydra::core::Type| Some(t.clone()), try_type.clone()(crate::hydra::lib::lists::head(alts.clone())))) ;
    {
      let alts0 = type_alternatives(typ.clone()) ;
      try_alts.clone()(alts0.clone())}} ;
  let try_type = |typ: crate::hydra::core::Type| {
    let supported_variant = crate::hydra::lib::sets::member(crate::hydra::reflect::type_variant(typ.clone()), constraints.clone().0.type_variants.clone()) ;
    crate::hydra::lib::logic::if_else(supported_variant.clone(), for_supported.clone()(typ.clone()), for_unsupported.clone()(typ.clone()))} ;
  let rewrite = |recurse: Rc<dyn Fn(crate::hydra::core::Type) -> Either<String, crate::hydra::core::Type>>, typ: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(recurse.clone()(typ.clone()), |type1: crate::hydra::core::Type| crate::hydra::lib::maybes::maybe(Left(crate::hydra::lib::strings::cat2(String::from("no alternatives for type: "), crate::hydra::show::core::type_(typ.clone()))), |type2: crate::hydra::core::Type| Right(type2.clone()), try_type.clone()(type1.clone()))) ;
  crate::hydra::rewriting::rewrite_type_m(rewrite.clone(), type0.clone())}

pub fn adapt_type_for_language(lang: crate::hydra::coders::Language, typ: crate::hydra::core::Type) -> Either<String, crate::hydra::core::Type> {
  let constraints = lang.clone().0.constraints.clone() ;
  let litmap = adapt_literal_types_map(constraints.clone()) ;
  adapt_type(constraints.clone(), litmap.clone(), typ.clone())}

pub fn adapt_type_scheme(constraints: crate::hydra::coders::LanguageConstraints, litmap: BTreeMap<crate::hydra::core::LiteralType, crate::hydra::core::LiteralType>, ts0: crate::hydra::core::TypeScheme) -> Either<String, crate::hydra::core::TypeScheme> {
  let vars0 = ts0.clone().0.variables.clone() ;
  let t0 = ts0.clone().0.type_.clone() ;
  crate::hydra::lib::eithers::bind(adapt_type(constraints.clone(), litmap.clone(), t0.clone()), |t1: crate::hydra::core::Type| Right(crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
    variables: vars0.clone(),
    type_: t1.clone(),
    constraints: ts0.clone().0.constraints.clone()}))))}

pub fn compose_coders(c1: crate::hydra::util::Coder, c2: crate::hydra::util::Coder) -> crate::hydra::util::Coder {
  crate::hydra::util::Coder(Rc::new(crate::hydra::util::Coder_Variant {
    encode: |cx: crate::hydra::context::Context, a: T0| crate::hydra::lib::eithers::bind(c1.clone().0.encode.clone(), |b1: T1| c2.clone().0.encode.clone()),
    decode: |cx: crate::hydra::context::Context, c: T2| crate::hydra::lib::eithers::bind(c2.clone().0.decode.clone(), |b2: T1| c1.clone().0.decode.clone())}))}

pub fn data_graph_to_definitions(constraints: crate::hydra::coders::LanguageConstraints, do_infer: bool, do_expand: bool, do_hoist_case_statements: bool, do_hoist_polymorphic_let_bindings: bool, original_bindings: Vec<crate::hydra::core::Binding>, graph0: crate::hydra::graph::Graph, namespaces: Vec<crate::hydra::module::Namespace>, cx: crate::hydra::context::Context) -> Either<String, (crate::hydra::graph::Graph, Vec<Vec<crate::hydra::module::TermDefinition>>)> {
  let namespaces_set = crate::hydra::lib::sets::from_list(namespaces.clone()) ;
  let is_parent_binding = |b: crate::hydra::core::Binding| crate::hydra::lib::maybes::maybe(false, |ns: crate::hydra::module::Namespace| crate::hydra::lib::sets::member(ns.clone(), namespaces_set.clone()), crate::hydra::names::namespace_of(b.clone().0.name.clone())) ;
  let hoist_cases = |bindings: Vec<crate::hydra::core::Binding>| {
    let stripped = crate::hydra::lib::lists::map(|b: crate::hydra::core::Binding| crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
      name: b.clone().0.name.clone(),
      term: crate::hydra::rewriting::strip_type_lambdas(b.clone().0.term.clone()),
      type_: b.clone().0.type_.clone()})), bindings.clone()) ;
    {
      let term0 = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
        bindings: stripped.clone(),
        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))))) ;
      {
        let unshadowed0 = crate::hydra::schemas::term_as_bindings(crate::hydra::rewriting::unshadow_variables(term0.clone())) ;
        {
          let hoisted = crate::hydra::hoisting::hoist_case_statements_in_graph(unshadowed0.clone()) ;
          {
            let term1 = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
              bindings: hoisted.clone(),
              body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))))) ;
            crate::hydra::schemas::term_as_bindings(crate::hydra::rewriting::unshadow_variables(term1.clone()))}}}}} ;
  let hoist_poly = |bindings: Vec<crate::hydra::core::Binding>| {
    let let_before = crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
      bindings: bindings.clone(),
      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))})) ;
    {
      let let_after = crate::hydra::hoisting::hoist_polymorphic_let_bindings(is_parent_binding.clone(), let_before.clone()) ;
      let_after.clone().0.bindings.clone()}} ;
  let check_bindings_typed = |debug_label: String, bindings: Vec<crate::hydra::core::Binding>| {
    let untyped_bindings = crate::hydra::lib::lists::map(|b: crate::hydra::core::Binding| b.clone().0.name.clone().0.0.clone(), crate::hydra::lib::lists::filter(|b: crate::hydra::core::Binding| crate::hydra::lib::logic::not(crate::hydra::lib::maybes::is_just(b.clone().0.type_.clone())), bindings.clone())) ;
    crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(untyped_bindings.clone()), Right(bindings.clone()), Left(crate::hydra::lib::strings::cat(Vec::from([
      String::from("Found untyped bindings ("),
      debug_label.clone(),
      String::from("): "),
      crate::hydra::lib::strings::intercalate(String::from(", "), untyped_bindings.clone())]))))} ;
  let normalize_bindings = |bindings: Vec<crate::hydra::core::Binding>| crate::hydra::lib::lists::map(|b: crate::hydra::core::Binding| crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
    name: b.clone().0.name.clone(),
    term: push_type_apps_inward(b.clone().0.term.clone()),
    type_: b.clone().0.type_.clone()})), bindings.clone()) ;
  let rebuild_graph = |bindings: Vec<crate::hydra::core::Binding>| {
    let g = crate::hydra::lexical::build_graph(bindings.clone(), crate::hydra::lib::maps::empty, graph0.clone().0.primitives.clone()) ;
    crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
      bound_terms: g.clone().0.bound_terms.clone(),
      bound_types: g.clone().0.bound_types.clone(),
      class_constraints: g.clone().0.class_constraints.clone(),
      lambda_variables: g.clone().0.lambda_variables.clone(),
      metadata: g.clone().0.metadata.clone(),
      primitives: g.clone().0.primitives.clone(),
      schema_types: graph0.clone().0.schema_types.clone(),
      type_variables: g.clone().0.type_variables.clone()}))} ;
  let bins0 = original_bindings.clone() ;
  let bins1 = crate::hydra::lib::logic::if_else(do_hoist_case_statements.clone(), hoist_cases.clone()(bins0.clone()), bins0.clone()) ;
  crate::hydra::lib::eithers::bind(crate::hydra::lib::logic::if_else(do_infer.clone(), crate::hydra::lib::eithers::map(|result: ((crate::hydra::graph::Graph, Vec<crate::hydra::core::Binding>), crate::hydra::context::Context)| crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::first(result.clone())), crate::hydra::lib::eithers::bimap(|ic: crate::hydra::context::InContext| crate::hydra::show::error::error(ic.clone().0.object.clone()), |x: ((crate::hydra::graph::Graph, Vec<crate::hydra::core::Binding>), crate::hydra::context::Context)| x.clone(), crate::hydra::inference::infer_graph_types(cx.clone(), bins1.clone(), rebuild_graph.clone()(bins1.clone())))), check_bindings_typed.clone()(String::from("after case hoisting"), bins1.clone())), |bins2: Vec<crate::hydra::core::Binding>| crate::hydra::lib::eithers::bind(crate::hydra::lib::logic::if_else(do_hoist_polymorphic_let_bindings.clone(), check_bindings_typed.clone()(String::from("after let hoisting"), hoist_poly.clone()(bins2.clone())), Right(bins2.clone())), |bins3: Vec<crate::hydra::core::Binding>| crate::hydra::lib::eithers::bind(adapt_data_graph(constraints.clone(), do_expand.clone(), bins3.clone(), cx.clone(), rebuild_graph.clone()(bins3.clone())), |adapt_result: (crate::hydra::graph::Graph, Vec<crate::hydra::core::Binding>)| {
    let adapted = crate::hydra::lib::pairs::first(adapt_result.clone()) ;
    {
      let adapted_bindings = crate::hydra::lib::pairs::second(adapt_result.clone()) ;
      crate::hydra::lib::eithers::bind(check_bindings_typed.clone()(String::from("after adaptation"), adapted_bindings.clone()), |bins4: Vec<crate::hydra::core::Binding>| {
        let bins5 = normalize_bindings.clone()(bins4.clone()) ;
        {
          let to_def = |el: crate::hydra::core::Binding| crate::hydra::lib::maybes::map(|ts: crate::hydra::core::TypeScheme| crate::hydra::module::TermDefinition(Rc::new(crate::hydra::module::TermDefinition_Variant {
            name: el.clone().0.name.clone(),
            term: el.clone().0.term.clone(),
            type_: ts.clone()})), el.clone().0.type_.clone()) ;
          {
            let selected_elements = crate::hydra::lib::lists::filter(|el: crate::hydra::core::Binding| crate::hydra::lib::maybes::maybe(false, |ns: crate::hydra::module::Namespace| crate::hydra::lib::sets::member(ns.clone(), namespaces_set.clone()), crate::hydra::names::namespace_of(el.clone().0.name.clone())), bins5.clone()) ;
            {
              let elements_by_namespace = crate::hydra::lib::lists::foldl(|acc: BTreeMap<crate::hydra::module::Namespace, Vec<crate::hydra::core::Binding>>, el: crate::hydra::core::Binding| crate::hydra::lib::maybes::maybe(acc.clone(), |ns: crate::hydra::module::Namespace| {
                let existing = crate::hydra::lib::maybes::maybe(Vec::from([]), crate::hydra::lib::equality::identity, crate::hydra::lib::maps::lookup(ns.clone(), acc.clone())) ;
                crate::hydra::lib::maps::insert(ns.clone(), crate::hydra::lib::lists::concat2(existing.clone(), Vec::from([
                  el.clone()])), acc.clone())}, crate::hydra::names::namespace_of(el.clone().0.name.clone())), crate::hydra::lib::maps::empty, selected_elements.clone()) ;
              {
                let defs_grouped = crate::hydra::lib::lists::map(|ns: crate::hydra::module::Namespace| {
                  let els_for_ns = crate::hydra::lib::maybes::maybe(Vec::from([]), crate::hydra::lib::equality::identity, crate::hydra::lib::maps::lookup(ns.clone(), elements_by_namespace.clone())) ;
                  crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(to_def.clone(), els_for_ns.clone()))}, namespaces.clone()) ;
                {
                  let g = crate::hydra::lexical::build_graph(bins5.clone(), crate::hydra::lib::maps::empty, adapted.clone().0.primitives.clone()) ;
                  Right((crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
                    bound_terms: g.clone().0.bound_terms.clone(),
                    bound_types: g.clone().0.bound_types.clone(),
                    class_constraints: g.clone().0.class_constraints.clone(),
                    lambda_variables: g.clone().0.lambda_variables.clone(),
                    metadata: g.clone().0.metadata.clone(),
                    primitives: g.clone().0.primitives.clone(),
                    schema_types: adapted.clone().0.schema_types.clone(),
                    type_variables: g.clone().0.type_variables.clone()})), defs_grouped.clone()))}}}}}})}})))}

pub fn literal_type_supported(constraints: crate::hydra::coders::LanguageConstraints, lt: crate::hydra::core::LiteralType) -> bool {
  let for_type = |lt2: crate::hydra::core::LiteralType| match &*lt2.clone().0 {
    crate::hydra::core::LiteralType_Variant::Float (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::sets::member(v0_.clone(), constraints.clone().0.float_types.clone())},
    crate::hydra::core::LiteralType_Variant::Integer (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::sets::member(v0_.clone(), constraints.clone().0.integer_types.clone())},
    _ => true} ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::sets::member(crate::hydra::reflect::literal_type_variant(lt.clone()), constraints.clone().0.literal_variants.clone()), for_type.clone()(lt.clone()), false)}

pub fn push_type_apps_inward(term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let push = |body: crate::hydra::core::Term, typ: crate::hydra::core::Type| match &*body.clone().0 {
    crate::hydra::core::Term_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      go.clone()(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
        function: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
          body: v0_.clone().0.function.clone(),
          type_: typ.clone()}))))),
        argument: v0_.clone().0.argument.clone()}))))))},
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Function_Variant::Lambda (v0_) => {
          let v0_ = v0_.clone() ;
          go.clone()(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: v0_.clone().0.parameter.clone(),
            domain: v0_.clone().0.domain.clone(),
            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
              body: v0_.clone().0.body.clone(),
              type_: typ.clone()})))))})))))))))},
        _ => crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
          body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(v0_.clone()))),
          type_: typ.clone()})))))}},
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      go.clone()(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
        bindings: v0_.clone().0.bindings.clone(),
        body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
          body: v0_.clone().0.body.clone(),
          type_: typ.clone()})))))}))))))},
    _ => crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
      body: body.clone(),
      type_: typ.clone()})))))} ;
  let go = |t: crate::hydra::core::Term| {
    let for_field = |fld: crate::hydra::core::Field| crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
      name: fld.clone().0.name.clone(),
      term: go.clone()(fld.clone().0.term.clone())})) ;
    {
      let for_elimination = |elm: crate::hydra::core::Elimination| match &*elm.clone().0 {
        crate::hydra::core::Elimination_Variant::Record (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Record(v0_.clone())))},
        crate::hydra::core::Elimination_Variant::Union (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
            type_name: v0_.clone().0.type_name.clone(),
            default_: crate::hydra::lib::maybes::map(go.clone(), v0_.clone().0.default_.clone()),
            cases: crate::hydra::lib::lists::map(for_field.clone(), v0_.clone().0.cases.clone())})))))},
        crate::hydra::core::Elimination_Variant::Wrap (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Wrap(v0_.clone())))}} ;
      {
        let for_function = |fun: crate::hydra::core::Function| match &*fun.clone().0 {
          crate::hydra::core::Function_Variant::Elimination (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(for_elimination.clone()(v0_.clone()))))},
          crate::hydra::core::Function_Variant::Lambda (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
              parameter: v0_.clone().0.parameter.clone(),
              domain: v0_.clone().0.domain.clone(),
              body: go.clone()(v0_.clone().0.body.clone())})))))},
          crate::hydra::core::Function_Variant::Primitive (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(v0_.clone())))}} ;
        {
          let for_let = |lt: crate::hydra::core::Let| {
            let map_binding = |b: crate::hydra::core::Binding| crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
              name: b.clone().0.name.clone(),
              term: go.clone()(b.clone().0.term.clone()),
              type_: b.clone().0.type_.clone()})) ;
            crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
              bindings: crate::hydra::lib::lists::map(map_binding.clone(), lt.clone().0.bindings.clone()),
              body: go.clone()(lt.clone().0.body.clone())}))} ;
          {
            let for_map = |m: BTreeMap<crate::hydra::core::Term, crate::hydra::core::Term>| {
              let for_pair = |p: (crate::hydra::core::Term, crate::hydra::core::Term)| (go.clone()(crate::hydra::lib::pairs::first(p.clone())), go.clone()(crate::hydra::lib::pairs::second(p.clone()))) ;
              crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(for_pair.clone(), crate::hydra::lib::maps::to_list(m.clone())))} ;
            match &*t.clone().0 {
              crate::hydra::core::Term_Variant::Annotated (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
                  body: go.clone()(v0_.clone().0.body.clone()),
                  annotation: v0_.clone().0.annotation.clone()})))))},
              crate::hydra::core::Term_Variant::Application (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: go.clone()(v0_.clone().0.function.clone()),
                  argument: go.clone()(v0_.clone().0.argument.clone())})))))},
              crate::hydra::core::Term_Variant::Either (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(crate::hydra::lib::eithers::either(|l: crate::hydra::core::Term| Left(go.clone()(l.clone())), |r: crate::hydra::core::Term| Right(go.clone()(r.clone())), v0_.clone()))))},
              crate::hydra::core::Term_Variant::Function (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(for_function.clone()(v0_.clone()))))},
              crate::hydra::core::Term_Variant::Let (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(for_let.clone()(v0_.clone()))))},
              crate::hydra::core::Term_Variant::List (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(go.clone(), v0_.clone()))))},
              crate::hydra::core::Term_Variant::Literal (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(v0_.clone())))},
              crate::hydra::core::Term_Variant::Map (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(for_map.clone()(v0_.clone()))))},
              crate::hydra::core::Term_Variant::Maybe (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(crate::hydra::lib::maybes::map(go.clone(), v0_.clone()))))},
              crate::hydra::core::Term_Variant::Pair (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Pair((go.clone()(crate::hydra::lib::pairs::first(v0_.clone())), go.clone()(crate::hydra::lib::pairs::second(v0_.clone()))))))},
              crate::hydra::core::Term_Variant::Record (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
                  type_name: v0_.clone().0.type_name.clone(),
                  fields: crate::hydra::lib::lists::map(for_field.clone(), v0_.clone().0.fields.clone())})))))},
              crate::hydra::core::Term_Variant::Set (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Set(crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::map(go.clone(), crate::hydra::lib::sets::to_list(v0_.clone()))))))},
              crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
                let v0_ = v0_.clone() ;
                {
                  let body1 = go.clone()(v0_.clone().0.body.clone()) ;
                  push.clone()(body1.clone(), v0_.clone().0.type_.clone())}},
              crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeLambda(crate::hydra::core::TypeLambda(Rc::new(crate::hydra::core::TypeLambda_Variant {
                  parameter: v0_.clone().0.parameter.clone(),
                  body: go.clone()(v0_.clone().0.body.clone())})))))},
              crate::hydra::core::Term_Variant::Union (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
                  type_name: v0_.clone().0.type_name.clone(),
                  field: for_field.clone()(v0_.clone().0.field.clone())})))))},
              crate::hydra::core::Term_Variant::Unit (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))},
              crate::hydra::core::Term_Variant::Variable (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(v0_.clone())))},
              crate::hydra::core::Term_Variant::Wrap (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                  type_name: v0_.clone().0.type_name.clone(),
                  body: go.clone()(v0_.clone().0.body.clone())})))))}}}}}}} ;
  go.clone()(term.clone())}

pub fn schema_graph_to_definitions(constraints: crate::hydra::coders::LanguageConstraints, graph: crate::hydra::graph::Graph, name_lists: Vec<Vec<crate::hydra::core::Name>>, cx: crate::hydra::context::Context) -> Either<String, (BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>, Vec<Vec<crate::hydra::module::TypeDefinition>>)> {
  let litmap = adapt_literal_types_map(constraints.clone()) ;
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|ic: crate::hydra::context::InContext| ic.clone().0.object.clone().0.0.clone(), |x: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>| x.clone(), crate::hydra::schemas::graph_as_types(cx.clone(), graph.clone(), crate::hydra::lexical::graph_to_bindings(graph.clone()))), |tmap0: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>| crate::hydra::lib::eithers::bind(adapt_graph_schema(constraints.clone(), litmap.clone(), tmap0.clone()), |tmap1: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>| {
    let to_def = |pair: (crate::hydra::core::Name, crate::hydra::core::Type)| crate::hydra::module::TypeDefinition(Rc::new(crate::hydra::module::TypeDefinition_Variant {
      name: crate::hydra::lib::pairs::first(pair.clone()),
      type_: crate::hydra::lib::pairs::second(pair.clone())})) ;
    Right((tmap1.clone(), crate::hydra::lib::lists::map(|names: Vec<crate::hydra::core::Name>| crate::hydra::lib::lists::map(to_def.clone(), crate::hydra::lib::lists::map(|n: crate::hydra::core::Name| (n.clone(), crate::hydra::lib::maybes::from_just(crate::hydra::lib::maps::lookup(n.clone(), tmap1.clone()))), names.clone())), name_lists.clone())))}))}

pub fn simple_language_adapter(lang: crate::hydra::coders::Language, cx: T0, g: crate::hydra::graph::Graph, typ: crate::hydra::core::Type) -> Either<String, crate::hydra::util::Adapter> {
  let constraints = lang.clone().0.constraints.clone() ;
  let litmap = adapt_literal_types_map(constraints.clone()) ;
  crate::hydra::lib::eithers::bind(adapt_type(constraints.clone(), litmap.clone(), typ.clone()), |adapted_type: crate::hydra::core::Type| Right(crate::hydra::util::Adapter(Rc::new(crate::hydra::util::Adapter_Variant {
    is_lossy: false,
    source: typ.clone(),
    target: adapted_type.clone(),
    coder: crate::hydra::util::Coder(Rc::new(crate::hydra::util::Coder_Variant {
      encode: |cx2: crate::hydra::context::Context, term: crate::hydra::core::Term| crate::hydra::lib::eithers::bimap(|_s: String| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
        object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(_s.clone())))))),
        context: cx2.clone()})), |_x: crate::hydra::core::Term| _x.clone(), adapt_term(constraints.clone(), litmap.clone(), cx2.clone(), g.clone(), term.clone())),
      decode: |cx2: crate::hydra::context::Context, term: crate::hydra::core::Term| Right(term.clone())}))}))))}

pub fn term_alternatives(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<String, Vec<crate::hydra::core::Term>> {
  match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let term2 = v0_.clone().0.body.clone() ;
        Right(Vec::from([
          term2.clone()]))}},
    crate::hydra::core::Term_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      Right(Vec::from([
        crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::maybes::maybe(Vec::from([]), |term2: crate::hydra::core::Term| Vec::from([
          term2.clone()]), v0_.clone()))))]))},
    crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let term2 = v0_.clone().0.body.clone() ;
        Right(Vec::from([
          term2.clone()]))}},
    crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let term2 = v0_.clone().0.body.clone() ;
        Right(Vec::from([
          term2.clone()]))}},
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let tname = v0_.clone().0.type_name.clone() ;
        {
          let field = v0_.clone().0.field.clone() ;
          {
            let fname = field.clone().0.name.clone() ;
            {
              let fterm = field.clone().0.term.clone() ;
              {
                let for_field_type = |ft: crate::hydra::core::FieldType| {
                  let ftname = ft.clone().0.name.clone() ;
                  crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                    name: fname.clone(),
                    term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(ftname.clone(), fname.clone()), Some(fterm.clone()), None))))}))} ;
                crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|ic: crate::hydra::context::InContext| crate::hydra::show::error::error(ic.clone().0.object.clone()), |x: Vec<crate::hydra::core::FieldType>| x.clone(), crate::hydra::schemas::require_union_type(cx.clone(), graph.clone(), tname.clone())), |rt: Vec<crate::hydra::core::FieldType>| Right(Vec::from([
                  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
                    type_name: tname.clone(),
                    fields: crate::hydra::lib::lists::map(for_field_type.clone(), rt.clone())})))))])))}}}}}},
    crate::hydra::core::Term_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      Right(Vec::from([
        crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Boolean(true))))))]))},
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let term2 = v0_.clone().0.body.clone() ;
        Right(Vec::from([
          term2.clone()]))}},
    _ => Right(Vec::from([]))}}

pub fn type_alternatives(type_: crate::hydra::core::Type) -> Vec<crate::hydra::core::Type> {
  match &*type_.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let type2 = v0_.clone().0.body.clone() ;
        Vec::from([
          type2.clone()])}},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(v0_.clone())))])},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let to_opt_field = |f: crate::hydra::core::FieldType| crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: f.clone().0.name.clone(),
          type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(f.clone().0.type_.clone())))})) ;
        {
          let opt_fields = crate::hydra::lib::lists::map(to_opt_field.clone(), v0_.clone()) ;
          Vec::from([
            crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(opt_fields.clone())))])}}},
    crate::hydra::core::Type_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Boolean)))))])},
    _ => Vec::from([])}}
