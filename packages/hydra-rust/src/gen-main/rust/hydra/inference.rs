#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::annotations::*;
use crate::hydra::checking::*;
use crate::hydra::extract::core::*;
use crate::hydra::lexical::*;
use crate::hydra::reflect::*;
use crate::hydra::rewriting::*;
use crate::hydra::schemas::*;
use crate::hydra::show::core::*;
use crate::hydra::show::error::*;
use crate::hydra::show::typing::*;
use crate::hydra::sorting::*;
use crate::hydra::substitution::*;
use crate::hydra::unification::*;
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

pub fn bind_constraints(flow_cx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, constraints: Vec<crate::hydra::typing::TypeConstraint>) -> Either<crate::hydra::context::InContext, crate::hydra::typing::TypeSubst> {
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|_ic: crate::hydra::context::InContext| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(_ic.clone().0.object.clone().0.message.clone())))))),
    context: _ic.clone().0.context.clone()})), |_a: crate::hydra::typing::TypeSubst| _a.clone(), crate::hydra::unification::unify_type_constraints(flow_cx.clone(), cx.clone().0.schema_types.clone(), constraints.clone())), |s: crate::hydra::typing::TypeSubst| crate::hydra::lib::eithers::bind(crate::hydra::checking::check_type_subst(flow_cx.clone(), cx.clone(), s.clone()), |_: crate::hydra::typing::TypeSubst| Right(s.clone())))}

pub fn bind_unbound_type_variables(cx: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let svars = crate::hydra::lib::sets::from_list(crate::hydra::lib::maps::keys(cx.clone().0.schema_types.clone())) ;
  let rewrite = |recurse: Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>, term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let for_binding = |b: crate::hydra::core::Binding| {
          let bname = b.clone().0.name.clone() ;
          {
            let bterm = b.clone().0.term.clone() ;
            crate::hydra::lib::maybes::maybe(crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
              name: bname.clone(),
              term: bind_unbound_type_variables(cx.clone(), bterm.clone()),
              type_: None})), |ts: crate::hydra::core::TypeScheme| {
              let bvars = crate::hydra::lib::sets::from_list(ts.clone().0.variables.clone()) ;
              {
                let unbound_in_type = crate::hydra::rewriting::free_variables_in_type(ts.clone().0.type_.clone()) ;
                {
                  let unbound_in_term = crate::hydra::rewriting::free_type_variables_in_term(bterm.clone()) ;
                  {
                    let unbound = crate::hydra::lib::sets::to_list(crate::hydra::lib::sets::difference(crate::hydra::lib::sets::union_(unbound_in_type.clone(), unbound_in_term.clone()), crate::hydra::lib::sets::union_(svars.clone(), bvars.clone()))) ;
                    {
                      let ts2 = crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
                        variables: crate::hydra::lib::lists::concat2(ts.clone().0.variables.clone(), unbound.clone()),
                        type_: ts.clone().0.type_.clone(),
                        constraints: ts.clone().0.constraints.clone()})) ;
                      {
                        let bterm2 = crate::hydra::lib::lists::foldl(|t: crate::hydra::core::Term, v: crate::hydra::core::Name| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeLambda(crate::hydra::core::TypeLambda(Rc::new(crate::hydra::core::TypeLambda_Variant {
                          parameter: v.clone(),
                          body: t.clone()}))))), bterm.clone(), unbound.clone()) ;
                        crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                          name: bname.clone(),
                          term: bterm2.clone(),
                          type_: Some(ts2.clone())}))}}}}}}, b.clone().0.type_.clone())}} ;
        crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
          bindings: crate::hydra::lib::lists::map(for_binding.clone(), v0_.clone().0.bindings.clone()),
          body: bind_unbound_type_variables(cx.clone(), v0_.clone().0.body.clone())})))))}},
    _ => recurse.clone()(term.clone())} ;
  crate::hydra::rewriting::rewrite_term(rewrite.clone(), term0.clone())}

pub fn build_type_application_term(tvars: Vec<crate::hydra::core::Name>, body: crate::hydra::core::Term) -> crate::hydra::core::Term {
  crate::hydra::lib::lists::foldl(|t: crate::hydra::core::Term, v: crate::hydra::core::Name| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
    body: t.clone(),
    type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v.clone())))}))))), body.clone(), tvars.clone())}

pub fn extend_context(pairs: Vec<(crate::hydra::core::Name, crate::hydra::core::TypeScheme)>, cx: crate::hydra::graph::Graph) -> crate::hydra::graph::Graph {
  crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
    bound_terms: cx.clone().0.bound_terms.clone(),
    bound_types: crate::hydra::lib::maps::union_(crate::hydra::lib::maps::from_list(pairs.clone()), cx.clone().0.bound_types.clone()),
    class_constraints: cx.clone().0.class_constraints.clone(),
    lambda_variables: cx.clone().0.lambda_variables.clone(),
    metadata: cx.clone().0.metadata.clone(),
    primitives: cx.clone().0.primitives.clone(),
    schema_types: cx.clone().0.schema_types.clone(),
    type_variables: cx.clone().0.type_variables.clone()}))}

pub fn finalize_inferred_term(flow_cx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Term> {
  let term2 = bind_unbound_type_variables(cx.clone(), term.clone()) ;
  crate::hydra::lib::eithers::bind(crate::hydra::checking::check_for_unbound_type_variables(flow_cx.clone(), cx.clone(), term2.clone()), |_: ()| Right(crate::hydra::rewriting::normalize_type_variables_in_term(term2.clone())))}

pub fn for_inferred_term(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, term: crate::hydra::core::Term, desc: String, f: impl Fn(crate::hydra::typing::InferenceResult) -> T0 + Clone) -> Either<crate::hydra::context::InContext, (T0, crate::hydra::context::Context)> {
  crate::hydra::lib::eithers::bind(infer_type_of_term(fcx.clone(), cx.clone(), term.clone(), desc.clone()), |rp: crate::hydra::typing::InferenceResult| Right((f.clone()(rp.clone()), rp.clone().0.context.clone())))}

pub fn free_variables_in_context(cx: crate::hydra::graph::Graph) -> BTreeSet<crate::hydra::core::Name> {
  crate::hydra::lib::lists::foldl(crate::hydra::lib::sets::union_, crate::hydra::lib::sets::empty, crate::hydra::lib::lists::map(crate::hydra::rewriting::free_variables_in_type_scheme_simple, crate::hydra::lib::maps::elems(cx.clone().0.bound_types.clone())))}

pub fn fresh_variable_type(cx: crate::hydra::context::Context) -> (crate::hydra::core::Type, crate::hydra::context::Context) {
  let result = crate::hydra::schemas::fresh_name(cx.clone()) ;
  let name = crate::hydra::lib::pairs::first(result.clone()) ;
  let cx2 = crate::hydra::lib::pairs::second(result.clone()) ;
  (crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(name.clone()))), cx2.clone())}

pub fn generalize(cx: crate::hydra::graph::Graph, typ: crate::hydra::core::Type) -> crate::hydra::core::TypeScheme {
  let is_type_var_name = |name: crate::hydra::core::Name| {
    let parts = crate::hydra::lib::strings::split_on(String::from("."), name.clone().0.0.clone()) ;
    crate::hydra::lib::equality::lte(crate::hydra::lib::lists::length(parts.clone()), 1i32)} ;
  let vars = crate::hydra::lib::lists::nub(crate::hydra::lib::lists::filter(|v: crate::hydra::core::Name| crate::hydra::lib::logic::and(is_unbound(cx.clone(), v.clone()), is_type_var_name.clone()(v.clone())), crate::hydra::rewriting::free_variables_in_type_ordered(typ.clone()))) ;
  let all_constraints = cx.clone().0.class_constraints.clone() ;
  let relevant_constraints = crate::hydra::lib::maps::from_list(crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(|v: crate::hydra::core::Name| crate::hydra::lib::maybes::map(|meta: crate::hydra::core::TypeVariableMetadata| (v.clone(), meta.clone()), crate::hydra::lib::maps::lookup(v.clone(), all_constraints.clone())), vars.clone()))) ;
  let constraints_maybe = crate::hydra::lib::logic::if_else(crate::hydra::lib::maps::null(relevant_constraints.clone()), None, Some(relevant_constraints.clone())) ;
  crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
    variables: vars.clone(),
    type_: typ.clone(),
    constraints: constraints_maybe.clone()}))}

pub fn infer_graph_types(fcx0: crate::hydra::context::Context, bindings0: Vec<crate::hydra::core::Binding>, g0: crate::hydra::graph::Graph) -> Either<crate::hydra::context::InContext, ((crate::hydra::graph::Graph, Vec<crate::hydra::core::Binding>), crate::hydra::context::Context)> {
  let fcx = crate::hydra::context::Context(Rc::new(crate::hydra::context::Context_Variant {
    trace: crate::hydra::lib::lists::cons(String::from("graph inference"), fcx0.clone().0.trace.clone()),
    messages: fcx0.clone().0.messages.clone(),
    other: fcx0.clone().0.other.clone()})) ;
  let let0 = crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
    bindings: bindings0.clone(),
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))})) ;
  let from_let_term = |l: crate::hydra::core::Let| {
    let bindings = l.clone().0.bindings.clone() ;
    {
      let prims = g0.clone().0.primitives.clone() ;
      {
        let schema_types = g0.clone().0.schema_types.clone() ;
        {
          let g = crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
            bound_terms: crate::hydra::lexical::build_graph(bindings.clone(), crate::hydra::lib::maps::empty, prims.clone()).0.bound_terms.clone(),
            bound_types: crate::hydra::lexical::build_graph(bindings.clone(), crate::hydra::lib::maps::empty, prims.clone()).0.bound_types.clone(),
            class_constraints: crate::hydra::lexical::build_graph(bindings.clone(), crate::hydra::lib::maps::empty, prims.clone()).0.class_constraints.clone(),
            lambda_variables: crate::hydra::lexical::build_graph(bindings.clone(), crate::hydra::lib::maps::empty, prims.clone()).0.lambda_variables.clone(),
            metadata: crate::hydra::lexical::build_graph(bindings.clone(), crate::hydra::lib::maps::empty, prims.clone()).0.metadata.clone(),
            primitives: crate::hydra::lexical::build_graph(bindings.clone(), crate::hydra::lib::maps::empty, prims.clone()).0.primitives.clone(),
            schema_types: schema_types.clone(),
            type_variables: crate::hydra::lexical::build_graph(bindings.clone(), crate::hydra::lib::maps::empty, prims.clone()).0.type_variables.clone()})) ;
          (g.clone(), bindings.clone())}}}} ;
  crate::hydra::lib::eithers::bind(infer_type_of_term(fcx.clone(), g0.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(let0.clone()))), String::from("graph term")), |result: crate::hydra::typing::InferenceResult| {
    let fcx2 = result.clone().0.context.clone() ;
    {
      let term = result.clone().0.term.clone() ;
      crate::hydra::lib::eithers::bind(finalize_inferred_term(fcx2.clone(), g0.clone(), term.clone()), |finalized: crate::hydra::core::Term| match &*finalized.clone().0 {
        crate::hydra::core::Term_Variant::Let (v0_) => {
          let v0_ = v0_.clone() ;
          Right((from_let_term.clone()(v0_.clone()), fcx2.clone()))},
        crate::hydra::core::Term_Variant::Variable (v0_) => {
          let v0_ = v0_.clone() ;
          Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
            object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(String::from("Expected inferred graph as let term"))))))),
            context: fcx2.clone()})))}})}})}

pub fn infer_in_graph_context(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  infer_type_of_term(fcx.clone(), cx.clone(), term.clone(), String::from("single term"))}

pub fn infer_many(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, pairs: Vec<(crate::hydra::core::Term, String)>) -> Either<crate::hydra::context::InContext, ((Vec<crate::hydra::core::Term>, (Vec<crate::hydra::core::Type>, (crate::hydra::typing::TypeSubst, BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>))), crate::hydra::context::Context)> {
  let dflt = {
    let e = crate::hydra::lib::pairs::first(crate::hydra::lib::lists::head(pairs.clone())) ;
    {
      let desc = crate::hydra::lib::pairs::second(crate::hydra::lib::lists::head(pairs.clone())) ;
      {
        let tl = crate::hydra::lib::lists::tail(pairs.clone()) ;
        crate::hydra::lib::eithers::bind(infer_type_of_term(fcx.clone(), cx.clone(), e.clone(), desc.clone()), |result1: crate::hydra::typing::InferenceResult| {
          let fcx2 = result1.clone().0.context.clone() ;
          {
            let e1 = result1.clone().0.term.clone() ;
            {
              let t1 = result1.clone().0.type_.clone() ;
              {
                let s1 = result1.clone().0.subst.clone() ;
                {
                  let c1 = result1.clone().0.class_constraints.clone() ;
                  crate::hydra::lib::eithers::bind(infer_many(fcx2.clone(), crate::hydra::substitution::subst_in_context(s1.clone(), cx.clone()), tl.clone()), |rp2: ((Vec<crate::hydra::core::Term>, (Vec<crate::hydra::core::Type>, (crate::hydra::typing::TypeSubst, BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>))), crate::hydra::context::Context)| {
                    let result2 = crate::hydra::lib::pairs::first(rp2.clone()) ;
                    {
                      let fcx3 = crate::hydra::lib::pairs::second(rp2.clone()) ;
                      {
                        let e2 = crate::hydra::lib::pairs::first(result2.clone()) ;
                        {
                          let t2 = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(result2.clone())) ;
                          {
                            let s2 = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(result2.clone()))) ;
                            {
                              let c2 = crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(result2.clone()))) ;
                              {
                                let c1_subst = crate::hydra::substitution::subst_in_class_constraints(s2.clone(), c1.clone()) ;
                                {
                                  let merged_constraints = merge_class_constraints(c1_subst.clone(), c2.clone()) ;
                                  Right(((crate::hydra::lib::lists::cons(crate::hydra::substitution::subst_types_in_term(s2.clone(), e1.clone()), e2.clone()), (crate::hydra::lib::lists::cons(crate::hydra::substitution::subst_in_type(s2.clone(), t1.clone()), t2.clone()), (crate::hydra::substitution::compose_type_subst(s1.clone(), s2.clone()), merged_constraints.clone()))), fcx3.clone()))}}}}}}}})}}}}})}}} ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(pairs.clone()), Right(((Vec::from([]), (Vec::from([]), (crate::hydra::substitution::id_type_subst, crate::hydra::lib::maps::empty))), fcx.clone())), dflt.clone())}

pub fn infer_type_of(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, ((crate::hydra::core::Term, crate::hydra::core::TypeScheme), crate::hydra::context::Context)> {
  let let_term = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
    bindings: Vec::from([
      crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
        name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("ignoredVariableName")))),
        term: term.clone(),
        type_: None}))]),
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("ignoredBody")))))))}))))) ;
  crate::hydra::lib::eithers::bind(infer_type_of_term(fcx.clone(), cx.clone(), let_term.clone(), String::from("infer type of term")), |result: crate::hydra::typing::InferenceResult| {
    let fcx2 = result.clone().0.context.clone() ;
    crate::hydra::lib::eithers::bind(finalize_inferred_term(fcx2.clone(), cx.clone(), result.clone().0.term.clone()), |finalized: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::core::let_(fcx2.clone(), cx.clone(), finalized.clone()), |let_result: crate::hydra::core::Let| {
      let bindings = let_result.clone().0.bindings.clone() ;
      crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(1i32, crate::hydra::lib::lists::length(bindings.clone())), {
        let binding = crate::hydra::lib::lists::head(bindings.clone()) ;
        {
          let term1 = binding.clone().0.term.clone() ;
          {
            let mts = binding.clone().0.type_.clone() ;
            crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
              object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(String::from("Expected a type scheme"))))))),
              context: fcx2.clone()}))), |ts: crate::hydra::core::TypeScheme| Right(((term1.clone(), ts.clone()), fcx2.clone())), mts.clone())}}}, Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
        object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("Expected a single binding with a type scheme, but got: "),
          crate::hydra::lib::literals::show_int32(crate::hydra::lib::lists::length(bindings.clone())),
          String::from(" bindings")])))))))),
        context: fcx2.clone()}))))}))})}

pub fn infer_type_of_annotated_term(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, at: crate::hydra::core::AnnotatedTerm) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  let term = at.clone().0.body.clone() ;
  let ann = at.clone().0.annotation.clone() ;
  crate::hydra::lib::eithers::bind(infer_type_of_term(fcx.clone(), cx.clone(), term.clone(), String::from("annotated term")), |result: crate::hydra::typing::InferenceResult| {
    let fcx2 = result.clone().0.context.clone() ;
    {
      let iterm = result.clone().0.term.clone() ;
      {
        let itype = result.clone().0.type_.clone() ;
        {
          let isubst = result.clone().0.subst.clone() ;
          {
            let iconstraints = result.clone().0.class_constraints.clone() ;
            Right(crate::hydra::typing::InferenceResult(Rc::new(crate::hydra::typing::InferenceResult_Variant {
              term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
                body: iterm.clone(),
                annotation: ann.clone()}))))),
              type_: itype.clone(),
              subst: isubst.clone(),
              class_constraints: iconstraints.clone(),
              context: fcx2.clone()})))}}}}})}

pub fn infer_type_of_application(fcx0: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, app: crate::hydra::core::Application) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  let fcx = crate::hydra::context::Context(Rc::new(crate::hydra::context::Context_Variant {
    trace: crate::hydra::lib::lists::cons(String::from("application"), fcx0.clone().0.trace.clone()),
    messages: fcx0.clone().0.messages.clone(),
    other: fcx0.clone().0.other.clone()})) ;
  let e0 = app.clone().0.function.clone() ;
  let e1 = app.clone().0.argument.clone() ;
  crate::hydra::lib::eithers::bind(infer_type_of_term(fcx.clone(), cx.clone(), e0.clone(), String::from("lhs")), |lhs_result: crate::hydra::typing::InferenceResult| {
    let fcx2 = lhs_result.clone().0.context.clone() ;
    {
      let a = lhs_result.clone().0.term.clone() ;
      {
        let t0 = lhs_result.clone().0.type_.clone() ;
        {
          let s0 = lhs_result.clone().0.subst.clone() ;
          {
            let c0 = lhs_result.clone().0.class_constraints.clone() ;
            crate::hydra::lib::eithers::bind(infer_type_of_term(fcx2.clone(), crate::hydra::substitution::subst_in_context(s0.clone(), cx.clone()), e1.clone(), String::from("rhs")), |rhs_result: crate::hydra::typing::InferenceResult| {
              let fcx3 = rhs_result.clone().0.context.clone() ;
              {
                let b = rhs_result.clone().0.term.clone() ;
                {
                  let t1 = rhs_result.clone().0.type_.clone() ;
                  {
                    let s1 = rhs_result.clone().0.subst.clone() ;
                    {
                      let c1 = rhs_result.clone().0.class_constraints.clone() ;
                      {
                        let v_result = crate::hydra::schemas::fresh_name(fcx3.clone()) ;
                        {
                          let v = crate::hydra::lib::pairs::first(v_result.clone()) ;
                          {
                            let fcx4 = crate::hydra::lib::pairs::second(v_result.clone()) ;
                            crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|_ic: crate::hydra::context::InContext| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
                              object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(_ic.clone().0.object.clone().0.message.clone())))))),
                              context: _ic.clone().0.context.clone()})), |_a: crate::hydra::typing::TypeSubst| _a.clone(), crate::hydra::unification::unify_types(fcx4.clone(), cx.clone().0.schema_types.clone(), crate::hydra::substitution::subst_in_type(s1.clone(), t0.clone()), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                              domain: t1.clone(),
                              codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v.clone())))}))))), String::from("application lhs"))), |s2: crate::hydra::typing::TypeSubst| crate::hydra::lib::eithers::bind(crate::hydra::checking::check_type_subst(fcx4.clone(), cx.clone(), s2.clone()), |_: crate::hydra::typing::TypeSubst| {
                              let r_expr = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                function: crate::hydra::substitution::subst_types_in_term(crate::hydra::substitution::compose_type_subst(s1.clone(), s2.clone()), a.clone()),
                                argument: crate::hydra::substitution::subst_types_in_term(s2.clone(), b.clone())}))))) ;
                              {
                                let r_type = crate::hydra::substitution::subst_in_type(s2.clone(), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v.clone())))) ;
                                {
                                  let r_subst = crate::hydra::substitution::compose_type_subst_list(Vec::from([
                                    s0.clone(),
                                    s1.clone(),
                                    s2.clone()])) ;
                                  {
                                    let c0_subst = crate::hydra::substitution::subst_in_class_constraints(s2.clone(), crate::hydra::substitution::subst_in_class_constraints(s1.clone(), c0.clone())) ;
                                    {
                                      let c1_subst = crate::hydra::substitution::subst_in_class_constraints(s2.clone(), c1.clone()) ;
                                      {
                                        let r_constraints = merge_class_constraints(c0_subst.clone(), c1_subst.clone()) ;
                                        Right(crate::hydra::typing::InferenceResult(Rc::new(crate::hydra::typing::InferenceResult_Variant {
                                          term: r_expr.clone(),
                                          type_: r_type.clone(),
                                          subst: r_subst.clone(),
                                          class_constraints: r_constraints.clone(),
                                          context: fcx4.clone()})))}}}}}}))}}}}}}}})}}}}})}

pub fn infer_type_of_case_statement(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, case_stmt: crate::hydra::core::CaseStatement) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  let tname = case_stmt.clone().0.type_name.clone() ;
  let dflt = case_stmt.clone().0.default_.clone() ;
  let cases = case_stmt.clone().0.cases.clone() ;
  let fnames = crate::hydra::lib::lists::map(|v| v.0.name.clone(), cases.clone()) ;
  crate::hydra::lib::eithers::bind(crate::hydra::schemas::require_schema_type(fcx.clone(), cx.clone().0.schema_types.clone(), tname.clone()), |st_rp: (crate::hydra::core::TypeScheme, crate::hydra::context::Context)| {
    let schema_type = crate::hydra::lib::pairs::first(st_rp.clone()) ;
    {
      let fcx2 = crate::hydra::lib::pairs::second(st_rp.clone()) ;
      {
        let svars = schema_type.clone().0.variables.clone() ;
        {
          let stype = schema_type.clone().0.type_.clone() ;
          crate::hydra::lib::eithers::bind(crate::hydra::extract::core::union_type(fcx2.clone(), tname.clone(), stype.clone()), |sfields: Vec<crate::hydra::core::FieldType>| crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_maybe(|t: crate::hydra::core::Term| infer_type_of_term(fcx2.clone(), cx.clone(), t.clone(), crate::hydra::lib::strings::cat(Vec::from([
            String::from("case "),
            tname.clone().0.0.clone(),
            String::from(".<default>")]))), dflt.clone()), |dflt_rp: Option<crate::hydra::typing::InferenceResult>| {
            let dflt_result = dflt_rp.clone() ;
            {
              let fcx3 = crate::hydra::lib::maybes::from_maybe(fcx2.clone(), crate::hydra::lib::maybes::map(|v| v.0.context.clone(), dflt_rp.clone())) ;
              crate::hydra::lib::eithers::bind(infer_many(fcx3.clone(), cx.clone(), crate::hydra::lib::lists::map(|f: crate::hydra::core::Field| (f.clone().0.term.clone(), crate::hydra::lib::strings::cat(Vec::from([
                String::from("case "),
                tname.clone().0.0.clone(),
                String::from("."),
                f.clone().0.name.clone().0.0.clone()]))), cases.clone())), |case_rp: ((Vec<crate::hydra::core::Term>, (Vec<crate::hydra::core::Type>, (crate::hydra::typing::TypeSubst, BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>))), crate::hydra::context::Context)| {
                let case_results = crate::hydra::lib::pairs::first(case_rp.clone()) ;
                {
                  let fcx4 = crate::hydra::lib::pairs::second(case_rp.clone()) ;
                  {
                    let iterms = crate::hydra::lib::pairs::first(case_results.clone()) ;
                    {
                      let itypes = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(case_results.clone())) ;
                      {
                        let isubst = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(case_results.clone()))) ;
                        {
                          let case_elem_constraints = crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(case_results.clone()))) ;
                          {
                            let codv_result = crate::hydra::schemas::fresh_name(fcx4.clone()) ;
                            {
                              let codv = crate::hydra::lib::pairs::first(codv_result.clone()) ;
                              {
                                let fcx5 = crate::hydra::lib::pairs::second(codv_result.clone()) ;
                                {
                                  let cod = crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(codv.clone()))) ;
                                  {
                                    let case_map = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|ft: crate::hydra::core::FieldType| (ft.clone().0.name.clone(), ft.clone().0.type_.clone()), sfields.clone())) ;
                                    {
                                      let dflt_constraints = crate::hydra::lib::maybes::to_list(crate::hydra::lib::maybes::map(|r: crate::hydra::typing::InferenceResult| crate::hydra::typing::TypeConstraint(Rc::new(crate::hydra::typing::TypeConstraint_Variant {
                                        left: cod.clone(),
                                        right: crate::hydra::substitution::subst_in_type(isubst.clone(), r.clone().0.type_.clone()),
                                        comment: String::from("match default")})), dflt_result.clone())) ;
                                      {
                                        let case_constraints = crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::zip_with(|fname: crate::hydra::core::Name, itype: crate::hydra::core::Type| crate::hydra::lib::maybes::map(|ftype: crate::hydra::core::Type| crate::hydra::typing::TypeConstraint(Rc::new(crate::hydra::typing::TypeConstraint_Variant {
                                          left: itype.clone(),
                                          right: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                                            domain: ftype.clone(),
                                            codomain: cod.clone()}))))),
                                          comment: String::from("case type")})), crate::hydra::lib::maps::lookup(fname.clone(), case_map.clone())), fnames.clone(), itypes.clone())) ;
                                        {
                                          let dflt_class_constraints = crate::hydra::lib::maybes::from_maybe(crate::hydra::lib::maps::empty, crate::hydra::lib::maybes::map(|v| v.0.class_constraints.clone(), dflt_result.clone())) ;
                                          {
                                            let all_elem_constraints = merge_class_constraints(case_elem_constraints.clone(), dflt_class_constraints.clone()) ;
                                            crate::hydra::lib::eithers::bind(map_constraints(fcx5.clone(), cx.clone(), |subst: crate::hydra::typing::TypeSubst| yield_with_constraints(fcx5.clone(), build_type_application_term(svars.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                                              type_name: tname.clone(),
                                              default_: crate::hydra::lib::maybes::map(|v| v.0.term.clone(), dflt_result.clone()),
                                              cases: crate::hydra::lib::lists::zip_with(|n: crate::hydra::core::Name, t: crate::hydra::core::Term| crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                                name: n.clone(),
                                                term: t.clone()})), fnames.clone(), iterms.clone())})))))))))))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                                              domain: crate::hydra::schemas::nominal_application(tname.clone(), crate::hydra::lib::lists::map(|x: crate::hydra::core::Name| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(x.clone()))), svars.clone())),
                                              codomain: cod.clone()}))))), crate::hydra::substitution::compose_type_subst_list(crate::hydra::lib::lists::concat(Vec::from([
                                              crate::hydra::lib::maybes::to_list(crate::hydra::lib::maybes::map(|v| v.0.subst.clone(), dflt_result.clone())),
                                              Vec::from([
                                                isubst.clone(),
                                                subst.clone()])]))), crate::hydra::substitution::subst_in_class_constraints(subst.clone(), all_elem_constraints.clone())), crate::hydra::lib::lists::concat(Vec::from([
                                              dflt_constraints.clone(),
                                              case_constraints.clone()]))), |mc_result: crate::hydra::typing::InferenceResult| Right(mc_result.clone()))}}}}}}}}}}}}}}})}}))}}}})}

pub fn infer_type_of_collection(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, typ_cons: impl Fn(crate::hydra::core::Type) -> crate::hydra::core::Type + Clone, trm_cons: impl Fn(Vec<crate::hydra::core::Term>) -> crate::hydra::core::Term + Clone, desc: String, class_names: BTreeSet<crate::hydra::core::Name>, els: Vec<crate::hydra::core::Term>) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  let var_result = crate::hydra::schemas::fresh_name(fcx.clone()) ;
  let var = crate::hydra::lib::pairs::first(var_result.clone()) ;
  let fcx2 = crate::hydra::lib::pairs::second(var_result.clone()) ;
  let class_constraints = crate::hydra::lib::logic::if_else(crate::hydra::lib::sets::null(class_names.clone()), crate::hydra::lib::maps::empty, crate::hydra::lib::maps::singleton(var.clone(), crate::hydra::core::TypeVariableMetadata(Rc::new(crate::hydra::core::TypeVariableMetadata_Variant {
    classes: class_names.clone()})))) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(els.clone()), Right(yield_with_constraints(fcx2.clone(), build_type_application_term(Vec::from([
    var.clone()]), trm_cons.clone()(Vec::from([]))), typ_cons.clone()(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(var.clone())))), crate::hydra::substitution::id_type_subst, class_constraints.clone())), crate::hydra::lib::eithers::bind(infer_many(fcx2.clone(), cx.clone(), crate::hydra::lib::lists::zip(els.clone(), crate::hydra::lib::lists::map(|i: i32| crate::hydra::lib::strings::cat(Vec::from([
    String::from("#"),
    crate::hydra::lib::literals::show_int32(i.clone())])), crate::hydra::lib::math::range(1i32, crate::hydra::lib::math::add(crate::hydra::lib::lists::length(els.clone()), 1i32))))), |results_rp: ((Vec<crate::hydra::core::Term>, (Vec<crate::hydra::core::Type>, (crate::hydra::typing::TypeSubst, BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>))), crate::hydra::context::Context)| {
    let results = crate::hydra::lib::pairs::first(results_rp.clone()) ;
    {
      let fcx3 = crate::hydra::lib::pairs::second(results_rp.clone()) ;
      {
        let terms = crate::hydra::lib::pairs::first(results.clone()) ;
        {
          let types = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(results.clone())) ;
          {
            let subst1 = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(results.clone()))) ;
            {
              let elem_constraints = crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(results.clone()))) ;
              {
                let constraints = crate::hydra::lib::lists::map(|t: crate::hydra::core::Type| crate::hydra::typing::TypeConstraint(Rc::new(crate::hydra::typing::TypeConstraint_Variant {
                  left: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(var.clone()))),
                  right: t.clone(),
                  comment: desc.clone()})), types.clone()) ;
                {
                  let all_constraints = merge_class_constraints(class_constraints.clone(), elem_constraints.clone()) ;
                  crate::hydra::lib::eithers::bind(map_constraints(fcx3.clone(), cx.clone(), |subst2: crate::hydra::typing::TypeSubst| {
                    let iterm = trm_cons.clone()(terms.clone()) ;
                    {
                      let itype = typ_cons.clone()(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(var.clone())))) ;
                      {
                        let isubst = crate::hydra::substitution::compose_type_subst(subst1.clone(), subst2.clone()) ;
                        yield_with_constraints(fcx3.clone(), iterm.clone(), itype.clone(), isubst.clone(), crate::hydra::substitution::subst_in_class_constraints(subst2.clone(), all_constraints.clone()))}}}, constraints.clone()), |mc_result: crate::hydra::typing::InferenceResult| Right(mc_result.clone()))}}}}}}}}))}

pub fn infer_type_of_either(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, e: Either<crate::hydra::core::Term, crate::hydra::core::Term>) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  crate::hydra::lib::eithers::either(|l: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(infer_type_of_term(fcx.clone(), cx.clone(), l.clone(), String::from("either left value")), |r1: crate::hydra::typing::InferenceResult| {
    let fcx2 = r1.clone().0.context.clone() ;
    {
      let iterm = r1.clone().0.term.clone() ;
      {
        let left_type = r1.clone().0.type_.clone() ;
        {
          let subst = r1.clone().0.subst.clone() ;
          {
            let fv_result = fresh_variable_type(fcx2.clone()) ;
            {
              let right_type = crate::hydra::lib::pairs::first(fv_result.clone()) ;
              {
                let fcx3 = crate::hydra::lib::pairs::second(fv_result.clone()) ;
                {
                  let either_term = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(iterm.clone())))) ;
                  {
                    let term_with_left_type = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
                      body: either_term.clone(),
                      type_: left_type.clone()}))))) ;
                    {
                      let term_with_both_types = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
                        body: term_with_left_type.clone(),
                        type_: right_type.clone()}))))) ;
                      {
                        let either_type = crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
                          left: left_type.clone(),
                          right: right_type.clone()}))))) ;
                        Right(yield_checked(fcx3.clone(), term_with_both_types.clone(), either_type.clone(), subst.clone()))}}}}}}}}}}}), |r: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(infer_type_of_term(fcx.clone(), cx.clone(), r.clone(), String::from("either right value")), |r1: crate::hydra::typing::InferenceResult| {
    let fcx2 = r1.clone().0.context.clone() ;
    {
      let iterm = r1.clone().0.term.clone() ;
      {
        let right_type = r1.clone().0.type_.clone() ;
        {
          let subst = r1.clone().0.subst.clone() ;
          {
            let fv_result = fresh_variable_type(fcx2.clone()) ;
            {
              let left_type = crate::hydra::lib::pairs::first(fv_result.clone()) ;
              {
                let fcx3 = crate::hydra::lib::pairs::second(fv_result.clone()) ;
                {
                  let either_term = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(iterm.clone())))) ;
                  {
                    let term_with_left_type = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
                      body: either_term.clone(),
                      type_: left_type.clone()}))))) ;
                    {
                      let term_with_both_types = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
                        body: term_with_left_type.clone(),
                        type_: right_type.clone()}))))) ;
                      {
                        let either_type = crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
                          left: left_type.clone(),
                          right: right_type.clone()}))))) ;
                        Right(yield_checked(fcx3.clone(), term_with_both_types.clone(), either_type.clone(), subst.clone()))}}}}}}}}}}}), e.clone())}

pub fn infer_type_of_elimination(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, elm: crate::hydra::core::Elimination) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  match &*elm.clone().0 {
    crate::hydra::core::Elimination_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_projection(fcx.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Elimination_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_case_statement(fcx.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Elimination_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_unwrap(fcx.clone(), cx.clone(), v0_.clone())}}}

pub fn infer_type_of_function(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, f: crate::hydra::core::Function) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  match &*f.clone().0 {
    crate::hydra::core::Function_Variant::Elimination (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_elimination(fcx.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Function_Variant::Lambda (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_lambda(fcx.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Function_Variant::Primitive (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_primitive(fcx.clone(), cx.clone(), v0_.clone())}}}

pub fn infer_type_of_injection(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, injection: crate::hydra::core::Injection) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  let tname = injection.clone().0.type_name.clone() ;
  let field = injection.clone().0.field.clone() ;
  let fname = field.clone().0.name.clone() ;
  let term = field.clone().0.term.clone() ;
  crate::hydra::lib::eithers::bind(infer_type_of_term(fcx.clone(), cx.clone(), term.clone(), String::from("injected term")), |result: crate::hydra::typing::InferenceResult| {
    let fcx2 = result.clone().0.context.clone() ;
    crate::hydra::lib::eithers::bind(crate::hydra::schemas::require_schema_type(fcx2.clone(), cx.clone().0.schema_types.clone(), tname.clone()), |st_rp: (crate::hydra::core::TypeScheme, crate::hydra::context::Context)| {
      let schema_type = crate::hydra::lib::pairs::first(st_rp.clone()) ;
      {
        let fcx3 = crate::hydra::lib::pairs::second(st_rp.clone()) ;
        {
          let svars = schema_type.clone().0.variables.clone() ;
          {
            let stype = schema_type.clone().0.type_.clone() ;
            {
              let iterm = result.clone().0.term.clone() ;
              {
                let ityp = result.clone().0.type_.clone() ;
                {
                  let isubst = result.clone().0.subst.clone() ;
                  crate::hydra::lib::eithers::bind(crate::hydra::extract::core::union_type(fcx3.clone(), tname.clone(), stype.clone()), |sfields: Vec<crate::hydra::core::FieldType>| crate::hydra::lib::eithers::bind(crate::hydra::schemas::find_field_type(fcx3.clone(), fname.clone(), sfields.clone()), |ftyp: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(map_constraints(fcx3.clone(), cx.clone(), |subst: crate::hydra::typing::TypeSubst| yield_(fcx3.clone(), build_type_application_term(svars.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
                    type_name: tname.clone(),
                    field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                      name: fname.clone(),
                      term: iterm.clone()}))})))))), crate::hydra::schemas::nominal_application(tname.clone(), crate::hydra::lib::lists::map(|x: crate::hydra::core::Name| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(x.clone()))), svars.clone())), crate::hydra::substitution::compose_type_subst(isubst.clone(), subst.clone())), Vec::from([
                    crate::hydra::typing::TypeConstraint(Rc::new(crate::hydra::typing::TypeConstraint_Variant {
                      left: ftyp.clone(),
                      right: ityp.clone(),
                      comment: String::from("schema type of injected field")}))])), |mc_result: crate::hydra::typing::InferenceResult| Right(mc_result.clone()))))}}}}}}})})}

pub fn infer_type_of_lambda(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, lambda: crate::hydra::core::Lambda) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  let var = lambda.clone().0.parameter.clone() ;
  let body = lambda.clone().0.body.clone() ;
  let vdom_result = crate::hydra::schemas::fresh_name(fcx.clone()) ;
  let vdom = crate::hydra::lib::pairs::first(vdom_result.clone()) ;
  let fcx2 = crate::hydra::lib::pairs::second(vdom_result.clone()) ;
  let dom = crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(vdom.clone()))) ;
  let cx2 = extend_context(Vec::from([
    (var.clone(), crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
      variables: Vec::from([]),
      type_: dom.clone(),
      constraints: None})))]), cx.clone()) ;
  crate::hydra::lib::eithers::bind(infer_type_of_term(fcx2.clone(), cx2.clone(), body.clone(), String::from("lambda body")), |result: crate::hydra::typing::InferenceResult| {
    let fcx3 = result.clone().0.context.clone() ;
    {
      let iterm = result.clone().0.term.clone() ;
      {
        let icod = result.clone().0.type_.clone() ;
        {
          let isubst = result.clone().0.subst.clone() ;
          {
            let rdom = crate::hydra::substitution::subst_in_type(isubst.clone(), dom.clone()) ;
            {
              let rterm = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                parameter: var.clone(),
                domain: Some(rdom.clone()),
                body: iterm.clone()})))))))) ;
              {
                let rtype = crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                  domain: rdom.clone(),
                  codomain: icod.clone()}))))) ;
                {
                  let vars = crate::hydra::lib::sets::unions(Vec::from([
                    crate::hydra::rewriting::free_variables_in_type(rdom.clone()),
                    crate::hydra::rewriting::free_variables_in_type(icod.clone()),
                    free_variables_in_context(crate::hydra::substitution::subst_in_context(isubst.clone(), cx2.clone()))])) ;
                  {
                    let cx3 = crate::hydra::substitution::subst_in_context(isubst.clone(), cx.clone()) ;
                    {
                      let iconstraints = crate::hydra::substitution::subst_in_class_constraints(isubst.clone(), result.clone().0.class_constraints.clone()) ;
                      Right(crate::hydra::typing::InferenceResult(Rc::new(crate::hydra::typing::InferenceResult_Variant {
                        term: rterm.clone(),
                        type_: rtype.clone(),
                        subst: isubst.clone(),
                        class_constraints: iconstraints.clone(),
                        context: fcx3.clone()})))}}}}}}}}}})}

pub fn infer_type_of_let(fcx0: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, let0: crate::hydra::core::Let) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  let fcx = crate::hydra::context::Context(Rc::new(crate::hydra::context::Context_Variant {
    trace: crate::hydra::lib::lists::cons(String::from("let"), fcx0.clone().0.trace.clone()),
    messages: fcx0.clone().0.messages.clone(),
    other: fcx0.clone().0.other.clone()})) ;
  let bindings0 = let0.clone().0.bindings.clone() ;
  let body0 = let0.clone().0.body.clone() ;
  let names = crate::hydra::lib::lists::map(|v| v.0.name.clone(), bindings0.clone()) ;
  let name_set = crate::hydra::lib::sets::from_list(names.clone()) ;
  let to_pair = |binding: crate::hydra::core::Binding| {
    let name = binding.clone().0.name.clone() ;
    {
      let term = binding.clone().0.term.clone() ;
      (name.clone(), crate::hydra::lib::lists::filter(|n: crate::hydra::core::Name| crate::hydra::lib::sets::member(n.clone(), name_set.clone()), crate::hydra::lib::sets::to_list(crate::hydra::rewriting::free_variables_in_term(term.clone()))))}} ;
  let adj_list = crate::hydra::lib::lists::map(to_pair.clone(), bindings0.clone()) ;
  let groups = crate::hydra::sorting::topological_sort_components(adj_list.clone()) ;
  let binding_map = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::zip(names.clone(), bindings0.clone())) ;
  let create_let = |e: crate::hydra::core::Term, group: Vec<crate::hydra::core::Name>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
    bindings: crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(|n: crate::hydra::core::Name| crate::hydra::lib::maps::lookup(n.clone(), binding_map.clone()), group.clone())),
    body: e.clone()}))))) ;
  let rewritten_let = crate::hydra::lib::lists::foldl(create_let.clone(), body0.clone(), crate::hydra::lib::lists::reverse(groups.clone())) ;
  let restore_let = |iterm: crate::hydra::core::Term| {
    let helper = |level: i32, bins: Vec<crate::hydra::core::Binding>, term: crate::hydra::core::Term| {
      let nonzero = |term2: crate::hydra::core::Term| match &*term2.clone().0 {
        crate::hydra::core::Term_Variant::Let (v0_) => {
          let v0_ = v0_.clone() ;
          {
            let bs = v0_.clone().0.bindings.clone() ;
            {
              let let_body = v0_.clone().0.body.clone() ;
              helper.clone()(crate::hydra::lib::math::sub(level.clone(), 1i32), crate::hydra::lib::lists::concat(Vec::from([
                bs.clone(),
                bins.clone()])), let_body.clone())}}}} ;
      crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(level.clone(), 0i32), (bins.clone(), term.clone()), nonzero.clone()(term.clone()))} ;
    {
      let result = helper.clone()(crate::hydra::lib::lists::length(groups.clone()), Vec::from([]), iterm.clone()) ;
      {
        let binding_list = crate::hydra::lib::pairs::first(result.clone()) ;
        {
          let e = crate::hydra::lib::pairs::second(result.clone()) ;
          {
            let binding_map2 = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|b: crate::hydra::core::Binding| (b.clone().0.name.clone(), b.clone()), binding_list.clone())) ;
            crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
              bindings: crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(|n: crate::hydra::core::Name| crate::hydra::lib::maps::lookup(n.clone(), binding_map2.clone()), names.clone())),
              body: e.clone()})))))}}}}} ;
  let rewrite_result = |iresult: crate::hydra::typing::InferenceResult| {
    let fcx_r = iresult.clone().0.context.clone() ;
    {
      let iterm = iresult.clone().0.term.clone() ;
      {
        let itype = iresult.clone().0.type_.clone() ;
        {
          let isubst = iresult.clone().0.subst.clone() ;
          {
            let iconstraints = iresult.clone().0.class_constraints.clone() ;
            crate::hydra::typing::InferenceResult(Rc::new(crate::hydra::typing::InferenceResult_Variant {
              term: restore_let.clone()(iterm.clone()),
              type_: itype.clone(),
              subst: isubst.clone(),
              class_constraints: iconstraints.clone(),
              context: fcx_r.clone()}))}}}}} ;
  let res = match &*rewritten_let.clone().0 {
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_let_normalized(fcx.clone(), cx.clone(), v0_.clone())},
    _ => infer_type_of_term(fcx.clone(), cx.clone(), rewritten_let.clone(), String::from("empty let term"))} ;
  crate::hydra::lib::eithers::map(rewrite_result.clone(), res.clone())}

pub fn infer_type_of_let_normalized(fcx0: crate::hydra::context::Context, cx0: crate::hydra::graph::Graph, let_term: crate::hydra::core::Let) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  let fcx = crate::hydra::context::Context(Rc::new(crate::hydra::context::Context_Variant {
    trace: crate::hydra::lib::lists::cons(String::from("let-normalized"), fcx0.clone().0.trace.clone()),
    messages: fcx0.clone().0.messages.clone(),
    other: fcx0.clone().0.other.clone()})) ;
  let bins0 = let_term.clone().0.bindings.clone() ;
  let body0 = let_term.clone().0.body.clone() ;
  let bnames = crate::hydra::lib::lists::map(|v| v.0.name.clone(), bins0.clone()) ;
  let bvars_result = crate::hydra::schemas::fresh_names(crate::hydra::lib::lists::length(bins0.clone()), fcx.clone()) ;
  let bvars = crate::hydra::lib::pairs::first(bvars_result.clone()) ;
  let fcx2 = crate::hydra::lib::pairs::second(bvars_result.clone()) ;
  let tbins0 = crate::hydra::lib::lists::map(|x: crate::hydra::core::Name| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(x.clone()))), bvars.clone()) ;
  let cx1 = extend_context(crate::hydra::lib::lists::zip(bnames.clone(), crate::hydra::lib::lists::map(|t: crate::hydra::core::Type| crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
    variables: Vec::from([]),
    type_: t.clone(),
    constraints: None})), tbins0.clone())), cx0.clone()) ;
  crate::hydra::lib::eithers::bind(infer_types_of_temporary_bindings(fcx2.clone(), cx1.clone(), bins0.clone()), |ir_rp: ((Vec<crate::hydra::core::Term>, (Vec<crate::hydra::core::Type>, (crate::hydra::typing::TypeSubst, BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>))), crate::hydra::context::Context)| {
    let inferred_result = crate::hydra::lib::pairs::first(ir_rp.clone()) ;
    {
      let fcx3 = crate::hydra::lib::pairs::second(ir_rp.clone()) ;
      {
        let bterms1 = crate::hydra::lib::pairs::first(inferred_result.clone()) ;
        {
          let tbins1 = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(inferred_result.clone())) ;
          {
            let subst_and_constraints = crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(inferred_result.clone())) ;
            {
              let s1 = crate::hydra::lib::pairs::first(subst_and_constraints.clone()) ;
              {
                let inferred_constraints = crate::hydra::lib::pairs::second(subst_and_constraints.clone()) ;
                crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|_ic: crate::hydra::context::InContext| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
                  object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(_ic.clone().0.object.clone().0.message.clone())))))),
                  context: _ic.clone().0.context.clone()})), |_a: crate::hydra::typing::TypeSubst| _a.clone(), crate::hydra::unification::unify_type_lists(fcx3.clone(), cx0.clone().0.schema_types.clone(), crate::hydra::lib::lists::map(|v1: crate::hydra::core::Type| crate::hydra::substitution::subst_in_type(s1.clone(), v1.clone()), tbins0.clone()), tbins1.clone(), String::from("temporary type bindings"))), |s2: crate::hydra::typing::TypeSubst| crate::hydra::lib::eithers::bind(crate::hydra::checking::check_type_subst(fcx3.clone(), cx0.clone(), s2.clone()), |_: crate::hydra::typing::TypeSubst| {
                  let g2base = crate::hydra::substitution::subst_in_context(crate::hydra::substitution::compose_type_subst(s1.clone(), s2.clone()), cx0.clone()) ;
                  {
                    let constraints_with_s2 = crate::hydra::substitution::subst_in_class_constraints(s2.clone(), inferred_constraints.clone()) ;
                    {
                      let composed_subst = crate::hydra::substitution::compose_type_subst(s1.clone(), s2.clone()) ;
                      {
                        let original_binding_constraints = crate::hydra::lib::lists::foldl(|acc: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>, b: crate::hydra::core::Binding| crate::hydra::lib::maybes::maybe(acc.clone(), |ts: crate::hydra::core::TypeScheme| crate::hydra::lib::maybes::maybe(acc.clone(), |c: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>| merge_class_constraints(acc.clone(), c.clone()), ts.clone().0.constraints.clone()), b.clone().0.type_.clone()), crate::hydra::lib::maps::empty, bins0.clone()) ;
                        {
                          let original_constraints_subst = crate::hydra::substitution::subst_in_class_constraints(composed_subst.clone(), original_binding_constraints.clone()) ;
                          {
                            let all_inferred_constraints = merge_class_constraints(constraints_with_s2.clone(), original_constraints_subst.clone()) ;
                            {
                              let merged_constraints = merge_class_constraints(g2base.clone().0.class_constraints.clone(), all_inferred_constraints.clone()) ;
                              {
                                let g2 = crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
                                  bound_terms: g2base.clone().0.bound_terms.clone(),
                                  bound_types: g2base.clone().0.bound_types.clone(),
                                  class_constraints: merged_constraints.clone(),
                                  lambda_variables: g2base.clone().0.lambda_variables.clone(),
                                  metadata: g2base.clone().0.metadata.clone(),
                                  primitives: g2base.clone().0.primitives.clone(),
                                  schema_types: g2base.clone().0.schema_types.clone(),
                                  type_variables: g2base.clone().0.type_variables.clone()})) ;
                                {
                                  let bterms1_subst = crate::hydra::lib::lists::map(|v1: crate::hydra::core::Term| crate::hydra::substitution::subst_types_in_term(s2.clone(), v1.clone()), bterms1.clone()) ;
                                  {
                                    let tsbins1 = crate::hydra::lib::lists::zip(bnames.clone(), crate::hydra::lib::lists::map(|t: crate::hydra::core::Type| generalize(g2.clone(), crate::hydra::substitution::subst_in_type(s2.clone(), t.clone())), tbins1.clone())) ;
                                    crate::hydra::lib::eithers::bind(infer_type_of_term(fcx3.clone(), extend_context(tsbins1.clone(), g2.clone()), body0.clone(), String::from("let body")), |body_result: crate::hydra::typing::InferenceResult| {
                                      let fcx4 = body_result.clone().0.context.clone() ;
                                      {
                                        let body1 = body_result.clone().0.term.clone() ;
                                        {
                                          let tbody = body_result.clone().0.type_.clone() ;
                                          {
                                            let sbody = body_result.clone().0.subst.clone() ;
                                            {
                                              let st1 = crate::hydra::typing::TermSubst(Rc::new(crate::hydra::typing::TermSubst_Variant(crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|pair: (crate::hydra::core::Name, crate::hydra::core::TypeScheme)| {
                                                let name = crate::hydra::lib::pairs::first(pair.clone()) ;
                                                {
                                                  let ts = crate::hydra::lib::pairs::second(pair.clone()) ;
                                                  (name.clone(), build_type_application_term(ts.clone().0.variables.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(name.clone())))))}}, tsbins1.clone()))))) ;
                                              {
                                                let create_binding = |binding_pair: ((crate::hydra::core::Name, crate::hydra::core::TypeScheme), crate::hydra::core::Term)| {
                                                  let name_ts_pair = crate::hydra::lib::pairs::first(binding_pair.clone()) ;
                                                  {
                                                    let term = crate::hydra::lib::pairs::second(binding_pair.clone()) ;
                                                    {
                                                      let name = crate::hydra::lib::pairs::first(name_ts_pair.clone()) ;
                                                      {
                                                        let ts = crate::hydra::lib::pairs::second(name_ts_pair.clone()) ;
                                                        {
                                                          let final_ts = crate::hydra::substitution::subst_in_type_scheme(sbody.clone(), ts.clone()) ;
                                                          {
                                                            let type_lambda_term = crate::hydra::lib::lists::foldl(|b: crate::hydra::core::Term, v: crate::hydra::core::Name| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeLambda(crate::hydra::core::TypeLambda(Rc::new(crate::hydra::core::TypeLambda_Variant {
                                                              parameter: v.clone(),
                                                              body: b.clone()}))))), crate::hydra::substitution::substitute_in_term(st1.clone(), term.clone()), crate::hydra::lib::lists::reverse(final_ts.clone().0.variables.clone())) ;
                                                            crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                                                              name: name.clone(),
                                                              term: crate::hydra::substitution::subst_types_in_term(crate::hydra::substitution::compose_type_subst(sbody.clone(), s2.clone()), type_lambda_term.clone()),
                                                              type_: Some(final_ts.clone())}))}}}}}} ;
                                                {
                                                  let bins1 = crate::hydra::lib::lists::map(create_binding.clone(), crate::hydra::lib::lists::zip(tsbins1.clone(), bterms1_subst.clone())) ;
                                                  {
                                                    let body_constraints = crate::hydra::substitution::subst_in_class_constraints(sbody.clone(), body_result.clone().0.class_constraints.clone()) ;
                                                    {
                                                      let binding_constraints_subst = crate::hydra::substitution::subst_in_class_constraints(sbody.clone(), constraints_with_s2.clone()) ;
                                                      {
                                                        let all_constraints = merge_class_constraints(binding_constraints_subst.clone(), body_constraints.clone()) ;
                                                        Right(crate::hydra::typing::InferenceResult(Rc::new(crate::hydra::typing::InferenceResult_Variant {
                                                          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
                                                            bindings: bins1.clone(),
                                                            body: body1.clone()}))))),
                                                          type_: tbody.clone(),
                                                          subst: crate::hydra::substitution::compose_type_subst_list(Vec::from([
                                                            s1.clone(),
                                                            s2.clone(),
                                                            sbody.clone()])),
                                                          class_constraints: all_constraints.clone(),
                                                          context: fcx4.clone()})))}}}}}}}}}})}}}}}}}}}}))}}}}}}})}

pub fn infer_type_of_list(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, v1: Vec<crate::hydra::core::Term>) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  infer_type_of_collection(fcx.clone(), cx.clone(), |x: crate::hydra::core::Type| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(x.clone()))), |x: Vec<crate::hydra::core::Term>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(x.clone()))), String::from("list element"), crate::hydra::lib::sets::empty, v1.clone())}

pub fn infer_type_of_literal(fcx: crate::hydra::context::Context, lit: crate::hydra::core::Literal) -> crate::hydra::typing::InferenceResult {
  crate::hydra::typing::InferenceResult(Rc::new(crate::hydra::typing::InferenceResult_Variant {
    term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(lit.clone()))),
    type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::reflect::literal_type(lit.clone())))),
    subst: crate::hydra::substitution::id_type_subst,
    class_constraints: crate::hydra::lib::maps::empty,
    context: fcx.clone()}))}

pub fn infer_type_of_map(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, m: BTreeMap<crate::hydra::core::Term, crate::hydra::core::Term>) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  let kvar_result = crate::hydra::schemas::fresh_name(fcx.clone()) ;
  let kvar = crate::hydra::lib::pairs::first(kvar_result.clone()) ;
  let fcx2 = crate::hydra::lib::pairs::second(kvar_result.clone()) ;
  let vvar_result = crate::hydra::schemas::fresh_name(fcx2.clone()) ;
  let vvar = crate::hydra::lib::pairs::first(vvar_result.clone()) ;
  let fcx3 = crate::hydra::lib::pairs::second(vvar_result.clone()) ;
  let key_constraints = crate::hydra::lib::maps::singleton(kvar.clone(), crate::hydra::core::TypeVariableMetadata(Rc::new(crate::hydra::core::TypeVariableMetadata_Variant {
    classes: crate::hydra::lib::sets::singleton(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("ordering")))))}))) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::maps::null(m.clone()), Right(yield_with_constraints(fcx3.clone(), build_type_application_term(Vec::from([
    kvar.clone(),
    vvar.clone()]), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(crate::hydra::lib::maps::empty)))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
    keys: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(kvar.clone()))),
    values: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(vvar.clone())))}))))), crate::hydra::substitution::id_type_subst, key_constraints.clone())), crate::hydra::lib::eithers::bind(infer_many(fcx3.clone(), cx.clone(), crate::hydra::lib::lists::map(|k: crate::hydra::core::Term| (k.clone(), String::from("map key")), crate::hydra::lib::maps::keys(m.clone()))), |k_rp: ((Vec<crate::hydra::core::Term>, (Vec<crate::hydra::core::Type>, (crate::hydra::typing::TypeSubst, BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>))), crate::hydra::context::Context)| {
    let k_results = crate::hydra::lib::pairs::first(k_rp.clone()) ;
    {
      let fcx4 = crate::hydra::lib::pairs::second(k_rp.clone()) ;
      {
        let kterms = crate::hydra::lib::pairs::first(k_results.clone()) ;
        {
          let ktypes = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(k_results.clone())) ;
          {
            let ksubst = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(k_results.clone()))) ;
            {
              let k_elem_constraints = crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(k_results.clone()))) ;
              crate::hydra::lib::eithers::bind(infer_many(fcx4.clone(), crate::hydra::substitution::subst_in_context(ksubst.clone(), cx.clone()), crate::hydra::lib::lists::map(|v: crate::hydra::core::Term| (v.clone(), String::from("map value")), crate::hydra::lib::maps::elems(m.clone()))), |v_rp: ((Vec<crate::hydra::core::Term>, (Vec<crate::hydra::core::Type>, (crate::hydra::typing::TypeSubst, BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>))), crate::hydra::context::Context)| {
                let v_results = crate::hydra::lib::pairs::first(v_rp.clone()) ;
                {
                  let fcx5 = crate::hydra::lib::pairs::second(v_rp.clone()) ;
                  {
                    let vterms = crate::hydra::lib::pairs::first(v_results.clone()) ;
                    {
                      let vtypes = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(v_results.clone())) ;
                      {
                        let vsubst = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(v_results.clone()))) ;
                        {
                          let v_elem_constraints = crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(v_results.clone()))) ;
                          {
                            let kcons = crate::hydra::lib::lists::map(|t: crate::hydra::core::Type| crate::hydra::typing::TypeConstraint(Rc::new(crate::hydra::typing::TypeConstraint_Variant {
                              left: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(kvar.clone()))),
                              right: t.clone(),
                              comment: String::from("map key")})), ktypes.clone()) ;
                            {
                              let vcons = crate::hydra::lib::lists::map(|t: crate::hydra::core::Type| crate::hydra::typing::TypeConstraint(Rc::new(crate::hydra::typing::TypeConstraint_Variant {
                                left: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(vvar.clone()))),
                                right: t.clone(),
                                comment: String::from("map value")})), vtypes.clone()) ;
                              {
                                let all_map_constraints = merge_class_constraints(key_constraints.clone(), merge_class_constraints(k_elem_constraints.clone(), v_elem_constraints.clone())) ;
                                crate::hydra::lib::eithers::bind(map_constraints(fcx5.clone(), cx.clone(), |subst: crate::hydra::typing::TypeSubst| yield_with_constraints(fcx5.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::zip(kterms.clone(), vterms.clone()))))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
                                  keys: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(kvar.clone()))),
                                  values: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(vvar.clone())))}))))), crate::hydra::substitution::compose_type_subst_list(Vec::from([
                                  ksubst.clone(),
                                  vsubst.clone(),
                                  subst.clone()])), crate::hydra::substitution::subst_in_class_constraints(subst.clone(), all_map_constraints.clone())), crate::hydra::lib::lists::concat(Vec::from([
                                  kcons.clone(),
                                  vcons.clone()]))), |mc_result: crate::hydra::typing::InferenceResult| Right(mc_result.clone()))}}}}}}}}})}}}}}}))}

pub fn infer_type_of_optional(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, m: Option<crate::hydra::core::Term>) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  let trm_cons = |terms: Vec<crate::hydra::core::Term>| crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(terms.clone()), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(None))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(Some(crate::hydra::lib::lists::head(terms.clone())))))) ;
  infer_type_of_collection(fcx.clone(), cx.clone(), |x: crate::hydra::core::Type| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(x.clone()))), trm_cons.clone(), String::from("optional element"), crate::hydra::lib::sets::empty, crate::hydra::lib::maybes::maybe(Vec::from([]), crate::hydra::lib::lists::singleton, m.clone()))}

pub fn infer_type_of_pair(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, p: (crate::hydra::core::Term, crate::hydra::core::Term)) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  crate::hydra::lib::eithers::bind(infer_many(fcx.clone(), cx.clone(), Vec::from([
    (crate::hydra::lib::pairs::first(p.clone()), String::from("pair first element")),
    (crate::hydra::lib::pairs::second(p.clone()), String::from("pair second element"))])), |rp: ((Vec<crate::hydra::core::Term>, (Vec<crate::hydra::core::Type>, (crate::hydra::typing::TypeSubst, BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>))), crate::hydra::context::Context)| {
    let results = crate::hydra::lib::pairs::first(rp.clone()) ;
    {
      let fcx2 = crate::hydra::lib::pairs::second(rp.clone()) ;
      {
        let iterms = crate::hydra::lib::pairs::first(results.clone()) ;
        {
          let itypes = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(results.clone())) ;
          {
            let isubst = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(results.clone()))) ;
            {
              let pair_elem_constraints = crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(results.clone()))) ;
              {
                let ifst = crate::hydra::lib::lists::head(iterms.clone()) ;
                {
                  let isnd = crate::hydra::lib::lists::head(crate::hydra::lib::lists::tail(iterms.clone())) ;
                  {
                    let ty_fst = crate::hydra::lib::lists::head(itypes.clone()) ;
                    {
                      let ty_snd = crate::hydra::lib::lists::head(crate::hydra::lib::lists::tail(itypes.clone())) ;
                      {
                        let pair_term = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Pair((ifst.clone(), isnd.clone())))) ;
                        {
                          let term_with_types = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
                            body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
                              body: pair_term.clone(),
                              type_: ty_fst.clone()}))))),
                            type_: ty_snd.clone()}))))) ;
                          Right(yield_with_constraints(fcx2.clone(), term_with_types.clone(), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Pair(crate::hydra::core::PairType(Rc::new(crate::hydra::core::PairType_Variant {
                            first: ty_fst.clone(),
                            second: ty_snd.clone()}))))), isubst.clone(), pair_elem_constraints.clone()))}}}}}}}}}}}})}

pub fn infer_type_of_primitive(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(String::from("No such primitive: "), name.clone().0.0.clone()))))))),
    context: fcx.clone()}))), |scheme: crate::hydra::core::TypeScheme| {
    let ts_result = crate::hydra::schemas::instantiate_type_scheme(fcx.clone(), scheme.clone()) ;
    {
      let ts = crate::hydra::lib::pairs::first(ts_result.clone()) ;
      {
        let fcx2 = crate::hydra::lib::pairs::second(ts_result.clone()) ;
        {
          let constraints = crate::hydra::lib::maybes::from_maybe(crate::hydra::lib::maps::empty, ts.clone().0.constraints.clone()) ;
          Right(yield_checked_with_constraints(fcx2.clone(), build_type_application_term(ts.clone().0.variables.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(name.clone()))))))), ts.clone().0.type_.clone(), crate::hydra::substitution::id_type_subst, constraints.clone()))}}}}, crate::hydra::lib::maybes::map(|v| v.0.type_.clone(), crate::hydra::lib::maps::lookup(name.clone(), cx.clone().0.primitives.clone())))}

pub fn infer_type_of_projection(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, proj: crate::hydra::core::Projection) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  let tname = proj.clone().0.type_name.clone() ;
  let fname = proj.clone().0.field.clone() ;
  crate::hydra::lib::eithers::bind(crate::hydra::schemas::require_schema_type(fcx.clone(), cx.clone().0.schema_types.clone(), tname.clone()), |st_rp: (crate::hydra::core::TypeScheme, crate::hydra::context::Context)| {
    let schema_type = crate::hydra::lib::pairs::first(st_rp.clone()) ;
    {
      let fcx2 = crate::hydra::lib::pairs::second(st_rp.clone()) ;
      {
        let svars = schema_type.clone().0.variables.clone() ;
        {
          let stype = schema_type.clone().0.type_.clone() ;
          crate::hydra::lib::eithers::bind(crate::hydra::extract::core::record_type(fcx2.clone(), tname.clone(), stype.clone()), |sfields: Vec<crate::hydra::core::FieldType>| crate::hydra::lib::eithers::bind(crate::hydra::schemas::find_field_type(fcx2.clone(), fname.clone(), sfields.clone()), |ftyp: crate::hydra::core::Type| Right(yield_(fcx2.clone(), build_type_application_term(svars.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Record(crate::hydra::core::Projection(Rc::new(crate::hydra::core::Projection_Variant {
            type_name: tname.clone(),
            field: fname.clone()})))))))))))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
            domain: crate::hydra::schemas::nominal_application(tname.clone(), crate::hydra::lib::lists::map(|x: crate::hydra::core::Name| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(x.clone()))), svars.clone())),
            codomain: ftyp.clone()}))))), crate::hydra::substitution::id_type_subst))))}}}})}

pub fn infer_type_of_record(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, record: crate::hydra::core::Record) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  let tname = record.clone().0.type_name.clone() ;
  let fields = record.clone().0.fields.clone() ;
  let fnames = crate::hydra::lib::lists::map(|v| v.0.name.clone(), fields.clone()) ;
  crate::hydra::lib::eithers::bind(crate::hydra::schemas::require_schema_type(fcx.clone(), cx.clone().0.schema_types.clone(), tname.clone()), |st_rp: (crate::hydra::core::TypeScheme, crate::hydra::context::Context)| {
    let schema_type = crate::hydra::lib::pairs::first(st_rp.clone()) ;
    {
      let fcx2 = crate::hydra::lib::pairs::second(st_rp.clone()) ;
      crate::hydra::lib::eithers::bind(infer_many(fcx2.clone(), cx.clone(), crate::hydra::lib::lists::map(|f: crate::hydra::core::Field| (f.clone().0.term.clone(), crate::hydra::lib::strings::cat2(String::from("field "), f.clone().0.name.clone().0.0.clone())), fields.clone())), |rp: ((Vec<crate::hydra::core::Term>, (Vec<crate::hydra::core::Type>, (crate::hydra::typing::TypeSubst, BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>))), crate::hydra::context::Context)| {
        let results = crate::hydra::lib::pairs::first(rp.clone()) ;
        {
          let fcx3 = crate::hydra::lib::pairs::second(rp.clone()) ;
          {
            let svars = schema_type.clone().0.variables.clone() ;
            {
              let stype = schema_type.clone().0.type_.clone() ;
              {
                let iterms = crate::hydra::lib::pairs::first(results.clone()) ;
                {
                  let itypes = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(results.clone())) ;
                  {
                    let isubst = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(results.clone()))) ;
                    {
                      let rec_elem_constraints = crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(results.clone()))) ;
                      {
                        let ityp = crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(crate::hydra::lib::lists::zip_with(|n: crate::hydra::core::Name, t: crate::hydra::core::Type| crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
                          name: n.clone(),
                          type_: t.clone()})), fnames.clone(), itypes.clone())))) ;
                        crate::hydra::lib::eithers::bind(map_constraints(fcx3.clone(), cx.clone(), |subst: crate::hydra::typing::TypeSubst| yield_with_constraints(fcx3.clone(), build_type_application_term(svars.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
                          type_name: tname.clone(),
                          fields: crate::hydra::lib::lists::zip_with(|n: crate::hydra::core::Name, t: crate::hydra::core::Term| crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                            name: n.clone(),
                            term: t.clone()})), fnames.clone(), iterms.clone())})))))), crate::hydra::schemas::nominal_application(tname.clone(), crate::hydra::lib::lists::map(|x: crate::hydra::core::Name| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(x.clone()))), svars.clone())), crate::hydra::substitution::compose_type_subst(isubst.clone(), subst.clone()), crate::hydra::substitution::subst_in_class_constraints(subst.clone(), rec_elem_constraints.clone())), Vec::from([
                          crate::hydra::typing::TypeConstraint(Rc::new(crate::hydra::typing::TypeConstraint_Variant {
                            left: stype.clone(),
                            right: ityp.clone(),
                            comment: String::from("schema type of record")}))])), |mc_result: crate::hydra::typing::InferenceResult| Right(mc_result.clone()))}}}}}}}}})}})}

pub fn infer_type_of_set(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, s: BTreeSet<crate::hydra::core::Term>) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  infer_type_of_collection(fcx.clone(), cx.clone(), |x: crate::hydra::core::Type| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(x.clone()))), |terms: Vec<crate::hydra::core::Term>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Set(crate::hydra::lib::sets::from_list(terms.clone())))), String::from("set element"), crate::hydra::lib::sets::singleton(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("ordering"))))), crate::hydra::lib::sets::to_list(s.clone()))}

pub fn infer_type_of_term(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, term: crate::hydra::core::Term, desc: String) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  let fcx2 = crate::hydra::context::Context(Rc::new(crate::hydra::context::Context_Variant {
    trace: crate::hydra::lib::lists::cons(desc.clone(), fcx.clone().0.trace.clone()),
    messages: fcx.clone().0.messages.clone(),
    other: fcx.clone().0.other.clone()})) ;
  match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_annotated_term(fcx2.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_application(fcx2.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_either(fcx2.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_function(fcx2.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_let(fcx2.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_list(fcx2.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      Right(infer_type_of_literal(fcx2.clone(), v0_.clone()))},
    crate::hydra::core::Term_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_map(fcx2.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_optional(fcx2.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_pair(fcx2.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_record(fcx2.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_set(fcx2.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_type_application(fcx2.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_type_lambda(fcx2.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_injection(fcx2.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      Right(infer_type_of_unit(fcx2.clone()))},
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_variable(fcx2.clone(), cx.clone(), v0_.clone())},
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      infer_type_of_wrapped_term(fcx2.clone(), cx.clone(), v0_.clone())}}}

pub fn infer_type_of_type_lambda(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, ta: crate::hydra::core::TypeLambda) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  infer_type_of_term(fcx.clone(), cx.clone(), ta.clone().0.body.clone(), String::from("type abstraction"))}

pub fn infer_type_of_type_application(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, tt: crate::hydra::core::TypeApplicationTerm) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  infer_type_of_term(fcx.clone(), cx.clone(), tt.clone().0.body.clone(), String::from("type application term"))}

pub fn infer_type_of_unit(fcx: crate::hydra::context::Context) -> crate::hydra::typing::InferenceResult {
  crate::hydra::typing::InferenceResult(Rc::new(crate::hydra::typing::InferenceResult_Variant {
    term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit)),
    type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)),
    subst: crate::hydra::substitution::id_type_subst,
    class_constraints: crate::hydra::lib::maps::empty,
    context: fcx.clone()}))}

pub fn infer_type_of_unwrap(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, tname: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  crate::hydra::lib::eithers::bind(crate::hydra::schemas::require_schema_type(fcx.clone(), cx.clone().0.schema_types.clone(), tname.clone()), |st_rp: (crate::hydra::core::TypeScheme, crate::hydra::context::Context)| {
    let schema_type = crate::hydra::lib::pairs::first(st_rp.clone()) ;
    {
      let fcx2 = crate::hydra::lib::pairs::second(st_rp.clone()) ;
      {
        let svars = schema_type.clone().0.variables.clone() ;
        {
          let stype = schema_type.clone().0.type_.clone() ;
          crate::hydra::lib::eithers::bind(crate::hydra::extract::core::wrapped_type(fcx2.clone(), tname.clone(), stype.clone()), |wtyp: crate::hydra::core::Type| Right(yield_(fcx2.clone(), build_type_application_term(svars.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Wrap(tname.clone())))))))))), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
            domain: crate::hydra::schemas::nominal_application(tname.clone(), crate::hydra::lib::lists::map(|x: crate::hydra::core::Name| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(x.clone()))), svars.clone())),
            codomain: wtyp.clone()}))))), crate::hydra::substitution::id_type_subst)))}}}})}

pub fn infer_type_of_variable(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, name: crate::hydra::core::Name) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat2(String::from("Variable not bound to type: "), name.clone().0.0.clone()))))))),
    context: fcx.clone()}))), |scheme: crate::hydra::core::TypeScheme| {
    let ts_result = crate::hydra::schemas::instantiate_type_scheme(fcx.clone(), scheme.clone()) ;
    {
      let ts = crate::hydra::lib::pairs::first(ts_result.clone()) ;
      {
        let fcx2 = crate::hydra::lib::pairs::second(ts_result.clone()) ;
        {
          let constraints = crate::hydra::lib::maybes::from_maybe(crate::hydra::lib::maps::empty, ts.clone().0.constraints.clone()) ;
          Right(crate::hydra::typing::InferenceResult(Rc::new(crate::hydra::typing::InferenceResult_Variant {
            term: build_type_application_term(ts.clone().0.variables.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(name.clone())))),
            type_: ts.clone().0.type_.clone(),
            subst: crate::hydra::substitution::id_type_subst,
            class_constraints: constraints.clone(),
            context: fcx2.clone()})))}}}}, crate::hydra::lib::maps::lookup(name.clone(), cx.clone().0.bound_types.clone()))}

pub fn infer_type_of_wrapped_term(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, wt: crate::hydra::core::WrappedTerm) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  let tname = wt.clone().0.type_name.clone() ;
  let term = wt.clone().0.body.clone() ;
  crate::hydra::lib::eithers::bind(crate::hydra::schemas::require_schema_type(fcx.clone(), cx.clone().0.schema_types.clone(), tname.clone()), |st_rp: (crate::hydra::core::TypeScheme, crate::hydra::context::Context)| {
    let schema_type = crate::hydra::lib::pairs::first(st_rp.clone()) ;
    {
      let fcx2 = crate::hydra::lib::pairs::second(st_rp.clone()) ;
      crate::hydra::lib::eithers::bind(infer_type_of_term(fcx2.clone(), cx.clone(), term.clone(), String::from("wrapped term")), |result: crate::hydra::typing::InferenceResult| {
        let fcx3 = result.clone().0.context.clone() ;
        {
          let svars = schema_type.clone().0.variables.clone() ;
          {
            let stype = schema_type.clone().0.type_.clone() ;
            {
              let iterm = result.clone().0.term.clone() ;
              {
                let itype = result.clone().0.type_.clone() ;
                {
                  let isubst = result.clone().0.subst.clone() ;
                  {
                    let ityp = crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Wrap(itype.clone()))) ;
                    crate::hydra::lib::eithers::bind(map_constraints(fcx3.clone(), cx.clone(), |subst: crate::hydra::typing::TypeSubst| yield_(fcx3.clone(), build_type_application_term(svars.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                      type_name: tname.clone(),
                      body: iterm.clone()})))))), crate::hydra::schemas::nominal_application(tname.clone(), crate::hydra::lib::lists::map(|x: crate::hydra::core::Name| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(x.clone()))), svars.clone())), crate::hydra::substitution::compose_type_subst(isubst.clone(), subst.clone())), Vec::from([
                      crate::hydra::typing::TypeConstraint(Rc::new(crate::hydra::typing::TypeConstraint_Variant {
                        left: stype.clone(),
                        right: ityp.clone(),
                        comment: String::from("schema type of wrapper")}))])), |mc_result: crate::hydra::typing::InferenceResult| Right(mc_result.clone()))}}}}}}})}})}

pub fn infer_types_of_temporary_bindings(fcx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, bins: Vec<crate::hydra::core::Binding>) -> Either<crate::hydra::context::InContext, ((Vec<crate::hydra::core::Term>, (Vec<crate::hydra::core::Type>, (crate::hydra::typing::TypeSubst, BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>))), crate::hydra::context::Context)> {
  let dflt = {
    let binding = crate::hydra::lib::lists::head(bins.clone()) ;
    {
      let k = binding.clone().0.name.clone() ;
      {
        let v = binding.clone().0.term.clone() ;
        {
          let tl = crate::hydra::lib::lists::tail(bins.clone()) ;
          crate::hydra::lib::eithers::bind(infer_type_of_term(fcx.clone(), cx.clone(), v.clone(), crate::hydra::lib::strings::cat(Vec::from([
            String::from("temporary let binding '"),
            k.clone().0.0.clone(),
            String::from("'")]))), |result1: crate::hydra::typing::InferenceResult| {
            let fcx2 = result1.clone().0.context.clone() ;
            {
              let j = result1.clone().0.term.clone() ;
              {
                let u_prime = result1.clone().0.type_.clone() ;
                {
                  let u = result1.clone().0.subst.clone() ;
                  {
                    let c1_inferred = result1.clone().0.class_constraints.clone() ;
                    crate::hydra::lib::eithers::bind(crate::hydra::lib::maybes::maybe(Right(crate::hydra::lib::maps::empty), |ts: crate::hydra::core::TypeScheme| {
                      let ts_result = crate::hydra::schemas::instantiate_type_scheme(fcx2.clone(), ts.clone()) ;
                      {
                        let instantiated_ts = crate::hydra::lib::pairs::first(ts_result.clone()) ;
                        {
                          let fresh_constraints = crate::hydra::lib::maybes::from_maybe(crate::hydra::lib::maps::empty, instantiated_ts.clone().0.constraints.clone()) ;
                          crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|_ic: crate::hydra::context::InContext| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
                            object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(_ic.clone().0.object.clone().0.message.clone())))))),
                            context: _ic.clone().0.context.clone()})), |_a: crate::hydra::typing::TypeSubst| _a.clone(), crate::hydra::unification::unify_types(fcx2.clone(), cx.clone().0.schema_types.clone(), instantiated_ts.clone().0.type_.clone(), u_prime.clone(), String::from("original binding type"))), |unify_subst: crate::hydra::typing::TypeSubst| Right(crate::hydra::substitution::subst_in_class_constraints(unify_subst.clone(), fresh_constraints.clone())))}}}, binding.clone().0.type_.clone()), |original_binding_constraints: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>| {
                      let c1 = merge_class_constraints(c1_inferred.clone(), original_binding_constraints.clone()) ;
                      crate::hydra::lib::eithers::bind(infer_types_of_temporary_bindings(fcx2.clone(), crate::hydra::substitution::subst_in_context(u.clone(), cx.clone()), tl.clone()), |rp2: ((Vec<crate::hydra::core::Term>, (Vec<crate::hydra::core::Type>, (crate::hydra::typing::TypeSubst, BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>))), crate::hydra::context::Context)| {
                        let result2 = crate::hydra::lib::pairs::first(rp2.clone()) ;
                        {
                          let fcx3 = crate::hydra::lib::pairs::second(rp2.clone()) ;
                          {
                            let h = crate::hydra::lib::pairs::first(result2.clone()) ;
                            {
                              let r_prime = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(result2.clone())) ;
                              {
                                let rest_pair = crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(result2.clone())) ;
                                {
                                  let r = crate::hydra::lib::pairs::first(rest_pair.clone()) ;
                                  {
                                    let c2 = crate::hydra::lib::pairs::second(rest_pair.clone()) ;
                                    {
                                      let c1_subst = crate::hydra::substitution::subst_in_class_constraints(r.clone(), c1.clone()) ;
                                      {
                                        let merged_constraints = merge_class_constraints(c1_subst.clone(), c2.clone()) ;
                                        Right(((crate::hydra::lib::lists::cons(crate::hydra::substitution::subst_types_in_term(r.clone(), j.clone()), h.clone()), (crate::hydra::lib::lists::cons(crate::hydra::substitution::subst_in_type(r.clone(), u_prime.clone()), r_prime.clone()), (crate::hydra::substitution::compose_type_subst(u.clone(), r.clone()), merged_constraints.clone()))), fcx3.clone()))}}}}}}}}})})}}}}})}}}} ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(bins.clone()), Right(((Vec::from([]), (Vec::from([]), (crate::hydra::substitution::id_type_subst, crate::hydra::lib::maps::empty))), fcx.clone())), dflt.clone())}

pub fn is_unbound(cx: crate::hydra::graph::Graph, v: crate::hydra::core::Name) -> bool {
  crate::hydra::lib::logic::and(crate::hydra::lib::logic::not(crate::hydra::lib::sets::member(v.clone(), free_variables_in_context(cx.clone()))), crate::hydra::lib::logic::not(crate::hydra::lib::maps::member(v.clone(), cx.clone().0.schema_types.clone())))}

pub fn map_constraints(flow_cx: crate::hydra::context::Context, cx: crate::hydra::graph::Graph, f: impl Fn(crate::hydra::typing::TypeSubst) -> T0 + Clone, constraints: Vec<crate::hydra::typing::TypeConstraint>) -> Either<crate::hydra::context::InContext, T0> {
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|_ic: crate::hydra::context::InContext| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(_ic.clone().0.object.clone().0.message.clone())))))),
    context: _ic.clone().0.context.clone()})), |_a: crate::hydra::typing::TypeSubst| _a.clone(), crate::hydra::unification::unify_type_constraints(flow_cx.clone(), cx.clone().0.schema_types.clone(), constraints.clone())), |s: crate::hydra::typing::TypeSubst| crate::hydra::lib::eithers::bind(crate::hydra::checking::check_type_subst(flow_cx.clone(), cx.clone(), s.clone()), |_: crate::hydra::typing::TypeSubst| Right(f.clone()(s.clone()))))}

pub fn merge_class_constraints(m1: BTreeMap<T0, crate::hydra::core::TypeVariableMetadata>, m2: BTreeMap<T0, crate::hydra::core::TypeVariableMetadata>) -> BTreeMap<T0, crate::hydra::core::TypeVariableMetadata> {
  crate::hydra::lib::lists::foldl(|acc: BTreeMap<T0, crate::hydra::core::TypeVariableMetadata>, pair: (T0, crate::hydra::core::TypeVariableMetadata)| {
    let k = crate::hydra::lib::pairs::first(pair.clone()) ;
    {
      let v = crate::hydra::lib::pairs::second(pair.clone()) ;
      crate::hydra::lib::maybes::maybe(crate::hydra::lib::maps::insert(k.clone(), v.clone(), acc.clone()), |existing: crate::hydra::core::TypeVariableMetadata| {
        let merged = crate::hydra::core::TypeVariableMetadata(Rc::new(crate::hydra::core::TypeVariableMetadata_Variant {
          classes: crate::hydra::lib::sets::union_(existing.clone().0.classes.clone(), v.clone().0.classes.clone())})) ;
        crate::hydra::lib::maps::insert(k.clone(), merged.clone(), acc.clone())}, crate::hydra::lib::maps::lookup(k.clone(), acc.clone()))}}, m1.clone(), crate::hydra::lib::maps::to_list(m2.clone()))}

pub fn show_inference_result(result: crate::hydra::typing::InferenceResult) -> String {
  let term = result.clone().0.term.clone() ;
  let typ = result.clone().0.type_.clone() ;
  let subst = result.clone().0.subst.clone() ;
  crate::hydra::lib::strings::cat(Vec::from([
    String::from("{term="),
    crate::hydra::show::core::term(term.clone()),
    String::from(", type="),
    crate::hydra::show::core::type_(typ.clone()),
    String::from(", subst="),
    crate::hydra::show::typing::type_subst(subst.clone()),
    String::from("}")]))}

pub fn yield_(fcx: crate::hydra::context::Context, term: crate::hydra::core::Term, typ: crate::hydra::core::Type, subst: crate::hydra::typing::TypeSubst) -> crate::hydra::typing::InferenceResult {
  crate::hydra::typing::InferenceResult(Rc::new(crate::hydra::typing::InferenceResult_Variant {
    term: crate::hydra::substitution::subst_types_in_term(subst.clone(), term.clone()),
    type_: crate::hydra::substitution::subst_in_type(subst.clone(), typ.clone()),
    subst: subst.clone(),
    class_constraints: crate::hydra::lib::maps::empty,
    context: fcx.clone()}))}

pub fn yield_checked(fcx: crate::hydra::context::Context, term: crate::hydra::core::Term, typ: crate::hydra::core::Type, subst: crate::hydra::typing::TypeSubst) -> crate::hydra::typing::InferenceResult {
  let iterm = crate::hydra::substitution::subst_types_in_term(subst.clone(), term.clone()) ;
  let itype = crate::hydra::substitution::subst_in_type(subst.clone(), typ.clone()) ;
  crate::hydra::typing::InferenceResult(Rc::new(crate::hydra::typing::InferenceResult_Variant {
    term: iterm.clone(),
    type_: itype.clone(),
    subst: subst.clone(),
    class_constraints: crate::hydra::lib::maps::empty,
    context: fcx.clone()}))}

pub fn yield_checked_with_constraints(fcx: crate::hydra::context::Context, term: crate::hydra::core::Term, typ: crate::hydra::core::Type, subst: crate::hydra::typing::TypeSubst, constraints: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>) -> crate::hydra::typing::InferenceResult {
  let iterm = crate::hydra::substitution::subst_types_in_term(subst.clone(), term.clone()) ;
  let itype = crate::hydra::substitution::subst_in_type(subst.clone(), typ.clone()) ;
  let iconstraints = crate::hydra::substitution::subst_in_class_constraints(subst.clone(), constraints.clone()) ;
  crate::hydra::typing::InferenceResult(Rc::new(crate::hydra::typing::InferenceResult_Variant {
    term: iterm.clone(),
    type_: itype.clone(),
    subst: subst.clone(),
    class_constraints: iconstraints.clone(),
    context: fcx.clone()}))}

pub fn yield_debug(fcx: crate::hydra::context::Context, cx: T0, debug_id: String, term: crate::hydra::core::Term, typ: crate::hydra::core::Type, subst: crate::hydra::typing::TypeSubst) -> Either<crate::hydra::context::InContext, crate::hydra::typing::InferenceResult> {
  let rterm = crate::hydra::substitution::subst_types_in_term(subst.clone(), term.clone()) ;
  let rtyp = crate::hydra::substitution::subst_in_type(subst.clone(), typ.clone()) ;
  crate::hydra::lib::eithers::bind(crate::hydra::annotations::debug_if(fcx.clone(), debug_id.clone(), crate::hydra::lib::strings::cat(Vec::from([
    String::from("\n\tterm: "),
    crate::hydra::show::core::term(term.clone()),
    String::from("\n\ttyp: "),
    crate::hydra::show::core::type_(typ.clone()),
    String::from("\n\tsubst: "),
    crate::hydra::show::typing::type_subst(subst.clone()),
    String::from("\n\trterm: "),
    crate::hydra::show::core::term(rterm.clone()),
    String::from("\n\trtyp: "),
    crate::hydra::show::core::type_(rtyp.clone())]))), |result: ()| Right(crate::hydra::typing::InferenceResult(Rc::new(crate::hydra::typing::InferenceResult_Variant {
    term: rterm.clone(),
    type_: rtyp.clone(),
    subst: subst.clone(),
    class_constraints: crate::hydra::lib::maps::empty,
    context: fcx.clone()}))))}

pub fn yield_with_constraints(fcx: crate::hydra::context::Context, term: crate::hydra::core::Term, typ: crate::hydra::core::Type, subst: crate::hydra::typing::TypeSubst, constraints: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>) -> crate::hydra::typing::InferenceResult {
  crate::hydra::typing::InferenceResult(Rc::new(crate::hydra::typing::InferenceResult_Variant {
    term: crate::hydra::substitution::subst_types_in_term(subst.clone(), term.clone()),
    type_: crate::hydra::substitution::subst_in_type(subst.clone(), typ.clone()),
    subst: subst.clone(),
    class_constraints: constraints.clone(),
    context: fcx.clone()}))}
