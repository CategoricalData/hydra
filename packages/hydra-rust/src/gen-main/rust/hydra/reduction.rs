#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::arity::*;
use crate::hydra::checking::*;
use crate::hydra::extract::core::*;
use crate::hydra::hoisting::*;
use crate::hydra::inference::*;
use crate::hydra::lexical::*;
use crate::hydra::rewriting::*;
use crate::hydra::schemas::*;
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

pub fn alpha_convert(vold: crate::hydra::core::Name, vnew: crate::hydra::core::Name, term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  crate::hydra::rewriting::replace_free_term_variable(vold.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(vnew.clone()))), term.clone())}

pub fn beta_reduce_type(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, typ: crate::hydra::core::Type) -> Either<crate::hydra::context::InContext, crate::hydra::core::Type> {
  let reduce_app = |app: crate::hydra::core::ApplicationType| {
    let lhs = app.clone().0.function.clone() ;
    {
      let rhs = app.clone().0.argument.clone() ;
      match &*lhs.clone().0 {
        crate::hydra::core::Type_Variant::Annotated (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::lib::eithers::bind(reduce_app.clone()(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
            function: v0_.clone().0.body.clone(),
            argument: rhs.clone()}))), |a: crate::hydra::core::Type| Right(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
            body: a.clone(),
            annotation: v0_.clone().0.annotation.clone()})))))))},
        crate::hydra::core::Type_Variant::Forall (v0_) => {
          let v0_ = v0_.clone() ;
          beta_reduce_type(cx.clone(), graph.clone(), crate::hydra::rewriting::replace_free_type_variable(v0_.clone().0.parameter.clone(), rhs.clone(), v0_.clone().0.body.clone()))},
        crate::hydra::core::Type_Variant::Variable (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::lib::eithers::bind(crate::hydra::schemas::require_type(cx.clone(), graph.clone(), v0_.clone()), |t_: crate::hydra::core::Type| beta_reduce_type(cx.clone(), graph.clone(), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
            function: t_.clone(),
            argument: rhs.clone()})))))))}}}} ;
  let map_expr = |recurse: Rc<dyn Fn(T0) -> Either<crate::hydra::context::InContext, crate::hydra::core::Type>>, t: T0| {
    let find_app = |r: crate::hydra::core::Type| match &*r.clone().0 {
      crate::hydra::core::Type_Variant::Application (v0_) => {
        let v0_ = v0_.clone() ;
        reduce_app.clone()(v0_.clone())},
      _ => Right(r.clone())} ;
    crate::hydra::lib::eithers::bind(recurse.clone()(t.clone()), |r: crate::hydra::core::Type| find_app.clone()(r.clone()))} ;
  crate::hydra::rewriting::rewrite_type_m(map_expr.clone(), typ.clone())}

pub fn contract_term(term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let rewrite = |recurse: Rc<dyn Fn(T0) -> crate::hydra::core::Term>, t: T0| {
    let rec = recurse.clone()(t.clone()) ;
    match &*rec.clone().0 {
      crate::hydra::core::Term_Variant::Application (v0_) => {
        let v0_ = v0_.clone() ;
        {
          let lhs = v0_.clone().0.function.clone() ;
          {
            let rhs = v0_.clone().0.argument.clone() ;
            match &*crate::hydra::rewriting::deannotate_term(lhs.clone()).0 {
              crate::hydra::core::Term_Variant::Function (v0_) => {
                let v0_ = v0_.clone() ;
                match &*v0_.clone().0 {
                  crate::hydra::core::Function_Variant::Lambda (v0_) => {
                    let v0_ = v0_.clone() ;
                    {
                      let v = v0_.clone().0.parameter.clone() ;
                      {
                        let body = v0_.clone().0.body.clone() ;
                        crate::hydra::lib::logic::if_else(crate::hydra::rewriting::is_free_variable_in_term(v.clone(), body.clone()), body.clone(), crate::hydra::rewriting::replace_free_term_variable(v.clone(), rhs.clone(), body.clone()))}}},
                  _ => rec.clone()}},
              _ => rec.clone()}}}},
      _ => rec.clone()}} ;
  crate::hydra::rewriting::rewrite_term(rewrite.clone(), term.clone())}

pub fn count_primitive_invocations() -> bool {
  true}

pub fn eta_reduce_term(term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let no_change = term.clone() ;
  let reduce_lambda = |l: crate::hydra::core::Lambda| {
    let v = l.clone().0.parameter.clone() ;
    {
      let d = l.clone().0.domain.clone() ;
      {
        let body = l.clone().0.body.clone() ;
        match &*eta_reduce_term(body.clone()).0 {
          crate::hydra::core::Term_Variant::Annotated (v0_) => {
            let v0_ = v0_.clone() ;
            reduce_lambda.clone()(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
              parameter: v.clone(),
              domain: d.clone(),
              body: v0_.clone().0.body.clone()})))},
          crate::hydra::core::Term_Variant::Application (v0_) => {
            let v0_ = v0_.clone() ;
            {
              let lhs = v0_.clone().0.function.clone() ;
              {
                let rhs = v0_.clone().0.argument.clone() ;
                match &*eta_reduce_term(rhs.clone()).0 {
                  crate::hydra::core::Term_Variant::Annotated (v0_) => {
                    let v0_ = v0_.clone() ;
                    reduce_lambda.clone()(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                      parameter: v.clone(),
                      domain: d.clone(),
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                        function: lhs.clone(),
                        argument: v0_.clone().0.body.clone()})))))})))},
                  crate::hydra::core::Term_Variant::Variable (v0_) => {
                    let v0_ = v0_.clone() ;
                    crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::and(crate::hydra::lib::equality::equal(v.clone().0.0.clone(), v0_.clone().0.0.clone()), crate::hydra::lib::logic::not(crate::hydra::rewriting::is_free_variable_in_term(v.clone(), lhs.clone()))), eta_reduce_term(lhs.clone()), no_change.clone())},
                  _ => no_change.clone()}}}},
          _ => no_change.clone()}}}} ;
  match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
        body: eta_reduce_term(v0_.clone().0.body.clone()),
        annotation: v0_.clone().0.annotation.clone()})))))},
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Function_Variant::Lambda (v0_) => {
          let v0_ = v0_.clone() ;
          reduce_lambda.clone()(v0_.clone())},
        _ => no_change.clone()}},
    _ => no_change.clone()}}

pub fn eta_expand_term(graph: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let expand = |args: Vec<crate::hydra::core::Term>, arity: i32, t: crate::hydra::core::Term| {
    let apps = crate::hydra::lib::lists::foldl(|lhs: crate::hydra::core::Term, arg: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
      function: lhs.clone(),
      argument: arg.clone()}))))), t.clone(), args.clone()) ;
    {
      let is = crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::lte(arity.clone(), crate::hydra::lib::lists::length(args.clone())), Vec::from([]), crate::hydra::lib::math::range(1i32, crate::hydra::lib::math::sub(arity.clone(), crate::hydra::lib::lists::length(args.clone())))) ;
      {
        let pad = |indices: Vec<i32>, t2: crate::hydra::core::Term| crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(indices.clone()), t2.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
          parameter: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat2(String::from("v"), crate::hydra::lib::literals::show_int32(crate::hydra::lib::lists::head(indices.clone())))))),
          domain: None,
          body: pad.clone()(crate::hydra::lib::lists::tail(indices.clone()), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
            function: t2.clone(),
            argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat2(String::from("v"), crate::hydra::lib::literals::show_int32(crate::hydra::lib::lists::head(indices.clone())))))))))}))))))}))))))))) ;
        pad.clone()(is.clone(), apps.clone())}}} ;
  let rewrite = |args: Vec<crate::hydra::core::Term>, recurse: Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>, t: crate::hydra::core::Term| {
    let after_recursion = |term2: crate::hydra::core::Term| expand.clone()(args.clone(), eta_expansion_arity(graph.clone(), term2.clone()), term2.clone()) ;
    {
      let t2 = crate::hydra::rewriting::detype_term(t.clone()) ;
      match &*t2.clone().0 {
        crate::hydra::core::Term_Variant::Application (v0_) => {
          let v0_ = v0_.clone() ;
          {
            let lhs = v0_.clone().0.function.clone() ;
            {
              let rhs = v0_.clone().0.argument.clone() ;
              {
                let erhs = rewrite.clone()(Vec::from([]), recurse.clone(), rhs.clone()) ;
                rewrite.clone()(crate::hydra::lib::lists::cons(erhs.clone(), args.clone()), recurse.clone(), lhs.clone())}}}},
        _ => after_recursion.clone()(recurse.clone()(t2.clone()))}}} ;
  contract_term(crate::hydra::rewriting::rewrite_term(|v1: Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>, v2: crate::hydra::core::Term| rewrite.clone()(Vec::from([]), v1.clone(), v2.clone()), term.clone()))}

pub fn eta_expand_term_new(tx0: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let term_arity_with_context = |tx: crate::hydra::graph::Graph, term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      term_arity_with_context.clone()(tx.clone(), v0_.clone().0.body.clone())},
    crate::hydra::core::Term_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::math::sub(term_arity_with_context.clone()(tx.clone(), v0_.clone().0.function.clone()), 1i32)},
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Function_Variant::Elimination (v0_) => {
          let v0_ = v0_.clone() ;
          1i32},
        crate::hydra::core::Function_Variant::Lambda (v0_) => {
          let v0_ = v0_.clone() ;
          0i32},
        crate::hydra::core::Function_Variant::Primitive (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::lib::maybes::maybe(0i32, crate::hydra::arity::type_scheme_arity, crate::hydra::lib::maps::lookup(v0_.clone(), crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|_gpt_p: crate::hydra::graph::Primitive| (_gpt_p.clone().0.name.clone(), _gpt_p.clone().0.type_.clone()), crate::hydra::lib::maps::elems(tx.clone().0.primitives.clone())))))}}},
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      term_arity_with_context.clone()(crate::hydra::schemas::extend_graph_for_let(|_: crate::hydra::graph::Graph, _2: crate::hydra::core::Binding| None, tx.clone(), v0_.clone()), v0_.clone().0.body.clone())},
    crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
      let v0_ = v0_.clone() ;
      term_arity_with_context.clone()(crate::hydra::schemas::extend_graph_for_type_lambda(tx.clone(), v0_.clone()), v0_.clone().0.body.clone())},
    crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
      let v0_ = v0_.clone() ;
      term_arity_with_context.clone()(tx.clone(), v0_.clone().0.body.clone())},
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::maybes::maybe(0i32, crate::hydra::arity::type_arity, crate::hydra::lib::maybes::map(crate::hydra::rewriting::type_scheme_to_f_type, crate::hydra::lib::maps::lookup(v0_.clone(), tx.clone().0.bound_types.clone())))},
    _ => 0i32} ;
  let domain_types = |n: i32, mt: Option<crate::hydra::core::Type>| crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::lte(n.clone(), 0i32), Vec::from([]), crate::hydra::lib::maybes::maybe(crate::hydra::lib::lists::map(|_: i32| None, crate::hydra::lib::math::range(1i32, n.clone())), |typ: crate::hydra::core::Type| match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::cons(Some(v0_.clone().0.domain.clone()), domain_types.clone()(crate::hydra::lib::math::sub(n.clone(), 1i32), Some(v0_.clone().0.codomain.clone())))},
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      domain_types.clone()(n.clone(), Some(v0_.clone().0.body.clone()))},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      domain_types.clone()(n.clone(), Some(v0_.clone().0.function.clone()))},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      domain_types.clone()(n.clone(), Some(v0_.clone().0.body.clone()))},
    _ => crate::hydra::lib::lists::map(|_: i32| None, crate::hydra::lib::math::range(1i32, n.clone()))}, mt.clone())) ;
  let peel_function_domains = |mtyp: Option<crate::hydra::core::Type>, n: i32| crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::lte(n.clone(), 0i32), mtyp.clone(), crate::hydra::lib::maybes::maybe(None, |typ: crate::hydra::core::Type| match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      peel_function_domains.clone()(Some(v0_.clone().0.codomain.clone()), crate::hydra::lib::math::sub(n.clone(), 1i32))},
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      peel_function_domains.clone()(Some(v0_.clone().0.body.clone()), n.clone())},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      peel_function_domains.clone()(Some(v0_.clone().0.function.clone()), n.clone())},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      peel_function_domains.clone()(Some(v0_.clone().0.body.clone()), n.clone())},
    _ => None}, mtyp.clone())) ;
  let expand = |always_pad: bool, args: Vec<crate::hydra::core::Term>, arity: i32, head_typ: Option<crate::hydra::core::Type>, head: crate::hydra::core::Term| {
    let applied = crate::hydra::lib::lists::foldl(|lhs: crate::hydra::core::Term, arg: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
      function: lhs.clone(),
      argument: arg.clone()}))))), head.clone(), args.clone()) ;
    {
      let num_args = crate::hydra::lib::lists::length(args.clone()) ;
      {
        let needed = crate::hydra::lib::math::sub(arity.clone(), num_args.clone()) ;
        crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::and(crate::hydra::lib::equality::gt(needed.clone(), 0i32), crate::hydra::lib::logic::or(always_pad.clone(), crate::hydra::lib::equality::gt(num_args.clone(), 0i32))), {
          let indices = crate::hydra::lib::math::range(1i32, needed.clone()) ;
          {
            let remaining_type = peel_function_domains.clone()(head_typ.clone(), num_args.clone()) ;
            {
              let domains = domain_types.clone()(needed.clone(), remaining_type.clone()) ;
              {
                let codomain_type = peel_function_domains.clone()(remaining_type.clone(), needed.clone()) ;
                {
                  let fully_applied_raw = crate::hydra::lib::lists::foldl(|body: crate::hydra::core::Term, i: i32| {
                    let vn = crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat2(String::from("v"), crate::hydra::lib::literals::show_int32(i.clone()))))) ;
                    crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                      function: body.clone(),
                      argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(vn.clone())))})))))}, applied.clone(), indices.clone()) ;
                  {
                    let fully_applied = crate::hydra::lib::maybes::maybe(fully_applied_raw.clone(), |ct: crate::hydra::core::Type| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
                      body: fully_applied_raw.clone(),
                      annotation: crate::hydra::lib::maps::singleton(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("type")))), crate::hydra::encode::core::type_(ct.clone()))}))))), codomain_type.clone()) ;
                    {
                      let indexed_domains = crate::hydra::lib::lists::zip(indices.clone(), domains.clone()) ;
                      crate::hydra::lib::lists::foldl(|body: crate::hydra::core::Term, id_pair: (i32, Option<crate::hydra::core::Type>)| {
                        let i = crate::hydra::lib::pairs::first(id_pair.clone()) ;
                        {
                          let dom = crate::hydra::lib::pairs::second(id_pair.clone()) ;
                          {
                            let vn = crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat2(String::from("v"), crate::hydra::lib::literals::show_int32(i.clone()))))) ;
                            crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                              parameter: vn.clone(),
                              domain: dom.clone(),
                              body: body.clone()}))))))))}}}, fully_applied.clone(), crate::hydra::lib::lists::reverse(indexed_domains.clone()))}}}}}}}, applied.clone())}}} ;
  let rewrite_with_args = |args: Vec<crate::hydra::core::Term>, tx: crate::hydra::graph::Graph, term: crate::hydra::core::Term| {
    let recurse = |tx1: crate::hydra::graph::Graph, term1: crate::hydra::core::Term| rewrite_with_args.clone()(Vec::from([]), tx1.clone(), term1.clone()) ;
    {
      let term_head_type = |tx2: crate::hydra::graph::Graph, trm2: crate::hydra::core::Term| match &*trm2.clone().0 {
        crate::hydra::core::Term_Variant::Annotated (v0_) => {
          let v0_ = v0_.clone() ;
          term_head_type.clone()(tx2.clone(), v0_.clone().0.body.clone())},
        crate::hydra::core::Term_Variant::Function (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::Function_Variant::Primitive (v0_) => {
              let v0_ = v0_.clone() ;
              crate::hydra::lib::maybes::map(|ts2: crate::hydra::core::TypeScheme| ts2.clone().0.type_.clone(), crate::hydra::lib::maps::lookup(v0_.clone(), crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|_gpt_p: crate::hydra::graph::Primitive| (_gpt_p.clone().0.name.clone(), _gpt_p.clone().0.type_.clone()), crate::hydra::lib::maps::elems(tx2.clone().0.primitives.clone())))))},
            _ => None}},
        crate::hydra::core::Term_Variant::Let (v0_) => {
          let v0_ = v0_.clone() ;
          term_head_type.clone()(crate::hydra::schemas::extend_graph_for_let(|_: crate::hydra::graph::Graph, _2: crate::hydra::core::Binding| None, tx2.clone(), v0_.clone()), v0_.clone().0.body.clone())},
        crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
          let v0_ = v0_.clone() ;
          term_head_type.clone()(crate::hydra::schemas::extend_graph_for_type_lambda(tx2.clone(), v0_.clone()), v0_.clone().0.body.clone())},
        crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::lib::maybes::bind(term_head_type.clone()(tx2.clone(), v0_.clone().0.body.clone()), |htyp2: crate::hydra::core::Type| match &*htyp2.clone().0 {
            crate::hydra::core::Type_Variant::Forall (v0_) => {
              let v0_ = v0_.clone() ;
              Some(crate::hydra::rewriting::replace_free_type_variable(v0_.clone().0.parameter.clone(), v0_.clone().0.type_.clone(), v0_.clone().0.body.clone()))},
            _ => Some(htyp2.clone())})},
        crate::hydra::core::Term_Variant::Variable (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::lib::maybes::map(crate::hydra::rewriting::type_scheme_to_f_type, crate::hydra::lib::maps::lookup(v0_.clone(), tx2.clone().0.bound_types.clone()))},
        _ => None} ;
      {
        let after_recursion = |trm: crate::hydra::core::Term| {
          let arity = term_arity_with_context.clone()(tx.clone(), trm.clone()) ;
          {
            let h_type = term_head_type.clone()(tx.clone(), trm.clone()) ;
            expand.clone()(false, args.clone(), arity.clone(), h_type.clone(), trm.clone())}} ;
        {
          let for_field = |f: crate::hydra::core::Field| crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
            name: f.clone().0.name.clone(),
            term: recurse.clone()(tx.clone(), f.clone().0.term.clone())})) ;
          {
            let for_case_branch = |f: crate::hydra::core::Field| {
              let branch_body = recurse.clone()(tx.clone(), f.clone().0.term.clone()) ;
              {
                let arty = term_arity_with_context.clone()(tx.clone(), branch_body.clone()) ;
                {
                  let branch_h_type = term_head_type.clone()(tx.clone(), branch_body.clone()) ;
                  crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                    name: f.clone().0.name.clone(),
                    term: expand.clone()(true, Vec::from([]), arty.clone(), branch_h_type.clone(), branch_body.clone())}))}}} ;
            {
              let for_elimination = |elm: crate::hydra::core::Elimination| match &*elm.clone().0 {
                crate::hydra::core::Elimination_Variant::Record (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Record(v0_.clone())))},
                crate::hydra::core::Elimination_Variant::Union (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                    type_name: v0_.clone().0.type_name.clone(),
                    default_: crate::hydra::lib::maybes::map(|t1: crate::hydra::core::Term| recurse.clone()(tx.clone(), t1.clone()), v0_.clone().0.default_.clone()),
                    cases: crate::hydra::lib::lists::map(for_case_branch.clone(), v0_.clone().0.cases.clone())})))))},
                crate::hydra::core::Elimination_Variant::Wrap (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Wrap(v0_.clone())))}} ;
              {
                let for_map = |mp: BTreeMap<crate::hydra::core::Term, crate::hydra::core::Term>| {
                  let for_pair = |pr: (crate::hydra::core::Term, crate::hydra::core::Term)| (recurse.clone()(tx.clone(), crate::hydra::lib::pairs::first(pr.clone())), recurse.clone()(tx.clone(), crate::hydra::lib::pairs::second(pr.clone()))) ;
                  crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(for_pair.clone(), crate::hydra::lib::maps::to_list(mp.clone())))} ;
                match &*term.clone().0 {
                  crate::hydra::core::Term_Variant::Annotated (v0_) => {
                    let v0_ = v0_.clone() ;
                    after_recursion.clone()(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
                      body: recurse.clone()(tx.clone(), v0_.clone().0.body.clone()),
                      annotation: v0_.clone().0.annotation.clone()}))))))},
                  crate::hydra::core::Term_Variant::Application (v0_) => {
                    let v0_ = v0_.clone() ;
                    {
                      let rhs = rewrite_with_args.clone()(Vec::from([]), tx.clone(), v0_.clone().0.argument.clone()) ;
                      rewrite_with_args.clone()(crate::hydra::lib::lists::cons(rhs.clone(), args.clone()), tx.clone(), v0_.clone().0.function.clone())}},
                  crate::hydra::core::Term_Variant::Either (v0_) => {
                    let v0_ = v0_.clone() ;
                    after_recursion.clone()(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(crate::hydra::lib::eithers::either(|l: crate::hydra::core::Term| Left(recurse.clone()(tx.clone(), l.clone())), |r: crate::hydra::core::Term| Right(recurse.clone()(tx.clone(), r.clone())), v0_.clone())))))},
                  crate::hydra::core::Term_Variant::Function (v0_) => {
                    let v0_ = v0_.clone() ;
                    match &*v0_.clone().0 {
                      crate::hydra::core::Function_Variant::Elimination (v0_) => {
                        let v0_ = v0_.clone() ;
                        {
                          let pad_elim = match &*v0_.clone().0 {
                            crate::hydra::core::Elimination_Variant::Record (v0_) => {
                              let v0_ = v0_.clone() ;
                              false},
                            crate::hydra::core::Elimination_Variant::Union (v0_) => {
                              let v0_ = v0_.clone() ;
                              true},
                            crate::hydra::core::Elimination_Variant::Wrap (v0_) => {
                              let v0_ = v0_.clone() ;
                              false}} ;
                          {
                            let elim_term = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(for_elimination.clone()(v0_.clone()))))))) ;
                            {
                              let elim_head_type = match &*v0_.clone().0 {
                                crate::hydra::core::Elimination_Variant::Union (v0_) => {
                                  let v0_ = v0_.clone() ;
                                  Some(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                                    domain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v0_.clone().0.type_name.clone()))),
                                    codomain: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))}))))))},
                                _ => None} ;
                              expand.clone()(pad_elim.clone(), args.clone(), 1i32, elim_head_type.clone(), elim_term.clone())}}}},
                      crate::hydra::core::Function_Variant::Lambda (v0_) => {
                        let v0_ = v0_.clone() ;
                        {
                          let tx1 = crate::hydra::schemas::extend_graph_for_lambda(tx.clone(), v0_.clone()) ;
                          {
                            let body = rewrite_with_args.clone()(Vec::from([]), tx1.clone(), v0_.clone().0.body.clone()) ;
                            {
                              let result = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                parameter: v0_.clone().0.parameter.clone(),
                                domain: v0_.clone().0.domain.clone(),
                                body: body.clone()})))))))) ;
                              {
                                let arty = term_arity_with_context.clone()(tx.clone(), result.clone()) ;
                                expand.clone()(false, args.clone(), arty.clone(), None, result.clone())}}}}},
                      crate::hydra::core::Function_Variant::Primitive (v0_) => {
                        let v0_ = v0_.clone() ;
                        {
                          let arty = term_arity_with_context.clone()(tx.clone(), term.clone()) ;
                          {
                            let prim_type = crate::hydra::lib::maybes::map(|ts: crate::hydra::core::TypeScheme| ts.clone().0.type_.clone(), crate::hydra::lib::maps::lookup(v0_.clone(), crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|_gpt_p: crate::hydra::graph::Primitive| (_gpt_p.clone().0.name.clone(), _gpt_p.clone().0.type_.clone()), crate::hydra::lib::maps::elems(tx.clone().0.primitives.clone()))))) ;
                            expand.clone()(false, args.clone(), arty.clone(), prim_type.clone(), term.clone())}}}}},
                  crate::hydra::core::Term_Variant::Let (v0_) => {
                    let v0_ = v0_.clone() ;
                    {
                      let tx1 = crate::hydra::schemas::extend_graph_for_let(|_: crate::hydra::graph::Graph, _2: crate::hydra::core::Binding| None, tx.clone(), v0_.clone()) ;
                      {
                        let map_binding = |b: crate::hydra::core::Binding| crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                          name: b.clone().0.name.clone(),
                          term: rewrite_with_args.clone()(Vec::from([]), tx1.clone(), b.clone().0.term.clone()),
                          type_: b.clone().0.type_.clone()})) ;
                        {
                          let result = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
                            bindings: crate::hydra::lib::lists::map(map_binding.clone(), v0_.clone().0.bindings.clone()),
                            body: rewrite_with_args.clone()(Vec::from([]), tx1.clone(), v0_.clone().0.body.clone())}))))) ;
                          after_recursion.clone()(result.clone())}}}},
                  crate::hydra::core::Term_Variant::List (v0_) => {
                    let v0_ = v0_.clone() ;
                    after_recursion.clone()(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(|el: crate::hydra::core::Term| recurse.clone()(tx.clone(), el.clone()), v0_.clone())))))},
                  crate::hydra::core::Term_Variant::Literal (v0_) => {
                    let v0_ = v0_.clone() ;
                    crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(v0_.clone())))},
                  crate::hydra::core::Term_Variant::Map (v0_) => {
                    let v0_ = v0_.clone() ;
                    after_recursion.clone()(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(for_map.clone()(v0_.clone())))))},
                  crate::hydra::core::Term_Variant::Maybe (v0_) => {
                    let v0_ = v0_.clone() ;
                    after_recursion.clone()(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(crate::hydra::lib::maybes::map(|v: crate::hydra::core::Term| recurse.clone()(tx.clone(), v.clone()), v0_.clone())))))},
                  crate::hydra::core::Term_Variant::Pair (v0_) => {
                    let v0_ = v0_.clone() ;
                    after_recursion.clone()(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Pair((recurse.clone()(tx.clone(), crate::hydra::lib::pairs::first(v0_.clone())), recurse.clone()(tx.clone(), crate::hydra::lib::pairs::second(v0_.clone())))))))},
                  crate::hydra::core::Term_Variant::Record (v0_) => {
                    let v0_ = v0_.clone() ;
                    after_recursion.clone()(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
                      type_name: v0_.clone().0.type_name.clone(),
                      fields: crate::hydra::lib::lists::map(for_field.clone(), v0_.clone().0.fields.clone())}))))))},
                  crate::hydra::core::Term_Variant::Set (v0_) => {
                    let v0_ = v0_.clone() ;
                    after_recursion.clone()(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Set(crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::map(|el: crate::hydra::core::Term| recurse.clone()(tx.clone(), el.clone()), crate::hydra::lib::sets::to_list(v0_.clone())))))))},
                  crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
                    let v0_ = v0_.clone() ;
                    after_recursion.clone()(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
                      body: recurse.clone()(tx.clone(), v0_.clone().0.body.clone()),
                      type_: v0_.clone().0.type_.clone()}))))))},
                  crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
                    let v0_ = v0_.clone() ;
                    {
                      let tx1 = crate::hydra::schemas::extend_graph_for_type_lambda(tx.clone(), v0_.clone()) ;
                      {
                        let result = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeLambda(crate::hydra::core::TypeLambda(Rc::new(crate::hydra::core::TypeLambda_Variant {
                          parameter: v0_.clone().0.parameter.clone(),
                          body: rewrite_with_args.clone()(Vec::from([]), tx1.clone(), v0_.clone().0.body.clone())}))))) ;
                        after_recursion.clone()(result.clone())}}},
                  crate::hydra::core::Term_Variant::Union (v0_) => {
                    let v0_ = v0_.clone() ;
                    after_recursion.clone()(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
                      type_name: v0_.clone().0.type_name.clone(),
                      field: for_field.clone()(v0_.clone().0.field.clone())}))))))},
                  crate::hydra::core::Term_Variant::Unit (v0_) => {
                    let v0_ = v0_.clone() ;
                    crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))},
                  crate::hydra::core::Term_Variant::Variable (v0_) => {
                    let v0_ = v0_.clone() ;
                    {
                      let arty = term_arity_with_context.clone()(tx.clone(), term.clone()) ;
                      {
                        let var_type = crate::hydra::lib::maybes::map(crate::hydra::rewriting::type_scheme_to_f_type, crate::hydra::lib::maps::lookup(v0_.clone(), tx.clone().0.bound_types.clone())) ;
                        expand.clone()(false, args.clone(), arty.clone(), var_type.clone(), term.clone())}}},
                  crate::hydra::core::Term_Variant::Wrap (v0_) => {
                    let v0_ = v0_.clone() ;
                    after_recursion.clone()(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                      type_name: v0_.clone().0.type_name.clone(),
                      body: recurse.clone()(tx.clone(), v0_.clone().0.body.clone())}))))))}}}}}}}}} ;
  contract_term(rewrite_with_args.clone()(Vec::from([]), tx0.clone(), term0.clone()))}

pub fn eta_expansion_arity(graph: crate::hydra::graph::Graph, term: crate::hydra::core::Term) -> i32 {
  match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      eta_expansion_arity(graph.clone(), v0_.clone().0.body.clone())},
    crate::hydra::core::Term_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::math::sub(eta_expansion_arity(graph.clone(), v0_.clone().0.function.clone()), 1i32)},
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Function_Variant::Elimination (v0_) => {
          let v0_ = v0_.clone() ;
          1i32},
        crate::hydra::core::Function_Variant::Lambda (v0_) => {
          let v0_ = v0_.clone() ;
          0i32},
        crate::hydra::core::Function_Variant::Primitive (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::arity::primitive_arity(crate::hydra::lib::maybes::from_just(crate::hydra::lexical::lookup_primitive(graph.clone(), v0_.clone())))}}},
    crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
      let v0_ = v0_.clone() ;
      eta_expansion_arity(graph.clone(), v0_.clone().0.body.clone())},
    crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
      let v0_ = v0_.clone() ;
      eta_expansion_arity(graph.clone(), v0_.clone().0.body.clone())},
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::maybes::maybe(0i32, |ts: crate::hydra::core::TypeScheme| crate::hydra::arity::type_arity(ts.clone().0.type_.clone()), crate::hydra::lib::maybes::bind(crate::hydra::lexical::lookup_element(graph.clone(), v0_.clone()), |b: crate::hydra::core::Binding| b.clone().0.type_.clone()))},
    _ => 0i32}}

pub fn eta_expand_typed_term(cx: crate::hydra::context::Context, tx0: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Term> {
  let rewrite = |top_level: bool, forced: bool, type_args: Vec<crate::hydra::core::Type>, recurse: Rc<dyn Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Term>>>, tx: crate::hydra::graph::Graph, term: crate::hydra::core::Term| {
    let rewrite_spine = |term2: crate::hydra::core::Term| match &*term2.clone().0 {
      crate::hydra::core::Term_Variant::Annotated (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::lib::eithers::bind(rewrite_spine.clone()(v0_.clone().0.body.clone()), |body: crate::hydra::core::Term| {
          let ann = v0_.clone().0.annotation.clone() ;
          Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
            body: body.clone(),
            annotation: ann.clone()}))))))})},
      crate::hydra::core::Term_Variant::Application (v0_) => {
        let v0_ = v0_.clone() ;
        {
          let l = crate::hydra::lib::logic::if_else(false, Vec::from([
            crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String)))))]), Vec::from([])) ;
          crate::hydra::lib::eithers::bind(rewrite_spine.clone()(v0_.clone().0.function.clone()), |lhs: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(rewrite.clone()(true, false, l.clone(), recurse.clone(), tx.clone(), v0_.clone().0.argument.clone()), |rhs: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
            function: lhs.clone(),
            argument: rhs.clone()}))))))))}},
      crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::lib::eithers::bind(rewrite_spine.clone()(v0_.clone().0.body.clone()), |body: crate::hydra::core::Term| {
          let typ = v0_.clone().0.type_.clone() ;
          Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
            body: body.clone(),
            type_: typ.clone()}))))))})},
      _ => rewrite.clone()(false, false, Vec::from([]), recurse.clone(), tx.clone(), term2.clone())} ;
    {
      let arity_of = |tx2: crate::hydra::graph::Graph, term2: crate::hydra::core::Term| {
        let dflt = crate::hydra::lib::eithers::map(|_tc: (crate::hydra::core::Type, crate::hydra::context::Context)| crate::hydra::arity::type_arity(crate::hydra::lib::pairs::first(_tc.clone())), crate::hydra::checking::type_of(cx.clone(), tx2.clone(), Vec::from([]), term2.clone())) ;
        {
          let for_function = |tx3: crate::hydra::graph::Graph, f: crate::hydra::core::Function| match &*f.clone().0 {
            crate::hydra::core::Function_Variant::Elimination (v0_) => {
              let v0_ = v0_.clone() ;
              Right(1i32)},
            crate::hydra::core::Function_Variant::Lambda (v0_) => {
              let v0_ = v0_.clone() ;
              {
                let txl = crate::hydra::schemas::extend_graph_for_lambda(tx3.clone(), v0_.clone()) ;
                arity_of.clone()(txl.clone(), v0_.clone().0.body.clone())}},
            crate::hydra::core::Function_Variant::Primitive (v0_) => {
              let v0_ = v0_.clone() ;
              crate::hydra::lib::eithers::map(|_ts: crate::hydra::core::TypeScheme| crate::hydra::arity::type_scheme_arity(_ts.clone()), crate::hydra::lexical::require_primitive_type(cx.clone(), tx3.clone(), v0_.clone()))}} ;
          match &*term2.clone().0 {
            crate::hydra::core::Term_Variant::Annotated (v0_) => {
              let v0_ = v0_.clone() ;
              arity_of.clone()(tx2.clone(), v0_.clone().0.body.clone())},
            crate::hydra::core::Term_Variant::Function (v0_) => {
              let v0_ = v0_.clone() ;
              for_function.clone()(tx2.clone(), v0_.clone())},
            crate::hydra::core::Term_Variant::Let (v0_) => {
              let v0_ = v0_.clone() ;
              {
                let txl = crate::hydra::schemas::extend_graph_for_let(|_: crate::hydra::graph::Graph, _2: crate::hydra::core::Binding| None, tx2.clone(), v0_.clone()) ;
                arity_of.clone()(txl.clone(), v0_.clone().0.body.clone())}},
            crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
              let v0_ = v0_.clone() ;
              arity_of.clone()(tx2.clone(), v0_.clone().0.body.clone())},
            crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
              let v0_ = v0_.clone() ;
              {
                let txt = crate::hydra::schemas::extend_graph_for_type_lambda(tx2.clone(), v0_.clone()) ;
                arity_of.clone()(txt.clone(), v0_.clone().0.body.clone())}},
            crate::hydra::core::Term_Variant::Variable (v0_) => {
              let v0_ = v0_.clone() ;
              crate::hydra::lib::maybes::maybe(crate::hydra::lib::eithers::map(|_tc: (crate::hydra::core::Type, crate::hydra::context::Context)| crate::hydra::arity::type_arity(crate::hydra::lib::pairs::first(_tc.clone())), crate::hydra::checking::type_of(cx.clone(), tx2.clone(), Vec::from([]), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(v0_.clone()))))), |t: crate::hydra::core::Type| Right(crate::hydra::arity::type_arity(t.clone())), crate::hydra::lib::maybes::map(crate::hydra::rewriting::type_scheme_to_f_type, crate::hydra::lib::maps::lookup(v0_.clone(), tx2.clone().0.bound_types.clone())))},
            _ => dflt.clone()}}} ;
      {
        let extra_variables = |n: i32| crate::hydra::lib::lists::map(|i: i32| crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat2(String::from("v"), crate::hydra::lib::literals::show_int32(i.clone()))))), crate::hydra::lib::math::range(1i32, n.clone())) ;
        {
          let pad = |vars: Vec<crate::hydra::core::Name>, body: crate::hydra::core::Term| crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(vars.clone()), body.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
            parameter: crate::hydra::lib::lists::head(vars.clone()),
            domain: None,
            body: pad.clone()(crate::hydra::lib::lists::tail(vars.clone()), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
              function: body.clone(),
              argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::lib::lists::head(vars.clone()))))}))))))}))))))))) ;
          {
            let padn = |n: i32, body: crate::hydra::core::Term| pad.clone()(extra_variables.clone()(n.clone()), body.clone()) ;
            {
              let unwind = |term2: crate::hydra::core::Term| crate::hydra::lib::lists::foldl(|e: crate::hydra::core::Term, t: crate::hydra::core::Type| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
                body: e.clone(),
                type_: t.clone()}))))), term2.clone(), type_args.clone()) ;
              {
                let force_expansion = |t: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::checking::type_of(cx.clone(), tx.clone(), Vec::from([]), t.clone()), |typ_cx: (crate::hydra::core::Type, crate::hydra::context::Context)| {
                  let arity = crate::hydra::arity::type_arity(crate::hydra::lib::pairs::first(typ_cx.clone())) ;
                  Right(padn.clone()(arity.clone(), unwind.clone()(t.clone())))}) ;
                {
                  let recurse_or_force = |term2: crate::hydra::core::Term| crate::hydra::lib::logic::if_else(forced.clone(), force_expansion.clone()(term2.clone()), recurse.clone()(tx.clone(), unwind.clone()(term2.clone()))) ;
                  {
                    let for_case = |f: crate::hydra::core::Field| crate::hydra::lib::eithers::bind(rewrite.clone()(false, true, Vec::from([]), recurse.clone(), tx.clone(), f.clone().0.term.clone()), |r: crate::hydra::core::Term| Right(crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                      name: f.clone().0.name.clone(),
                      term: r.clone()})))) ;
                    {
                      let for_case_statement = |cs: crate::hydra::core::CaseStatement| {
                        let tname = cs.clone().0.type_name.clone() ;
                        {
                          let dflt = cs.clone().0.default_.clone() ;
                          {
                            let cases = cs.clone().0.cases.clone() ;
                            crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_maybe(|v1: crate::hydra::core::Term| rewrite.clone()(false, false, Vec::from([]), recurse.clone(), tx.clone(), v1.clone()), dflt.clone()), |rdflt: Option<crate::hydra::core::Term>| crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(for_case.clone(), cases.clone()), |rcases: Vec<crate::hydra::core::Field>| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                              type_name: tname.clone(),
                              default_: rdflt.clone(),
                              cases: rcases.clone()}))))))))))))))}}} ;
                      {
                        let for_elimination = |elm: crate::hydra::core::Elimination| {
                          let check_base = |elm2: crate::hydra::core::Elimination| match &*elm2.clone().0 {
                            crate::hydra::core::Elimination_Variant::Union (v0_) => {
                              let v0_ = v0_.clone() ;
                              for_case_statement.clone()(v0_.clone())},
                            _ => recurse.clone()(tx.clone(), term.clone())} ;
                          crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map(unwind.clone(), check_base.clone()(elm.clone())), |base: crate::hydra::core::Term| Right(crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::or(top_level.clone(), forced.clone()), padn.clone()(1i32, base.clone()), base.clone())))} ;
                        match &*term.clone().0 {
                          crate::hydra::core::Term_Variant::Application (v0_) => {
                            let v0_ = v0_.clone() ;
                            {
                              let lhs = v0_.clone().0.function.clone() ;
                              {
                                let rhs = v0_.clone().0.argument.clone() ;
                                crate::hydra::lib::eithers::bind(rewrite.clone()(true, false, Vec::from([]), recurse.clone(), tx.clone(), rhs.clone()), |rhs2: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(arity_of.clone()(tx.clone(), lhs.clone()), |lhsarity: i32| crate::hydra::lib::eithers::bind(rewrite_spine.clone()(lhs.clone()), |lhs2: crate::hydra::core::Term| {
                                  let a2 = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                    function: lhs2.clone(),
                                    argument: rhs2.clone()}))))) ;
                                  Right(crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::gt(lhsarity.clone(), 1i32), padn.clone()(crate::hydra::lib::math::sub(lhsarity.clone(), 1i32), a2.clone()), a2.clone()))})))}}},
                          crate::hydra::core::Term_Variant::Function (v0_) => {
                            let v0_ = v0_.clone() ;
                            match &*v0_.clone().0 {
                              crate::hydra::core::Function_Variant::Elimination (v0_) => {
                                let v0_ = v0_.clone() ;
                                for_elimination.clone()(v0_.clone())},
                              crate::hydra::core::Function_Variant::Lambda (v0_) => {
                                let v0_ = v0_.clone() ;
                                {
                                  let txl = crate::hydra::schemas::extend_graph_for_lambda(tx.clone(), v0_.clone()) ;
                                  crate::hydra::lib::eithers::map(unwind.clone(), recurse.clone()(txl.clone(), term.clone()))}},
                              _ => recurse_or_force.clone()(term.clone())}},
                          crate::hydra::core::Term_Variant::Let (v0_) => {
                            let v0_ = v0_.clone() ;
                            {
                              let txlt = crate::hydra::schemas::extend_graph_for_let(|_: crate::hydra::graph::Graph, _2: crate::hydra::core::Binding| None, tx.clone(), v0_.clone()) ;
                              recurse.clone()(txlt.clone(), term.clone())}},
                          crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
                            let v0_ = v0_.clone() ;
                            rewrite.clone()(top_level.clone(), forced.clone(), crate::hydra::lib::lists::cons(v0_.clone().0.type_.clone(), type_args.clone()), recurse.clone(), tx.clone(), v0_.clone().0.body.clone())},
                          crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
                            let v0_ = v0_.clone() ;
                            {
                              let txt = crate::hydra::schemas::extend_graph_for_type_lambda(tx.clone(), v0_.clone()) ;
                              recurse.clone()(txt.clone(), term.clone())}},
                          _ => recurse_or_force.clone()(term.clone())}}}}}}}}}}}} ;
  crate::hydra::rewriting::rewrite_term_with_context_m(|v1: Rc<dyn Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Term>>>, v2: crate::hydra::graph::Graph, v3: crate::hydra::core::Term| rewrite.clone()(true, false, Vec::from([]), v1.clone(), v2.clone(), v3.clone()), tx0.clone(), term0.clone())}

pub fn reduce_term(cx: crate::hydra::context::Context, graph: crate::hydra::graph::Graph, eager: bool, term: crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Term> {
  let reduce = |eager2: bool, v1: crate::hydra::core::Term| reduce_term(cx.clone(), graph.clone(), eager2.clone(), v1.clone()) ;
  let do_recurse = |eager2: bool, term2: crate::hydra::core::Term| {
    let is_non_lambda = |f: crate::hydra::core::Function| match &*f.clone().0 {
      crate::hydra::core::Function_Variant::Lambda (v0_) => {
        let v0_ = v0_.clone() ;
        false},
      _ => true} ;
    {
      let is_non_lambda_term = match &*term2.clone().0 {
        crate::hydra::core::Term_Variant::Function (v0_) => {
          let v0_ = v0_.clone() ;
          is_non_lambda.clone()(v0_.clone())},
        crate::hydra::core::Term_Variant::Let (v0_) => {
          let v0_ = v0_.clone() ;
          false},
        _ => true} ;
      crate::hydra::lib::logic::and(eager2.clone(), is_non_lambda_term.clone())}} ;
  let reduce_arg = |eager2: bool, arg: crate::hydra::core::Term| crate::hydra::lib::logic::if_else(eager2.clone(), Right(arg.clone()), reduce.clone()(false, arg.clone())) ;
  let apply_to_arguments = |fun: crate::hydra::core::Term, args: Vec<crate::hydra::core::Term>| crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(args.clone()), fun.clone(), apply_to_arguments.clone()(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
    function: fun.clone(),
    argument: crate::hydra::lib::lists::head(args.clone())}))))), crate::hydra::lib::lists::tail(args.clone()))) ;
  let map_error_to_string = |ic: crate::hydra::context::InContext| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
    object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::show::error::error(ic.clone().0.object.clone()))))))),
    context: ic.clone().0.context.clone()})) ;
  let apply_elimination = |elm: crate::hydra::core::Elimination, reduced_arg: crate::hydra::core::Term| match &*elm.clone().0 {
    crate::hydra::core::Elimination_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(crate::hydra::extract::core::record(cx.clone(), v0_.clone().0.type_name.clone(), graph.clone(), crate::hydra::rewriting::deannotate_term(reduced_arg.clone())), |fields: Vec<crate::hydra::core::Field>| {
        let matching_fields = crate::hydra::lib::lists::filter(|f: crate::hydra::core::Field| crate::hydra::lib::equality::equal(f.clone().0.name.clone(), v0_.clone().0.field.clone()), fields.clone()) ;
        crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(matching_fields.clone()), Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
          object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat(Vec::from([
            String::from("no such field: "),
            v0_.clone().0.field.clone().0.0.clone(),
            String::from(" in "),
            v0_.clone().0.type_name.clone().0.0.clone(),
            String::from(" record")])))))))),
          context: cx.clone()}))), Right(crate::hydra::lib::lists::head(matching_fields.clone()).0.term.clone()))})},
    crate::hydra::core::Elimination_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(crate::hydra::extract::core::injection(cx.clone(), v0_.clone().0.type_name.clone(), graph.clone(), reduced_arg.clone()), |field: crate::hydra::core::Field| {
        let matching_fields = crate::hydra::lib::lists::filter(|f: crate::hydra::core::Field| crate::hydra::lib::equality::equal(f.clone().0.name.clone(), field.clone().0.name.clone()), v0_.clone().0.cases.clone()) ;
        crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(matching_fields.clone()), crate::hydra::lib::maybes::maybe(Left(crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
          object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(crate::hydra::lib::strings::cat(Vec::from([
            String::from("no such field "),
            field.clone().0.name.clone().0.0.clone(),
            String::from(" in "),
            v0_.clone().0.type_name.clone().0.0.clone(),
            String::from(" case statement")])))))))),
          context: cx.clone()}))), |x: crate::hydra::core::Term| Right(x.clone()), v0_.clone().0.default_.clone()), Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: crate::hydra::lib::lists::head(matching_fields.clone()).0.term.clone(),
          argument: field.clone().0.term.clone()})))))))})},
    crate::hydra::core::Elimination_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::extract::core::wrap(cx.clone(), v0_.clone(), graph.clone(), reduced_arg.clone())}} ;
  let apply_if_nullary = |eager2: bool, original: crate::hydra::core::Term, args: Vec<crate::hydra::core::Term>| {
    let stripped = crate::hydra::rewriting::deannotate_term(original.clone()) ;
    {
      let for_elimination = |elm: crate::hydra::core::Elimination, args2: Vec<crate::hydra::core::Term>| {
        let arg = crate::hydra::lib::lists::head(args2.clone()) ;
        {
          let remaining_args = crate::hydra::lib::lists::tail(args2.clone()) ;
          crate::hydra::lib::eithers::bind(reduce_arg.clone()(eager2.clone(), crate::hydra::rewriting::deannotate_term(arg.clone())), |reduced_arg: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bind(apply_elimination.clone()(elm.clone(), reduced_arg.clone()), |v1: crate::hydra::core::Term| reduce.clone()(eager2.clone(), v1.clone())), |reduced_result: crate::hydra::core::Term| apply_if_nullary.clone()(eager2.clone(), reduced_result.clone(), remaining_args.clone())))}} ;
      {
        let for_lambda = |l: crate::hydra::core::Lambda, args2: Vec<crate::hydra::core::Term>| {
          let param = l.clone().0.parameter.clone() ;
          {
            let body = l.clone().0.body.clone() ;
            {
              let arg = crate::hydra::lib::lists::head(args2.clone()) ;
              {
                let remaining_args = crate::hydra::lib::lists::tail(args2.clone()) ;
                crate::hydra::lib::eithers::bind(reduce.clone()(eager2.clone(), crate::hydra::rewriting::deannotate_term(arg.clone())), |reduced_arg: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(reduce.clone()(eager2.clone(), crate::hydra::rewriting::replace_free_term_variable(param.clone(), reduced_arg.clone(), body.clone())), |reduced_result: crate::hydra::core::Term| apply_if_nullary.clone()(eager2.clone(), reduced_result.clone(), remaining_args.clone())))}}}} ;
        {
          let for_primitive = |prim: crate::hydra::graph::Primitive, arity: i32, args2: Vec<crate::hydra::core::Term>| {
            let arg_list = crate::hydra::lib::lists::take(arity.clone(), args2.clone()) ;
            {
              let remaining_args = crate::hydra::lib::lists::drop(arity.clone(), args2.clone()) ;
              crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(|v1: crate::hydra::core::Term| reduce_arg.clone()(eager2.clone(), v1.clone()), arg_list.clone()), |reduced_args: Vec<crate::hydra::core::Term>| {
                let stripped_args = crate::hydra::lib::lists::map(crate::hydra::rewriting::deannotate_term, reduced_args.clone()) ;
                crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(map_error_to_string.clone(), |x: crate::hydra::core::Term| x.clone(), prim.clone().0.implementation.clone()), |prim_result: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(reduce.clone()(eager2.clone(), prim_result.clone()), |reduced_result: crate::hydra::core::Term| apply_if_nullary.clone()(eager2.clone(), reduced_result.clone(), remaining_args.clone())))})}} ;
          match &*stripped.clone().0 {
            crate::hydra::core::Term_Variant::Application (v0_) => {
              let v0_ = v0_.clone() ;
              apply_if_nullary.clone()(eager2.clone(), v0_.clone().0.function.clone(), crate::hydra::lib::lists::cons(v0_.clone().0.argument.clone(), args.clone()))},
            crate::hydra::core::Term_Variant::Function (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Function_Variant::Elimination (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(args.clone()), Right(original.clone()), for_elimination.clone()(v0_.clone(), args.clone()))},
                crate::hydra::core::Function_Variant::Lambda (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(args.clone()), Right(original.clone()), for_lambda.clone()(v0_.clone(), args.clone()))},
                crate::hydra::core::Function_Variant::Primitive (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::lib::eithers::bind(crate::hydra::lexical::require_primitive(cx.clone(), graph.clone(), v0_.clone()), |prim: crate::hydra::graph::Primitive| {
                    let arity = crate::hydra::arity::primitive_arity(prim.clone()) ;
                    crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::gt(arity.clone(), crate::hydra::lib::lists::length(args.clone())), Right(apply_to_arguments.clone()(original.clone(), args.clone())), for_primitive.clone()(prim.clone(), arity.clone(), args.clone()))})}}},
            crate::hydra::core::Term_Variant::Variable (v0_) => {
              let v0_ = v0_.clone() ;
              {
                let m_binding = crate::hydra::lexical::dereference_element(graph.clone(), v0_.clone()) ;
                crate::hydra::lib::maybes::maybe(Right(apply_to_arguments.clone()(original.clone(), args.clone())), |binding: crate::hydra::core::Binding| apply_if_nullary.clone()(eager2.clone(), binding.clone().0.term.clone(), args.clone()), m_binding.clone())}},
            crate::hydra::core::Term_Variant::Let (v0_) => {
              let v0_ = v0_.clone() ;
              {
                let bindings = v0_.clone().0.bindings.clone() ;
                {
                  let body = v0_.clone().0.body.clone() ;
                  {
                    let let_expr = |b: crate::hydra::core::Binding| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
                      bindings: Vec::from([
                        b.clone()]),
                      body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(b.clone().0.name.clone())))}))))) ;
                    {
                      let expand_binding = |b: crate::hydra::core::Binding| crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                        name: b.clone().0.name.clone(),
                        term: crate::hydra::rewriting::replace_free_term_variable(b.clone().0.name.clone(), let_expr.clone()(b.clone()), b.clone().0.term.clone()),
                        type_: b.clone().0.type_.clone()})) ;
                      {
                        let expanded_bindings = crate::hydra::lib::lists::map(expand_binding.clone(), bindings.clone()) ;
                        {
                          let substitute_binding = |term2: crate::hydra::core::Term, b: crate::hydra::core::Binding| crate::hydra::rewriting::replace_free_term_variable(b.clone().0.name.clone(), b.clone().0.term.clone(), term2.clone()) ;
                          {
                            let substitute_all = |bs: Vec<crate::hydra::core::Binding>, term2: crate::hydra::core::Term| crate::hydra::lib::lists::foldl(substitute_binding.clone(), term2.clone(), bs.clone()) ;
                            {
                              let expanded_body = substitute_all.clone()(expanded_bindings.clone(), body.clone()) ;
                              crate::hydra::lib::eithers::bind(reduce.clone()(eager2.clone(), expanded_body.clone()), |reduced_body: crate::hydra::core::Term| apply_if_nullary.clone()(eager2.clone(), reduced_body.clone(), args.clone()))}}}}}}}}},
            _ => Right(apply_to_arguments.clone()(original.clone(), args.clone()))}}}}} ;
  let mapping = |recurse: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::context::InContext, crate::hydra::core::Term>>, mid: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::lib::logic::if_else(do_recurse.clone()(eager.clone(), mid.clone()), recurse.clone()(mid.clone()), Right(mid.clone())), |inner: crate::hydra::core::Term| apply_if_nullary.clone()(eager.clone(), inner.clone(), Vec::from([]))) ;
  crate::hydra::rewriting::rewrite_term_m(mapping.clone(), term.clone())}

pub fn term_is_closed(term: crate::hydra::core::Term) -> bool {
  crate::hydra::lib::sets::null(crate::hydra::rewriting::free_variables_in_term(term.clone()))}

pub fn term_is_value(term: crate::hydra::core::Term) -> bool {
  let for_list = |els: Vec<crate::hydra::core::Term>| crate::hydra::lib::lists::foldl(|b: bool, t: crate::hydra::core::Term| crate::hydra::lib::logic::and(b.clone(), term_is_value(t.clone())), true, els.clone()) ;
  let check_field = |f: crate::hydra::core::Field| term_is_value(f.clone().0.term.clone()) ;
  let check_fields = |fields: Vec<crate::hydra::core::Field>| crate::hydra::lib::lists::foldl(|b: bool, f: crate::hydra::core::Field| crate::hydra::lib::logic::and(b.clone(), check_field.clone()(f.clone())), true, fields.clone()) ;
  let function_is_value = |f: crate::hydra::core::Function| match &*f.clone().0 {
    crate::hydra::core::Function_Variant::Elimination (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Elimination_Variant::Wrap (v0_) => {
          let v0_ = v0_.clone() ;
          true},
        crate::hydra::core::Elimination_Variant::Record (v0_) => {
          let v0_ = v0_.clone() ;
          true},
        crate::hydra::core::Elimination_Variant::Union (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::lib::logic::and(check_fields.clone()(v0_.clone().0.cases.clone()), crate::hydra::lib::maybes::maybe(true, term_is_value, v0_.clone().0.default_.clone()))}}},
    crate::hydra::core::Function_Variant::Lambda (v0_) => {
      let v0_ = v0_.clone() ;
      term_is_value(v0_.clone().0.body.clone())},
    crate::hydra::core::Function_Variant::Primitive (v0_) => {
      let v0_ = v0_.clone() ;
      true}} ;
  match &*crate::hydra::rewriting::deannotate_term(term.clone()).0 {
    crate::hydra::core::Term_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      false},
    crate::hydra::core::Term_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::either(|l: crate::hydra::core::Term| term_is_value(l.clone()), |r: crate::hydra::core::Term| term_is_value(r.clone()), v0_.clone())},
    crate::hydra::core::Term_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      function_is_value.clone()(v0_.clone())},
    crate::hydra::core::Term_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      for_list.clone()(v0_.clone())},
    crate::hydra::core::Term_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::foldl(|b: bool, kv: (crate::hydra::core::Term, crate::hydra::core::Term)| crate::hydra::lib::logic::and(b.clone(), crate::hydra::lib::logic::and(term_is_value(crate::hydra::lib::pairs::first(kv.clone())), term_is_value(crate::hydra::lib::pairs::second(kv.clone())))), true, crate::hydra::lib::maps::to_list(v0_.clone()))},
    crate::hydra::core::Term_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::maybes::maybe(true, term_is_value, v0_.clone())},
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      check_fields.clone()(v0_.clone().0.fields.clone())},
    crate::hydra::core::Term_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      for_list.clone()(crate::hydra::lib::sets::to_list(v0_.clone()))},
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      check_field.clone()(v0_.clone().0.field.clone())},
    crate::hydra::core::Term_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      false},
    _ => false}}
