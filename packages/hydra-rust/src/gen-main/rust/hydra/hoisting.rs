#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::lexical::*;
use crate::hydra::rewriting::*;
use crate::hydra::schemas::*;
use crate::hydra::sorting::*;
use crate::hydra::substitution::*;
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

pub fn augment_bindings_with_new_free_vars(cx: crate::hydra::graph::Graph, bound_vars: BTreeSet<crate::hydra::core::Name>, bindings: Vec<crate::hydra::core::Binding>) -> (Vec<crate::hydra::core::Binding>, crate::hydra::typing::TermSubst) {
  let types = crate::hydra::lib::maps::map(crate::hydra::rewriting::type_scheme_to_f_type, cx.clone().0.bound_types.clone()) ;
  let wrap_after_type_lambdas = |vars: Vec<(crate::hydra::core::Name, Option<crate::hydra::core::Type>)>, term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeLambda(crate::hydra::core::TypeLambda(Rc::new(crate::hydra::core::TypeLambda_Variant {
        parameter: v0_.clone().0.parameter.clone(),
        body: wrap_after_type_lambdas.clone()(vars.clone(), v0_.clone().0.body.clone())})))))},
    _ => crate::hydra::lib::lists::foldl(|t: crate::hydra::core::Term, p: (crate::hydra::core::Name, Option<crate::hydra::core::Type>)| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
      parameter: crate::hydra::lib::pairs::first(p.clone()),
      domain: crate::hydra::lib::pairs::second(p.clone()),
      body: t.clone()})))))))), term.clone(), crate::hydra::lib::lists::reverse(vars.clone()))} ;
  let augment = |b: crate::hydra::core::Binding| {
    let free_vars = crate::hydra::lib::sets::to_list(crate::hydra::lib::sets::intersection(bound_vars.clone(), crate::hydra::rewriting::free_variables_in_term(b.clone().0.term.clone()))) ;
    {
      let var_type_pairs = crate::hydra::lib::lists::map(|v: crate::hydra::core::Name| (v.clone(), crate::hydra::lib::maps::lookup(v.clone(), types.clone())), free_vars.clone()) ;
      {
        let var_types = crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(crate::hydra::lib::pairs::second, var_type_pairs.clone())) ;
        crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::or(crate::hydra::lib::lists::null(free_vars.clone()), crate::hydra::lib::logic::not(crate::hydra::lib::equality::equal(crate::hydra::lib::lists::length(var_types.clone()), crate::hydra::lib::lists::length(var_type_pairs.clone())))), (b.clone(), None), (crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
          name: b.clone().0.name.clone(),
          term: wrap_after_type_lambdas.clone()(var_type_pairs.clone(), b.clone().0.term.clone()),
          type_: crate::hydra::lib::maybes::map(|ts: crate::hydra::core::TypeScheme| crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
            variables: ts.clone().0.variables.clone(),
            type_: crate::hydra::lib::lists::foldl(|acc: crate::hydra::core::Type, t: crate::hydra::core::Type| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
              domain: t.clone(),
              codomain: acc.clone()}))))), ts.clone().0.type_.clone(), crate::hydra::lib::lists::reverse(var_types.clone())),
            constraints: ts.clone().0.constraints.clone()})), b.clone().0.type_.clone())})), Some((b.clone().0.name.clone(), crate::hydra::lib::lists::foldl(|t: crate::hydra::core::Term, v: crate::hydra::core::Name| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: t.clone(),
          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(v.clone())))}))))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(b.clone().0.name.clone()))), free_vars.clone())))))}}} ;
  let results = crate::hydra::lib::lists::map(augment.clone(), bindings.clone()) ;
  (crate::hydra::lib::lists::map(crate::hydra::lib::pairs::first, results.clone()), crate::hydra::typing::TermSubst(Rc::new(crate::hydra::typing::TermSubst_Variant(crate::hydra::lib::maps::from_list(crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(crate::hydra::lib::pairs::second, results.clone())))))))}

pub fn binding_is_polymorphic(binding: crate::hydra::core::Binding) -> bool {
  crate::hydra::lib::maybes::maybe(false, |ts: crate::hydra::core::TypeScheme| crate::hydra::lib::logic::not(crate::hydra::lib::lists::null(ts.clone().0.variables.clone())), binding.clone().0.type_.clone())}

pub fn binding_uses_context_type_vars(cx: crate::hydra::graph::Graph, binding: crate::hydra::core::Binding) -> bool {
  crate::hydra::lib::maybes::maybe(false, |ts: crate::hydra::core::TypeScheme| {
    let free_in_type = crate::hydra::rewriting::free_variables_in_type(ts.clone().0.type_.clone()) ;
    {
      let context_type_vars = cx.clone().0.type_variables.clone() ;
      crate::hydra::lib::logic::not(crate::hydra::lib::sets::null(crate::hydra::lib::sets::intersection(free_in_type.clone(), context_type_vars.clone())))}}, binding.clone().0.type_.clone())}

pub fn count_var_occurrences(name: crate::hydra::core::Name, term: crate::hydra::core::Term) -> i32 {
  let child_count = crate::hydra::lib::lists::foldl(|acc: i32, t: crate::hydra::core::Term| crate::hydra::lib::math::add(acc.clone(), count_var_occurrences(name.clone(), t.clone())), 0i32, crate::hydra::rewriting::subterms(term.clone())) ;
  match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(v0_.clone(), name.clone()), crate::hydra::lib::math::add(1i32, child_count.clone()), child_count.clone())},
    _ => child_count.clone()}}

pub fn hoist_all_let_bindings(let0: crate::hydra::core::Let) -> crate::hydra::core::Let {
  let empty_cx = crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
    bound_terms: crate::hydra::lib::maps::empty,
    bound_types: crate::hydra::lib::maps::empty,
    class_constraints: crate::hydra::lib::maps::empty,
    lambda_variables: crate::hydra::lib::sets::empty,
    metadata: crate::hydra::lib::maps::empty,
    primitives: crate::hydra::lib::maps::empty,
    schema_types: crate::hydra::lib::maps::empty,
    type_variables: crate::hydra::lib::sets::empty})) ;
  hoist_let_bindings_with_predicate(|_: crate::hydra::core::Binding| true, should_hoist_all, empty_cx.clone(), let0.clone())}

pub fn hoist_case_statements(v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term) -> crate::hydra::core::Term {
  hoist_subterms(should_hoist_case_statement, v1.clone(), v2.clone())}

pub fn hoist_case_statements_in_graph(bindings: Vec<crate::hydra::core::Binding>) -> Vec<crate::hydra::core::Binding> {
  let empty_tx = crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
    bound_terms: crate::hydra::lib::maps::empty,
    bound_types: crate::hydra::lib::maps::empty,
    class_constraints: crate::hydra::lib::maps::empty,
    lambda_variables: crate::hydra::lib::sets::empty,
    metadata: crate::hydra::lib::maps::empty,
    primitives: crate::hydra::lib::maps::empty,
    schema_types: crate::hydra::lib::maps::empty,
    type_variables: crate::hydra::lib::sets::empty})) ;
  let term0 = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
    bindings: bindings.clone(),
    body: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit))}))))) ;
  let term1 = hoist_case_statements(empty_tx.clone(), term0.clone()) ;
  crate::hydra::schemas::term_as_bindings(term1.clone())}

pub fn hoist_let_bindings_with_context(is_parent_binding: impl Fn(crate::hydra::core::Binding) -> bool + Clone, cx: crate::hydra::graph::Graph, let0: crate::hydra::core::Let) -> crate::hydra::core::Let {
  hoist_let_bindings_with_predicate(is_parent_binding.clone(), should_hoist_polymorphic, cx.clone(), let0.clone())}

pub fn hoist_let_bindings_with_predicate(is_parent_binding: impl Fn(crate::hydra::core::Binding) -> bool + Clone, should_hoist_binding: impl Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Binding) -> bool> + Clone, cx0: crate::hydra::graph::Graph, let0: crate::hydra::core::Let) -> crate::hydra::core::Let {
  let hoist_one = |prefix: String, cx: crate::hydra::graph::Graph, pair: (Vec<(crate::hydra::core::Binding, crate::hydra::core::Term)>, BTreeSet<crate::hydra::core::Name>), binding_with_captured_vars: (crate::hydra::core::Binding, Vec<crate::hydra::core::Name>)| {
    let binding_and_replacement_pairs = crate::hydra::lib::pairs::first(pair.clone()) ;
    {
      let already_used_names = crate::hydra::lib::pairs::second(pair.clone()) ;
      {
        let b = crate::hydra::lib::pairs::first(binding_with_captured_vars.clone()) ;
        {
          let captured_term_vars = crate::hydra::lib::pairs::second(binding_with_captured_vars.clone()) ;
          {
            let types = crate::hydra::lib::maps::map(crate::hydra::rewriting::type_scheme_to_f_type, cx.clone().0.bound_types.clone()) ;
            {
              let captured_term_var_type_pairs = crate::hydra::lib::lists::map(|v: crate::hydra::core::Name| (v.clone(), crate::hydra::lib::maps::lookup(v.clone(), types.clone())), captured_term_vars.clone()) ;
              {
                let captured_term_var_types = crate::hydra::lib::lists::map(|typ: crate::hydra::core::Type| crate::hydra::rewriting::deannotate_type_parameters(typ.clone()), crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(crate::hydra::lib::pairs::second, captured_term_var_type_pairs.clone()))) ;
                {
                  let free_in_binding_type = crate::hydra::lib::maybes::maybe(crate::hydra::lib::sets::empty, |ts: crate::hydra::core::TypeScheme| crate::hydra::rewriting::free_variables_in_type(ts.clone().0.type_.clone()), b.clone().0.type_.clone()) ;
                  {
                    let free_in_captured_var_types = crate::hydra::lib::sets::unions(crate::hydra::lib::lists::map(|t: crate::hydra::core::Type| crate::hydra::rewriting::free_variables_in_type(t.clone()), captured_term_var_types.clone())) ;
                    {
                      let captured_type_vars = crate::hydra::lib::sets::to_list(crate::hydra::lib::sets::intersection(cx.clone().0.type_variables.clone(), crate::hydra::lib::sets::union_(free_in_binding_type.clone(), free_in_captured_var_types.clone()))) ;
                      {
                        let global_binding_name = crate::hydra::lexical::choose_unique_name(already_used_names.clone(), crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat2(prefix.clone(), b.clone().0.name.clone().0.0.clone()))))) ;
                        {
                          let new_used_names = crate::hydra::lib::sets::insert(global_binding_name.clone(), already_used_names.clone()) ;
                          {
                            let new_type_scheme = crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(crate::hydra::lib::lists::length(captured_term_var_types.clone()), crate::hydra::lib::lists::length(captured_term_var_type_pairs.clone())), crate::hydra::lib::maybes::map(|ts: crate::hydra::core::TypeScheme| crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
                              variables: crate::hydra::lib::lists::nub(crate::hydra::lib::lists::concat2(captured_type_vars.clone(), ts.clone().0.variables.clone())),
                              type_: crate::hydra::lib::lists::foldl(|t: crate::hydra::core::Type, a: crate::hydra::core::Type| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
                                domain: a.clone(),
                                codomain: t.clone()}))))), ts.clone().0.type_.clone(), crate::hydra::lib::lists::reverse(captured_term_var_types.clone())),
                              constraints: ts.clone().0.constraints.clone()})), b.clone().0.type_.clone()), None) ;
                            {
                              let stripped_term = crate::hydra::rewriting::strip_type_lambdas(b.clone().0.term.clone()) ;
                              {
                                let term_with_lambdas = crate::hydra::lib::lists::foldl(|t: crate::hydra::core::Term, p: (crate::hydra::core::Name, Option<crate::hydra::core::Type>)| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                  parameter: crate::hydra::lib::pairs::first(p.clone()),
                                  domain: crate::hydra::lib::maybes::map(|dom: crate::hydra::core::Type| crate::hydra::rewriting::deannotate_type_parameters(dom.clone()), crate::hydra::lib::pairs::second(p.clone())),
                                  body: t.clone()})))))))), stripped_term.clone(), crate::hydra::lib::lists::reverse(captured_term_var_type_pairs.clone())) ;
                                {
                                  let term_with_type_lambdas = crate::hydra::lib::lists::foldl(|t: crate::hydra::core::Term, v: crate::hydra::core::Name| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeLambda(crate::hydra::core::TypeLambda(Rc::new(crate::hydra::core::TypeLambda_Variant {
                                    parameter: v.clone(),
                                    body: t.clone()}))))), term_with_lambdas.clone(), crate::hydra::lib::lists::reverse(crate::hydra::lib::maybes::maybe(Vec::from([]), |v| v.0.variables.clone(), new_type_scheme.clone()))) ;
                                  {
                                    let with_type_apps = crate::hydra::lib::lists::foldl(|t: crate::hydra::core::Term, v: crate::hydra::core::Name| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
                                      body: t.clone(),
                                      type_: crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v.clone())))}))))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(global_binding_name.clone()))), captured_type_vars.clone()) ;
                                    {
                                      let replacement = crate::hydra::lib::lists::foldl(|t: crate::hydra::core::Term, v: crate::hydra::core::Name| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                        function: t.clone(),
                                        argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(v.clone())))}))))), with_type_apps.clone(), captured_term_vars.clone()) ;
                                      {
                                        let new_binding_and_replacement = (crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                                          name: global_binding_name.clone(),
                                          term: term_with_type_lambdas.clone(),
                                          type_: new_type_scheme.clone()})), replacement.clone()) ;
                                        {
                                          let new_pairs = crate::hydra::lib::lists::cons(new_binding_and_replacement.clone(), binding_and_replacement_pairs.clone()) ;
                                          (new_pairs.clone(), new_used_names.clone())}}}}}}}}}}}}}}}}}}}} ;
  let rewrite = |prefix: String, recurse: Rc<dyn Fn((Vec<T0>, T1)) -> Rc<dyn Fn(T2) -> ((Vec<crate::hydra::core::Binding>, BTreeSet<crate::hydra::core::Name>), crate::hydra::core::Term)>>, cx: crate::hydra::graph::Graph, bindings_and_names: (Vec<crate::hydra::core::Binding>, T1), term: T2| {
    let previously_finished_bindings = crate::hydra::lib::pairs::first(bindings_and_names.clone()) ;
    {
      let empty_bindings_and_names = (Vec::from([]), crate::hydra::lib::pairs::second(bindings_and_names.clone())) ;
      {
        let result = recurse.clone()(empty_bindings_and_names.clone(), term.clone()) ;
        {
          let new_bindings_and_names = crate::hydra::lib::pairs::first(result.clone()) ;
          {
            let bindings_so_far = crate::hydra::lib::pairs::first(new_bindings_and_names.clone()) ;
            {
              let already_used_names = crate::hydra::lib::pairs::second(new_bindings_and_names.clone()) ;
              {
                let new_term = crate::hydra::lib::pairs::second(result.clone()) ;
                match &*new_term.clone().0 {
                  crate::hydra::core::Term_Variant::Let (v0_) => {
                    let v0_ = v0_.clone() ;
                    {
                      let body = v0_.clone().0.body.clone() ;
                      {
                        let partition_pair = crate::hydra::lib::lists::partition(|v1: crate::hydra::core::Binding| should_hoist_binding.clone()(cx.clone(), v1.clone()), v0_.clone().0.bindings.clone()) ;
                        {
                          let hoist_us = crate::hydra::lib::pairs::first(partition_pair.clone()) ;
                          {
                            let keep_us = crate::hydra::lib::pairs::second(partition_pair.clone()) ;
                            {
                              let hoisted_binding_names = crate::hydra::lib::lists::map(|v| v.0.name.clone(), hoist_us.clone()) ;
                              {
                                let poly_let_variables = crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::filter(|v: crate::hydra::core::Name| crate::hydra::lib::maybes::maybe(false, crate::hydra::schemas::f_type_is_polymorphic, crate::hydra::lib::maybes::map(crate::hydra::rewriting::type_scheme_to_f_type, crate::hydra::lib::maps::lookup(v.clone(), cx.clone().0.bound_types.clone()))), crate::hydra::lib::sets::to_list(crate::hydra::lib::sets::difference(crate::hydra::lib::sets::from_list(crate::hydra::lib::maps::keys(cx.clone().0.bound_terms.clone())), cx.clone().0.lambda_variables.clone())))) ;
                                {
                                  let bound_term_variables = crate::hydra::lib::sets::union_(cx.clone().0.lambda_variables.clone(), crate::hydra::lib::sets::difference(crate::hydra::lib::sets::from_list(crate::hydra::lib::maps::keys(cx.clone().0.bound_terms.clone())), cx.clone().0.lambda_variables.clone())) ;
                                  {
                                    let free_variables_in_each_binding = crate::hydra::lib::lists::map(|b: crate::hydra::core::Binding| crate::hydra::lib::sets::to_list(crate::hydra::lib::sets::intersection(bound_term_variables.clone(), crate::hydra::rewriting::free_variables_in_term(b.clone().0.term.clone()))), hoist_us.clone()) ;
                                    {
                                      let binding_dependencies = crate::hydra::lib::lists::map(|vars: Vec<crate::hydra::core::Name>| crate::hydra::lib::lists::partition(|v: crate::hydra::core::Name| crate::hydra::lib::sets::member(v.clone(), crate::hydra::lib::sets::from_list(hoisted_binding_names.clone())), vars.clone()), free_variables_in_each_binding.clone()) ;
                                      {
                                        let binding_edges = crate::hydra::lib::lists::zip(hoisted_binding_names.clone(), crate::hydra::lib::lists::map(crate::hydra::lib::pairs::first, binding_dependencies.clone())) ;
                                        {
                                          let binding_immediate_captured_vars = crate::hydra::lib::lists::zip(hoisted_binding_names.clone(), crate::hydra::lib::lists::map(crate::hydra::lib::pairs::second, binding_dependencies.clone())) ;
                                          {
                                            let captured_vars_map = crate::hydra::lib::maps::from_list(crate::hydra::sorting::propagate_tags(binding_edges.clone(), binding_immediate_captured_vars.clone())) ;
                                            {
                                              let bindings_with_captured_vars = crate::hydra::lib::lists::map(|b: crate::hydra::core::Binding| (b.clone(), crate::hydra::lib::maybes::maybe(Vec::from([]), |vars: BTreeSet<crate::hydra::core::Name>| crate::hydra::lib::sets::to_list(crate::hydra::lib::sets::difference(vars.clone(), poly_let_variables.clone())), crate::hydra::lib::maps::lookup(b.clone().0.name.clone(), captured_vars_map.clone()))), hoist_us.clone()) ;
                                              {
                                                let hoist_pairs_and_names = crate::hydra::lib::lists::foldl(|v1: (Vec<(crate::hydra::core::Binding, crate::hydra::core::Term)>, BTreeSet<crate::hydra::core::Name>), v2: (crate::hydra::core::Binding, Vec<crate::hydra::core::Name>)| hoist_one.clone()(prefix.clone(), cx.clone(), v1.clone(), v2.clone()), (Vec::from([]), already_used_names.clone()), bindings_with_captured_vars.clone()) ;
                                                {
                                                  let hoist_pairs = crate::hydra::lib::lists::reverse(crate::hydra::lib::pairs::first(hoist_pairs_and_names.clone())) ;
                                                  {
                                                    let hoisted_bindings = crate::hydra::lib::lists::map(crate::hydra::lib::pairs::first, hoist_pairs.clone()) ;
                                                    {
                                                      let replacements = crate::hydra::lib::lists::map(crate::hydra::lib::pairs::second, hoist_pairs.clone()) ;
                                                      {
                                                        let final_used_names = crate::hydra::lib::pairs::second(hoist_pairs_and_names.clone()) ;
                                                        {
                                                          let hoist_name_replacement_pairs = crate::hydra::lib::lists::zip(crate::hydra::lib::lists::map(|v| v.0.name.clone(), hoist_us.clone()), replacements.clone()) ;
                                                          {
                                                            let hoist_binding_map = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|b: crate::hydra::core::Binding| (b.clone().0.name.clone(), b.clone()), hoist_us.clone())) ;
                                                            {
                                                              let is_cacheable = |name: crate::hydra::core::Name| {
                                                                let multi_ref = crate::hydra::lib::equality::gte(count_var_occurrences(name.clone(), body.clone()), 2i32) ;
                                                                {
                                                                  let is_poly = crate::hydra::lib::maybes::maybe(false, |b: crate::hydra::core::Binding| binding_is_polymorphic(b.clone()), crate::hydra::lib::maps::lookup(name.clone(), hoist_binding_map.clone())) ;
                                                                  crate::hydra::lib::logic::and(multi_ref.clone(), crate::hydra::lib::logic::not(is_poly.clone()))}} ;
                                                              {
                                                                let single_ref_pairs = crate::hydra::lib::lists::filter(|p: (crate::hydra::core::Name, crate::hydra::core::Term)| crate::hydra::lib::logic::not(is_cacheable.clone()(crate::hydra::lib::pairs::first(p.clone()))), hoist_name_replacement_pairs.clone()) ;
                                                                {
                                                                  let multi_ref_pairs = crate::hydra::lib::lists::filter(|p: (crate::hydra::core::Name, crate::hydra::core::Term)| is_cacheable.clone()(crate::hydra::lib::pairs::first(p.clone())), hoist_name_replacement_pairs.clone()) ;
                                                                  {
                                                                    let full_subst = crate::hydra::typing::TermSubst(Rc::new(crate::hydra::typing::TermSubst_Variant(crate::hydra::lib::maps::from_list(hoist_name_replacement_pairs.clone())))) ;
                                                                    {
                                                                      let body_only_subst = crate::hydra::typing::TermSubst(Rc::new(crate::hydra::typing::TermSubst_Variant(crate::hydra::lib::maps::from_list(single_ref_pairs.clone())))) ;
                                                                      {
                                                                        let body_subst = crate::hydra::substitution::substitute_in_term(body_only_subst.clone(), body.clone()) ;
                                                                        {
                                                                          let cache_bindings = crate::hydra::lib::lists::map(|p: (crate::hydra::core::Name, crate::hydra::core::Term)| {
                                                                            let orig_type = crate::hydra::lib::maybes::maybe(None, |b: crate::hydra::core::Binding| b.clone().0.type_.clone(), crate::hydra::lib::maps::lookup(crate::hydra::lib::pairs::first(p.clone()), hoist_binding_map.clone())) ;
                                                                            crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                                                                              name: crate::hydra::lib::pairs::first(p.clone()),
                                                                              term: crate::hydra::lib::pairs::second(p.clone()),
                                                                              type_: orig_type.clone()}))}, multi_ref_pairs.clone()) ;
                                                                          {
                                                                            let body_with_cache = crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(cache_bindings.clone()), body_subst.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
                                                                              bindings: cache_bindings.clone(),
                                                                              body: body_subst.clone()})))))) ;
                                                                            {
                                                                              let keep_us_subst = crate::hydra::lib::lists::map(|v1: crate::hydra::core::Binding| crate::hydra::substitution::substitute_in_binding(full_subst.clone(), v1.clone()), keep_us.clone()) ;
                                                                              {
                                                                                let hoisted_bindings_subst = crate::hydra::lib::lists::map(|v1: crate::hydra::core::Binding| crate::hydra::substitution::substitute_in_binding(full_subst.clone(), v1.clone()), hoisted_bindings.clone()) ;
                                                                                {
                                                                                  let bindings_so_far_subst = crate::hydra::lib::lists::map(|v1: crate::hydra::core::Binding| crate::hydra::substitution::substitute_in_binding(full_subst.clone(), v1.clone()), bindings_so_far.clone()) ;
                                                                                  {
                                                                                    let augment_result = augment_bindings_with_new_free_vars(cx.clone(), crate::hydra::lib::sets::difference(bound_term_variables.clone(), poly_let_variables.clone()), bindings_so_far_subst.clone()) ;
                                                                                    {
                                                                                      let bindings_so_far_augmented = crate::hydra::lib::pairs::first(augment_result.clone()) ;
                                                                                      {
                                                                                        let augment_subst = crate::hydra::lib::pairs::second(augment_result.clone()) ;
                                                                                        {
                                                                                          let hoisted_bindings_final = crate::hydra::lib::lists::map(|v1: crate::hydra::core::Binding| crate::hydra::substitution::substitute_in_binding(augment_subst.clone(), v1.clone()), hoisted_bindings_subst.clone()) ;
                                                                                          {
                                                                                            let bindings_so_far_final = crate::hydra::lib::lists::map(|v1: crate::hydra::core::Binding| crate::hydra::substitution::substitute_in_binding(augment_subst.clone(), v1.clone()), bindings_so_far_augmented.clone()) ;
                                                                                            {
                                                                                              let body_final = crate::hydra::substitution::substitute_in_term(augment_subst.clone(), body_with_cache.clone()) ;
                                                                                              {
                                                                                                let keep_us_final = crate::hydra::lib::lists::map(|v1: crate::hydra::core::Binding| crate::hydra::substitution::substitute_in_binding(augment_subst.clone(), v1.clone()), keep_us_subst.clone()) ;
                                                                                                {
                                                                                                  let final_term = crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(keep_us_final.clone()), body_final.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
                                                                                                    bindings: keep_us_final.clone(),
                                                                                                    body: body_final.clone()})))))) ;
                                                                                                  ((crate::hydra::lib::lists::concat(Vec::from([
                                                                                                    previously_finished_bindings.clone(),
                                                                                                    hoisted_bindings_final.clone(),
                                                                                                    bindings_so_far_final.clone()])), final_used_names.clone()), final_term.clone())}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}},
                  _ => ((crate::hydra::lib::lists::concat2(previously_finished_bindings.clone(), bindings_so_far.clone()), already_used_names.clone()), new_term.clone())}}}}}}}} ;
  let cx1 = crate::hydra::schemas::extend_graph_for_let(|c: crate::hydra::graph::Graph, b: crate::hydra::core::Binding| None, cx0.clone(), let0.clone()) ;
  let for_active_binding = |b: crate::hydra::core::Binding| {
    let prefix = crate::hydra::lib::strings::cat2(b.clone().0.name.clone().0.0.clone(), String::from("_")) ;
    {
      let init = (Vec::from([]), crate::hydra::lib::sets::singleton(b.clone().0.name.clone())) ;
      {
        let result_pair = rewrite_and_fold_term_with_type_context(|v1: Rc<dyn Fn((Vec<crate::hydra::core::Binding>, BTreeSet<crate::hydra::core::Name>)) -> Rc<dyn Fn(crate::hydra::core::Term) -> ((Vec<crate::hydra::core::Binding>, BTreeSet<crate::hydra::core::Name>), crate::hydra::core::Term)>>, v2: crate::hydra::graph::Graph, v3: (Vec<crate::hydra::core::Binding>, BTreeSet<crate::hydra::core::Name>), v4: crate::hydra::core::Term| rewrite.clone()(prefix.clone(), v1.clone(), v2.clone(), v3.clone(), v4.clone()), cx1.clone(), init.clone(), b.clone().0.term.clone()) ;
        {
          let result_bindings = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::first(result_pair.clone())) ;
          {
            let result_term = crate::hydra::lib::pairs::second(result_pair.clone()) ;
            crate::hydra::lib::lists::cons(crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
              name: b.clone().0.name.clone(),
              term: result_term.clone(),
              type_: b.clone().0.type_.clone()})), result_bindings.clone())}}}}} ;
  let for_binding = |b: crate::hydra::core::Binding| crate::hydra::lib::logic::if_else(is_parent_binding.clone()(b.clone()), for_active_binding.clone()(b.clone()), Vec::from([
    b.clone()])) ;
  crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
    bindings: crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(for_binding.clone(), let0.clone().0.bindings.clone())),
    body: let0.clone().0.body.clone()}))}

pub fn hoist_polymorphic_let_bindings(is_parent_binding: impl Fn(crate::hydra::core::Binding) -> bool + Clone, let0: crate::hydra::core::Let) -> crate::hydra::core::Let {
  let empty_cx = crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
    bound_terms: crate::hydra::lib::maps::empty,
    bound_types: crate::hydra::lib::maps::empty,
    class_constraints: crate::hydra::lib::maps::empty,
    lambda_variables: crate::hydra::lib::sets::empty,
    metadata: crate::hydra::lib::maps::empty,
    primitives: crate::hydra::lib::maps::empty,
    schema_types: crate::hydra::lib::maps::empty,
    type_variables: crate::hydra::lib::sets::empty})) ;
  hoist_let_bindings_with_predicate(is_parent_binding.clone(), should_hoist_polymorphic, empty_cx.clone(), let0.clone())}

pub fn hoist_subterms(should_hoist: impl Fn((Vec<crate::hydra::accessors::TermAccessor>, crate::hydra::core::Term)) -> bool + Clone, cx0: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let process_immediate_subterm = |cx: crate::hydra::graph::Graph, counter: i32, name_prefix: String, path_prefix: Vec<crate::hydra::accessors::TermAccessor>, subterm: crate::hydra::core::Term| {
    let baseline_lambda_vars = cx.clone().0.lambda_variables.clone() ;
    {
      let collect_and_replace = |recurse: Rc<dyn Fn((i32, Vec<crate::hydra::core::Binding>)) -> Rc<dyn Fn(crate::hydra::core::Term) -> ((i32, Vec<crate::hydra::core::Binding>), crate::hydra::core::Term)>>, path: Vec<crate::hydra::accessors::TermAccessor>, cx_inner: crate::hydra::graph::Graph, acc: (i32, Vec<crate::hydra::core::Binding>), term: crate::hydra::core::Term| {
        let current_counter = crate::hydra::lib::pairs::first(acc.clone()) ;
        {
          let collected_bindings = crate::hydra::lib::pairs::second(acc.clone()) ;
          match &*term.clone().0 {
            crate::hydra::core::Term_Variant::Let (v0_) => {
              let v0_ = v0_.clone() ;
              (acc.clone(), term.clone())},
            crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
              let v0_ = v0_.clone() ;
              (acc.clone(), term.clone())},
            _ => {
              let result = recurse.clone()(acc.clone(), term.clone()) ;
              {
                let new_acc = crate::hydra::lib::pairs::first(result.clone()) ;
                {
                  let processed_term = crate::hydra::lib::pairs::second(result.clone()) ;
                  {
                    let new_counter = crate::hydra::lib::pairs::first(new_acc.clone()) ;
                    {
                      let new_bindings = crate::hydra::lib::pairs::second(new_acc.clone()) ;
                      {
                        let full_path = crate::hydra::lib::lists::concat2(path_prefix.clone(), path.clone()) ;
                        crate::hydra::lib::logic::if_else(should_hoist.clone()((full_path.clone(), processed_term.clone())), {
                          let binding_name = crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat(Vec::from([
                            String::from("_hoist_"),
                            name_prefix.clone(),
                            String::from("_"),
                            crate::hydra::lib::literals::show_int32(new_counter.clone())]))))) ;
                          {
                            let all_lambda_vars = cx_inner.clone().0.lambda_variables.clone() ;
                            {
                              let new_lambda_vars = crate::hydra::lib::sets::difference(all_lambda_vars.clone(), baseline_lambda_vars.clone()) ;
                              {
                                let free_vars = crate::hydra::rewriting::free_variables_in_term(processed_term.clone()) ;
                                {
                                  let captured_vars = crate::hydra::lib::sets::to_list(crate::hydra::lib::sets::intersection(new_lambda_vars.clone(), free_vars.clone())) ;
                                  {
                                    let type_map = crate::hydra::lib::maps::map(crate::hydra::rewriting::type_scheme_to_f_type, cx_inner.clone().0.bound_types.clone()) ;
                                    {
                                      let wrapped_term = crate::hydra::lib::lists::foldl(|body: crate::hydra::core::Term, var_name: crate::hydra::core::Name| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                                        parameter: var_name.clone(),
                                        domain: crate::hydra::lib::maps::lookup(var_name.clone(), type_map.clone()),
                                        body: body.clone()})))))))), processed_term.clone(), crate::hydra::lib::lists::reverse(captured_vars.clone())) ;
                                      {
                                        let reference = crate::hydra::lib::lists::foldl(|fn_: crate::hydra::core::Term, var_name: crate::hydra::core::Name| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                                          function: fn_.clone(),
                                          argument: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(var_name.clone())))}))))), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(binding_name.clone()))), captured_vars.clone()) ;
                                        {
                                          let new_binding = crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                                            name: binding_name.clone(),
                                            term: wrapped_term.clone(),
                                            type_: None})) ;
                                          ((crate::hydra::lib::math::add(new_counter.clone(), 1i32), crate::hydra::lib::lists::cons(new_binding.clone(), new_bindings.clone())), reference.clone())}}}}}}}}}, (new_acc.clone(), processed_term.clone()))}}}}}}}}} ;
      {
        let result = rewrite_and_fold_term_with_type_context_and_path(collect_and_replace.clone(), cx.clone(), (counter.clone(), Vec::from([])), subterm.clone()) ;
        {
          let final_acc = crate::hydra::lib::pairs::first(result.clone()) ;
          {
            let transformed_subterm = crate::hydra::lib::pairs::second(result.clone()) ;
            {
              let final_counter = crate::hydra::lib::pairs::first(final_acc.clone()) ;
              {
                let bindings = crate::hydra::lib::pairs::second(final_acc.clone()) ;
                crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(bindings.clone()), (final_counter.clone(), transformed_subterm.clone()), {
                  let local_let = crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
                    bindings: crate::hydra::lib::lists::reverse(bindings.clone()),
                    body: transformed_subterm.clone()}))))) ;
                  (final_counter.clone(), local_let.clone())})}}}}}}} ;
  let process_let_term = |cx: crate::hydra::graph::Graph, counter: T0, path: Vec<crate::hydra::accessors::TermAccessor>, lt: crate::hydra::core::Let| {
    let bindings = lt.clone().0.bindings.clone() ;
    {
      let body = lt.clone().0.body.clone() ;
      {
        let process_binding = |acc: Vec<crate::hydra::core::Binding>, binding: crate::hydra::core::Binding| {
          let name_prefix = crate::hydra::lib::strings::intercalate(String::from("_"), crate::hydra::lib::strings::split_on(String::from("."), binding.clone().0.name.clone().0.0.clone())) ;
          {
            let binding_path_prefix = crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
              crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::LetBinding(binding.clone().0.name.clone())))])) ;
            {
              let result = process_immediate_subterm.clone()(cx.clone(), 1i32, name_prefix.clone(), binding_path_prefix.clone(), binding.clone().0.term.clone()) ;
              {
                let new_value = crate::hydra::lib::pairs::second(result.clone()) ;
                {
                  let new_binding = crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                    name: binding.clone().0.name.clone(),
                    term: new_value.clone(),
                    type_: binding.clone().0.type_.clone()})) ;
                  crate::hydra::lib::lists::cons(new_binding.clone(), acc.clone())}}}}} ;
        {
          let new_bindings_reversed = crate::hydra::lib::lists::foldl(process_binding.clone(), Vec::from([]), bindings.clone()) ;
          {
            let new_bindings = crate::hydra::lib::lists::reverse(new_bindings_reversed.clone()) ;
            {
              let body_path_prefix = crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
                crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::LetBody))])) ;
              {
                let body_result = process_immediate_subterm.clone()(cx.clone(), 1i32, String::from("_body"), body_path_prefix.clone(), body.clone()) ;
                {
                  let new_body = crate::hydra::lib::pairs::second(body_result.clone()) ;
                  (counter.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
                    bindings: new_bindings.clone(),
                    body: new_body.clone()}))))))}}}}}}}} ;
  let rewrite = |recurse: Rc<dyn Fn(T0) -> Rc<dyn Fn(crate::hydra::core::Term) -> (T1, crate::hydra::core::Term)>>, path: Vec<crate::hydra::accessors::TermAccessor>, cx: crate::hydra::graph::Graph, counter: T0, term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let recursed = recurse.clone()(counter.clone(), term.clone()) ;
        {
          let new_counter = crate::hydra::lib::pairs::first(recursed.clone()) ;
          {
            let recursed_term = crate::hydra::lib::pairs::second(recursed.clone()) ;
            match &*recursed_term.clone().0 {
              crate::hydra::core::Term_Variant::Let (v0_) => {
                let v0_ = v0_.clone() ;
                process_let_term.clone()(cx.clone(), new_counter.clone(), path.clone(), v0_.clone())},
              _ => (new_counter.clone(), recursed_term.clone())}}}}},
    _ => recurse.clone()(counter.clone(), term.clone())} ;
  crate::hydra::lib::pairs::second(rewrite_and_fold_term_with_type_context_and_path(rewrite.clone(), cx0.clone(), 1i32, term0.clone()))}

pub fn is_application_function(acc: crate::hydra::accessors::TermAccessor) -> bool {
  match &*acc.clone().0 {
    crate::hydra::accessors::TermAccessor_Variant::ApplicationFunction (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    _ => false}}

pub fn is_elimination_union(f: crate::hydra::core::Function) -> bool {
  match &*f.clone().0 {
    crate::hydra::core::Function_Variant::Elimination (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Elimination_Variant::Union (v0_) => {
          let v0_ = v0_.clone() ;
          true},
        _ => false}},
    _ => false}}

pub fn is_lambda_body(acc: crate::hydra::accessors::TermAccessor) -> bool {
  match &*acc.clone().0 {
    crate::hydra::accessors::TermAccessor_Variant::LambdaBody (v0_) => {
      let v0_ = v0_.clone() ;
      true},
    _ => false}}

pub fn is_union_elimination(term: crate::hydra::core::Term) -> bool {
  match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      is_elimination_union(v0_.clone())},
    _ => false}}

pub fn is_union_elimination_application(term: crate::hydra::core::Term) -> bool {
  match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      is_union_elimination(crate::hydra::rewriting::deannotate_and_detype_term(v0_.clone().0.function.clone()))},
    _ => false}}

pub fn normalize_path_for_hoisting(path: Vec<crate::hydra::accessors::TermAccessor>) -> Vec<crate::hydra::accessors::TermAccessor> {
  let go = |remaining: Vec<crate::hydra::accessors::TermAccessor>| crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::or(crate::hydra::lib::lists::null(remaining.clone()), crate::hydra::lib::lists::null(crate::hydra::lib::lists::tail(remaining.clone()))), remaining.clone(), {
    let first = crate::hydra::lib::lists::head(remaining.clone()) ;
    {
      let second = crate::hydra::lib::lists::head(crate::hydra::lib::lists::tail(remaining.clone())) ;
      {
        let rest = crate::hydra::lib::lists::tail(crate::hydra::lib::lists::tail(remaining.clone())) ;
        crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::and(is_application_function(first.clone()), is_lambda_body(second.clone())), crate::hydra::lib::lists::cons(crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::LetBody)), go.clone()(rest.clone())), crate::hydra::lib::lists::cons(first.clone(), go.clone()(crate::hydra::lib::lists::tail(remaining.clone()))))}}}) ;
  go.clone()(path.clone())}

pub fn rewrite_and_fold_term_with_type_context(f: impl Fn(Rc<dyn Fn(T0) -> Rc<dyn Fn(crate::hydra::core::Term) -> (T0, crate::hydra::core::Term)>>) -> Rc<dyn Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(T0) -> Rc<dyn Fn(crate::hydra::core::Term) -> (T0, crate::hydra::core::Term)>>> + Clone, cx0: crate::hydra::graph::Graph, val0: T0, term0: crate::hydra::core::Term) -> (T0, crate::hydra::core::Term) {
  let wrapper = |low_level_recurse: Rc<dyn Fn((T0, crate::hydra::graph::Graph)) -> Rc<dyn Fn(crate::hydra::core::Term) -> ((T0, T1), crate::hydra::core::Term)>>, val_and_cx: (T0, crate::hydra::graph::Graph), term: crate::hydra::core::Term| {
    let val = crate::hydra::lib::pairs::first(val_and_cx.clone()) ;
    {
      let cx = crate::hydra::lib::pairs::second(val_and_cx.clone()) ;
      {
        let cx1 = match &*term.clone().0 {
          crate::hydra::core::Term_Variant::Function (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Function_Variant::Lambda (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::schemas::extend_graph_for_lambda(cx.clone(), v0_.clone())},
              _ => cx.clone()}},
          crate::hydra::core::Term_Variant::Let (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::schemas::extend_graph_for_let(|_: crate::hydra::graph::Graph, _2: crate::hydra::core::Binding| None, cx.clone(), v0_.clone())},
          crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::schemas::extend_graph_for_type_lambda(cx.clone(), v0_.clone())},
          _ => cx.clone()} ;
        {
          let recurse_for_user = |new_val: T0, subterm: crate::hydra::core::Term| {
            let result = low_level_recurse.clone()((new_val.clone(), cx1.clone()), subterm.clone()) ;
            (crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::first(result.clone())), crate::hydra::lib::pairs::second(result.clone()))} ;
          {
            let f_result = f.clone()(recurse_for_user.clone(), cx1.clone(), val.clone(), term.clone()) ;
            ((crate::hydra::lib::pairs::first(f_result.clone()), cx.clone()), crate::hydra::lib::pairs::second(f_result.clone()))}}}}} ;
  let result = crate::hydra::rewriting::rewrite_and_fold_term(wrapper.clone(), (val0.clone(), cx0.clone()), term0.clone()) ;
  (crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::first(result.clone())), crate::hydra::lib::pairs::second(result.clone()))}

pub fn rewrite_and_fold_term_with_type_context_and_path(f: impl Fn(Rc<dyn Fn(T0) -> Rc<dyn Fn(crate::hydra::core::Term) -> (T0, crate::hydra::core::Term)>>) -> Rc<dyn Fn(Vec<crate::hydra::accessors::TermAccessor>) -> Rc<dyn Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(T0) -> Rc<dyn Fn(crate::hydra::core::Term) -> (T0, crate::hydra::core::Term)>>>> + Clone, cx0: crate::hydra::graph::Graph, val0: T0, term0: crate::hydra::core::Term) -> (T0, crate::hydra::core::Term) {
  let wrapper = |recurse: Rc<dyn Fn(Vec<crate::hydra::accessors::TermAccessor>) -> Rc<dyn Fn((crate::hydra::graph::Graph, T0)) -> Rc<dyn Fn(crate::hydra::core::Term) -> ((T1, T0), crate::hydra::core::Term)>>>, path: Vec<crate::hydra::accessors::TermAccessor>, cx_and_val: (crate::hydra::graph::Graph, T0), term: crate::hydra::core::Term| {
    let cx = crate::hydra::lib::pairs::first(cx_and_val.clone()) ;
    {
      let val = crate::hydra::lib::pairs::second(cx_and_val.clone()) ;
      {
        let cx1 = match &*term.clone().0 {
          crate::hydra::core::Term_Variant::Function (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Function_Variant::Lambda (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::schemas::extend_graph_for_lambda(cx.clone(), v0_.clone())},
              _ => cx.clone()}},
          crate::hydra::core::Term_Variant::Let (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::schemas::extend_graph_for_let(|_: crate::hydra::graph::Graph, _2: crate::hydra::core::Binding| None, cx.clone(), v0_.clone())},
          crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::schemas::extend_graph_for_type_lambda(cx.clone(), v0_.clone())},
          _ => cx.clone()} ;
        {
          let recurse_for_user = |val_in: T0, term_in: crate::hydra::core::Term| {
            let result = recurse.clone()(path.clone(), (cx1.clone(), val_in.clone()), term_in.clone()) ;
            (crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::first(result.clone())), crate::hydra::lib::pairs::second(result.clone()))} ;
          {
            let f_result = f.clone()(recurse_for_user.clone(), path.clone(), cx1.clone(), val.clone(), term.clone()) ;
            ((cx.clone(), crate::hydra::lib::pairs::first(f_result.clone())), crate::hydra::lib::pairs::second(f_result.clone()))}}}}} ;
  let result = crate::hydra::rewriting::rewrite_and_fold_term_with_path(wrapper.clone(), (cx0.clone(), val0.clone()), term0.clone()) ;
  (crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::first(result.clone())), crate::hydra::lib::pairs::second(result.clone()))}

pub fn rewrite_term_with_type_context(f: impl Fn(Rc<dyn Fn(crate::hydra::core::Term) -> T0>) -> Rc<dyn Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> T0>> + Clone, cx0: crate::hydra::graph::Graph, term0: crate::hydra::core::Term) -> T0 {
  let f2 = |recurse: Rc<dyn Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(crate::hydra::core::Term) -> T0>>, cx: crate::hydra::graph::Graph, term: crate::hydra::core::Term| {
    let recurse1 = |term2: crate::hydra::core::Term| recurse.clone()(cx.clone(), term2.clone()) ;
    match &*term.clone().0 {
      crate::hydra::core::Term_Variant::Function (v0_) => {
        let v0_ = v0_.clone() ;
        match &*v0_.clone().0 {
          crate::hydra::core::Function_Variant::Lambda (v0_) => {
            let v0_ = v0_.clone() ;
            {
              let cx1 = crate::hydra::schemas::extend_graph_for_lambda(cx.clone(), v0_.clone()) ;
              {
                let recurse2 = |term2: crate::hydra::core::Term| recurse.clone()(cx1.clone(), term2.clone()) ;
                f.clone()(recurse2.clone(), cx1.clone(), term.clone())}}},
          _ => f.clone()(recurse1.clone(), cx.clone(), term.clone())}},
      crate::hydra::core::Term_Variant::Let (v0_) => {
        let v0_ = v0_.clone() ;
        {
          let cx1 = crate::hydra::schemas::extend_graph_for_let(|_: crate::hydra::graph::Graph, _2: crate::hydra::core::Binding| None, cx.clone(), v0_.clone()) ;
          {
            let recurse2 = |term2: crate::hydra::core::Term| recurse.clone()(cx1.clone(), term2.clone()) ;
            f.clone()(recurse2.clone(), cx1.clone(), term.clone())}}},
      crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
        let v0_ = v0_.clone() ;
        {
          let cx1 = crate::hydra::schemas::extend_graph_for_type_lambda(cx.clone(), v0_.clone()) ;
          {
            let recurse2 = |term2: crate::hydra::core::Term| recurse.clone()(cx1.clone(), term2.clone()) ;
            f.clone()(recurse2.clone(), cx1.clone(), term.clone())}}},
      _ => f.clone()(recurse1.clone(), cx.clone(), term.clone())}} ;
  let rewrite = |cx: crate::hydra::graph::Graph, term: crate::hydra::core::Term| f2.clone()(rewrite.clone(), cx.clone(), term.clone()) ;
  rewrite.clone()(cx0.clone(), term0.clone())}

pub fn should_hoist_all(_: T0, _2: T1) -> bool {
  true}

pub fn should_hoist_case_statement(path_and_term: (Vec<crate::hydra::accessors::TermAccessor>, crate::hydra::core::Term)) -> bool {
  let path = crate::hydra::lib::pairs::first(path_and_term.clone()) ;
  let term = crate::hydra::lib::pairs::second(path_and_term.clone()) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::not(crate::hydra::lib::logic::or(is_union_elimination(term.clone()), is_union_elimination_application(term.clone()))), false, {
    let final_state = crate::hydra::lib::lists::foldl(|st: (bool, bool), acc: crate::hydra::accessors::TermAccessor| update_hoist_state(acc.clone(), st.clone()), (true, false), path.clone()) ;
    crate::hydra::lib::logic::not(crate::hydra::lib::pairs::first(final_state.clone()))})}

pub fn should_hoist_polymorphic(cx: crate::hydra::graph::Graph, binding: crate::hydra::core::Binding) -> bool {
  crate::hydra::lib::logic::or(binding_is_polymorphic(binding.clone()), binding_uses_context_type_vars(cx.clone(), binding.clone()))}

pub fn update_hoist_state(accessor: crate::hydra::accessors::TermAccessor, state: (bool, bool)) -> (bool, bool) {
  let at_top = crate::hydra::lib::pairs::first(state.clone()) ;
  let used_app = crate::hydra::lib::pairs::second(state.clone()) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::not(at_top.clone()), (false, used_app.clone()), match &*accessor.clone().0 {
    crate::hydra::accessors::TermAccessor_Variant::AnnotatedBody (v0_) => {
      let v0_ = v0_.clone() ;
      (true, used_app.clone())},
    crate::hydra::accessors::TermAccessor_Variant::LetBody (v0_) => {
      let v0_ = v0_.clone() ;
      (true, used_app.clone())},
    crate::hydra::accessors::TermAccessor_Variant::LetBinding (v0_) => {
      let v0_ = v0_.clone() ;
      (true, used_app.clone())},
    crate::hydra::accessors::TermAccessor_Variant::LambdaBody (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(used_app.clone(), (false, true), (true, false))},
    crate::hydra::accessors::TermAccessor_Variant::UnionCasesBranch (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(used_app.clone(), (false, true), (true, false))},
    crate::hydra::accessors::TermAccessor_Variant::UnionCasesDefault (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(used_app.clone(), (false, true), (true, false))},
    crate::hydra::accessors::TermAccessor_Variant::ApplicationFunction (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(used_app.clone(), (false, true), (true, true))},
    crate::hydra::accessors::TermAccessor_Variant::ApplicationArgument (v0_) => {
      let v0_ = v0_.clone() ;
      (false, used_app.clone())},
    _ => (false, used_app.clone())})}
