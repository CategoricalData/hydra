#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::adapt::*;
use crate::hydra::annotations::*;
use crate::hydra::inference::*;
use crate::hydra::json::decode::*;
use crate::hydra::lexical::*;
use crate::hydra::names::*;
use crate::hydra::rewriting::*;
use crate::hydra::schemas::*;
use crate::hydra::show::core::*;
use crate::hydra::show::error::*;
use crate::hydra::decoding::*;
use crate::hydra::encoding::*;
use crate::hydra::json::encode::*;
use crate::hydra::json::writer::*;
use crate::hydra::decode::core::*;
use crate::hydra::decode::module::*;
use crate::hydra::encode::module::*;
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

pub fn namespace_to_path(ns: crate::hydra::module::Namespace) -> String {
  crate::hydra::lib::strings::intercalate(String::from("/"), crate::hydra::lib::strings::split_on(String::from("."), ns.clone().0.0.clone()))}

pub fn strip_module_type_schemes(m: crate::hydra::module::Module) -> crate::hydra::module::Module {
  let strip_if_term = |b: crate::hydra::core::Binding| crate::hydra::lib::logic::if_else(crate::hydra::annotations::is_native_type(b.clone()), b.clone(), crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
    name: b.clone().0.name.clone(),
    term: b.clone().0.term.clone(),
    type_: None}))) ;
  crate::hydra::module::Module(Rc::new(crate::hydra::module::Module_Variant {
    namespace: m.clone().0.namespace.clone(),
    elements: crate::hydra::lib::lists::map(strip_if_term.clone(), m.clone().0.elements.clone()),
    term_dependencies: m.clone().0.term_dependencies.clone(),
    type_dependencies: m.clone().0.type_dependencies.clone(),
    description: m.clone().0.description.clone()}))}

pub fn transitive_deps(get_deps: impl Fn(crate::hydra::module::Module) -> Vec<crate::hydra::module::Namespace> + Clone, ns_map: BTreeMap<crate::hydra::module::Namespace, crate::hydra::module::Module>, start_mods: Vec<crate::hydra::module::Module>) -> BTreeSet<crate::hydra::module::Namespace> {
  let initial_deps = crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|m: crate::hydra::module::Module| crate::hydra::lib::lists::filter(|dep: crate::hydra::module::Namespace| crate::hydra::lib::logic::not(crate::hydra::lib::equality::equal(dep.clone(), m.clone().0.namespace.clone())), get_deps.clone()(m.clone())), start_mods.clone()))) ;
  let go = |pending: BTreeSet<crate::hydra::module::Namespace>, visited: BTreeSet<crate::hydra::module::Namespace>| crate::hydra::lib::logic::if_else(crate::hydra::lib::sets::null(pending.clone()), visited.clone(), {
    let new_visited = crate::hydra::lib::sets::union_(visited.clone(), pending.clone()) ;
    {
      let next_deps = crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|nsv: crate::hydra::module::Namespace| crate::hydra::lib::maybes::maybe(Vec::from([]), |dep_mod: crate::hydra::module::Module| get_deps.clone()(dep_mod.clone()), crate::hydra::lib::maps::lookup(nsv.clone(), ns_map.clone())), crate::hydra::lib::sets::to_list(pending.clone())))) ;
      {
        let new_pending = crate::hydra::lib::sets::difference(next_deps.clone(), new_visited.clone()) ;
        go.clone()(new_pending.clone(), new_visited.clone())}}}) ;
  go.clone()(initial_deps.clone(), crate::hydra::lib::sets::empty)}

pub fn module_term_deps_transitive(ns_map: BTreeMap<crate::hydra::module::Namespace, crate::hydra::module::Module>, modules: Vec<crate::hydra::module::Module>) -> Vec<crate::hydra::module::Module> {
  let closure = crate::hydra::lib::sets::union_(transitive_deps(|m: crate::hydra::module::Module| m.clone().0.term_dependencies.clone(), ns_map.clone(), modules.clone()), crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::map(|m: crate::hydra::module::Module| m.clone().0.namespace.clone(), modules.clone()))) ;
  crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(|n: crate::hydra::module::Namespace| crate::hydra::lib::maps::lookup(n.clone(), ns_map.clone()), crate::hydra::lib::sets::to_list(closure.clone())))}

pub fn module_type_deps_transitive(ns_map: BTreeMap<crate::hydra::module::Namespace, crate::hydra::module::Module>, modules: Vec<crate::hydra::module::Module>) -> Vec<crate::hydra::module::Module> {
  let term_mods = module_term_deps_transitive(ns_map.clone(), modules.clone()) ;
  let type_namespaces = crate::hydra::lib::sets::to_list(transitive_deps(|m: crate::hydra::module::Module| m.clone().0.type_dependencies.clone(), ns_map.clone(), term_mods.clone())) ;
  crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(|n: crate::hydra::module::Namespace| crate::hydra::lib::maps::lookup(n.clone(), ns_map.clone()), type_namespaces.clone()))}

pub fn modules_to_graph(bs_graph: crate::hydra::graph::Graph, universe_modules: Vec<crate::hydra::module::Module>, modules: Vec<crate::hydra::module::Module>) -> crate::hydra::graph::Graph {
  let universe = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|m: crate::hydra::module::Module| (m.clone().0.namespace.clone(), m.clone()), crate::hydra::lib::lists::concat2(universe_modules.clone(), modules.clone()))) ;
  let schema_modules = module_type_deps_transitive(universe.clone(), modules.clone()) ;
  let data_modules = module_term_deps_transitive(universe.clone(), modules.clone()) ;
  let schema_elements = crate::hydra::lib::lists::filter(|e: crate::hydra::core::Binding| crate::hydra::annotations::is_native_type(e.clone()), crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|m: crate::hydra::module::Module| m.clone().0.elements.clone(), crate::hydra::lib::lists::concat2(schema_modules.clone(), modules.clone())))) ;
  let data_elements = crate::hydra::lib::lists::filter(|e: crate::hydra::core::Binding| crate::hydra::lib::logic::not(crate::hydra::annotations::is_native_type(e.clone())), crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|m: crate::hydra::module::Module| m.clone().0.elements.clone(), data_modules.clone()))) ;
  let schema_graph = crate::hydra::lexical::elements_to_graph(bs_graph.clone(), crate::hydra::lib::maps::empty, schema_elements.clone()) ;
  let schema_types = crate::hydra::lib::eithers::either(|_: crate::hydra::context::InContext| crate::hydra::lib::maps::empty, |_r: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeScheme>| _r.clone(), crate::hydra::schemas::schema_graph_to_typing_environment(crate::hydra::lexical::empty_context, schema_graph.clone())) ;
  crate::hydra::lexical::elements_to_graph(bs_graph.clone(), schema_types.clone(), data_elements.clone())}

pub fn generate_source_files(print_definitions: impl Fn(crate::hydra::module::Module) -> Rc<dyn Fn(Vec<crate::hydra::module::Definition>) -> Rc<dyn Fn(crate::hydra::context::Context) -> Rc<dyn Fn(crate::hydra::graph::Graph) -> Either<crate::hydra::context::InContext, BTreeMap<T0, T1>>>>> + Clone, lang: crate::hydra::coders::Language, do_infer: bool, do_expand: bool, do_hoist_case_statements: bool, do_hoist_polymorphic_let_bindings: bool, bs_graph: crate::hydra::graph::Graph, universe_modules: Vec<crate::hydra::module::Module>, mods_to_generate: Vec<crate::hydra::module::Module>, cx: crate::hydra::context::Context) -> Either<crate::hydra::context::InContext, Vec<(T0, T1)>> {
  let namespace_map = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|m: crate::hydra::module::Module| (m.clone().0.namespace.clone(), m.clone()), crate::hydra::lib::lists::concat2(universe_modules.clone(), mods_to_generate.clone()))) ;
  let constraints = lang.clone().0.constraints.clone() ;
  let is_type_module = |mod_: crate::hydra::module::Module| crate::hydra::lib::logic::not(crate::hydra::lib::lists::null(crate::hydra::lib::lists::filter(|e: crate::hydra::core::Binding| crate::hydra::annotations::is_native_type(e.clone()), mod_.clone().0.elements.clone()))) ;
  let partitioned = crate::hydra::lib::lists::partition(is_type_module.clone(), mods_to_generate.clone()) ;
  let type_modules_to_generate = crate::hydra::lib::pairs::first(partitioned.clone()) ;
  let term_modules_to_generate = crate::hydra::lib::pairs::second(partitioned.clone()) ;
  let schema_mods = module_type_deps_transitive(namespace_map.clone(), mods_to_generate.clone()) ;
  let schema_elements = crate::hydra::lib::lists::filter(|e: crate::hydra::core::Binding| crate::hydra::annotations::is_native_type(e.clone()), crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|m: crate::hydra::module::Module| m.clone().0.elements.clone(), crate::hydra::lib::lists::concat2(schema_mods.clone(), type_modules_to_generate.clone())))) ;
  let data_mods = module_term_deps_transitive(namespace_map.clone(), mods_to_generate.clone()) ;
  let data_elements = crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|m: crate::hydra::module::Module| m.clone().0.elements.clone(), data_mods.clone())) ;
  let schema_graph = crate::hydra::lexical::elements_to_graph(bs_graph.clone(), crate::hydra::lib::maps::empty, schema_elements.clone()) ;
  let schema_types2 = crate::hydra::lib::eithers::either(|_: crate::hydra::context::InContext| crate::hydra::lib::maps::empty, |_r: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeScheme>| _r.clone(), crate::hydra::schemas::schema_graph_to_typing_environment(crate::hydra::lexical::empty_context, schema_graph.clone())) ;
  let data_graph = crate::hydra::lexical::elements_to_graph(bs_graph.clone(), schema_types2.clone(), data_elements.clone()) ;
  crate::hydra::lib::eithers::bind(crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(type_modules_to_generate.clone()), Right(Vec::from([])), {
    let name_lists = crate::hydra::lib::lists::map(|m: crate::hydra::module::Module| crate::hydra::lib::lists::map(|e: crate::hydra::core::Binding| e.clone().0.name.clone(), crate::hydra::lib::lists::filter(|e: crate::hydra::core::Binding| crate::hydra::annotations::is_native_type(e.clone()), m.clone().0.elements.clone())), type_modules_to_generate.clone()) ;
    crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|s: String| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(s.clone())))))),
      context: cx.clone()})), |r: (BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>, Vec<Vec<crate::hydra::module::TypeDefinition>>)| r.clone(), crate::hydra::adapt::schema_graph_to_definitions(constraints.clone(), schema_graph.clone(), name_lists.clone(), cx.clone())), |schema_result: (BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>, Vec<Vec<crate::hydra::module::TypeDefinition>>)| {
      let def_lists = crate::hydra::lib::pairs::second(schema_result.clone()) ;
      {
        let schema_graph_with_types = crate::hydra::graph::Graph(Rc::new(crate::hydra::graph::Graph_Variant {
          bound_terms: schema_graph.clone().0.bound_terms.clone(),
          bound_types: schema_graph.clone().0.bound_types.clone(),
          class_constraints: schema_graph.clone().0.class_constraints.clone(),
          lambda_variables: schema_graph.clone().0.lambda_variables.clone(),
          metadata: schema_graph.clone().0.metadata.clone(),
          primitives: schema_graph.clone().0.primitives.clone(),
          schema_types: schema_types2.clone(),
          type_variables: schema_graph.clone().0.type_variables.clone()})) ;
        crate::hydra::lib::eithers::map(|xs: Vec<Vec<(T0, T1)>>| crate::hydra::lib::lists::concat(xs.clone()), crate::hydra::lib::eithers::map_list(|p: (crate::hydra::module::Module, Vec<crate::hydra::module::TypeDefinition>)| {
          let mod_ = crate::hydra::lib::pairs::first(p.clone()) ;
          {
            let defs = crate::hydra::lib::pairs::second(p.clone()) ;
            crate::hydra::lib::eithers::map(|m: BTreeMap<T0, T1>| crate::hydra::lib::maps::to_list(m.clone()), print_definitions.clone()(mod_.clone(), crate::hydra::lib::lists::map(|d: crate::hydra::module::TypeDefinition| crate::hydra::module::Definition(Rc::new(crate::hydra::module::Definition_Variant::Type(d.clone()))), defs.clone()), cx.clone(), schema_graph_with_types.clone()))}}, crate::hydra::lib::lists::zip(type_modules_to_generate.clone(), def_lists.clone())))}})}), |schema_files: Vec<(T0, T1)>| crate::hydra::lib::eithers::bind(crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(term_modules_to_generate.clone()), Right(Vec::from([])), {
    let namespaces = crate::hydra::lib::lists::map(|m: crate::hydra::module::Module| m.clone().0.namespace.clone(), term_modules_to_generate.clone()) ;
    crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|s: String| crate::hydra::context::InContext(Rc::new(crate::hydra::context::InContext_Variant {
      object: crate::hydra::error::Error(Rc::new(crate::hydra::error::Error_Variant::Other(crate::hydra::error::OtherError(Rc::new(crate::hydra::error::OtherError_Variant(s.clone())))))),
      context: cx.clone()})), |r: (crate::hydra::graph::Graph, Vec<Vec<crate::hydra::module::TermDefinition>>)| r.clone(), crate::hydra::adapt::data_graph_to_definitions(constraints.clone(), do_infer.clone(), do_expand.clone(), do_hoist_case_statements.clone(), do_hoist_polymorphic_let_bindings.clone(), data_elements.clone(), data_graph.clone(), namespaces.clone(), cx.clone())), |data_result: (crate::hydra::graph::Graph, Vec<Vec<crate::hydra::module::TermDefinition>>)| {
      let g1 = crate::hydra::lib::pairs::first(data_result.clone()) ;
      {
        let def_lists = crate::hydra::lib::pairs::second(data_result.clone()) ;
        {
          let refresh_module = |els: Vec<crate::hydra::core::Binding>, m: crate::hydra::module::Module| crate::hydra::module::Module(Rc::new(crate::hydra::module::Module_Variant {
            namespace: m.clone().0.namespace.clone(),
            elements: crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(|e: crate::hydra::core::Binding| crate::hydra::lib::lists::find(|b: crate::hydra::core::Binding| crate::hydra::lib::equality::equal(b.clone().0.name.clone(), e.clone().0.name.clone()), els.clone()), m.clone().0.elements.clone())),
            term_dependencies: m.clone().0.term_dependencies.clone(),
            type_dependencies: m.clone().0.type_dependencies.clone(),
            description: m.clone().0.description.clone()})) ;
          {
            let refreshed_mods = crate::hydra::lib::lists::map(|m: crate::hydra::module::Module| refresh_module.clone()(crate::hydra::lexical::graph_to_bindings(g1.clone()), m.clone()), term_modules_to_generate.clone()) ;
            crate::hydra::lib::eithers::map(|xs: Vec<Vec<(T0, T1)>>| crate::hydra::lib::lists::concat(xs.clone()), crate::hydra::lib::eithers::map_list(|p: (crate::hydra::module::Module, Vec<crate::hydra::module::TermDefinition>)| {
              let mod_ = crate::hydra::lib::pairs::first(p.clone()) ;
              {
                let defs = crate::hydra::lib::pairs::second(p.clone()) ;
                crate::hydra::lib::eithers::map(|m: BTreeMap<T0, T1>| crate::hydra::lib::maps::to_list(m.clone()), print_definitions.clone()(mod_.clone(), crate::hydra::lib::lists::map(|d: crate::hydra::module::TermDefinition| crate::hydra::module::Definition(Rc::new(crate::hydra::module::Definition_Variant::Term(d.clone()))), defs.clone()), cx.clone(), g1.clone()))}}, crate::hydra::lib::lists::zip(refreshed_mods.clone(), def_lists.clone())))}}}})}), |term_files: Vec<(T0, T1)>| Right(crate::hydra::lib::lists::concat2(schema_files.clone(), term_files.clone()))))}

pub fn format_term_binding(binding: crate::hydra::core::Binding) -> String {
  let name = binding.clone().0.name.clone().0.0.clone() ;
  let type_str = crate::hydra::lib::maybes::maybe(String::from("?"), |scheme: crate::hydra::core::TypeScheme| crate::hydra::show::core::type_scheme(scheme.clone()), binding.clone().0.type_.clone()) ;
  crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("  "), name.clone()), String::from(" : ")), type_str.clone())}

pub fn format_primitive(prim: crate::hydra::graph::Primitive) -> String {
  let name = prim.clone().0.name.clone().0.0.clone() ;
  let type_str = crate::hydra::show::core::type_scheme(prim.clone().0.type_.clone()) ;
  crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("  "), name.clone()), String::from(" : ")), type_str.clone())}

pub fn format_type_binding(graph: crate::hydra::graph::Graph, binding: crate::hydra::core::Binding) -> Either<crate::hydra::error::DecodingError, String> {
  crate::hydra::lib::eithers::bind(crate::hydra::decode::core::type_(graph.clone(), binding.clone().0.term.clone()), |typ: crate::hydra::core::Type| Right(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("  "), binding.clone().0.name.clone().0.0.clone()), String::from(" = ")), crate::hydra::show::core::type_(typ.clone()))))}

pub fn build_schema_map(g: crate::hydra::graph::Graph) -> BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type> {
  crate::hydra::lib::maps::map(|ts: crate::hydra::core::TypeScheme| crate::hydra::rewriting::deannotate_type(ts.clone().0.type_.clone()), g.clone().0.schema_types.clone())}

pub fn module_to_source_module(m: crate::hydra::module::Module) -> crate::hydra::module::Module {
  let source_ns = crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(crate::hydra::lib::strings::cat2(String::from("hydra.sources."), crate::hydra::lib::strings::intercalate(String::from("."), crate::hydra::lib::lists::drop(1i32, crate::hydra::lib::strings::split_on(String::from("."), m.clone().0.namespace.clone().0.0.clone()))))))) ;
  let mod_type_ns = crate::hydra::module::Namespace(Rc::new(crate::hydra::module::Namespace_Variant(String::from("hydra.module")))) ;
  let module_binding = crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
    name: crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat2(source_ns.clone().0.0.clone(), String::from(".module_"))))),
    term: crate::hydra::encode::module::module(m.clone()),
    type_: None})) ;
  crate::hydra::module::Module(Rc::new(crate::hydra::module::Module_Variant {
    namespace: source_ns.clone(),
    elements: Vec::from([
      module_binding.clone()]),
    term_dependencies: Vec::from([
      mod_type_ns.clone()]),
    type_dependencies: Vec::from([
      mod_type_ns.clone()]),
    description: Some(crate::hydra::lib::strings::cat2(String::from("Source module for "), m.clone().0.namespace.clone().0.0.clone()))}))}

pub fn generate_lexicon(graph: crate::hydra::graph::Graph) -> Either<crate::hydra::error::DecodingError, String> {
  let bindings = crate::hydra::lexical::graph_to_bindings(graph.clone()) ;
  let primitives = crate::hydra::lib::maps::elems(graph.clone().0.primitives.clone()) ;
  let partitioned = crate::hydra::lib::lists::partition(|b: crate::hydra::core::Binding| crate::hydra::annotations::is_native_type(b.clone()), bindings.clone()) ;
  let type_bindings = crate::hydra::lib::pairs::first(partitioned.clone()) ;
  let term_bindings = crate::hydra::lib::pairs::second(partitioned.clone()) ;
  let sorted_primitives = crate::hydra::lib::lists::sort_on(|p: crate::hydra::graph::Primitive| p.clone().0.name.clone(), primitives.clone()) ;
  let sorted_types = crate::hydra::lib::lists::sort_on(|b: crate::hydra::core::Binding| b.clone().0.name.clone(), type_bindings.clone()) ;
  let sorted_terms = crate::hydra::lib::lists::sort_on(|b: crate::hydra::core::Binding| b.clone().0.name.clone(), term_bindings.clone()) ;
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(|b: crate::hydra::core::Binding| format_type_binding(graph.clone(), b.clone()), sorted_types.clone()), |type_lines: Vec<String>| {
    let term_lines = crate::hydra::lib::lists::map(|b: crate::hydra::core::Binding| format_term_binding(b.clone()), sorted_terms.clone()) ;
    {
      let primitive_lines = crate::hydra::lib::lists::map(|p: crate::hydra::graph::Primitive| format_primitive(p.clone()), sorted_primitives.clone()) ;
      Right(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("Primitives:\n"), crate::hydra::lib::strings::unlines(primitive_lines.clone())), String::from("\nTypes:\n")), crate::hydra::lib::strings::unlines(type_lines.clone())), String::from("\nTerms:\n")), crate::hydra::lib::strings::unlines(term_lines.clone())))}})}

pub fn module_to_json(m: crate::hydra::module::Module) -> Either<String, String> {
  let term = crate::hydra::encode::module::module(m.clone()) ;
  crate::hydra::lib::eithers::map(|json: crate::hydra::json::model::Value| crate::hydra::json::writer::print_json(json.clone()), crate::hydra::json::encode::to_json(term.clone()))}

pub fn infer_modules(cx: crate::hydra::context::Context, bs_graph: crate::hydra::graph::Graph, universe_mods: Vec<crate::hydra::module::Module>, target_mods: Vec<crate::hydra::module::Module>) -> Either<crate::hydra::context::InContext, Vec<crate::hydra::module::Module>> {
  let g0 = modules_to_graph(bs_graph.clone(), universe_mods.clone(), universe_mods.clone()) ;
  let data_elements = crate::hydra::lib::lists::filter(|e: crate::hydra::core::Binding| crate::hydra::lib::logic::not(crate::hydra::annotations::is_native_type(e.clone())), crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|m: crate::hydra::module::Module| m.clone().0.elements.clone(), universe_mods.clone()))) ;
  crate::hydra::lib::eithers::bind(crate::hydra::inference::infer_graph_types(cx.clone(), data_elements.clone(), g0.clone()), |infer_result_with_cx: ((crate::hydra::graph::Graph, Vec<crate::hydra::core::Binding>), crate::hydra::context::Context)| {
    let infer_result = crate::hydra::lib::pairs::first(infer_result_with_cx.clone()) ;
    {
      let g1 = crate::hydra::lib::pairs::first(infer_result.clone()) ;
      {
        let inferred_elements = crate::hydra::lib::pairs::second(infer_result.clone()) ;
        {
          let is_type_module = |mod_: crate::hydra::module::Module| crate::hydra::lib::lists::null(crate::hydra::lib::lists::filter(|e: crate::hydra::core::Binding| crate::hydra::lib::logic::not(crate::hydra::annotations::is_native_type(e.clone())), mod_.clone().0.elements.clone())) ;
          {
            let refresh_module = |m: crate::hydra::module::Module| crate::hydra::lib::logic::if_else(is_type_module.clone()(m.clone()), m.clone(), crate::hydra::module::Module(Rc::new(crate::hydra::module::Module_Variant {
              namespace: m.clone().0.namespace.clone(),
              elements: crate::hydra::lib::maybes::cat(crate::hydra::lib::lists::map(|e: crate::hydra::core::Binding| crate::hydra::lib::lists::find(|b: crate::hydra::core::Binding| crate::hydra::lib::equality::equal(b.clone().0.name.clone(), e.clone().0.name.clone()), inferred_elements.clone()), m.clone().0.elements.clone())),
              term_dependencies: m.clone().0.term_dependencies.clone(),
              type_dependencies: m.clone().0.type_dependencies.clone(),
              description: m.clone().0.description.clone()}))) ;
            Right(crate::hydra::lib::lists::map(refresh_module.clone(), target_mods.clone()))}}}}})}

pub fn generate_coder_modules(codec: impl Fn(T0) -> Rc<dyn Fn(crate::hydra::graph::Graph) -> Rc<dyn Fn(T1) -> Either<T2, Option<T3>>>> + Clone, bs_graph: crate::hydra::graph::Graph, universe_modules: Vec<crate::hydra::module::Module>, type_modules: Vec<T1>, cx: T0) -> Either<T2, Vec<T3>> {
  let universe = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|m: crate::hydra::module::Module| (m.clone().0.namespace.clone(), m.clone()), crate::hydra::lib::lists::concat2(universe_modules.clone(), universe_modules.clone()))) ;
  let schema_modules = module_type_deps_transitive(universe.clone(), universe_modules.clone()) ;
  let data_modules = module_term_deps_transitive(universe.clone(), universe_modules.clone()) ;
  let schema_elements = crate::hydra::lib::lists::filter(|e: crate::hydra::core::Binding| crate::hydra::annotations::is_native_type(e.clone()), crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|m: crate::hydra::module::Module| m.clone().0.elements.clone(), crate::hydra::lib::lists::concat2(schema_modules.clone(), universe_modules.clone())))) ;
  let data_elements = crate::hydra::lib::lists::filter(|e: crate::hydra::core::Binding| crate::hydra::lib::logic::not(crate::hydra::annotations::is_native_type(e.clone())), crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|m: crate::hydra::module::Module| m.clone().0.elements.clone(), data_modules.clone()))) ;
  let schema_graph = crate::hydra::lexical::elements_to_graph(bs_graph.clone(), crate::hydra::lib::maps::empty, schema_elements.clone()) ;
  let schema_types = crate::hydra::lib::eithers::either(|_: crate::hydra::context::InContext| crate::hydra::lib::maps::empty, |_r: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeScheme>| _r.clone(), crate::hydra::schemas::schema_graph_to_typing_environment(crate::hydra::lexical::empty_context, schema_graph.clone())) ;
  let all_elements = crate::hydra::lib::lists::concat2(schema_elements.clone(), data_elements.clone()) ;
  let graph = crate::hydra::lexical::elements_to_graph(bs_graph.clone(), schema_types.clone(), all_elements.clone()) ;
  crate::hydra::lib::eithers::map(|results: Vec<Option<T3>>| crate::hydra::lib::maybes::cat(results.clone()), crate::hydra::lib::eithers::map_list(|m: T1| codec.clone()(cx.clone(), graph.clone(), m.clone()), type_modules.clone()))}

pub fn infer_and_generate_lexicon(cx: crate::hydra::context::Context, bs_graph: crate::hydra::graph::Graph, kernel_modules: Vec<crate::hydra::module::Module>) -> Either<String, String> {
  let g0 = modules_to_graph(bs_graph.clone(), kernel_modules.clone(), kernel_modules.clone()) ;
  let data_elements = crate::hydra::lib::lists::filter(|e: crate::hydra::core::Binding| crate::hydra::lib::logic::not(crate::hydra::annotations::is_native_type(e.clone())), crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|m: crate::hydra::module::Module| m.clone().0.elements.clone(), kernel_modules.clone()))) ;
  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::bimap(|ic: crate::hydra::context::InContext| crate::hydra::show::error::error(ic.clone().0.object.clone()), |x: ((crate::hydra::graph::Graph, Vec<crate::hydra::core::Binding>), crate::hydra::context::Context)| x.clone(), crate::hydra::inference::infer_graph_types(cx.clone(), data_elements.clone(), g0.clone())), |infer_result_with_cx: ((crate::hydra::graph::Graph, Vec<crate::hydra::core::Binding>), crate::hydra::context::Context)| {
    let g1 = crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::first(infer_result_with_cx.clone())) ;
    crate::hydra::lib::eithers::bimap(|v| v.0.0.clone(), |x: String| x.clone(), generate_lexicon(g1.clone()))})}

pub fn escape_control_chars_in_json(input: Vec<i32>) -> Vec<i32> {
  let hex_digit = |n: i32| crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::lt(n.clone(), 10i32), crate::hydra::lib::math::add(48i32, n.clone()), crate::hydra::lib::math::add(97i32, crate::hydra::lib::math::sub(n.clone(), 10i32))) ;
  let escape_to_unicode = |b: i32| Vec::from([
    92i32,
    117i32,
    48i32,
    48i32,
    hex_digit.clone()(crate::hydra::lib::math::div(b.clone(), 16i32)),
    hex_digit.clone()(crate::hydra::lib::math::mod_(b.clone(), 16i32))]) ;
  let go = |in_str: bool, esc: bool, bytes: Vec<i32>| crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(bytes.clone()), Vec::from([]), {
    let b = crate::hydra::lib::lists::head(bytes.clone()) ;
    {
      let bs = crate::hydra::lib::lists::tail(bytes.clone()) ;
      crate::hydra::lib::logic::if_else(esc.clone(), crate::hydra::lib::lists::cons(b.clone(), go.clone()(in_str.clone(), false, bs.clone())), crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::and(crate::hydra::lib::equality::equal(b.clone(), 92i32), in_str.clone()), crate::hydra::lib::lists::cons(b.clone(), go.clone()(in_str.clone(), true, bs.clone())), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(b.clone(), 34i32), crate::hydra::lib::lists::cons(b.clone(), go.clone()(crate::hydra::lib::logic::not(in_str.clone()), false, bs.clone())), crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::and(in_str.clone(), crate::hydra::lib::equality::lt(b.clone(), 32i32)), crate::hydra::lib::lists::concat2(escape_to_unicode.clone()(b.clone()), go.clone()(in_str.clone(), false, bs.clone())), crate::hydra::lib::lists::cons(b.clone(), go.clone()(in_str.clone(), false, bs.clone()))))))}}) ;
  go.clone()(false, false, input.clone())}

pub fn decode_module_from_json(bs_graph: crate::hydra::graph::Graph, universe_modules: Vec<crate::hydra::module::Module>, do_strip_type_schemes: bool, json_val: crate::hydra::json::model::Value) -> Either<String, crate::hydra::module::Module> {
  let graph = modules_to_graph(bs_graph.clone(), universe_modules.clone(), universe_modules.clone()) ;
  let schema_map = build_schema_map(graph.clone()) ;
  let mod_type = crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.Module"))))))) ;
  crate::hydra::lib::eithers::either(|err: String| Left(err.clone()), |term: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|dec_err: crate::hydra::error::DecodingError| Left(dec_err.clone().0.0.clone()), |mod_: crate::hydra::module::Module| Right(crate::hydra::lib::logic::if_else(do_strip_type_schemes.clone(), strip_module_type_schemes(mod_.clone()), mod_.clone())), crate::hydra::decode::module::module(graph.clone(), term.clone())), crate::hydra::json::decode::from_json(schema_map.clone(), crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("hydra.module.Module")))), mod_type.clone(), json_val.clone()))}
