#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::names::*;
use crate::hydra::sorting::*;
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

pub fn apply_inside_type_lambdas_and_annotations(f: impl Fn(crate::hydra::core::Term) -> crate::hydra::core::Term + Clone, term0: crate::hydra::core::Term) -> crate::hydra::core::Term {
  match &*term0.clone().0 {
    crate::hydra::core::Term_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
        body: apply_inside_type_lambdas_and_annotations(f.clone(), v0_.clone().0.body.clone()),
        annotation: v0_.clone().0.annotation.clone()})))))},
    crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeLambda(crate::hydra::core::TypeLambda(Rc::new(crate::hydra::core::TypeLambda_Variant {
        parameter: v0_.clone().0.parameter.clone(),
        body: apply_inside_type_lambdas_and_annotations(f.clone(), v0_.clone().0.body.clone())})))))},
    _ => f.clone()(term0.clone())}}

pub fn deannotate_and_detype_term(t: crate::hydra::core::Term) -> crate::hydra::core::Term {
  match &*t.clone().0 {
    crate::hydra::core::Term_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      deannotate_and_detype_term(v0_.clone().0.body.clone())},
    crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
      let v0_ = v0_.clone() ;
      deannotate_and_detype_term(v0_.clone().0.body.clone())},
    crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
      let v0_ = v0_.clone() ;
      deannotate_and_detype_term(v0_.clone().0.body.clone())},
    _ => t.clone()}}

pub fn deannotate_term(t: crate::hydra::core::Term) -> crate::hydra::core::Term {
  match &*t.clone().0 {
    crate::hydra::core::Term_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      deannotate_term(v0_.clone().0.body.clone())},
    _ => t.clone()}}

pub fn deannotate_type(t: crate::hydra::core::Type) -> crate::hydra::core::Type {
  match &*t.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      deannotate_type(v0_.clone().0.body.clone())},
    _ => t.clone()}}

pub fn deannotate_type_parameters(t: crate::hydra::core::Type) -> crate::hydra::core::Type {
  match &*deannotate_type(t.clone()).0 {
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      deannotate_type_parameters(v0_.clone().0.body.clone())},
    _ => t.clone()}}

pub fn deannotate_type_recursive(typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  let strip = |recurse: Rc<dyn Fn(T0) -> crate::hydra::core::Type>, typ2: T0| {
    let rewritten = recurse.clone()(typ2.clone()) ;
    match &*rewritten.clone().0 {
      crate::hydra::core::Type_Variant::Annotated (v0_) => {
        let v0_ = v0_.clone() ;
        v0_.clone().0.body.clone()},
      _ => rewritten.clone()}} ;
  rewrite_type(strip.clone(), typ.clone())}

pub fn deannotate_type_scheme_recursive(ts: crate::hydra::core::TypeScheme) -> crate::hydra::core::TypeScheme {
  let vars = ts.clone().0.variables.clone() ;
  let typ = ts.clone().0.type_.clone() ;
  let constraints = ts.clone().0.constraints.clone() ;
  crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
    variables: vars.clone(),
    type_: deannotate_type_recursive(typ.clone()),
    constraints: constraints.clone()}))}

pub fn detype_term(t: crate::hydra::core::Term) -> crate::hydra::core::Term {
  match &*t.clone().0 {
    crate::hydra::core::Term_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let subj = v0_.clone().0.body.clone() ;
        {
          let ann = v0_.clone().0.annotation.clone() ;
          crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
            body: detype_term(subj.clone()),
            annotation: ann.clone()})))))}}},
    crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
      let v0_ = v0_.clone() ;
      deannotate_and_detype_term(v0_.clone().0.body.clone())},
    crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
      let v0_ = v0_.clone() ;
      deannotate_and_detype_term(v0_.clone().0.body.clone())},
    _ => t.clone()}}

pub fn flatten_let_terms(term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let rewrite_binding = |binding: crate::hydra::core::Binding| {
    let key0 = binding.clone().0.name.clone() ;
    {
      let val0 = binding.clone().0.term.clone() ;
      {
        let t = binding.clone().0.type_.clone() ;
        match &*val0.clone().0 {
          crate::hydra::core::Term_Variant::Annotated (v0_) => {
            let v0_ = v0_.clone() ;
            {
              let val1 = v0_.clone().0.body.clone() ;
              {
                let ann = v0_.clone().0.annotation.clone() ;
                {
                  let recursive = rewrite_binding.clone()(crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                    name: key0.clone(),
                    term: val1.clone(),
                    type_: t.clone()}))) ;
                  {
                    let inner_binding = crate::hydra::lib::pairs::first(recursive.clone()) ;
                    {
                      let deps = crate::hydra::lib::pairs::second(recursive.clone()) ;
                      {
                        let val2 = inner_binding.clone().0.term.clone() ;
                        (crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                          name: key0.clone(),
                          term: crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
                            body: val2.clone(),
                            annotation: ann.clone()}))))),
                          type_: t.clone()})), deps.clone())}}}}}}},
          crate::hydra::core::Term_Variant::Let (v0_) => {
            let v0_ = v0_.clone() ;
            {
              let bindings1 = v0_.clone().0.bindings.clone() ;
              {
                let body1 = v0_.clone().0.body.clone() ;
                {
                  let prefix = crate::hydra::lib::strings::cat2(key0.clone().0.0.clone(), String::from("_")) ;
                  {
                    let qualify = |n: crate::hydra::core::Name| crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat2(prefix.clone(), n.clone().0.0.clone())))) ;
                    {
                      let to_subst_pair = |b: crate::hydra::core::Binding| (b.clone().0.name.clone(), qualify.clone()(b.clone().0.name.clone())) ;
                      {
                        let subst = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(to_subst_pair.clone(), bindings1.clone())) ;
                        {
                          let replace_vars = |v1: crate::hydra::core::Term| substitute_variables(subst.clone(), v1.clone()) ;
                          {
                            let new_body = replace_vars.clone()(body1.clone()) ;
                            {
                              let new_binding = |b: crate::hydra::core::Binding| crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                                name: qualify.clone()(b.clone().0.name.clone()),
                                term: replace_vars.clone()(b.clone().0.term.clone()),
                                type_: b.clone().0.type_.clone()})) ;
                              (crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                                name: key0.clone(),
                                term: new_body.clone(),
                                type_: t.clone()})), crate::hydra::lib::lists::map(new_binding.clone(), bindings1.clone()))}}}}}}}}}},
          _ => (crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
            name: key0.clone(),
            term: val0.clone(),
            type_: t.clone()})), Vec::from([]))}}}} ;
  let flatten_body_let = |bindings: Vec<crate::hydra::core::Binding>, body: crate::hydra::core::Term| match &*body.clone().0 {
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let inner_bindings = v0_.clone().0.bindings.clone() ;
        {
          let inner_body = v0_.clone().0.body.clone() ;
          flatten_body_let.clone()(crate::hydra::lib::lists::concat2(bindings.clone(), inner_bindings.clone()), inner_body.clone())}}},
    _ => (crate::hydra::lib::lists::concat2(Vec::from([]), bindings.clone()), body.clone())} ;
  let flatten = |recurse: Rc<dyn Fn(T0) -> crate::hydra::core::Term>, term2: T0| {
    let rewritten = recurse.clone()(term2.clone()) ;
    match &*rewritten.clone().0 {
      crate::hydra::core::Term_Variant::Let (v0_) => {
        let v0_ = v0_.clone() ;
        {
          let bindings = v0_.clone().0.bindings.clone() ;
          {
            let body = v0_.clone().0.body.clone() ;
            {
              let for_result = |hr: (T1, Vec<T1>)| crate::hydra::lib::lists::concat2(crate::hydra::lib::pairs::second(hr.clone()), crate::hydra::lib::lists::pure(crate::hydra::lib::pairs::first(hr.clone()))) ;
              {
                let flattened_bindings = crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|arg_: crate::hydra::core::Binding| for_result.clone()(rewrite_binding.clone()(arg_.clone())), bindings.clone())) ;
                {
                  let merged = flatten_body_let.clone()(flattened_bindings.clone(), body.clone()) ;
                  {
                    let new_bindings = crate::hydra::lib::pairs::first(merged.clone()) ;
                    {
                      let new_body = crate::hydra::lib::pairs::second(merged.clone()) ;
                      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
                        bindings: new_bindings.clone(),
                        body: new_body.clone()})))))}}}}}}}},
      _ => rewritten.clone()}} ;
  rewrite_term(flatten.clone(), term.clone())}

pub fn fold_over_term(order: crate::hydra::coders::TraversalOrder, fld: impl Fn(T0) -> Rc<dyn Fn(crate::hydra::core::Term) -> T0> + Clone, b0: T0, term: crate::hydra::core::Term) -> T0 {
  match &*order.clone().0 {
    crate::hydra::coders::TraversalOrder_Variant::Pre (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::foldl(|v1: T0, v2: crate::hydra::core::Term| fold_over_term(order.clone(), fld.clone(), v1.clone(), v2.clone()), fld.clone()(b0.clone(), term.clone()), subterms(term.clone()))},
    crate::hydra::coders::TraversalOrder_Variant::Post (v0_) => {
      let v0_ = v0_.clone() ;
      fld.clone()(crate::hydra::lib::lists::foldl(|v1: T0, v2: crate::hydra::core::Term| fold_over_term(order.clone(), fld.clone(), v1.clone(), v2.clone()), b0.clone(), subterms(term.clone())), term.clone())}}}

pub fn fold_over_type(order: crate::hydra::coders::TraversalOrder, fld: impl Fn(T0) -> Rc<dyn Fn(crate::hydra::core::Type) -> T0> + Clone, b0: T0, typ: crate::hydra::core::Type) -> T0 {
  match &*order.clone().0 {
    crate::hydra::coders::TraversalOrder_Variant::Pre (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::foldl(|v1: T0, v2: crate::hydra::core::Type| fold_over_type(order.clone(), fld.clone(), v1.clone(), v2.clone()), fld.clone()(b0.clone(), typ.clone()), subtypes(typ.clone()))},
    crate::hydra::coders::TraversalOrder_Variant::Post (v0_) => {
      let v0_ = v0_.clone() ;
      fld.clone()(crate::hydra::lib::lists::foldl(|v1: T0, v2: crate::hydra::core::Type| fold_over_type(order.clone(), fld.clone(), v1.clone(), v2.clone()), b0.clone(), subtypes(typ.clone())), typ.clone())}}}

pub fn f_type_to_type_scheme(typ: crate::hydra::core::Type) -> crate::hydra::core::TypeScheme {
  let gather_forall = |vars: Vec<crate::hydra::core::Name>, typ2: crate::hydra::core::Type| match &*deannotate_type(typ2.clone()).0 {
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      gather_forall.clone()(crate::hydra::lib::lists::cons(v0_.clone().0.parameter.clone(), vars.clone()), v0_.clone().0.body.clone())},
    _ => crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
      variables: crate::hydra::lib::lists::reverse(vars.clone()),
      type_: typ2.clone(),
      constraints: None}))} ;
  gather_forall.clone()(Vec::from([]), typ.clone())}

pub fn free_type_variables_in_term(term0: crate::hydra::core::Term) -> BTreeSet<crate::hydra::core::Name> {
  let all_of = |sets: Vec<BTreeSet<T0>>| crate::hydra::lib::lists::foldl(crate::hydra::lib::sets::union_, crate::hydra::lib::sets::empty, sets.clone()) ;
  let try_type = |tvars: BTreeSet<crate::hydra::core::Name>, typ: crate::hydra::core::Type| crate::hydra::lib::sets::difference(free_variables_in_type(typ.clone()), tvars.clone()) ;
  let get_all = |vars: BTreeSet<crate::hydra::core::Name>, term: crate::hydra::core::Term| {
    let recurse = |v1: crate::hydra::core::Term| get_all.clone()(vars.clone(), v1.clone()) ;
    {
      let dflt = all_of.clone()(crate::hydra::lib::lists::map(recurse.clone(), subterms(term.clone()))) ;
      match &*term.clone().0 {
        crate::hydra::core::Term_Variant::Function (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::Function_Variant::Elimination (v0_) => {
              let v0_ = v0_.clone() ;
              dflt.clone()},
            crate::hydra::core::Function_Variant::Lambda (v0_) => {
              let v0_ = v0_.clone() ;
              {
                let domt = crate::hydra::lib::maybes::maybe(crate::hydra::lib::sets::empty, |v1: crate::hydra::core::Type| try_type.clone()(vars.clone(), v1.clone()), v0_.clone().0.domain.clone()) ;
                crate::hydra::lib::sets::union_(domt.clone(), recurse.clone()(v0_.clone().0.body.clone()))}},
            _ => dflt.clone()}},
        crate::hydra::core::Term_Variant::Let (v0_) => {
          let v0_ = v0_.clone() ;
          {
            let for_binding = |b: crate::hydra::core::Binding| {
              let new_vars = crate::hydra::lib::maybes::maybe(vars.clone(), |ts: crate::hydra::core::TypeScheme| crate::hydra::lib::sets::union_(vars.clone(), crate::hydra::lib::sets::from_list(ts.clone().0.variables.clone())), b.clone().0.type_.clone()) ;
              crate::hydra::lib::sets::union_(get_all.clone()(new_vars.clone(), b.clone().0.term.clone()), crate::hydra::lib::maybes::maybe(crate::hydra::lib::sets::empty, |ts: crate::hydra::core::TypeScheme| try_type.clone()(new_vars.clone(), ts.clone().0.type_.clone()), b.clone().0.type_.clone()))} ;
            crate::hydra::lib::sets::union_(all_of.clone()(crate::hydra::lib::lists::map(for_binding.clone(), v0_.clone().0.bindings.clone())), recurse.clone()(v0_.clone().0.body.clone()))}},
        crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::lib::sets::union_(try_type.clone()(vars.clone(), v0_.clone().0.type_.clone()), recurse.clone()(v0_.clone().0.body.clone()))},
        crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::lib::sets::union_(try_type.clone()(vars.clone(), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v0_.clone().0.parameter.clone())))), recurse.clone()(v0_.clone().0.body.clone()))},
        _ => dflt.clone()}}} ;
  get_all.clone()(crate::hydra::lib::sets::empty, term0.clone())}

pub fn free_variables_in_term(term: crate::hydra::core::Term) -> BTreeSet<crate::hydra::core::Name> {
  let dflt_vars = crate::hydra::lib::lists::foldl(|s: BTreeSet<crate::hydra::core::Name>, t: crate::hydra::core::Term| crate::hydra::lib::sets::union_(s.clone(), free_variables_in_term(t.clone())), crate::hydra::lib::sets::empty, subterms(term.clone())) ;
  match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Function_Variant::Lambda (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::lib::sets::delete(v0_.clone().0.parameter.clone(), free_variables_in_term(v0_.clone().0.body.clone()))},
        _ => dflt_vars.clone()}},
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::sets::difference(dflt_vars.clone(), crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::map(|v| v.0.name.clone(), v0_.clone().0.bindings.clone())))},
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::sets::singleton(v0_.clone())},
    _ => dflt_vars.clone()}}

pub fn free_variables_in_type(typ: crate::hydra::core::Type) -> BTreeSet<crate::hydra::core::Name> {
  let dflt_vars = crate::hydra::lib::lists::foldl(|s: BTreeSet<crate::hydra::core::Name>, t: crate::hydra::core::Type| crate::hydra::lib::sets::union_(s.clone(), free_variables_in_type(t.clone())), crate::hydra::lib::sets::empty, subtypes(typ.clone())) ;
  match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::sets::delete(v0_.clone().0.parameter.clone(), free_variables_in_type(v0_.clone().0.body.clone()))},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::sets::singleton(v0_.clone())},
    _ => dflt_vars.clone()}}

pub fn free_variables_in_type_ordered(typ: crate::hydra::core::Type) -> Vec<crate::hydra::core::Name> {
  let collect_vars = |bound_vars: BTreeSet<crate::hydra::core::Name>, t: crate::hydra::core::Type| match &*t.clone().0 {
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(crate::hydra::lib::sets::member(v0_.clone(), bound_vars.clone()), Vec::from([]), Vec::from([
        v0_.clone()]))},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      collect_vars.clone()(crate::hydra::lib::sets::insert(v0_.clone().0.parameter.clone(), bound_vars.clone()), v0_.clone().0.body.clone())},
    _ => crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|v1: crate::hydra::core::Type| collect_vars.clone()(bound_vars.clone(), v1.clone()), subtypes(t.clone())))} ;
  crate::hydra::lib::lists::nub(collect_vars.clone()(crate::hydra::lib::sets::empty, typ.clone()))}

pub fn free_variables_in_type_scheme_simple(ts: crate::hydra::core::TypeScheme) -> BTreeSet<crate::hydra::core::Name> {
  let vars = ts.clone().0.variables.clone() ;
  let t = ts.clone().0.type_.clone() ;
  crate::hydra::lib::sets::difference(free_variables_in_type_simple(t.clone()), crate::hydra::lib::sets::from_list(vars.clone()))}

pub fn free_variables_in_type_scheme(ts: crate::hydra::core::TypeScheme) -> BTreeSet<crate::hydra::core::Name> {
  let vars = ts.clone().0.variables.clone() ;
  let t = ts.clone().0.type_.clone() ;
  crate::hydra::lib::sets::difference(free_variables_in_type(t.clone()), crate::hydra::lib::sets::from_list(vars.clone()))}

pub fn free_variables_in_type_simple(typ: crate::hydra::core::Type) -> BTreeSet<crate::hydra::core::Name> {
  let helper = |types: BTreeSet<crate::hydra::core::Name>, typ2: crate::hydra::core::Type| match &*typ2.clone().0 {
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::sets::insert(v0_.clone(), types.clone())},
    _ => types.clone()} ;
  fold_over_type(crate::hydra::coders::TraversalOrder(Rc::new(crate::hydra::coders::TraversalOrder_Variant::Pre)), helper.clone(), crate::hydra::lib::sets::empty, typ.clone())}

pub fn inline_type(schema: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Type>, typ: crate::hydra::core::Type) -> Either<String, crate::hydra::core::Type> {
  let f = |recurse: Rc<dyn Fn(T0) -> Either<String, crate::hydra::core::Type>>, typ2: T0| {
    let after_recurse = |tr: crate::hydra::core::Type| match &*tr.clone().0 {
      crate::hydra::core::Type_Variant::Variable (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::lib::strings::cat2(String::from("No such type in schema: "), v0_.clone().0.0.clone())), |v1: crate::hydra::core::Type| inline_type(schema.clone(), v1.clone()), crate::hydra::lib::maps::lookup(v0_.clone(), schema.clone()))},
      _ => Right(tr.clone())} ;
    crate::hydra::lib::eithers::bind(recurse.clone()(typ2.clone()), |tr: crate::hydra::core::Type| after_recurse.clone()(tr.clone()))} ;
  rewrite_type_m(f.clone(), typ.clone())}

pub fn is_free_variable_in_term(v: crate::hydra::core::Name, term: crate::hydra::core::Term) -> bool {
  crate::hydra::lib::logic::not(crate::hydra::lib::sets::member(v.clone(), free_variables_in_term(term.clone())))}

pub fn is_lambda(term: crate::hydra::core::Term) -> bool {
  match &*deannotate_term(term.clone()).0 {
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Function_Variant::Lambda (v0_) => {
          let v0_ = v0_.clone() ;
          true},
        _ => false}},
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      is_lambda(v0_.clone().0.body.clone())},
    _ => false}}

pub fn lift_lambda_above_let(term0: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let rewrite = |recurse: Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>, term: crate::hydra::core::Term| {
    let rewrite_binding = |b: crate::hydra::core::Binding| crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
      name: b.clone().0.name.clone(),
      term: rewrite.clone()(recurse.clone(), b.clone().0.term.clone()),
      type_: b.clone().0.type_.clone()})) ;
    {
      let rewrite_bindings = |bs: Vec<crate::hydra::core::Binding>| crate::hydra::lib::lists::map(rewrite_binding.clone(), bs.clone()) ;
      {
        let dig_for_lambdas = |original: crate::hydra::core::Term, cons: Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>, term2: crate::hydra::core::Term| match &*term2.clone().0 {
          crate::hydra::core::Term_Variant::Annotated (v0_) => {
            let v0_ = v0_.clone() ;
            dig_for_lambdas.clone()(original.clone(), |t: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
              body: cons.clone()(t.clone()),
              annotation: v0_.clone().0.annotation.clone()}))))), v0_.clone().0.body.clone())},
          crate::hydra::core::Term_Variant::Function (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Function_Variant::Lambda (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                  parameter: v0_.clone().0.parameter.clone(),
                  domain: v0_.clone().0.domain.clone(),
                  body: dig_for_lambdas.clone()(cons.clone()(v0_.clone().0.body.clone()), |t: crate::hydra::core::Term| cons.clone()(t.clone()), v0_.clone().0.body.clone())}))))))))},
              _ => recurse.clone()(original.clone())}},
          crate::hydra::core::Term_Variant::Let (v0_) => {
            let v0_ = v0_.clone() ;
            dig_for_lambdas.clone()(original.clone(), |t: crate::hydra::core::Term| cons.clone()(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
              bindings: rewrite_bindings.clone()(v0_.clone().0.bindings.clone()),
              body: t.clone()})))))), v0_.clone().0.body.clone())},
          _ => recurse.clone()(original.clone())} ;
        match &*term.clone().0 {
          crate::hydra::core::Term_Variant::Let (v0_) => {
            let v0_ = v0_.clone() ;
            dig_for_lambdas.clone()(term.clone(), |t: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
              bindings: rewrite_bindings.clone()(v0_.clone().0.bindings.clone()),
              body: t.clone()}))))), v0_.clone().0.body.clone())},
          _ => recurse.clone()(term.clone())}}}} ;
  rewrite_term(rewrite.clone(), term0.clone())}

pub fn map_beneath_type_annotations(f: impl Fn(crate::hydra::core::Type) -> crate::hydra::core::Type + Clone, t: crate::hydra::core::Type) -> crate::hydra::core::Type {
  match &*t.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
        body: map_beneath_type_annotations(f.clone(), v0_.clone().0.body.clone()),
        annotation: v0_.clone().0.annotation.clone()})))))},
    _ => f.clone()(t.clone())}}

pub fn normalize_type_variables_in_term(term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let replace_name = |subst: BTreeMap<T0, T0>, v: T0| crate::hydra::lib::maybes::from_maybe(v.clone(), crate::hydra::lib::maps::lookup(v.clone(), subst.clone())) ;
  let subst_type = |subst: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Name>, typ: crate::hydra::core::Type| {
    let rewrite = |recurse: Rc<dyn Fn(crate::hydra::core::Type) -> crate::hydra::core::Type>, typ2: crate::hydra::core::Type| match &*typ2.clone().0 {
      crate::hydra::core::Type_Variant::Variable (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(replace_name.clone()(subst.clone(), v0_.clone()))))},
      _ => recurse.clone()(typ2.clone())} ;
    rewrite_type(rewrite.clone(), typ.clone())} ;
  let rewrite_with_subst = |state: ((BTreeMap<crate::hydra::core::Name, crate::hydra::core::Name>, BTreeSet<crate::hydra::core::Name>), i32), term0: crate::hydra::core::Term| {
    let sb = crate::hydra::lib::pairs::first(state.clone()) ;
    {
      let next = crate::hydra::lib::pairs::second(state.clone()) ;
      {
        let subst = crate::hydra::lib::pairs::first(sb.clone()) ;
        {
          let bound_vars = crate::hydra::lib::pairs::second(sb.clone()) ;
          {
            let rewrite = |recurse: Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>, term2: crate::hydra::core::Term| match &*term2.clone().0 {
              crate::hydra::core::Term_Variant::Function (v0_) => {
                let v0_ = v0_.clone() ;
                match &*v0_.clone().0 {
                  crate::hydra::core::Function_Variant::Elimination (v0_) => {
                    let v0_ = v0_.clone() ;
                    recurse.clone()(term2.clone())},
                  crate::hydra::core::Function_Variant::Lambda (v0_) => {
                    let v0_ = v0_.clone() ;
                    {
                      let domain = v0_.clone().0.domain.clone() ;
                      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                        parameter: v0_.clone().0.parameter.clone(),
                        domain: crate::hydra::lib::maybes::map(|v12: crate::hydra::core::Type| subst_type.clone()(subst.clone(), v12.clone()), domain.clone()),
                        body: rewrite_with_subst.clone()(((subst.clone(), bound_vars.clone()), next.clone()), v0_.clone().0.body.clone())}))))))))}},
                  _ => recurse.clone()(term2.clone())}},
              crate::hydra::core::Term_Variant::Let (v0_) => {
                let v0_ = v0_.clone() ;
                {
                  let bindings0 = v0_.clone().0.bindings.clone() ;
                  {
                    let body0 = v0_.clone().0.body.clone() ;
                    {
                      let step = |acc: Vec<crate::hydra::core::Binding>, bs: Vec<crate::hydra::core::Binding>| {
                        let b = crate::hydra::lib::lists::head(bs.clone()) ;
                        {
                          let tl = crate::hydra::lib::lists::tail(bs.clone()) ;
                          {
                            let no_type = {
                              let new_val = rewrite_with_subst.clone()(((subst.clone(), bound_vars.clone()), next.clone()), b.clone().0.term.clone()) ;
                              {
                                let b1 = crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                                  name: b.clone().0.name.clone(),
                                  term: new_val.clone(),
                                  type_: None})) ;
                                step.clone()(crate::hydra::lib::lists::cons(b1.clone(), acc.clone()), tl.clone())}} ;
                            {
                              let with_type = |ts: crate::hydra::core::TypeScheme| {
                                let vars = ts.clone().0.variables.clone() ;
                                {
                                  let typ = ts.clone().0.type_.clone() ;
                                  {
                                    let k = crate::hydra::lib::lists::length(vars.clone()) ;
                                    {
                                      let gen = |i: i32, rem: i32, acc2: Vec<crate::hydra::core::Name>| {
                                        let ti = crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat2(String::from("t"), crate::hydra::lib::literals::show_int32(crate::hydra::lib::math::add(next.clone(), i.clone())))))) ;
                                        crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(rem.clone(), 0i32), crate::hydra::lib::lists::reverse(acc2.clone()), gen.clone()(crate::hydra::lib::math::add(i.clone(), 1i32), crate::hydra::lib::math::sub(rem.clone(), 1i32), crate::hydra::lib::lists::cons(ti.clone(), acc2.clone())))} ;
                                      {
                                        let new_vars = gen.clone()(0i32, k.clone(), Vec::from([])) ;
                                        {
                                          let new_subst = crate::hydra::lib::maps::union_(crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::zip(vars.clone(), new_vars.clone())), subst.clone()) ;
                                          {
                                            let new_bound = crate::hydra::lib::sets::union_(bound_vars.clone(), crate::hydra::lib::sets::from_list(new_vars.clone())) ;
                                            {
                                              let new_val = rewrite_with_subst.clone()(((new_subst.clone(), new_bound.clone()), crate::hydra::lib::math::add(next.clone(), k.clone())), b.clone().0.term.clone()) ;
                                              {
                                                let rename_constraint_keys = |constraint_map: BTreeMap<crate::hydra::core::Name, T0>| crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|p: (crate::hydra::core::Name, T0)| {
                                                  let old_name = crate::hydra::lib::pairs::first(p.clone()) ;
                                                  {
                                                    let meta = crate::hydra::lib::pairs::second(p.clone()) ;
                                                    {
                                                      let new_name = crate::hydra::lib::maybes::from_maybe(old_name.clone(), crate::hydra::lib::maps::lookup(old_name.clone(), new_subst.clone())) ;
                                                      (new_name.clone(), meta.clone())}}}, crate::hydra::lib::maps::to_list(constraint_map.clone()))) ;
                                                {
                                                  let old_constraints = ts.clone().0.constraints.clone() ;
                                                  {
                                                    let new_constraints = crate::hydra::lib::maybes::map(rename_constraint_keys.clone(), old_constraints.clone()) ;
                                                    {
                                                      let b1 = crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                                                        name: b.clone().0.name.clone(),
                                                        term: new_val.clone(),
                                                        type_: Some(crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
                                                          variables: new_vars.clone(),
                                                          type_: subst_type.clone()(new_subst.clone(), typ.clone()),
                                                          constraints: new_constraints.clone()})))})) ;
                                                      step.clone()(crate::hydra::lib::lists::cons(b1.clone(), acc.clone()), tl.clone())}}}}}}}}}}}} ;
                              crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(bs.clone()), crate::hydra::lib::lists::reverse(acc.clone()), crate::hydra::lib::maybes::maybe(no_type.clone(), |ts: crate::hydra::core::TypeScheme| with_type.clone()(ts.clone()), b.clone().0.type_.clone()))}}}} ;
                      {
                        let bindings1 = step.clone()(Vec::from([]), bindings0.clone()) ;
                        crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
                          bindings: bindings1.clone(),
                          body: rewrite_with_subst.clone()(((subst.clone(), bound_vars.clone()), next.clone()), body0.clone())})))))}}}}},
              crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
                  body: rewrite_with_subst.clone()(((subst.clone(), bound_vars.clone()), next.clone()), v0_.clone().0.body.clone()),
                  type_: subst_type.clone()(subst.clone(), v0_.clone().0.type_.clone())})))))},
              crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeLambda(crate::hydra::core::TypeLambda(Rc::new(crate::hydra::core::TypeLambda_Variant {
                  parameter: replace_name.clone()(subst.clone(), v0_.clone().0.parameter.clone()),
                  body: rewrite_with_subst.clone()(((subst.clone(), bound_vars.clone()), next.clone()), v0_.clone().0.body.clone())})))))},
              _ => recurse.clone()(term2.clone())} ;
            rewrite_term(rewrite.clone(), term0.clone())}}}}} ;
  rewrite_with_subst.clone()(((crate::hydra::lib::maps::empty, crate::hydra::lib::sets::empty), 0i32), term.clone())}

pub fn prune_let(l: crate::hydra::core::Let) -> crate::hydra::core::Let {
  let binding_map = crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(|b: crate::hydra::core::Binding| (b.clone().0.name.clone(), b.clone().0.term.clone()), l.clone().0.bindings.clone())) ;
  let root_name = crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("[[[root]]]")))) ;
  let adj = |n: crate::hydra::core::Name| crate::hydra::lib::sets::intersection(crate::hydra::lib::sets::from_list(crate::hydra::lib::maps::keys(binding_map.clone())), free_variables_in_term(crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(n.clone(), root_name.clone()), l.clone().0.body.clone(), crate::hydra::lib::maybes::from_just(crate::hydra::lib::maps::lookup(n.clone(), binding_map.clone()))))) ;
  let reachable = crate::hydra::sorting::find_reachable_nodes(adj.clone(), root_name.clone()) ;
  let pruned_bindings = crate::hydra::lib::lists::filter(|b: crate::hydra::core::Binding| crate::hydra::lib::sets::member(b.clone().0.name.clone(), reachable.clone()), l.clone().0.bindings.clone()) ;
  crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
    bindings: pruned_bindings.clone(),
    body: l.clone().0.body.clone()}))}

pub fn remove_term_annotations(term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let remove = |recurse: Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>, term2: crate::hydra::core::Term| {
    let rewritten = recurse.clone()(term2.clone()) ;
    match &*term2.clone().0 {
      crate::hydra::core::Term_Variant::Annotated (v0_) => {
        let v0_ = v0_.clone() ;
        v0_.clone().0.body.clone()},
      _ => rewritten.clone()}} ;
  rewrite_term(remove.clone(), term.clone())}

pub fn remove_type_annotations(typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  let remove = |recurse: Rc<dyn Fn(T0) -> crate::hydra::core::Type>, typ2: T0| {
    let rewritten = recurse.clone()(typ2.clone()) ;
    match &*rewritten.clone().0 {
      crate::hydra::core::Type_Variant::Annotated (v0_) => {
        let v0_ = v0_.clone() ;
        v0_.clone().0.body.clone()},
      _ => rewritten.clone()}} ;
  rewrite_type(remove.clone(), typ.clone())}

pub fn remove_type_annotations_from_term(term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let strip = |recurse: Rc<dyn Fn(T0) -> crate::hydra::core::Term>, term2: T0| {
    let rewritten = recurse.clone()(term2.clone()) ;
    {
      let strip_binding = |b: crate::hydra::core::Binding| crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
        name: b.clone().0.name.clone(),
        term: b.clone().0.term.clone(),
        type_: None})) ;
      match &*rewritten.clone().0 {
        crate::hydra::core::Term_Variant::Let (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
            bindings: crate::hydra::lib::lists::map(strip_binding.clone(), v0_.clone().0.bindings.clone()),
            body: v0_.clone().0.body.clone()})))))},
        crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
          let v0_ = v0_.clone() ;
          v0_.clone().0.body.clone()},
        crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
          let v0_ = v0_.clone() ;
          v0_.clone().0.body.clone()},
        _ => rewritten.clone()}}} ;
  rewrite_term(strip.clone(), term.clone())}

pub fn remove_types_from_term(term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let strip = |recurse: Rc<dyn Fn(T0) -> crate::hydra::core::Term>, term2: T0| {
    let rewritten = recurse.clone()(term2.clone()) ;
    {
      let strip_binding = |b: crate::hydra::core::Binding| crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
        name: b.clone().0.name.clone(),
        term: b.clone().0.term.clone(),
        type_: None})) ;
      match &*rewritten.clone().0 {
        crate::hydra::core::Term_Variant::Function (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::Function_Variant::Elimination (v0_) => {
              let v0_ = v0_.clone() ;
              crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(v0_.clone()))))))},
            crate::hydra::core::Function_Variant::Lambda (v0_) => {
              let v0_ = v0_.clone() ;
              crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                parameter: v0_.clone().0.parameter.clone(),
                domain: None,
                body: v0_.clone().0.body.clone()}))))))))},
            _ => crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(v0_.clone())))}},
        crate::hydra::core::Term_Variant::Let (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
            bindings: crate::hydra::lib::lists::map(strip_binding.clone(), v0_.clone().0.bindings.clone()),
            body: v0_.clone().0.body.clone()})))))},
        crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
          let v0_ = v0_.clone() ;
          v0_.clone().0.body.clone()},
        crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
          let v0_ = v0_.clone() ;
          v0_.clone().0.body.clone()},
        _ => rewritten.clone()}}} ;
  rewrite_term(strip.clone(), term.clone())}

pub fn replace_free_term_variable(vold: crate::hydra::core::Name, tnew: crate::hydra::core::Term, term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let rewrite = |recurse: Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>, t: crate::hydra::core::Term| match &*t.clone().0 {
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Function_Variant::Lambda (v0_) => {
          let v0_ = v0_.clone() ;
          {
            let v = v0_.clone().0.parameter.clone() ;
            crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(v.clone(), vold.clone()), t.clone(), recurse.clone()(t.clone()))}},
        _ => recurse.clone()(t.clone())}},
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(v0_.clone(), vold.clone()), tnew.clone(), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(v0_.clone()))))},
    _ => recurse.clone()(t.clone())} ;
  rewrite_term(rewrite.clone(), term.clone())}

pub fn replace_free_type_variable(v: crate::hydra::core::Name, rep: crate::hydra::core::Type, typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  let map_expr = |recurse: Rc<dyn Fn(crate::hydra::core::Type) -> crate::hydra::core::Type>, t: crate::hydra::core::Type| match &*t.clone().0 {
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(v.clone(), v0_.clone().0.parameter.clone()), t.clone(), crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
        parameter: v0_.clone().0.parameter.clone(),
        body: recurse.clone()(v0_.clone().0.body.clone())}))))))},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(v.clone(), v0_.clone()), rep.clone(), t.clone())},
    _ => recurse.clone()(t.clone())} ;
  rewrite_type(map_expr.clone(), typ.clone())}

pub fn replace_typedefs(types: BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeScheme>, typ0: crate::hydra::core::Type) -> crate::hydra::core::Type {
  let rewrite = |recurse: Rc<dyn Fn(crate::hydra::core::Type) -> crate::hydra::core::Type>, typ: crate::hydra::core::Type| match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
        body: rewrite.clone()(recurse.clone(), v0_.clone().0.body.clone()),
        annotation: v0_.clone().0.annotation.clone()})))))},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      typ.clone()},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      typ.clone()},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let for_mono = |t: crate::hydra::core::Type| match &*t.clone().0 {
          crate::hydra::core::Type_Variant::Record (v0_) => {
            let v0_ = v0_.clone() ;
            typ.clone()},
          crate::hydra::core::Type_Variant::Union (v0_) => {
            let v0_ = v0_.clone() ;
            typ.clone()},
          crate::hydra::core::Type_Variant::Wrap (v0_) => {
            let v0_ = v0_.clone() ;
            typ.clone()},
          _ => rewrite.clone()(recurse.clone(), t.clone())} ;
        {
          let for_type_scheme = |ts: crate::hydra::core::TypeScheme| {
            let t = ts.clone().0.type_.clone() ;
            crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(ts.clone().0.variables.clone()), for_mono.clone()(t.clone()), typ.clone())} ;
          crate::hydra::lib::maybes::maybe(typ.clone(), |ts: crate::hydra::core::TypeScheme| for_type_scheme.clone()(ts.clone()), crate::hydra::lib::maps::lookup(v0_.clone(), types.clone()))}}},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      typ.clone()},
    _ => recurse.clone()(typ.clone())} ;
  rewrite_type(rewrite.clone(), typ0.clone())}

pub fn rewrite_and_fold_term(f: impl Fn(Rc<dyn Fn(T0) -> Rc<dyn Fn(crate::hydra::core::Term) -> (T0, crate::hydra::core::Term)>>) -> Rc<dyn Fn(T0) -> Rc<dyn Fn(crate::hydra::core::Term) -> (T0, crate::hydra::core::Term)>> + Clone, term0: T0, v1: crate::hydra::core::Term) -> (T0, crate::hydra::core::Term) {
  let fsub = |recurse: Rc<dyn Fn(T1) -> Rc<dyn Fn(crate::hydra::core::Term) -> (T1, crate::hydra::core::Term)>>, val0: T1, term02: crate::hydra::core::Term| {
    let for_single = |rec: Rc<dyn Fn(T2) -> Rc<dyn Fn(T3) -> (T4, T5)>>, cons: Rc<dyn Fn(T5) -> T6>, val: T2, term: T3| {
      let r = rec.clone()(val.clone(), term.clone()) ;
      (crate::hydra::lib::pairs::first(r.clone()), cons.clone()(crate::hydra::lib::pairs::second(r.clone())))} ;
    {
      let for_many = |rec: Rc<dyn Fn(T2) -> Rc<dyn Fn(T3) -> (T2, T4)>>, cons: Rc<dyn Fn(Vec<T4>) -> T5>, val: T2, els: Vec<T3>| {
        let rr = crate::hydra::lib::lists::foldl(|r: (T2, Vec<T4>), el: T3| {
          let r2 = rec.clone()(crate::hydra::lib::pairs::first(r.clone()), el.clone()) ;
          (crate::hydra::lib::pairs::first(r2.clone()), crate::hydra::lib::lists::cons(crate::hydra::lib::pairs::second(r2.clone()), crate::hydra::lib::pairs::second(r.clone())))}, (val.clone(), Vec::from([])), els.clone()) ;
        (crate::hydra::lib::pairs::first(rr.clone()), cons.clone()(crate::hydra::lib::lists::reverse(crate::hydra::lib::pairs::second(rr.clone()))))} ;
      {
        let for_field = |val: T1, field: crate::hydra::core::Field| {
          let r = recurse.clone()(val.clone(), field.clone().0.term.clone()) ;
          (crate::hydra::lib::pairs::first(r.clone()), crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
            name: field.clone().0.name.clone(),
            term: crate::hydra::lib::pairs::second(r.clone())})))} ;
        {
          let for_fields = |v1: T1, v2: Vec<crate::hydra::core::Field>| for_many.clone()(for_field.clone(), |x: Vec<crate::hydra::core::Field>| x.clone(), v1.clone(), v2.clone()) ;
          {
            let for_pair = |val: T1, kv: (crate::hydra::core::Term, crate::hydra::core::Term)| {
              let rk = recurse.clone()(val.clone(), crate::hydra::lib::pairs::first(kv.clone())) ;
              {
                let rv = recurse.clone()(crate::hydra::lib::pairs::first(rk.clone()), crate::hydra::lib::pairs::second(kv.clone())) ;
                (crate::hydra::lib::pairs::first(rv.clone()), (crate::hydra::lib::pairs::second(rk.clone()), crate::hydra::lib::pairs::second(rv.clone())))}} ;
            {
              let for_binding = |val: T1, binding: crate::hydra::core::Binding| {
                let r = recurse.clone()(val.clone(), binding.clone().0.term.clone()) ;
                (crate::hydra::lib::pairs::first(r.clone()), crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                  name: binding.clone().0.name.clone(),
                  term: crate::hydra::lib::pairs::second(r.clone()),
                  type_: binding.clone().0.type_.clone()})))} ;
              {
                let for_elimination = |val: T1, elm: crate::hydra::core::Elimination| {
                  let r = match &*elm.clone().0 {
                    crate::hydra::core::Elimination_Variant::Union (v0_) => {
                      let v0_ = v0_.clone() ;
                      {
                        let rmd = crate::hydra::lib::maybes::map(|v1: crate::hydra::core::Term| recurse.clone()(val.clone(), v1.clone()), v0_.clone().0.default_.clone()) ;
                        {
                          let val1 = crate::hydra::lib::maybes::maybe(val.clone(), crate::hydra::lib::pairs::first, rmd.clone()) ;
                          {
                            let rcases = for_fields.clone()(val1.clone(), v0_.clone().0.cases.clone()) ;
                            (crate::hydra::lib::pairs::first(rcases.clone()), crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                              type_name: v0_.clone().0.type_name.clone(),
                              default_: crate::hydra::lib::maybes::map(crate::hydra::lib::pairs::second, rmd.clone()),
                              cases: crate::hydra::lib::pairs::second(rcases.clone())}))))))}}}},
                    _ => (val.clone(), elm.clone())} ;
                  (crate::hydra::lib::pairs::first(r.clone()), crate::hydra::lib::pairs::second(r.clone()))} ;
                {
                  let for_function = |val: T1, fun: crate::hydra::core::Function| match &*fun.clone().0 {
                    crate::hydra::core::Function_Variant::Elimination (v0_) => {
                      let v0_ = v0_.clone() ;
                      {
                        let re = for_elimination.clone()(val.clone(), v0_.clone()) ;
                        (crate::hydra::lib::pairs::first(re.clone()), crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::lib::pairs::second(re.clone())))))}},
                    crate::hydra::core::Function_Variant::Lambda (v0_) => {
                      let v0_ = v0_.clone() ;
                      {
                        let rl = recurse.clone()(val.clone(), v0_.clone().0.body.clone()) ;
                        (crate::hydra::lib::pairs::first(rl.clone()), crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                          parameter: v0_.clone().0.parameter.clone(),
                          domain: v0_.clone().0.domain.clone(),
                          body: crate::hydra::lib::pairs::second(rl.clone())}))))))}},
                    _ => (val.clone(), fun.clone())} ;
                  {
                    let dflt = (val0.clone(), term02.clone()) ;
                    match &*term02.clone().0 {
                      crate::hydra::core::Term_Variant::Annotated (v0_) => {
                        let v0_ = v0_.clone() ;
                        for_single.clone()(recurse.clone(), |t: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
                          body: t.clone(),
                          annotation: v0_.clone().0.annotation.clone()}))))), val0.clone(), v0_.clone().0.body.clone())},
                      crate::hydra::core::Term_Variant::Application (v0_) => {
                        let v0_ = v0_.clone() ;
                        {
                          let rlhs = recurse.clone()(val0.clone(), v0_.clone().0.function.clone()) ;
                          {
                            let rrhs = recurse.clone()(crate::hydra::lib::pairs::first(rlhs.clone()), v0_.clone().0.argument.clone()) ;
                            (crate::hydra::lib::pairs::first(rrhs.clone()), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                              function: crate::hydra::lib::pairs::second(rlhs.clone()),
                              argument: crate::hydra::lib::pairs::second(rrhs.clone())}))))))}}},
                      crate::hydra::core::Term_Variant::Either (v0_) => {
                        let v0_ = v0_.clone() ;
                        crate::hydra::lib::eithers::either(|l: crate::hydra::core::Term| {
                          let rl = recurse.clone()(val0.clone(), l.clone()) ;
                          (crate::hydra::lib::pairs::first(rl.clone()), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::lib::pairs::second(rl.clone()))))))}, |r: crate::hydra::core::Term| {
                          let rr = recurse.clone()(val0.clone(), r.clone()) ;
                          (crate::hydra::lib::pairs::first(rr.clone()), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::lib::pairs::second(rr.clone()))))))}, v0_.clone())},
                      crate::hydra::core::Term_Variant::Function (v0_) => {
                        let v0_ = v0_.clone() ;
                        for_single.clone()(for_function.clone(), |f3: crate::hydra::core::Function| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(f3.clone()))), val0.clone(), v0_.clone())},
                      crate::hydra::core::Term_Variant::Let (v0_) => {
                        let v0_ = v0_.clone() ;
                        {
                          let renv = recurse.clone()(val0.clone(), v0_.clone().0.body.clone()) ;
                          for_many.clone()(for_binding.clone(), |bins: Vec<crate::hydra::core::Binding>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
                            bindings: bins.clone(),
                            body: crate::hydra::lib::pairs::second(renv.clone())}))))), crate::hydra::lib::pairs::first(renv.clone()), v0_.clone().0.bindings.clone())}},
                      crate::hydra::core::Term_Variant::List (v0_) => {
                        let v0_ = v0_.clone() ;
                        for_many.clone()(recurse.clone(), |x: Vec<crate::hydra::core::Term>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(x.clone()))), val0.clone(), v0_.clone())},
                      crate::hydra::core::Term_Variant::Map (v0_) => {
                        let v0_ = v0_.clone() ;
                        for_many.clone()(for_pair.clone(), |pairs: Vec<(crate::hydra::core::Term, crate::hydra::core::Term)>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(crate::hydra::lib::maps::from_list(pairs.clone())))), val0.clone(), crate::hydra::lib::maps::to_list(v0_.clone()))},
                      crate::hydra::core::Term_Variant::Maybe (v0_) => {
                        let v0_ = v0_.clone() ;
                        crate::hydra::lib::maybes::maybe(dflt.clone(), |t: crate::hydra::core::Term| for_single.clone()(recurse.clone(), |t1: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(Some(t1.clone())))), val0.clone(), t.clone()), v0_.clone())},
                      crate::hydra::core::Term_Variant::Pair (v0_) => {
                        let v0_ = v0_.clone() ;
                        {
                          let rf = recurse.clone()(val0.clone(), crate::hydra::lib::pairs::first(v0_.clone())) ;
                          {
                            let rs = recurse.clone()(crate::hydra::lib::pairs::first(rf.clone()), crate::hydra::lib::pairs::second(v0_.clone())) ;
                            (crate::hydra::lib::pairs::first(rs.clone()), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Pair((crate::hydra::lib::pairs::second(rf.clone()), crate::hydra::lib::pairs::second(rs.clone()))))))}}},
                      crate::hydra::core::Term_Variant::Record (v0_) => {
                        let v0_ = v0_.clone() ;
                        for_many.clone()(for_field.clone(), |fields: Vec<crate::hydra::core::Field>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
                          type_name: v0_.clone().0.type_name.clone(),
                          fields: fields.clone()}))))), val0.clone(), v0_.clone().0.fields.clone())},
                      crate::hydra::core::Term_Variant::Set (v0_) => {
                        let v0_ = v0_.clone() ;
                        for_many.clone()(recurse.clone(), |e: Vec<crate::hydra::core::Term>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Set(crate::hydra::lib::sets::from_list(e.clone())))), val0.clone(), crate::hydra::lib::sets::to_list(v0_.clone()))},
                      crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
                        let v0_ = v0_.clone() ;
                        for_single.clone()(recurse.clone(), |t: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
                          body: t.clone(),
                          type_: v0_.clone().0.type_.clone()}))))), val0.clone(), v0_.clone().0.body.clone())},
                      crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
                        let v0_ = v0_.clone() ;
                        for_single.clone()(recurse.clone(), |t: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeLambda(crate::hydra::core::TypeLambda(Rc::new(crate::hydra::core::TypeLambda_Variant {
                          parameter: v0_.clone().0.parameter.clone(),
                          body: t.clone()}))))), val0.clone(), v0_.clone().0.body.clone())},
                      crate::hydra::core::Term_Variant::Union (v0_) => {
                        let v0_ = v0_.clone() ;
                        for_single.clone()(recurse.clone(), |t: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
                          type_name: v0_.clone().0.type_name.clone(),
                          field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                            name: v0_.clone().0.field.clone().0.name.clone(),
                            term: t.clone()}))}))))), val0.clone(), v0_.clone().0.field.clone().0.term.clone())},
                      crate::hydra::core::Term_Variant::Wrap (v0_) => {
                        let v0_ = v0_.clone() ;
                        for_single.clone()(recurse.clone(), |t: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                          type_name: v0_.clone().0.type_name.clone(),
                          body: t.clone()}))))), val0.clone(), v0_.clone().0.body.clone())},
                      _ => dflt.clone()}}}}}}}}}} ;
  let recurse = |v1: T0, v2: crate::hydra::core::Term| f.clone()(|v12: T0, v22: crate::hydra::core::Term| fsub.clone()(recurse.clone(), v12.clone(), v22.clone()), v1.clone(), v2.clone()) ;
  recurse.clone()(term0.clone(), v1.clone())}

pub fn rewrite_and_fold_term_with_path(f: impl Fn(Rc<dyn Fn(Vec<crate::hydra::accessors::TermAccessor>) -> Rc<dyn Fn(T0) -> Rc<dyn Fn(crate::hydra::core::Term) -> (T0, crate::hydra::core::Term)>>>) -> Rc<dyn Fn(Vec<crate::hydra::accessors::TermAccessor>) -> Rc<dyn Fn(T0) -> Rc<dyn Fn(crate::hydra::core::Term) -> (T0, crate::hydra::core::Term)>>> + Clone, term0: T0, v1: crate::hydra::core::Term) -> (T0, crate::hydra::core::Term) {
  let fsub = |recurse: Rc<dyn Fn(Vec<crate::hydra::accessors::TermAccessor>) -> Rc<dyn Fn(T1) -> Rc<dyn Fn(crate::hydra::core::Term) -> (T1, crate::hydra::core::Term)>>>, path: Vec<crate::hydra::accessors::TermAccessor>, val0: T1, term02: crate::hydra::core::Term| {
    let for_single_with_accessor = |rec: Rc<dyn Fn(Vec<crate::hydra::accessors::TermAccessor>) -> Rc<dyn Fn(T2) -> Rc<dyn Fn(T3) -> (T4, T5)>>>, cons: Rc<dyn Fn(T5) -> T6>, accessor: crate::hydra::accessors::TermAccessor, val: T2, term: T3| {
      let r = rec.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
        accessor.clone()])), val.clone(), term.clone()) ;
      (crate::hydra::lib::pairs::first(r.clone()), cons.clone()(crate::hydra::lib::pairs::second(r.clone())))} ;
    {
      let for_many_with_accessors = |rec: Rc<dyn Fn(Vec<crate::hydra::accessors::TermAccessor>) -> Rc<dyn Fn(T2) -> Rc<dyn Fn(T3) -> (T2, T4)>>>, cons: Rc<dyn Fn(Vec<T4>) -> T5>, val: T2, accessor_term_pairs: Vec<(crate::hydra::accessors::TermAccessor, T3)>| {
        let rr = crate::hydra::lib::lists::foldl(|r: (T2, Vec<T4>), atp: (crate::hydra::accessors::TermAccessor, T3)| {
          let r2 = rec.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
            crate::hydra::lib::pairs::first(atp.clone())])), crate::hydra::lib::pairs::first(r.clone()), crate::hydra::lib::pairs::second(atp.clone())) ;
          (crate::hydra::lib::pairs::first(r2.clone()), crate::hydra::lib::lists::cons(crate::hydra::lib::pairs::second(r2.clone()), crate::hydra::lib::pairs::second(r.clone())))}, (val.clone(), Vec::from([])), accessor_term_pairs.clone()) ;
        (crate::hydra::lib::pairs::first(rr.clone()), cons.clone()(crate::hydra::lib::lists::reverse(crate::hydra::lib::pairs::second(rr.clone()))))} ;
      {
        let for_field_with_accessor = |mk_accessor: Rc<dyn Fn(crate::hydra::core::Name) -> crate::hydra::accessors::TermAccessor>, val: T1, field: crate::hydra::core::Field| {
          let r = recurse.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
            mk_accessor.clone()(field.clone().0.name.clone())])), val.clone(), field.clone().0.term.clone()) ;
          (crate::hydra::lib::pairs::first(r.clone()), crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
            name: field.clone().0.name.clone(),
            term: crate::hydra::lib::pairs::second(r.clone())})))} ;
        {
          let for_fields_with_accessor = |mk_accessor: Rc<dyn Fn(crate::hydra::core::Name) -> crate::hydra::accessors::TermAccessor>, v1: T1, v2: Vec<(crate::hydra::accessors::TermAccessor, crate::hydra::core::Field)>| for_many_with_accessors.clone()(|path1: Vec<crate::hydra::accessors::TermAccessor>, val1: T1, field1: crate::hydra::core::Field| for_field_with_accessor.clone()(mk_accessor.clone(), val1.clone(), field1.clone()), |x: Vec<crate::hydra::core::Field>| x.clone(), v1.clone(), v2.clone()) ;
          {
            let for_pair_with_accessors = |key_accessor: crate::hydra::accessors::TermAccessor, val_accessor: crate::hydra::accessors::TermAccessor, val: T1, kv: (crate::hydra::core::Term, crate::hydra::core::Term)| {
              let rk = recurse.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
                key_accessor.clone()])), val.clone(), crate::hydra::lib::pairs::first(kv.clone())) ;
              {
                let rv = recurse.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
                  val_accessor.clone()])), crate::hydra::lib::pairs::first(rk.clone()), crate::hydra::lib::pairs::second(kv.clone())) ;
                (crate::hydra::lib::pairs::first(rv.clone()), (crate::hydra::lib::pairs::second(rk.clone()), crate::hydra::lib::pairs::second(rv.clone())))}} ;
            {
              let for_binding_with_accessor = |val: T1, binding: crate::hydra::core::Binding| {
                let r = recurse.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
                  crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::LetBinding(binding.clone().0.name.clone())))])), val.clone(), binding.clone().0.term.clone()) ;
                (crate::hydra::lib::pairs::first(r.clone()), crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                  name: binding.clone().0.name.clone(),
                  term: crate::hydra::lib::pairs::second(r.clone()),
                  type_: binding.clone().0.type_.clone()})))} ;
              {
                let for_elimination = |val: T1, elm: crate::hydra::core::Elimination| {
                  let r = match &*elm.clone().0 {
                    crate::hydra::core::Elimination_Variant::Union (v0_) => {
                      let v0_ = v0_.clone() ;
                      {
                        let rmd = crate::hydra::lib::maybes::map(|def: crate::hydra::core::Term| recurse.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
                          crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::UnionCasesDefault))])), val.clone(), def.clone()), v0_.clone().0.default_.clone()) ;
                        {
                          let val1 = crate::hydra::lib::maybes::maybe(val.clone(), crate::hydra::lib::pairs::first, rmd.clone()) ;
                          {
                            let rcases = for_many_with_accessors.clone()(recurse.clone(), |x: Vec<crate::hydra::core::Term>| x.clone(), val1.clone(), crate::hydra::lib::lists::map(|f2: crate::hydra::core::Field| (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::UnionCasesBranch(f2.clone().0.name.clone()))), f2.clone().0.term.clone()), v0_.clone().0.cases.clone())) ;
                            (crate::hydra::lib::pairs::first(rcases.clone()), crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                              type_name: v0_.clone().0.type_name.clone(),
                              default_: crate::hydra::lib::maybes::map(crate::hydra::lib::pairs::second, rmd.clone()),
                              cases: crate::hydra::lib::lists::map(|ft: (crate::hydra::core::Name, crate::hydra::core::Term)| crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                                name: crate::hydra::lib::pairs::first(ft.clone()),
                                term: crate::hydra::lib::pairs::second(ft.clone())})), crate::hydra::lib::lists::zip(crate::hydra::lib::lists::map(|v| v.0.name.clone(), v0_.clone().0.cases.clone()), crate::hydra::lib::pairs::second(rcases.clone())))}))))))}}}},
                    _ => (val.clone(), elm.clone())} ;
                  (crate::hydra::lib::pairs::first(r.clone()), crate::hydra::lib::pairs::second(r.clone()))} ;
                {
                  let for_function = |val: T1, fun: crate::hydra::core::Function| match &*fun.clone().0 {
                    crate::hydra::core::Function_Variant::Elimination (v0_) => {
                      let v0_ = v0_.clone() ;
                      {
                        let re = for_elimination.clone()(val.clone(), v0_.clone()) ;
                        (crate::hydra::lib::pairs::first(re.clone()), crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::lib::pairs::second(re.clone())))))}},
                    crate::hydra::core::Function_Variant::Lambda (v0_) => {
                      let v0_ = v0_.clone() ;
                      {
                        let rl = recurse.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
                          crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::LambdaBody))])), val.clone(), v0_.clone().0.body.clone()) ;
                        (crate::hydra::lib::pairs::first(rl.clone()), crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                          parameter: v0_.clone().0.parameter.clone(),
                          domain: v0_.clone().0.domain.clone(),
                          body: crate::hydra::lib::pairs::second(rl.clone())}))))))}},
                    _ => (val.clone(), fun.clone())} ;
                  {
                    let dflt = (val0.clone(), term02.clone()) ;
                    match &*term02.clone().0 {
                      crate::hydra::core::Term_Variant::Annotated (v0_) => {
                        let v0_ = v0_.clone() ;
                        for_single_with_accessor.clone()(recurse.clone(), |t: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
                          body: t.clone(),
                          annotation: v0_.clone().0.annotation.clone()}))))), crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::AnnotatedBody)), val0.clone(), v0_.clone().0.body.clone())},
                      crate::hydra::core::Term_Variant::Application (v0_) => {
                        let v0_ = v0_.clone() ;
                        {
                          let rlhs = recurse.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
                            crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::ApplicationFunction))])), val0.clone(), v0_.clone().0.function.clone()) ;
                          {
                            let rrhs = recurse.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
                              crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::ApplicationArgument))])), crate::hydra::lib::pairs::first(rlhs.clone()), v0_.clone().0.argument.clone()) ;
                            (crate::hydra::lib::pairs::first(rrhs.clone()), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                              function: crate::hydra::lib::pairs::second(rlhs.clone()),
                              argument: crate::hydra::lib::pairs::second(rrhs.clone())}))))))}}},
                      crate::hydra::core::Term_Variant::Either (v0_) => {
                        let v0_ = v0_.clone() ;
                        crate::hydra::lib::eithers::either(|l: crate::hydra::core::Term| {
                          let rl = recurse.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
                            crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::SumTerm))])), val0.clone(), l.clone()) ;
                          (crate::hydra::lib::pairs::first(rl.clone()), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Left(crate::hydra::lib::pairs::second(rl.clone()))))))}, |r: crate::hydra::core::Term| {
                          let rr = recurse.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
                            crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::SumTerm))])), val0.clone(), r.clone()) ;
                          (crate::hydra::lib::pairs::first(rr.clone()), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(Right(crate::hydra::lib::pairs::second(rr.clone()))))))}, v0_.clone())},
                      crate::hydra::core::Term_Variant::Function (v0_) => {
                        let v0_ = v0_.clone() ;
                        {
                          let rf = for_function.clone()(val0.clone(), v0_.clone()) ;
                          (crate::hydra::lib::pairs::first(rf.clone()), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::lib::pairs::second(rf.clone())))))}},
                      crate::hydra::core::Term_Variant::Let (v0_) => {
                        let v0_ = v0_.clone() ;
                        {
                          let renv = recurse.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
                            crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::LetBody))])), val0.clone(), v0_.clone().0.body.clone()) ;
                          {
                            let rbindings = crate::hydra::lib::lists::foldl(|r: (T1, Vec<crate::hydra::core::Binding>), binding: crate::hydra::core::Binding| {
                              let rb = for_binding_with_accessor.clone()(crate::hydra::lib::pairs::first(r.clone()), binding.clone()) ;
                              (crate::hydra::lib::pairs::first(rb.clone()), crate::hydra::lib::lists::cons(crate::hydra::lib::pairs::second(rb.clone()), crate::hydra::lib::pairs::second(r.clone())))}, (crate::hydra::lib::pairs::first(renv.clone()), Vec::from([])), v0_.clone().0.bindings.clone()) ;
                            (crate::hydra::lib::pairs::first(rbindings.clone()), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
                              bindings: crate::hydra::lib::lists::reverse(crate::hydra::lib::pairs::second(rbindings.clone())),
                              body: crate::hydra::lib::pairs::second(renv.clone())}))))))}}},
                      crate::hydra::core::Term_Variant::List (v0_) => {
                        let v0_ = v0_.clone() ;
                        {
                          let idx = 0i32 ;
                          {
                            let rr = crate::hydra::lib::lists::foldl(|r: (i32, (T1, Vec<crate::hydra::core::Term>)), el: crate::hydra::core::Term| {
                              let r2 = recurse.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
                                crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::ListElement(crate::hydra::lib::pairs::first(r.clone()))))])), crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(r.clone())), el.clone()) ;
                              (crate::hydra::lib::math::add(crate::hydra::lib::pairs::first(r.clone()), 1i32), (crate::hydra::lib::pairs::first(r2.clone()), crate::hydra::lib::lists::cons(crate::hydra::lib::pairs::second(r2.clone()), crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(r.clone())))))}, (idx.clone(), (val0.clone(), Vec::from([]))), v0_.clone()) ;
                            (crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(rr.clone())), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::reverse(crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(rr.clone())))))))}}},
                      crate::hydra::core::Term_Variant::Map (v0_) => {
                        let v0_ = v0_.clone() ;
                        {
                          let idx = 0i32 ;
                          {
                            let rr = crate::hydra::lib::lists::foldl(|r: (i32, (T1, Vec<(crate::hydra::core::Term, crate::hydra::core::Term)>)), kv: (crate::hydra::core::Term, crate::hydra::core::Term)| {
                              let rk = recurse.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
                                crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::MapKey(crate::hydra::lib::pairs::first(r.clone()))))])), crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(r.clone())), crate::hydra::lib::pairs::first(kv.clone())) ;
                              {
                                let rv = recurse.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
                                  crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::MapValue(crate::hydra::lib::pairs::first(r.clone()))))])), crate::hydra::lib::pairs::first(rk.clone()), crate::hydra::lib::pairs::second(kv.clone())) ;
                                (crate::hydra::lib::math::add(crate::hydra::lib::pairs::first(r.clone()), 1i32), (crate::hydra::lib::pairs::first(rv.clone()), crate::hydra::lib::lists::cons((crate::hydra::lib::pairs::second(rk.clone()), crate::hydra::lib::pairs::second(rv.clone())), crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(r.clone())))))}}, (idx.clone(), (val0.clone(), Vec::from([]))), crate::hydra::lib::maps::to_list(v0_.clone())) ;
                            (crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(rr.clone())), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::reverse(crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(rr.clone()))))))))}}},
                      crate::hydra::core::Term_Variant::Maybe (v0_) => {
                        let v0_ = v0_.clone() ;
                        crate::hydra::lib::maybes::maybe(dflt.clone(), |t: crate::hydra::core::Term| for_single_with_accessor.clone()(recurse.clone(), |t1: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(Some(t1.clone())))), crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::MaybeTerm)), val0.clone(), t.clone()), v0_.clone())},
                      crate::hydra::core::Term_Variant::Pair (v0_) => {
                        let v0_ = v0_.clone() ;
                        {
                          let rf = recurse.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
                            crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::ProductTerm(0i32)))])), val0.clone(), crate::hydra::lib::pairs::first(v0_.clone())) ;
                          {
                            let rs = recurse.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
                              crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::ProductTerm(1i32)))])), crate::hydra::lib::pairs::first(rf.clone()), crate::hydra::lib::pairs::second(v0_.clone())) ;
                            (crate::hydra::lib::pairs::first(rs.clone()), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Pair((crate::hydra::lib::pairs::second(rf.clone()), crate::hydra::lib::pairs::second(rs.clone()))))))}}},
                      crate::hydra::core::Term_Variant::Record (v0_) => {
                        let v0_ = v0_.clone() ;
                        {
                          let rfields = for_many_with_accessors.clone()(recurse.clone(), |x: Vec<crate::hydra::core::Term>| x.clone(), val0.clone(), crate::hydra::lib::lists::map(|f2: crate::hydra::core::Field| (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::RecordField(f2.clone().0.name.clone()))), f2.clone().0.term.clone()), v0_.clone().0.fields.clone())) ;
                          (crate::hydra::lib::pairs::first(rfields.clone()), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
                            type_name: v0_.clone().0.type_name.clone(),
                            fields: crate::hydra::lib::lists::map(|ft: (crate::hydra::core::Name, crate::hydra::core::Term)| crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                              name: crate::hydra::lib::pairs::first(ft.clone()),
                              term: crate::hydra::lib::pairs::second(ft.clone())})), crate::hydra::lib::lists::zip(crate::hydra::lib::lists::map(|v| v.0.name.clone(), v0_.clone().0.fields.clone()), crate::hydra::lib::pairs::second(rfields.clone())))}))))))}},
                      crate::hydra::core::Term_Variant::Set (v0_) => {
                        let v0_ = v0_.clone() ;
                        {
                          let idx = 0i32 ;
                          {
                            let rr = crate::hydra::lib::lists::foldl(|r: (i32, (T1, Vec<crate::hydra::core::Term>)), el: crate::hydra::core::Term| {
                              let r2 = recurse.clone()(crate::hydra::lib::lists::concat2(path.clone(), Vec::from([
                                crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::SetElement(crate::hydra::lib::pairs::first(r.clone()))))])), crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(r.clone())), el.clone()) ;
                              (crate::hydra::lib::math::add(crate::hydra::lib::pairs::first(r.clone()), 1i32), (crate::hydra::lib::pairs::first(r2.clone()), crate::hydra::lib::lists::cons(crate::hydra::lib::pairs::second(r2.clone()), crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(r.clone())))))}, (idx.clone(), (val0.clone(), Vec::from([]))), crate::hydra::lib::sets::to_list(v0_.clone())) ;
                            (crate::hydra::lib::pairs::first(crate::hydra::lib::pairs::second(rr.clone())), crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Set(crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::reverse(crate::hydra::lib::pairs::second(crate::hydra::lib::pairs::second(rr.clone()))))))))}}},
                      crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
                        let v0_ = v0_.clone() ;
                        for_single_with_accessor.clone()(recurse.clone(), |t: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
                          body: t.clone(),
                          type_: v0_.clone().0.type_.clone()}))))), crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::TypeApplicationTerm)), val0.clone(), v0_.clone().0.body.clone())},
                      crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
                        let v0_ = v0_.clone() ;
                        for_single_with_accessor.clone()(recurse.clone(), |t: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeLambda(crate::hydra::core::TypeLambda(Rc::new(crate::hydra::core::TypeLambda_Variant {
                          parameter: v0_.clone().0.parameter.clone(),
                          body: t.clone()}))))), crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::TypeLambdaBody)), val0.clone(), v0_.clone().0.body.clone())},
                      crate::hydra::core::Term_Variant::Union (v0_) => {
                        let v0_ = v0_.clone() ;
                        for_single_with_accessor.clone()(recurse.clone(), |t: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
                          type_name: v0_.clone().0.type_name.clone(),
                          field: crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
                            name: v0_.clone().0.field.clone().0.name.clone(),
                            term: t.clone()}))}))))), crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::InjectionTerm)), val0.clone(), v0_.clone().0.field.clone().0.term.clone())},
                      crate::hydra::core::Term_Variant::Wrap (v0_) => {
                        let v0_ = v0_.clone() ;
                        for_single_with_accessor.clone()(recurse.clone(), |t: crate::hydra::core::Term| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                          type_name: v0_.clone().0.type_name.clone(),
                          body: t.clone()}))))), crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::WrappedTerm)), val0.clone(), v0_.clone().0.body.clone())},
                      _ => dflt.clone()}}}}}}}}}} ;
  let recurse = |v1: Vec<crate::hydra::accessors::TermAccessor>, v2: T0, v3: crate::hydra::core::Term| f.clone()(|v12: Vec<crate::hydra::accessors::TermAccessor>, v22: T0, v32: crate::hydra::core::Term| fsub.clone()(recurse.clone(), v12.clone(), v22.clone(), v32.clone()), v1.clone(), v2.clone(), v3.clone()) ;
  recurse.clone()(Vec::from([]), term0.clone(), v1.clone())}

pub fn rewrite_term(f: impl Fn(Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>) -> Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term> + Clone, term0: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let fsub = |recurse: Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>, term: crate::hydra::core::Term| {
    let for_field = |f2: crate::hydra::core::Field| crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
      name: f2.clone().0.name.clone(),
      term: recurse.clone()(f2.clone().0.term.clone())})) ;
    {
      let for_elimination = |elm: crate::hydra::core::Elimination| match &*elm.clone().0 {
        crate::hydra::core::Elimination_Variant::Record (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Record(v0_.clone())))},
        crate::hydra::core::Elimination_Variant::Union (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
            type_name: v0_.clone().0.type_name.clone(),
            default_: crate::hydra::lib::maybes::map(recurse.clone(), v0_.clone().0.default_.clone()),
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
              body: recurse.clone()(v0_.clone().0.body.clone())})))))},
          crate::hydra::core::Function_Variant::Primitive (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(v0_.clone())))}} ;
        {
          let for_let = |lt: crate::hydra::core::Let| {
            let map_binding = |b: crate::hydra::core::Binding| crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
              name: b.clone().0.name.clone(),
              term: recurse.clone()(b.clone().0.term.clone()),
              type_: b.clone().0.type_.clone()})) ;
            crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
              bindings: crate::hydra::lib::lists::map(map_binding.clone(), lt.clone().0.bindings.clone()),
              body: recurse.clone()(lt.clone().0.body.clone())}))} ;
          {
            let for_map = |m: BTreeMap<crate::hydra::core::Term, crate::hydra::core::Term>| {
              let for_pair = |p: (crate::hydra::core::Term, crate::hydra::core::Term)| (recurse.clone()(crate::hydra::lib::pairs::first(p.clone())), recurse.clone()(crate::hydra::lib::pairs::second(p.clone()))) ;
              crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(for_pair.clone(), crate::hydra::lib::maps::to_list(m.clone())))} ;
            match &*term.clone().0 {
              crate::hydra::core::Term_Variant::Annotated (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
                  body: recurse.clone()(v0_.clone().0.body.clone()),
                  annotation: v0_.clone().0.annotation.clone()})))))},
              crate::hydra::core::Term_Variant::Application (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                  function: recurse.clone()(v0_.clone().0.function.clone()),
                  argument: recurse.clone()(v0_.clone().0.argument.clone())})))))},
              crate::hydra::core::Term_Variant::Either (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(crate::hydra::lib::eithers::either(|l: crate::hydra::core::Term| Left(recurse.clone()(l.clone())), |r: crate::hydra::core::Term| Right(recurse.clone()(r.clone())), v0_.clone()))))},
              crate::hydra::core::Term_Variant::Function (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(for_function.clone()(v0_.clone()))))},
              crate::hydra::core::Term_Variant::Let (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(for_let.clone()(v0_.clone()))))},
              crate::hydra::core::Term_Variant::List (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(recurse.clone(), v0_.clone()))))},
              crate::hydra::core::Term_Variant::Literal (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(v0_.clone())))},
              crate::hydra::core::Term_Variant::Map (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(for_map.clone()(v0_.clone()))))},
              crate::hydra::core::Term_Variant::Maybe (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(crate::hydra::lib::maybes::map(recurse.clone(), v0_.clone()))))},
              crate::hydra::core::Term_Variant::Pair (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Pair((recurse.clone()(crate::hydra::lib::pairs::first(v0_.clone())), recurse.clone()(crate::hydra::lib::pairs::second(v0_.clone()))))))},
              crate::hydra::core::Term_Variant::Record (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
                  type_name: v0_.clone().0.type_name.clone(),
                  fields: crate::hydra::lib::lists::map(for_field.clone(), v0_.clone().0.fields.clone())})))))},
              crate::hydra::core::Term_Variant::Set (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Set(crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::map(recurse.clone(), crate::hydra::lib::sets::to_list(v0_.clone()))))))},
              crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
                  body: recurse.clone()(v0_.clone().0.body.clone()),
                  type_: v0_.clone().0.type_.clone()})))))},
              crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
                let v0_ = v0_.clone() ;
                crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeLambda(crate::hydra::core::TypeLambda(Rc::new(crate::hydra::core::TypeLambda_Variant {
                  parameter: v0_.clone().0.parameter.clone(),
                  body: recurse.clone()(v0_.clone().0.body.clone())})))))},
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
                  body: recurse.clone()(v0_.clone().0.body.clone())})))))}}}}}}} ;
  let recurse = |v1: crate::hydra::core::Term| f.clone()(|v12: crate::hydra::core::Term| fsub.clone()(recurse.clone(), v12.clone()), v1.clone()) ;
  recurse.clone()(term0.clone())}

pub fn rewrite_term_m(f: impl Fn(Rc<dyn Fn(crate::hydra::core::Term) -> Either<T0, crate::hydra::core::Term>>) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<T0, crate::hydra::core::Term>> + Clone, term0: crate::hydra::core::Term) -> Either<T0, crate::hydra::core::Term> {
  let fsub = |recurse: Rc<dyn Fn(crate::hydra::core::Term) -> Either<T1, crate::hydra::core::Term>>, term: crate::hydra::core::Term| {
    let for_field = |field: crate::hydra::core::Field| crate::hydra::lib::eithers::bind(recurse.clone()(field.clone().0.term.clone()), |t: crate::hydra::core::Term| Right(crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
      name: field.clone().0.name.clone(),
      term: t.clone()})))) ;
    {
      let for_pair = |kv: (crate::hydra::core::Term, crate::hydra::core::Term)| crate::hydra::lib::eithers::bind(recurse.clone()(crate::hydra::lib::pairs::first(kv.clone())), |k: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(recurse.clone()(crate::hydra::lib::pairs::second(kv.clone())), |v: crate::hydra::core::Term| Right((k.clone(), v.clone())))) ;
      {
        let map_binding = |b: crate::hydra::core::Binding| crate::hydra::lib::eithers::bind(recurse.clone()(b.clone().0.term.clone()), |v: crate::hydra::core::Term| Right(crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
          name: b.clone().0.name.clone(),
          term: v.clone(),
          type_: b.clone().0.type_.clone()})))) ;
        match &*term.clone().0 {
          crate::hydra::core::Term_Variant::Annotated (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.body.clone()), |ex: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
              body: ex.clone(),
              annotation: v0_.clone().0.annotation.clone()})))))))},
          crate::hydra::core::Term_Variant::Application (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.function.clone()), |lhs: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.argument.clone()), |rhs: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
              function: lhs.clone(),
              argument: rhs.clone()}))))))))},
          crate::hydra::core::Term_Variant::Either (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::either(|l: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|x: crate::hydra::core::Term| Left(x.clone()), recurse.clone()(l.clone())), |r: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|x: crate::hydra::core::Term| Right(x.clone()), recurse.clone()(r.clone())), v0_.clone()), |re: Either<crate::hydra::core::Term, crate::hydra::core::Term>| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(re.clone())))))},
          crate::hydra::core::Term_Variant::Function (v0_) => {
            let v0_ = v0_.clone() ;
            {
              let for_elm = |e: crate::hydra::core::Elimination| match &*e.clone().0 {
                crate::hydra::core::Elimination_Variant::Record (v0_) => {
                  let v0_ = v0_.clone() ;
                  Right(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Record(v0_.clone())))))))},
                crate::hydra::core::Elimination_Variant::Union (v0_) => {
                  let v0_ = v0_.clone() ;
                  {
                    let n = v0_.clone().0.type_name.clone() ;
                    {
                      let def = v0_.clone().0.default_.clone() ;
                      {
                        let cases = v0_.clone().0.cases.clone() ;
                        crate::hydra::lib::eithers::bind(crate::hydra::lib::maybes::maybe(Right(None), |t: crate::hydra::core::Term| crate::hydra::lib::eithers::map(crate::hydra::lib::maybes::pure, recurse.clone()(t.clone())), def.clone()), |rdef: Option<crate::hydra::core::Term>| crate::hydra::lib::eithers::map(|rcases: Vec<crate::hydra::core::Field>| crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                          type_name: n.clone(),
                          default_: rdef.clone(),
                          cases: rcases.clone()})))))))), crate::hydra::lib::eithers::map_list(for_field.clone(), cases.clone())))}}}},
                crate::hydra::core::Elimination_Variant::Wrap (v0_) => {
                  let v0_ = v0_.clone() ;
                  Right(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Wrap(v0_.clone())))))))}} ;
              {
                let for_fun = |fun2: crate::hydra::core::Function| match &*fun2.clone().0 {
                  crate::hydra::core::Function_Variant::Elimination (v0_) => {
                    let v0_ = v0_.clone() ;
                    for_elm.clone()(v0_.clone())},
                  crate::hydra::core::Function_Variant::Lambda (v0_) => {
                    let v0_ = v0_.clone() ;
                    {
                      let v = v0_.clone().0.parameter.clone() ;
                      {
                        let d = v0_.clone().0.domain.clone() ;
                        {
                          let body = v0_.clone().0.body.clone() ;
                          crate::hydra::lib::eithers::bind(recurse.clone()(body.clone()), |rbody: crate::hydra::core::Term| Right(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                            parameter: v.clone(),
                            domain: d.clone(),
                            body: rbody.clone()})))))))}}}},
                  crate::hydra::core::Function_Variant::Primitive (v0_) => {
                    let v0_ = v0_.clone() ;
                    Right(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(v0_.clone()))))}} ;
                crate::hydra::lib::eithers::bind(for_fun.clone()(v0_.clone()), |rfun: crate::hydra::core::Function| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(rfun.clone())))))}}},
          crate::hydra::core::Term_Variant::Let (v0_) => {
            let v0_ = v0_.clone() ;
            {
              let bindings = v0_.clone().0.bindings.clone() ;
              {
                let env = v0_.clone().0.body.clone() ;
                crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(map_binding.clone(), bindings.clone()), |rbindings: Vec<crate::hydra::core::Binding>| crate::hydra::lib::eithers::bind(recurse.clone()(env.clone()), |renv: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
                  bindings: rbindings.clone(),
                  body: renv.clone()}))))))))}}},
          crate::hydra::core::Term_Variant::List (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(recurse.clone(), v0_.clone()), |rels: Vec<crate::hydra::core::Term>| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(rels.clone())))))},
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(v0_.clone()))))},
          crate::hydra::core::Term_Variant::Map (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(for_pair.clone(), crate::hydra::lib::maps::to_list(v0_.clone())), |pairs: Vec<(crate::hydra::core::Term, crate::hydra::core::Term)>| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(crate::hydra::lib::maps::from_list(pairs.clone()))))))},
          crate::hydra::core::Term_Variant::Maybe (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_maybe(recurse.clone(), v0_.clone()), |rm: Option<crate::hydra::core::Term>| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(rm.clone())))))},
          crate::hydra::core::Term_Variant::Pair (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::lib::eithers::bind(recurse.clone()(crate::hydra::lib::pairs::first(v0_.clone())), |rf: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(recurse.clone()(crate::hydra::lib::pairs::second(v0_.clone())), |rs: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Pair((rf.clone(), rs.clone())))))))},
          crate::hydra::core::Term_Variant::Record (v0_) => {
            let v0_ = v0_.clone() ;
            {
              let n = v0_.clone().0.type_name.clone() ;
              {
                let fields = v0_.clone().0.fields.clone() ;
                crate::hydra::lib::eithers::map(|rfields: Vec<crate::hydra::core::Field>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
                  type_name: n.clone(),
                  fields: rfields.clone()}))))), crate::hydra::lib::eithers::map_list(for_field.clone(), fields.clone()))}}},
          crate::hydra::core::Term_Variant::Set (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(recurse.clone(), crate::hydra::lib::sets::to_list(v0_.clone())), |rlist: Vec<crate::hydra::core::Term>| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Set(crate::hydra::lib::sets::from_list(rlist.clone()))))))},
          crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.body.clone()), |t: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
              body: t.clone(),
              type_: v0_.clone().0.type_.clone()})))))))},
          crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
            let v0_ = v0_.clone() ;
            {
              let v = v0_.clone().0.parameter.clone() ;
              {
                let body = v0_.clone().0.body.clone() ;
                crate::hydra::lib::eithers::bind(recurse.clone()(body.clone()), |rbody: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeLambda(crate::hydra::core::TypeLambda(Rc::new(crate::hydra::core::TypeLambda_Variant {
                  parameter: v.clone(),
                  body: rbody.clone()})))))))}}},
          crate::hydra::core::Term_Variant::Union (v0_) => {
            let v0_ = v0_.clone() ;
            {
              let n = v0_.clone().0.type_name.clone() ;
              {
                let field = v0_.clone().0.field.clone() ;
                crate::hydra::lib::eithers::map(|rfield: crate::hydra::core::Field| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
                  type_name: n.clone(),
                  field: rfield.clone()}))))), for_field.clone()(field.clone()))}}},
          crate::hydra::core::Term_Variant::Unit (v0_) => {
            let v0_ = v0_.clone() ;
            Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit)))},
          crate::hydra::core::Term_Variant::Variable (v0_) => {
            let v0_ = v0_.clone() ;
            Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(v0_.clone()))))},
          crate::hydra::core::Term_Variant::Wrap (v0_) => {
            let v0_ = v0_.clone() ;
            {
              let name = v0_.clone().0.type_name.clone() ;
              {
                let t = v0_.clone().0.body.clone() ;
                crate::hydra::lib::eithers::bind(recurse.clone()(t.clone()), |rt: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                  type_name: name.clone(),
                  body: rt.clone()})))))))}}}}}}} ;
  let recurse = |v1: crate::hydra::core::Term| f.clone()(|v12: crate::hydra::core::Term| fsub.clone()(recurse.clone(), v12.clone()), v1.clone()) ;
  recurse.clone()(term0.clone())}

pub fn rewrite_term_with_context(f: impl Fn(Rc<dyn Fn(T0) -> Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>>) -> Rc<dyn Fn(T0) -> Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>> + Clone, cx0: T0, term0: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let for_subterms = |recurse0: Rc<dyn Fn(T1) -> Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>>, cx: T1, term: crate::hydra::core::Term| {
    let recurse = |v1: crate::hydra::core::Term| recurse0.clone()(cx.clone(), v1.clone()) ;
    {
      let for_field = |field: crate::hydra::core::Field| crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: field.clone().0.name.clone(),
        term: recurse.clone()(field.clone().0.term.clone())})) ;
      {
        let for_elimination = |elm: crate::hydra::core::Elimination| match &*elm.clone().0 {
          crate::hydra::core::Elimination_Variant::Record (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Record(v0_.clone())))},
          crate::hydra::core::Elimination_Variant::Union (v0_) => {
            let v0_ = v0_.clone() ;
            crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
              type_name: v0_.clone().0.type_name.clone(),
              default_: crate::hydra::lib::maybes::map(recurse.clone(), v0_.clone().0.default_.clone()),
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
                body: recurse.clone()(v0_.clone().0.body.clone())})))))},
            crate::hydra::core::Function_Variant::Primitive (v0_) => {
              let v0_ = v0_.clone() ;
              crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(v0_.clone())))}} ;
          {
            let for_let = |lt: crate::hydra::core::Let| {
              let map_binding = |b: crate::hydra::core::Binding| crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                name: b.clone().0.name.clone(),
                term: recurse.clone()(b.clone().0.term.clone()),
                type_: b.clone().0.type_.clone()})) ;
              crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
                bindings: crate::hydra::lib::lists::map(map_binding.clone(), lt.clone().0.bindings.clone()),
                body: recurse.clone()(lt.clone().0.body.clone())}))} ;
            {
              let for_map = |m: BTreeMap<crate::hydra::core::Term, crate::hydra::core::Term>| {
                let for_pair = |p: (crate::hydra::core::Term, crate::hydra::core::Term)| (recurse.clone()(crate::hydra::lib::pairs::first(p.clone())), recurse.clone()(crate::hydra::lib::pairs::second(p.clone()))) ;
                crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::map(for_pair.clone(), crate::hydra::lib::maps::to_list(m.clone())))} ;
              match &*term.clone().0 {
                crate::hydra::core::Term_Variant::Annotated (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
                    body: recurse.clone()(v0_.clone().0.body.clone()),
                    annotation: v0_.clone().0.annotation.clone()})))))},
                crate::hydra::core::Term_Variant::Application (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: recurse.clone()(v0_.clone().0.function.clone()),
                    argument: recurse.clone()(v0_.clone().0.argument.clone())})))))},
                crate::hydra::core::Term_Variant::Either (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(crate::hydra::lib::eithers::either(|l: crate::hydra::core::Term| Left(recurse.clone()(l.clone())), |r: crate::hydra::core::Term| Right(recurse.clone()(r.clone())), v0_.clone()))))},
                crate::hydra::core::Term_Variant::Function (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(for_function.clone()(v0_.clone()))))},
                crate::hydra::core::Term_Variant::Let (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(for_let.clone()(v0_.clone()))))},
                crate::hydra::core::Term_Variant::List (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(crate::hydra::lib::lists::map(recurse.clone(), v0_.clone()))))},
                crate::hydra::core::Term_Variant::Literal (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(v0_.clone())))},
                crate::hydra::core::Term_Variant::Map (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(for_map.clone()(v0_.clone()))))},
                crate::hydra::core::Term_Variant::Maybe (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(crate::hydra::lib::maybes::map(recurse.clone(), v0_.clone()))))},
                crate::hydra::core::Term_Variant::Pair (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Pair((recurse.clone()(crate::hydra::lib::pairs::first(v0_.clone())), recurse.clone()(crate::hydra::lib::pairs::second(v0_.clone()))))))},
                crate::hydra::core::Term_Variant::Record (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
                    type_name: v0_.clone().0.type_name.clone(),
                    fields: crate::hydra::lib::lists::map(for_field.clone(), v0_.clone().0.fields.clone())})))))},
                crate::hydra::core::Term_Variant::Set (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Set(crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::map(recurse.clone(), crate::hydra::lib::sets::to_list(v0_.clone()))))))},
                crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
                    body: recurse.clone()(v0_.clone().0.body.clone()),
                    type_: v0_.clone().0.type_.clone()})))))},
                crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeLambda(crate::hydra::core::TypeLambda(Rc::new(crate::hydra::core::TypeLambda_Variant {
                    parameter: v0_.clone().0.parameter.clone(),
                    body: recurse.clone()(v0_.clone().0.body.clone())})))))},
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
                    body: recurse.clone()(v0_.clone().0.body.clone())})))))}}}}}}}} ;
  let rewrite = |cx: T0, term: crate::hydra::core::Term| f.clone()(|v1: T0, v2: crate::hydra::core::Term| for_subterms.clone()(rewrite.clone(), v1.clone(), v2.clone()), cx.clone(), term.clone()) ;
  rewrite.clone()(cx0.clone(), term0.clone())}

pub fn rewrite_term_with_context_m(f: impl Fn(Rc<dyn Fn(T0) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<T1, crate::hydra::core::Term>>>) -> Rc<dyn Fn(T0) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<T1, crate::hydra::core::Term>>> + Clone, cx0: T0, term0: crate::hydra::core::Term) -> Either<T1, crate::hydra::core::Term> {
  let for_subterms = |recurse0: Rc<dyn Fn(T2) -> Rc<dyn Fn(crate::hydra::core::Term) -> Either<T3, crate::hydra::core::Term>>>, cx: T2, term: crate::hydra::core::Term| {
    let recurse = |v1: crate::hydra::core::Term| recurse0.clone()(cx.clone(), v1.clone()) ;
    {
      let for_field = |field: crate::hydra::core::Field| crate::hydra::lib::eithers::bind(recurse.clone()(field.clone().0.term.clone()), |t: crate::hydra::core::Term| Right(crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
        name: field.clone().0.name.clone(),
        term: t.clone()})))) ;
      {
        let for_pair = |kv: (crate::hydra::core::Term, crate::hydra::core::Term)| crate::hydra::lib::eithers::bind(recurse.clone()(crate::hydra::lib::pairs::first(kv.clone())), |k: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(recurse.clone()(crate::hydra::lib::pairs::second(kv.clone())), |v: crate::hydra::core::Term| Right((k.clone(), v.clone())))) ;
        {
          let for_elimination = |e: crate::hydra::core::Elimination| match &*e.clone().0 {
            crate::hydra::core::Elimination_Variant::Record (v0_) => {
              let v0_ = v0_.clone() ;
              Right(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Record(v0_.clone())))))))},
            crate::hydra::core::Elimination_Variant::Union (v0_) => {
              let v0_ = v0_.clone() ;
              {
                let n = v0_.clone().0.type_name.clone() ;
                {
                  let def = v0_.clone().0.default_.clone() ;
                  {
                    let cases = v0_.clone().0.cases.clone() ;
                    crate::hydra::lib::eithers::bind(crate::hydra::lib::maybes::maybe(Right(None), |t: crate::hydra::core::Term| crate::hydra::lib::eithers::map(crate::hydra::lib::maybes::pure, recurse.clone()(t.clone())), def.clone()), |rdef: Option<crate::hydra::core::Term>| crate::hydra::lib::eithers::map(|rcases: Vec<crate::hydra::core::Field>| crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
                      type_name: n.clone(),
                      default_: rdef.clone(),
                      cases: rcases.clone()})))))))), crate::hydra::lib::eithers::map_list(for_field.clone(), cases.clone())))}}}},
            crate::hydra::core::Elimination_Variant::Wrap (v0_) => {
              let v0_ = v0_.clone() ;
              Right(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Wrap(v0_.clone())))))))}} ;
          {
            let for_function = |fun: crate::hydra::core::Function| match &*fun.clone().0 {
              crate::hydra::core::Function_Variant::Elimination (v0_) => {
                let v0_ = v0_.clone() ;
                for_elimination.clone()(v0_.clone())},
              crate::hydra::core::Function_Variant::Lambda (v0_) => {
                let v0_ = v0_.clone() ;
                {
                  let v = v0_.clone().0.parameter.clone() ;
                  {
                    let d = v0_.clone().0.domain.clone() ;
                    {
                      let body = v0_.clone().0.body.clone() ;
                      crate::hydra::lib::eithers::bind(recurse.clone()(body.clone()), |rbody: crate::hydra::core::Term| Right(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                        parameter: v.clone(),
                        domain: d.clone(),
                        body: rbody.clone()})))))))}}}},
              crate::hydra::core::Function_Variant::Primitive (v0_) => {
                let v0_ = v0_.clone() ;
                Right(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(v0_.clone()))))}} ;
            {
              let map_binding = |b: crate::hydra::core::Binding| crate::hydra::lib::eithers::bind(recurse.clone()(b.clone().0.term.clone()), |v: crate::hydra::core::Term| Right(crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
                name: b.clone().0.name.clone(),
                term: v.clone(),
                type_: b.clone().0.type_.clone()})))) ;
              match &*term.clone().0 {
                crate::hydra::core::Term_Variant::Annotated (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.body.clone()), |ex: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
                    body: ex.clone(),
                    annotation: v0_.clone().0.annotation.clone()})))))))},
                crate::hydra::core::Term_Variant::Application (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.function.clone()), |lhs: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.argument.clone()), |rhs: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
                    function: lhs.clone(),
                    argument: rhs.clone()}))))))))},
                crate::hydra::core::Term_Variant::Either (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::either(|l: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|x: crate::hydra::core::Term| Left(x.clone()), recurse.clone()(l.clone())), |r: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|x: crate::hydra::core::Term| Right(x.clone()), recurse.clone()(r.clone())), v0_.clone()), |re: Either<crate::hydra::core::Term, crate::hydra::core::Term>| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(re.clone())))))},
                crate::hydra::core::Term_Variant::Function (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::lib::eithers::bind(for_function.clone()(v0_.clone()), |rfun: crate::hydra::core::Function| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(rfun.clone())))))},
                crate::hydra::core::Term_Variant::Let (v0_) => {
                  let v0_ = v0_.clone() ;
                  {
                    let bindings = v0_.clone().0.bindings.clone() ;
                    {
                      let body = v0_.clone().0.body.clone() ;
                      crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(map_binding.clone(), bindings.clone()), |rbindings: Vec<crate::hydra::core::Binding>| crate::hydra::lib::eithers::bind(recurse.clone()(body.clone()), |rbody: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
                        bindings: rbindings.clone(),
                        body: rbody.clone()}))))))))}}},
                crate::hydra::core::Term_Variant::List (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(recurse.clone(), v0_.clone()), |rels: Vec<crate::hydra::core::Term>| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(rels.clone())))))},
                crate::hydra::core::Term_Variant::Literal (v0_) => {
                  let v0_ = v0_.clone() ;
                  Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(v0_.clone()))))},
                crate::hydra::core::Term_Variant::Map (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(for_pair.clone(), crate::hydra::lib::maps::to_list(v0_.clone())), |pairs: Vec<(crate::hydra::core::Term, crate::hydra::core::Term)>| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(crate::hydra::lib::maps::from_list(pairs.clone()))))))},
                crate::hydra::core::Term_Variant::Maybe (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_maybe(recurse.clone(), v0_.clone()), |rm: Option<crate::hydra::core::Term>| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(rm.clone())))))},
                crate::hydra::core::Term_Variant::Pair (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::lib::eithers::bind(recurse.clone()(crate::hydra::lib::pairs::first(v0_.clone())), |rfirst: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(recurse.clone()(crate::hydra::lib::pairs::second(v0_.clone())), |rsecond: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Pair((rfirst.clone(), rsecond.clone())))))))},
                crate::hydra::core::Term_Variant::Record (v0_) => {
                  let v0_ = v0_.clone() ;
                  {
                    let n = v0_.clone().0.type_name.clone() ;
                    {
                      let fields = v0_.clone().0.fields.clone() ;
                      crate::hydra::lib::eithers::map(|rfields: Vec<crate::hydra::core::Field>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
                        type_name: n.clone(),
                        fields: rfields.clone()}))))), crate::hydra::lib::eithers::map_list(for_field.clone(), fields.clone()))}}},
                crate::hydra::core::Term_Variant::Set (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(recurse.clone(), crate::hydra::lib::sets::to_list(v0_.clone())), |rlist: Vec<crate::hydra::core::Term>| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Set(crate::hydra::lib::sets::from_list(rlist.clone()))))))},
                crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.body.clone()), |t: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
                    body: t.clone(),
                    type_: v0_.clone().0.type_.clone()})))))))},
                crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
                  let v0_ = v0_.clone() ;
                  {
                    let v = v0_.clone().0.parameter.clone() ;
                    {
                      let body = v0_.clone().0.body.clone() ;
                      crate::hydra::lib::eithers::bind(recurse.clone()(body.clone()), |rbody: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeLambda(crate::hydra::core::TypeLambda(Rc::new(crate::hydra::core::TypeLambda_Variant {
                        parameter: v.clone(),
                        body: rbody.clone()})))))))}}},
                crate::hydra::core::Term_Variant::Union (v0_) => {
                  let v0_ = v0_.clone() ;
                  {
                    let n = v0_.clone().0.type_name.clone() ;
                    {
                      let field = v0_.clone().0.field.clone() ;
                      crate::hydra::lib::eithers::map(|rfield: crate::hydra::core::Field| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
                        type_name: n.clone(),
                        field: rfield.clone()}))))), for_field.clone()(field.clone()))}}},
                crate::hydra::core::Term_Variant::Unit (v0_) => {
                  let v0_ = v0_.clone() ;
                  Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit)))},
                crate::hydra::core::Term_Variant::Variable (v0_) => {
                  let v0_ = v0_.clone() ;
                  Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(v0_.clone()))))},
                crate::hydra::core::Term_Variant::Wrap (v0_) => {
                  let v0_ = v0_.clone() ;
                  {
                    let name = v0_.clone().0.type_name.clone() ;
                    {
                      let t = v0_.clone().0.body.clone() ;
                      crate::hydra::lib::eithers::bind(recurse.clone()(t.clone()), |rt: crate::hydra::core::Term| Right(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
                        type_name: name.clone(),
                        body: rt.clone()})))))))}}}}}}}}}} ;
  let rewrite = |cx: T0, term: crate::hydra::core::Term| f.clone()(|v1: T0, v2: crate::hydra::core::Term| for_subterms.clone()(rewrite.clone(), v1.clone(), v2.clone()), cx.clone(), term.clone()) ;
  rewrite.clone()(cx0.clone(), term0.clone())}

pub fn rewrite_type(f: impl Fn(Rc<dyn Fn(crate::hydra::core::Type) -> crate::hydra::core::Type>) -> Rc<dyn Fn(crate::hydra::core::Type) -> crate::hydra::core::Type> + Clone, typ0: crate::hydra::core::Type) -> crate::hydra::core::Type {
  let fsub = |recurse: Rc<dyn Fn(crate::hydra::core::Type) -> crate::hydra::core::Type>, typ: crate::hydra::core::Type| {
    let for_field = |field: crate::hydra::core::FieldType| crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
      name: field.clone().0.name.clone(),
      type_: recurse.clone()(field.clone().0.type_.clone())})) ;
    match &*typ.clone().0 {
      crate::hydra::core::Type_Variant::Annotated (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
          body: recurse.clone()(v0_.clone().0.body.clone()),
          annotation: v0_.clone().0.annotation.clone()})))))},
      crate::hydra::core::Type_Variant::Application (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
          function: recurse.clone()(v0_.clone().0.function.clone()),
          argument: recurse.clone()(v0_.clone().0.argument.clone())})))))},
      crate::hydra::core::Type_Variant::Either (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
          left: recurse.clone()(v0_.clone().0.left.clone()),
          right: recurse.clone()(v0_.clone().0.right.clone())})))))},
      crate::hydra::core::Type_Variant::Pair (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Pair(crate::hydra::core::PairType(Rc::new(crate::hydra::core::PairType_Variant {
          first: recurse.clone()(v0_.clone().0.first.clone()),
          second: recurse.clone()(v0_.clone().0.second.clone())})))))},
      crate::hydra::core::Type_Variant::Function (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
          domain: recurse.clone()(v0_.clone().0.domain.clone()),
          codomain: recurse.clone()(v0_.clone().0.codomain.clone())})))))},
      crate::hydra::core::Type_Variant::Forall (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
          parameter: v0_.clone().0.parameter.clone(),
          body: recurse.clone()(v0_.clone().0.body.clone())})))))},
      crate::hydra::core::Type_Variant::List (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(recurse.clone()(v0_.clone()))))},
      crate::hydra::core::Type_Variant::Literal (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(v0_.clone())))},
      crate::hydra::core::Type_Variant::Map (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
          keys: recurse.clone()(v0_.clone().0.keys.clone()),
          values: recurse.clone()(v0_.clone().0.values.clone())})))))},
      crate::hydra::core::Type_Variant::Maybe (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(recurse.clone()(v0_.clone()))))},
      crate::hydra::core::Type_Variant::Record (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(crate::hydra::lib::lists::map(for_field.clone(), v0_.clone()))))},
      crate::hydra::core::Type_Variant::Set (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(recurse.clone()(v0_.clone()))))},
      crate::hydra::core::Type_Variant::Union (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(crate::hydra::lib::lists::map(for_field.clone(), v0_.clone()))))},
      crate::hydra::core::Type_Variant::Unit (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit))},
      crate::hydra::core::Type_Variant::Variable (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v0_.clone())))},
      crate::hydra::core::Type_Variant::Wrap (v0_) => {
        let v0_ = v0_.clone() ;
        crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Wrap(recurse.clone()(v0_.clone()))))}}} ;
  let recurse = |v1: crate::hydra::core::Type| f.clone()(|v12: crate::hydra::core::Type| fsub.clone()(recurse.clone(), v12.clone()), v1.clone()) ;
  recurse.clone()(typ0.clone())}

pub fn rewrite_type_m(f: impl Fn(Rc<dyn Fn(crate::hydra::core::Type) -> Either<T0, crate::hydra::core::Type>>) -> Rc<dyn Fn(crate::hydra::core::Type) -> Either<T0, crate::hydra::core::Type>> + Clone, typ0: crate::hydra::core::Type) -> Either<T0, crate::hydra::core::Type> {
  let fsub = |recurse: Rc<dyn Fn(crate::hydra::core::Type) -> Either<T1, crate::hydra::core::Type>>, typ: crate::hydra::core::Type| match &*typ.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.body.clone()), |t: crate::hydra::core::Type| Right(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
        body: t.clone(),
        annotation: v0_.clone().0.annotation.clone()})))))))},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.function.clone()), |lhs: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.argument.clone()), |rhs: crate::hydra::core::Type| Right(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
        function: lhs.clone(),
        argument: rhs.clone()}))))))))},
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.left.clone()), |left: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.right.clone()), |right: crate::hydra::core::Type| Right(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
        left: left.clone(),
        right: right.clone()}))))))))},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.first.clone()), |pair_first: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.second.clone()), |pair_second: crate::hydra::core::Type| Right(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Pair(crate::hydra::core::PairType(Rc::new(crate::hydra::core::PairType_Variant {
        first: pair_first.clone(),
        second: pair_second.clone()}))))))))},
    crate::hydra::core::Type_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.domain.clone()), |dom: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.codomain.clone()), |cod: crate::hydra::core::Type| Right(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
        domain: dom.clone(),
        codomain: cod.clone()}))))))))},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.body.clone()), |b: crate::hydra::core::Type| Right(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
        parameter: v0_.clone().0.parameter.clone(),
        body: b.clone()})))))))},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone()), |rt: crate::hydra::core::Type| Right(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(rt.clone())))))},
    crate::hydra::core::Type_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(v0_.clone()))))},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.keys.clone()), |kt: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone().0.values.clone()), |vt: crate::hydra::core::Type| Right(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
        keys: kt.clone(),
        values: vt.clone()}))))))))},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone()), |rt: crate::hydra::core::Type| Right(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(rt.clone())))))},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let for_field = |f2: crate::hydra::core::FieldType| crate::hydra::lib::eithers::bind(recurse.clone()(f2.clone().0.type_.clone()), |t: crate::hydra::core::Type| Right(crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: f2.clone().0.name.clone(),
          type_: t.clone()})))) ;
        crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(for_field.clone(), v0_.clone()), |rfields: Vec<crate::hydra::core::FieldType>| Right(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(rfields.clone())))))}},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone()), |rt: crate::hydra::core::Type| Right(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(rt.clone())))))},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let for_field = |f2: crate::hydra::core::FieldType| crate::hydra::lib::eithers::bind(recurse.clone()(f2.clone().0.type_.clone()), |t: crate::hydra::core::Type| Right(crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: f2.clone().0.name.clone(),
          type_: t.clone()})))) ;
        crate::hydra::lib::eithers::bind(crate::hydra::lib::eithers::map_list(for_field.clone(), v0_.clone()), |rfields: Vec<crate::hydra::core::FieldType>| Right(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(rfields.clone())))))}},
    crate::hydra::core::Type_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)))},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      Right(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(v0_.clone()))))},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::bind(recurse.clone()(v0_.clone()), |t: crate::hydra::core::Type| Right(crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Wrap(t.clone())))))}} ;
  let recurse = |v1: crate::hydra::core::Type| f.clone()(|v12: crate::hydra::core::Type| fsub.clone()(recurse.clone(), v12.clone()), v1.clone()) ;
  recurse.clone()(typ0.clone())}

pub fn simplify_term(term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let simplify = |recurse: Rc<dyn Fn(crate::hydra::core::Term) -> T0>, term2: crate::hydra::core::Term| {
    let for_rhs = |rhs: crate::hydra::core::Term, var: crate::hydra::core::Name, body: crate::hydra::core::Term| match &*deannotate_term(rhs.clone()).0 {
      crate::hydra::core::Term_Variant::Variable (v0_) => {
        let v0_ = v0_.clone() ;
        simplify_term(substitute_variable(var.clone(), v0_.clone(), body.clone()))},
      _ => term2.clone()} ;
    {
      let for_lhs = |lhs: crate::hydra::core::Term, rhs: crate::hydra::core::Term| {
        let for_fun = |fun: crate::hydra::core::Function| match &*fun.clone().0 {
          crate::hydra::core::Function_Variant::Lambda (v0_) => {
            let v0_ = v0_.clone() ;
            {
              let var = v0_.clone().0.parameter.clone() ;
              {
                let body = v0_.clone().0.body.clone() ;
                crate::hydra::lib::logic::if_else(crate::hydra::lib::sets::member(var.clone(), free_variables_in_term(body.clone())), for_rhs.clone()(rhs.clone(), var.clone(), body.clone()), simplify_term(body.clone()))}}},
          _ => term2.clone()} ;
        match &*deannotate_term(lhs.clone()).0 {
          crate::hydra::core::Term_Variant::Function (v0_) => {
            let v0_ = v0_.clone() ;
            for_fun.clone()(v0_.clone())},
          _ => term2.clone()}} ;
      {
        let for_term = |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
          crate::hydra::core::Term_Variant::Application (v0_) => {
            let v0_ = v0_.clone() ;
            {
              let lhs = v0_.clone().0.function.clone() ;
              {
                let rhs = v0_.clone().0.argument.clone() ;
                for_lhs.clone()(lhs.clone(), rhs.clone())}}},
          _ => term2.clone()} ;
        {
          let stripped = deannotate_term(term2.clone()) ;
          recurse.clone()(for_term.clone()(stripped.clone()))}}}} ;
  rewrite_term(simplify.clone(), term.clone())}

pub fn substitute_type_variables(subst: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Name>, typ: crate::hydra::core::Type) -> crate::hydra::core::Type {
  let replace = |recurse: Rc<dyn Fn(crate::hydra::core::Type) -> crate::hydra::core::Type>, typ2: crate::hydra::core::Type| match &*typ2.clone().0 {
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(crate::hydra::lib::maybes::from_maybe(v0_.clone(), crate::hydra::lib::maps::lookup(v0_.clone(), subst.clone())))))},
    _ => recurse.clone()(typ2.clone())} ;
  rewrite_type(replace.clone(), typ.clone())}

pub fn substitute_variable(from: crate::hydra::core::Name, to: crate::hydra::core::Name, term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let replace = |recurse: Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>, term2: crate::hydra::core::Term| match &*term2.clone().0 {
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(v0_.clone(), from.clone()), to.clone(), v0_.clone()))))},
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Function_Variant::Lambda (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(v0_.clone().0.parameter.clone(), from.clone()), term2.clone(), recurse.clone()(term2.clone()))},
        _ => recurse.clone()(term2.clone())}},
    _ => recurse.clone()(term2.clone())} ;
  rewrite_term(replace.clone(), term.clone())}

pub fn substitute_variables(subst: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Name>, term: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let replace = |recurse: Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>, term2: crate::hydra::core::Term| match &*term2.clone().0 {
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::lib::maybes::from_maybe(v0_.clone(), crate::hydra::lib::maps::lookup(v0_.clone(), subst.clone())))))},
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Function_Variant::Lambda (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::lib::maybes::maybe(recurse.clone()(term2.clone()), |_: crate::hydra::core::Name| term2.clone(), crate::hydra::lib::maps::lookup(v0_.clone().0.parameter.clone(), subst.clone()))},
        _ => recurse.clone()(term2.clone())}},
    _ => recurse.clone()(term2.clone())} ;
  rewrite_term(replace.clone(), term.clone())}

pub fn strip_type_lambdas(t: crate::hydra::core::Term) -> crate::hydra::core::Term {
  match &*t.clone().0 {
    crate::hydra::core::Term_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let subj = v0_.clone().0.body.clone() ;
        {
          let ann = v0_.clone().0.annotation.clone() ;
          crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
            body: strip_type_lambdas(subj.clone()),
            annotation: ann.clone()})))))}}},
    crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
      let v0_ = v0_.clone() ;
      strip_type_lambdas(v0_.clone().0.body.clone())},
    _ => t.clone()}}

pub fn subterms(v1: crate::hydra::core::Term) -> Vec<crate::hydra::core::Term> {
  match &*v1.clone().0 {
    crate::hydra::core::Term_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone().0.body.clone()])},
    crate::hydra::core::Term_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone().0.function.clone(),
        v0_.clone().0.argument.clone()])},
    crate::hydra::core::Term_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::either(|l: crate::hydra::core::Term| Vec::from([
        l.clone()]), |r: crate::hydra::core::Term| Vec::from([
        r.clone()]), v0_.clone())},
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Function_Variant::Elimination (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::Elimination_Variant::Union (v0_) => {
              let v0_ = v0_.clone() ;
              crate::hydra::lib::lists::concat2(crate::hydra::lib::maybes::maybe(Vec::from([]), |t: crate::hydra::core::Term| Vec::from([
                t.clone()]), v0_.clone().0.default_.clone()), crate::hydra::lib::lists::map(|v| v.0.term.clone(), v0_.clone().0.cases.clone()))},
            _ => Vec::from([])}},
        crate::hydra::core::Function_Variant::Lambda (v0_) => {
          let v0_ = v0_.clone() ;
          Vec::from([
            v0_.clone().0.body.clone()])},
        _ => Vec::from([])}},
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::cons(v0_.clone().0.body.clone(), crate::hydra::lib::lists::map(|v| v.0.term.clone(), v0_.clone().0.bindings.clone()))},
    crate::hydra::core::Term_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      v0_.clone()},
    crate::hydra::core::Term_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([])},
    crate::hydra::core::Term_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|p: (crate::hydra::core::Term, crate::hydra::core::Term)| Vec::from([
        crate::hydra::lib::pairs::first(p.clone()),
        crate::hydra::lib::pairs::second(p.clone())]), crate::hydra::lib::maps::to_list(v0_.clone())))},
    crate::hydra::core::Term_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::maybes::maybe(Vec::from([]), |t: crate::hydra::core::Term| Vec::from([
        t.clone()]), v0_.clone())},
    crate::hydra::core::Term_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        crate::hydra::lib::pairs::first(v0_.clone()),
        crate::hydra::lib::pairs::second(v0_.clone())])},
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::map(|v| v.0.term.clone(), v0_.clone().0.fields.clone())},
    crate::hydra::core::Term_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::sets::to_list(v0_.clone())},
    crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone().0.body.clone()])},
    crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone().0.body.clone()])},
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone().0.field.clone().0.term.clone()])},
    crate::hydra::core::Term_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([])},
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([])},
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone().0.body.clone()])}}}

pub fn subterms_with_accessors(v1: crate::hydra::core::Term) -> Vec<(crate::hydra::accessors::TermAccessor, crate::hydra::core::Term)> {
  match &*v1.clone().0 {
    crate::hydra::core::Term_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::AnnotatedBody)), v0_.clone().0.body.clone())])},
    crate::hydra::core::Term_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::ApplicationFunction)), v0_.clone().0.function.clone()),
        (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::ApplicationArgument)), v0_.clone().0.argument.clone())])},
    crate::hydra::core::Term_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([])},
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Function_Variant::Elimination (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::Elimination_Variant::Union (v0_) => {
              let v0_ = v0_.clone() ;
              crate::hydra::lib::lists::concat2(crate::hydra::lib::maybes::maybe(Vec::from([]), |t: crate::hydra::core::Term| Vec::from([
                (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::UnionCasesDefault)), t.clone())]), v0_.clone().0.default_.clone()), crate::hydra::lib::lists::map(|f: crate::hydra::core::Field| (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::UnionCasesBranch(f.clone().0.name.clone()))), f.clone().0.term.clone()), v0_.clone().0.cases.clone()))},
            _ => Vec::from([])}},
        crate::hydra::core::Function_Variant::Lambda (v0_) => {
          let v0_ = v0_.clone() ;
          Vec::from([
            (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::LambdaBody)), v0_.clone().0.body.clone())])},
        _ => Vec::from([])}},
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::cons((crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::LetBody)), v0_.clone().0.body.clone()), crate::hydra::lib::lists::map(|b: crate::hydra::core::Binding| (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::LetBinding(b.clone().0.name.clone()))), b.clone().0.term.clone()), v0_.clone().0.bindings.clone()))},
    crate::hydra::core::Term_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::map(|e: crate::hydra::core::Term| (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::ListElement(0i32))), e.clone()), v0_.clone())},
    crate::hydra::core::Term_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([])},
    crate::hydra::core::Term_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(|p: (crate::hydra::core::Term, crate::hydra::core::Term)| Vec::from([
        (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::MapKey(0i32))), crate::hydra::lib::pairs::first(p.clone())),
        (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::MapValue(0i32))), crate::hydra::lib::pairs::second(p.clone()))]), crate::hydra::lib::maps::to_list(v0_.clone())))},
    crate::hydra::core::Term_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::maybes::maybe(Vec::from([]), |t: crate::hydra::core::Term| Vec::from([
        (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::MaybeTerm)), t.clone())]), v0_.clone())},
    crate::hydra::core::Term_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([])},
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::map(|f: crate::hydra::core::Field| (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::RecordField(f.clone().0.name.clone()))), f.clone().0.term.clone()), v0_.clone().0.fields.clone())},
    crate::hydra::core::Term_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::map(|e: crate::hydra::core::Term| (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::ListElement(0i32))), e.clone()), crate::hydra::lib::sets::to_list(v0_.clone()))},
    crate::hydra::core::Term_Variant::TypeApplication (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::TypeApplicationTerm)), v0_.clone().0.body.clone())])},
    crate::hydra::core::Term_Variant::TypeLambda (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::TypeLambdaBody)), v0_.clone().0.body.clone())])},
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::InjectionTerm)), v0_.clone().0.field.clone().0.term.clone())])},
    crate::hydra::core::Term_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([])},
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([])},
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        (crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::WrappedTerm)), v0_.clone().0.body.clone())])}}}

pub fn subtypes(v1: crate::hydra::core::Type) -> Vec<crate::hydra::core::Type> {
  match &*v1.clone().0 {
    crate::hydra::core::Type_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone().0.body.clone()])},
    crate::hydra::core::Type_Variant::Application (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone().0.function.clone(),
        v0_.clone().0.argument.clone()])},
    crate::hydra::core::Type_Variant::Either (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone().0.left.clone(),
        v0_.clone().0.right.clone()])},
    crate::hydra::core::Type_Variant::Pair (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone().0.first.clone(),
        v0_.clone().0.second.clone()])},
    crate::hydra::core::Type_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone().0.domain.clone(),
        v0_.clone().0.codomain.clone()])},
    crate::hydra::core::Type_Variant::Forall (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone().0.body.clone()])},
    crate::hydra::core::Type_Variant::List (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone()])},
    crate::hydra::core::Type_Variant::Literal (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([])},
    crate::hydra::core::Type_Variant::Map (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone().0.keys.clone(),
        v0_.clone().0.values.clone()])},
    crate::hydra::core::Type_Variant::Maybe (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone()])},
    crate::hydra::core::Type_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::map(|v| v.0.type_.clone(), v0_.clone())},
    crate::hydra::core::Type_Variant::Set (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone()])},
    crate::hydra::core::Type_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::lists::map(|v| v.0.type_.clone(), v0_.clone())},
    crate::hydra::core::Type_Variant::Unit (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([])},
    crate::hydra::core::Type_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([])},
    crate::hydra::core::Type_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      Vec::from([
        v0_.clone()])}}}

pub fn term_dependency_names(binds: bool, with_prims: bool, with_noms: bool, term0: crate::hydra::core::Term) -> BTreeSet<crate::hydra::core::Name> {
  let add_names = |names: BTreeSet<crate::hydra::core::Name>, term: crate::hydra::core::Term| {
    let nominal = |name: crate::hydra::core::Name| crate::hydra::lib::logic::if_else(with_noms.clone(), crate::hydra::lib::sets::insert(name.clone(), names.clone()), names.clone()) ;
    {
      let prim = |name: crate::hydra::core::Name| crate::hydra::lib::logic::if_else(with_prims.clone(), crate::hydra::lib::sets::insert(name.clone(), names.clone()), names.clone()) ;
      {
        let var = |name: crate::hydra::core::Name| crate::hydra::lib::logic::if_else(binds.clone(), crate::hydra::lib::sets::insert(name.clone(), names.clone()), names.clone()) ;
        match &*term.clone().0 {
          crate::hydra::core::Term_Variant::Function (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Function_Variant::Primitive (v0_) => {
                let v0_ = v0_.clone() ;
                prim.clone()(v0_.clone())},
              crate::hydra::core::Function_Variant::Elimination (v0_) => {
                let v0_ = v0_.clone() ;
                match &*v0_.clone().0 {
                  crate::hydra::core::Elimination_Variant::Record (v0_) => {
                    let v0_ = v0_.clone() ;
                    nominal.clone()(v0_.clone().0.type_name.clone())},
                  crate::hydra::core::Elimination_Variant::Union (v0_) => {
                    let v0_ = v0_.clone() ;
                    nominal.clone()(v0_.clone().0.type_name.clone())},
                  crate::hydra::core::Elimination_Variant::Wrap (v0_) => {
                    let v0_ = v0_.clone() ;
                    nominal.clone()(v0_.clone())}}},
              _ => names.clone()}},
          crate::hydra::core::Term_Variant::Record (v0_) => {
            let v0_ = v0_.clone() ;
            nominal.clone()(v0_.clone().0.type_name.clone())},
          crate::hydra::core::Term_Variant::Union (v0_) => {
            let v0_ = v0_.clone() ;
            nominal.clone()(v0_.clone().0.type_name.clone())},
          crate::hydra::core::Term_Variant::Variable (v0_) => {
            let v0_ = v0_.clone() ;
            var.clone()(v0_.clone())},
          crate::hydra::core::Term_Variant::Wrap (v0_) => {
            let v0_ = v0_.clone() ;
            nominal.clone()(v0_.clone().0.type_name.clone())},
          _ => names.clone()}}}} ;
  fold_over_term(crate::hydra::coders::TraversalOrder(Rc::new(crate::hydra::coders::TraversalOrder_Variant::Pre)), add_names.clone(), crate::hydra::lib::sets::empty, term0.clone())}

pub fn to_short_names(original: Vec<crate::hydra::core::Name>) -> BTreeMap<crate::hydra::core::Name, crate::hydra::core::Name> {
  let add_name = |acc: BTreeMap<String, BTreeSet<crate::hydra::core::Name>>, name: crate::hydra::core::Name| {
    let local = crate::hydra::names::local_name_of(name.clone()) ;
    {
      let group = crate::hydra::lib::maybes::from_maybe(crate::hydra::lib::sets::empty, crate::hydra::lib::maps::lookup(local.clone(), acc.clone())) ;
      crate::hydra::lib::maps::insert(local.clone(), crate::hydra::lib::sets::insert(name.clone(), group.clone()), acc.clone())}} ;
  let group_names_by_local = |names: Vec<crate::hydra::core::Name>| crate::hydra::lib::lists::foldl(add_name.clone(), crate::hydra::lib::maps::empty, names.clone()) ;
  let groups = group_names_by_local.clone()(original.clone()) ;
  let rename_group = |local_names: (String, BTreeSet<T0>)| {
    let local = crate::hydra::lib::pairs::first(local_names.clone()) ;
    {
      let names = crate::hydra::lib::pairs::second(local_names.clone()) ;
      {
        let range_from = |start: i32| crate::hydra::lib::lists::cons(start.clone(), range_from.clone()(crate::hydra::lib::math::add(start.clone(), 1i32))) ;
        {
          let rename = |name: T1, i: i32| (name.clone(), crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::gt(i.clone(), 1i32), crate::hydra::lib::strings::cat2(local.clone(), crate::hydra::lib::literals::show_int32(i.clone())), local.clone()))))) ;
          crate::hydra::lib::lists::zip_with(rename.clone(), crate::hydra::lib::sets::to_list(names.clone()), range_from.clone()(1i32))}}}} ;
  crate::hydra::lib::maps::from_list(crate::hydra::lib::lists::concat(crate::hydra::lib::lists::map(rename_group.clone(), crate::hydra::lib::maps::to_list(groups.clone()))))}

pub fn topological_sort_binding_map(binding_map: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term>) -> Vec<Vec<(crate::hydra::core::Name, crate::hydra::core::Term)>> {
  let bindings = crate::hydra::lib::maps::to_list(binding_map.clone()) ;
  let keys = crate::hydra::lib::sets::from_list(crate::hydra::lib::lists::map(crate::hydra::lib::pairs::first, bindings.clone())) ;
  let has_type_annotation = |term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Annotated (v0_) => {
      let v0_ = v0_.clone() ;
      has_type_annotation.clone()(v0_.clone().0.body.clone())},
    _ => false} ;
  let deps_of = |name_and_term: (T0, crate::hydra::core::Term)| {
    let name = crate::hydra::lib::pairs::first(name_and_term.clone()) ;
    {
      let term = crate::hydra::lib::pairs::second(name_and_term.clone()) ;
      (name.clone(), crate::hydra::lib::logic::if_else(has_type_annotation.clone()(term.clone()), Vec::from([]), crate::hydra::lib::sets::to_list(crate::hydra::lib::sets::intersection(keys.clone(), free_variables_in_term(term.clone())))))}} ;
  let to_pair = |name: crate::hydra::core::Name| (name.clone(), crate::hydra::lib::maybes::from_maybe(crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(String::from("Impossible!"))))))), crate::hydra::lib::maps::lookup(name.clone(), binding_map.clone()))) ;
  crate::hydra::lib::lists::map(|v1: Vec<X>| crate::hydra::lib::lists::map(to_pair.clone(), v1.clone()), crate::hydra::sorting::topological_sort_components(crate::hydra::lib::lists::map(deps_of.clone(), bindings.clone())))}

pub fn topological_sort_bindings(els: Vec<crate::hydra::core::Binding>) -> Either<Vec<Vec<crate::hydra::core::Name>>, Vec<crate::hydra::core::Name>> {
  let adjlist = |e: crate::hydra::core::Binding| (e.clone().0.name.clone(), crate::hydra::lib::sets::to_list(term_dependency_names(false, true, true, e.clone().0.term.clone()))) ;
  crate::hydra::sorting::topological_sort(crate::hydra::lib::lists::map(adjlist.clone(), els.clone()))}

pub fn type_dependency_names(with_schema: bool, typ: crate::hydra::core::Type) -> BTreeSet<crate::hydra::core::Name> {
  crate::hydra::lib::logic::if_else(with_schema.clone(), crate::hydra::lib::sets::union_(free_variables_in_type(typ.clone()), type_names_in_type(typ.clone())), free_variables_in_type(typ.clone()))}

pub fn type_names_in_type(typ0: crate::hydra::core::Type) -> BTreeSet<T0> {
  let add_names = |names: T1, typ: T2| names.clone() ;
  fold_over_type(crate::hydra::coders::TraversalOrder(Rc::new(crate::hydra::coders::TraversalOrder_Variant::Pre)), add_names.clone(), crate::hydra::lib::sets::empty, typ0.clone())}

pub fn type_scheme_to_f_type(ts: crate::hydra::core::TypeScheme) -> crate::hydra::core::Type {
  let vars = ts.clone().0.variables.clone() ;
  let body = ts.clone().0.type_.clone() ;
  crate::hydra::lib::lists::foldl(|t: crate::hydra::core::Type, v: crate::hydra::core::Name| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
    parameter: v.clone(),
    body: t.clone()}))))), body.clone(), crate::hydra::lib::lists::reverse(vars.clone()))}

pub fn unshadow_variables(term0: crate::hydra::core::Term) -> crate::hydra::core::Term {
  let fresh_name = |base: crate::hydra::core::Name, i: i32, m: BTreeMap<crate::hydra::core::Name, T0>| {
    let candidate = crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(crate::hydra::lib::strings::cat2(base.clone().0.0.clone(), crate::hydra::lib::literals::show_int32(i.clone()))))) ;
    crate::hydra::lib::logic::if_else(crate::hydra::lib::maps::member(candidate.clone(), m.clone()), fresh_name.clone()(base.clone(), crate::hydra::lib::math::add(i.clone(), 1i32), m.clone()), candidate.clone())} ;
  let f = |recurse: Rc<dyn Fn(BTreeMap<crate::hydra::core::Name, crate::hydra::core::Name>) -> Rc<dyn Fn(crate::hydra::core::Term) -> crate::hydra::core::Term>>, m: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Name>, term: crate::hydra::core::Term| match &*term.clone().0 {
    crate::hydra::core::Term_Variant::Function (v0_) => {
      let v0_ = v0_.clone() ;
      match &*v0_.clone().0 {
        crate::hydra::core::Function_Variant::Lambda (v0_) => {
          let v0_ = v0_.clone() ;
          {
            let v = v0_.clone().0.parameter.clone() ;
            {
              let domain = v0_.clone().0.domain.clone() ;
              {
                let body = v0_.clone().0.body.clone() ;
                crate::hydra::lib::logic::if_else(crate::hydra::lib::maps::member(v.clone(), m.clone()), {
                  let v2 = fresh_name.clone()(v.clone(), 2i32, m.clone()) ;
                  {
                    let m2 = crate::hydra::lib::maps::insert(v.clone(), v2.clone(), crate::hydra::lib::maps::insert(v2.clone(), v2.clone(), m.clone())) ;
                    crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                      parameter: v2.clone(),
                      domain: domain.clone(),
                      body: f.clone()(recurse.clone(), m2.clone(), body.clone())}))))))))}}, crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
                  parameter: v.clone(),
                  domain: domain.clone(),
                  body: f.clone()(recurse.clone(), crate::hydra::lib::maps::insert(v.clone(), v.clone(), m.clone()), body.clone())})))))))))}}}},
        _ => recurse.clone()(m.clone(), term.clone())}},
    crate::hydra::core::Term_Variant::Let (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let m2 = crate::hydra::lib::lists::foldl(|acc: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Name>, b: crate::hydra::core::Binding| {
          let bname = b.clone().0.name.clone() ;
          crate::hydra::lib::logic::if_else(crate::hydra::lib::maps::member(bname.clone(), acc.clone()), acc.clone(), crate::hydra::lib::maps::insert(bname.clone(), bname.clone(), acc.clone()))}, m.clone(), v0_.clone().0.bindings.clone()) ;
        recurse.clone()(m2.clone(), term.clone())}},
    crate::hydra::core::Term_Variant::Variable (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(crate::hydra::lib::maybes::maybe(v0_.clone(), |renamed: crate::hydra::core::Name| renamed.clone(), crate::hydra::lib::maps::lookup(v0_.clone(), m.clone())))))},
    _ => recurse.clone()(m.clone(), term.clone())} ;
  rewrite_term_with_context(f.clone(), crate::hydra::lib::maps::empty, term0.clone())}
