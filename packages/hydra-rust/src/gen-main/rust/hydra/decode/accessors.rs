#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::extract::helpers::*;
use crate::hydra::lexical::*;
use crate::hydra::rewriting::*;
use crate::hydra::decode::core::*;
use crate::hydra::accessors::*;
use crate::hydra::util::*;

pub fn accessor_edge(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::accessors::AccessorEdge> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("source"), accessor_node, field_map.clone(), cx.clone()), |field_source: crate::hydra::accessors::AccessorNode| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("path"), accessor_path, field_map.clone(), cx.clone()), |field_path: crate::hydra::accessors::AccessorPath| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("target"), accessor_node, field_map.clone(), cx.clone()), |field_target: crate::hydra::accessors::AccessorNode| Right(crate::hydra::accessors::AccessorEdge(Rc::new(crate::hydra::accessors::AccessorEdge_Variant {
          source: field_source.clone(),
          path: field_path.clone(),
          target: field_target.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn accessor_graph(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::accessors::AccessorGraph> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("nodes"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(accessor_node, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_nodes: Vec<crate::hydra::accessors::AccessorNode>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("edges"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(accessor_edge, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_edges: Vec<crate::hydra::accessors::AccessorEdge>| Right(crate::hydra::accessors::AccessorGraph(Rc::new(crate::hydra::accessors::AccessorGraph_Variant {
          nodes: field_nodes.clone(),
          edges: field_edges.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn accessor_node(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::accessors::AccessorNode> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("name"), crate::hydra::decode::core::name, field_map.clone(), cx.clone()), |field_name: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("label"), |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), field_map.clone(), cx.clone()), |field_label: String| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("id"), |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), field_map.clone(), cx.clone()), |field_id: String| Right(crate::hydra::accessors::AccessorNode(Rc::new(crate::hydra::accessors::AccessorNode_Variant {
          name: field_name.clone(),
          label: field_label.clone(),
          id: field_id.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn accessor_path(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::accessors::AccessorPath> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: Vec<crate::hydra::accessors::TermAccessor>| crate::hydra::accessors::AccessorPath(Rc::new(crate::hydra::accessors::AccessorPath_Variant(b.clone()))), crate::hydra::extract::helpers::decode_list(term_accessor, cx.clone(), v0_.clone().0.body.clone()))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn term_accessor(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::accessors::TermAccessor> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("annotatedBody")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::AnnotatedBody)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("applicationFunction")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::ApplicationFunction)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("applicationArgument")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::ApplicationArgument)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("lambdaBody")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::LambdaBody)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unionCasesDefault")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::UnionCasesDefault)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unionCasesBranch")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Name| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::UnionCasesBranch(t.clone()))), crate::hydra::decode::core::name(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("letBody")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::LetBody)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("letBinding")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Name| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::LetBinding(t.clone()))), crate::hydra::decode::core::name(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("listElement")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: i32| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::ListElement(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Integer (v0_) => {
                  let v0_ = v0_.clone() ;
                  match &*v0_.clone().0 {
                    crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
                      let v0_ = v0_.clone() ;
                      Right(v0_.clone())},
                    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 value")))))}},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("mapKey")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: i32| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::MapKey(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Integer (v0_) => {
                  let v0_ = v0_.clone() ;
                  match &*v0_.clone().0 {
                    crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
                      let v0_ = v0_.clone() ;
                      Right(v0_.clone())},
                    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 value")))))}},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("mapValue")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: i32| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::MapValue(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Integer (v0_) => {
                  let v0_ = v0_.clone() ;
                  match &*v0_.clone().0 {
                    crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
                      let v0_ = v0_.clone() ;
                      Right(v0_.clone())},
                    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 value")))))}},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("maybeTerm")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::MaybeTerm)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("productTerm")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: i32| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::ProductTerm(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Integer (v0_) => {
                  let v0_ = v0_.clone() ;
                  match &*v0_.clone().0 {
                    crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
                      let v0_ = v0_.clone() ;
                      Right(v0_.clone())},
                    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 value")))))}},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("recordField")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Name| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::RecordField(t.clone()))), crate::hydra::decode::core::name(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("setElement")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: i32| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::SetElement(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Integer (v0_) => {
                  let v0_ = v0_.clone() ;
                  match &*v0_.clone().0 {
                    crate::hydra::core::IntegerValue_Variant::Int32 (v0_) => {
                      let v0_ = v0_.clone() ;
                      Right(v0_.clone())},
                    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 value")))))}},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int32 literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("sumTerm")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::SumTerm)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeLambdaBody")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::TypeLambdaBody)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeApplicationTerm")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::TypeApplicationTerm)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("injectionTerm")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::InjectionTerm)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("wrappedTerm")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::accessors::TermAccessor(Rc::new(crate::hydra::accessors::TermAccessor_Variant::WrappedTerm)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::accessors::TermAccessor>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}
