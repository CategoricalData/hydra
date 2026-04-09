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
use crate::hydra::topology::*;
use crate::hydra::util::*;

pub fn graph(v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, BTreeMap<i32, Vec<i32>>> {
  crate::hydra::extract::helpers::decode_map(vertex, |v12: crate::hydra::graph::Graph, v22: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(vertex, v12.clone(), v22.clone()), v1.clone(), v2.clone())}

pub fn tarjan_state(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::topology::TarjanState> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("counter"), |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
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
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), field_map.clone(), cx.clone()), |field_counter: i32| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("indices"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_map(vertex, |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
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
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_indices: BTreeMap<i32, i32>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("lowLinks"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_map(vertex, |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
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
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_low_links: BTreeMap<i32, i32>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("stack"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(vertex, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_stack: Vec<i32>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("onStack"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_set(vertex, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_on_stack: BTreeSet<i32>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("sccs"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(|v12: crate::hydra::graph::Graph, v22: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(vertex, v12.clone(), v22.clone()), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_sccs: Vec<Vec<i32>>| Right(crate::hydra::topology::TarjanState(Rc::new(crate::hydra::topology::TarjanState_Variant {
          counter: field_counter.clone(),
          indices: field_indices.clone(),
          low_links: field_low_links.clone(),
          stack: field_stack.clone(),
          on_stack: field_on_stack.clone(),
          sccs: field_sccs.clone()})))))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn vertex(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, i32> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
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
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}
