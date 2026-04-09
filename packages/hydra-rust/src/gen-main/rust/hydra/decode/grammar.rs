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
use crate::hydra::grammar::*;
use crate::hydra::util::*;

pub fn constant(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::grammar::Constant> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: String| crate::hydra::grammar::Constant(Rc::new(crate::hydra::grammar::Constant_Variant(b.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
        crate::hydra::core::Term_Variant::Literal (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::Literal_Variant::String (v0_) => {
              let v0_ = v0_.clone() ;
              Right(v0_.clone())},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
        _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), v0_.clone().0.body.clone())))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn grammar(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::grammar::Grammar> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: Vec<crate::hydra::grammar::Production>| crate::hydra::grammar::Grammar(Rc::new(crate::hydra::grammar::Grammar_Variant(b.clone()))), crate::hydra::extract::helpers::decode_list(production, cx.clone(), v0_.clone().0.body.clone()))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn label(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::grammar::Label> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: String| crate::hydra::grammar::Label(Rc::new(crate::hydra::grammar::Label_Variant(b.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
        crate::hydra::core::Term_Variant::Literal (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::Literal_Variant::String (v0_) => {
              let v0_ = v0_.clone() ;
              Right(v0_.clone())},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
        _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), v0_.clone().0.body.clone())))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn labeled_pattern(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::grammar::LabeledPattern> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("label"), label, field_map.clone(), cx.clone()), |field_label: crate::hydra::grammar::Label| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("pattern"), pattern, field_map.clone(), cx.clone()), |field_pattern: crate::hydra::grammar::Pattern| Right(crate::hydra::grammar::LabeledPattern(Rc::new(crate::hydra::grammar::LabeledPattern_Variant {
          label: field_label.clone(),
          pattern: field_pattern.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn pattern(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::grammar::Pattern> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("alternatives")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: Vec<crate::hydra::grammar::Pattern>| crate::hydra::grammar::Pattern(Rc::new(crate::hydra::grammar::Pattern_Variant::Alternatives(t.clone()))), crate::hydra::extract::helpers::decode_list(pattern, cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("constant")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::grammar::Constant| crate::hydra::grammar::Pattern(Rc::new(crate::hydra::grammar::Pattern_Variant::Constant(t.clone()))), constant(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("ignored")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::grammar::Pattern| crate::hydra::grammar::Pattern(Rc::new(crate::hydra::grammar::Pattern_Variant::Ignored(t.clone()))), pattern(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("labeled")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::grammar::LabeledPattern| crate::hydra::grammar::Pattern(Rc::new(crate::hydra::grammar::Pattern_Variant::Labeled(t.clone()))), labeled_pattern(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("nil")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::grammar::Pattern(Rc::new(crate::hydra::grammar::Pattern_Variant::Nil)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("nonterminal")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::grammar::Symbol| crate::hydra::grammar::Pattern(Rc::new(crate::hydra::grammar::Pattern_Variant::Nonterminal(t.clone()))), symbol(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("option")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::grammar::Pattern| crate::hydra::grammar::Pattern(Rc::new(crate::hydra::grammar::Pattern_Variant::Option(t.clone()))), pattern(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("plus")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::grammar::Pattern| crate::hydra::grammar::Pattern(Rc::new(crate::hydra::grammar::Pattern_Variant::Plus(t.clone()))), pattern(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("regex")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::grammar::Regex| crate::hydra::grammar::Pattern(Rc::new(crate::hydra::grammar::Pattern_Variant::Regex(t.clone()))), regex(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("sequence")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: Vec<crate::hydra::grammar::Pattern>| crate::hydra::grammar::Pattern(Rc::new(crate::hydra::grammar::Pattern_Variant::Sequence(t.clone()))), crate::hydra::extract::helpers::decode_list(pattern, cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("star")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::grammar::Pattern| crate::hydra::grammar::Pattern(Rc::new(crate::hydra::grammar::Pattern_Variant::Star(t.clone()))), pattern(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::grammar::Pattern>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn production(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::grammar::Production> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("symbol"), symbol, field_map.clone(), cx.clone()), |field_symbol: crate::hydra::grammar::Symbol| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("pattern"), pattern, field_map.clone(), cx.clone()), |field_pattern: crate::hydra::grammar::Pattern| Right(crate::hydra::grammar::Production(Rc::new(crate::hydra::grammar::Production_Variant {
          symbol: field_symbol.clone(),
          pattern: field_pattern.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn regex(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::grammar::Regex> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: String| crate::hydra::grammar::Regex(Rc::new(crate::hydra::grammar::Regex_Variant(b.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
        crate::hydra::core::Term_Variant::Literal (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::Literal_Variant::String (v0_) => {
              let v0_ = v0_.clone() ;
              Right(v0_.clone())},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
        _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), v0_.clone().0.body.clone())))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn symbol(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::grammar::Symbol> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: String| crate::hydra::grammar::Symbol(Rc::new(crate::hydra::grammar::Symbol_Variant(b.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
        crate::hydra::core::Term_Variant::Literal (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::Literal_Variant::String (v0_) => {
              let v0_ = v0_.clone() ;
              Right(v0_.clone())},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
        _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), v0_.clone().0.body.clone())))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}
