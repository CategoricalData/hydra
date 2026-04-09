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
use crate::hydra::core::*;
use crate::hydra::util::*;

pub fn annotated_term(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::AnnotatedTerm> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("body"), term, field_map.clone(), cx.clone()), |field_body: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("annotation"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_map(name, term, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_annotation: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term>| Right(crate::hydra::core::AnnotatedTerm(Rc::new(crate::hydra::core::AnnotatedTerm_Variant {
          body: field_body.clone(),
          annotation: field_annotation.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn annotated_type(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::AnnotatedType> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("body"), type_, field_map.clone(), cx.clone()), |field_body: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("annotation"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_map(name, term, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_annotation: BTreeMap<crate::hydra::core::Name, crate::hydra::core::Term>| Right(crate::hydra::core::AnnotatedType(Rc::new(crate::hydra::core::AnnotatedType_Variant {
          body: field_body.clone(),
          annotation: field_annotation.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn application(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Application> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("function"), term, field_map.clone(), cx.clone()), |field_function: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("argument"), term, field_map.clone(), cx.clone()), |field_argument: crate::hydra::core::Term| Right(crate::hydra::core::Application(Rc::new(crate::hydra::core::Application_Variant {
          function: field_function.clone(),
          argument: field_argument.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn application_type(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::ApplicationType> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("function"), type_, field_map.clone(), cx.clone()), |field_function: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("argument"), type_, field_map.clone(), cx.clone()), |field_argument: crate::hydra::core::Type| Right(crate::hydra::core::ApplicationType(Rc::new(crate::hydra::core::ApplicationType_Variant {
          function: field_function.clone(),
          argument: field_argument.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn binding(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Binding> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("name"), name, field_map.clone(), cx.clone()), |field_name: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("term"), term, field_map.clone(), cx.clone()), |field_term: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("type"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_maybe(type_scheme, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_type: Option<crate::hydra::core::TypeScheme>| Right(crate::hydra::core::Binding(Rc::new(crate::hydra::core::Binding_Variant {
          name: field_name.clone(),
          term: field_term.clone(),
          type_: field_type.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn case_statement(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::CaseStatement> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("typeName"), name, field_map.clone(), cx.clone()), |field_type_name: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("default"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_maybe(term, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_default: Option<crate::hydra::core::Term>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("cases"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(field, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_cases: Vec<crate::hydra::core::Field>| Right(crate::hydra::core::CaseStatement(Rc::new(crate::hydra::core::CaseStatement_Variant {
          type_name: field_type_name.clone(),
          default_: field_default.clone(),
          cases: field_cases.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn either_type(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::EitherType> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("left"), type_, field_map.clone(), cx.clone()), |field_left: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("right"), type_, field_map.clone(), cx.clone()), |field_right: crate::hydra::core::Type| Right(crate::hydra::core::EitherType(Rc::new(crate::hydra::core::EitherType_Variant {
          left: field_left.clone(),
          right: field_right.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn pair_type(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::PairType> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("first"), type_, field_map.clone(), cx.clone()), |field_first: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("second"), type_, field_map.clone(), cx.clone()), |field_second: crate::hydra::core::Type| Right(crate::hydra::core::PairType(Rc::new(crate::hydra::core::PairType_Variant {
          first: field_first.clone(),
          second: field_second.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn elimination(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Elimination> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("record")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Projection| crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Record(t.clone()))), projection(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("union")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::CaseStatement| crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Union(t.clone()))), case_statement(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("wrap")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Name| crate::hydra::core::Elimination(Rc::new(crate::hydra::core::Elimination_Variant::Wrap(t.clone()))), name(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Elimination>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn field(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Field> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("name"), name, field_map.clone(), cx.clone()), |field_name: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("term"), term, field_map.clone(), cx.clone()), |field_term: crate::hydra::core::Term| Right(crate::hydra::core::Field(Rc::new(crate::hydra::core::Field_Variant {
          name: field_name.clone(),
          term: field_term.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn field_type(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::FieldType> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("name"), name, field_map.clone(), cx.clone()), |field_name: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("type"), type_, field_map.clone(), cx.clone()), |field_type: crate::hydra::core::Type| Right(crate::hydra::core::FieldType(Rc::new(crate::hydra::core::FieldType_Variant {
          name: field_name.clone(),
          type_: field_type.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn float_type(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::FloatType> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("bigfloat")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::core::FloatType(Rc::new(crate::hydra::core::FloatType_Variant::Bigfloat)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float32")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::core::FloatType(Rc::new(crate::hydra::core::FloatType_Variant::Float32)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float64")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::core::FloatType(Rc::new(crate::hydra::core::FloatType_Variant::Float64)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::FloatType>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn float_value(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::FloatValue> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("bigfloat")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: OrderedFloat<f64>| crate::hydra::core::FloatValue(Rc::new(crate::hydra::core::FloatValue_Variant::Bigfloat(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Float (v0_) => {
                  let v0_ = v0_.clone() ;
                  match &*v0_.clone().0 {
                    crate::hydra::core::FloatValue_Variant::Bigfloat (v0_) => {
                      let v0_ = v0_.clone() ;
                      Right(v0_.clone())},
                    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected bigfloat value")))))}},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected bigfloat literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float32")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: OrderedFloat<f32>| crate::hydra::core::FloatValue(Rc::new(crate::hydra::core::FloatValue_Variant::Float32(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Float (v0_) => {
                  let v0_ = v0_.clone() ;
                  match &*v0_.clone().0 {
                    crate::hydra::core::FloatValue_Variant::Float32 (v0_) => {
                      let v0_ = v0_.clone() ;
                      Right(v0_.clone())},
                    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected float32 value")))))}},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected float32 literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float64")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: OrderedFloat<f64>| crate::hydra::core::FloatValue(Rc::new(crate::hydra::core::FloatValue_Variant::Float64(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Float (v0_) => {
                  let v0_ = v0_.clone() ;
                  match &*v0_.clone().0 {
                    crate::hydra::core::FloatValue_Variant::Float64 (v0_) => {
                      let v0_ = v0_.clone() ;
                      Right(v0_.clone())},
                    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected float64 value")))))}},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected float64 literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone()))))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::FloatValue>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn forall_type(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::ForallType> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("parameter"), name, field_map.clone(), cx.clone()), |field_parameter: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("body"), type_, field_map.clone(), cx.clone()), |field_body: crate::hydra::core::Type| Right(crate::hydra::core::ForallType(Rc::new(crate::hydra::core::ForallType_Variant {
          parameter: field_parameter.clone(),
          body: field_body.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn function(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Function> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("elimination")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Elimination| crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Elimination(t.clone()))), elimination(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("lambda")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Lambda| crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Lambda(t.clone()))), lambda(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("primitive")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Name| crate::hydra::core::Function(Rc::new(crate::hydra::core::Function_Variant::Primitive(t.clone()))), name(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Function>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn function_type(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::FunctionType> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("domain"), type_, field_map.clone(), cx.clone()), |field_domain: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("codomain"), type_, field_map.clone(), cx.clone()), |field_codomain: crate::hydra::core::Type| Right(crate::hydra::core::FunctionType(Rc::new(crate::hydra::core::FunctionType_Variant {
          domain: field_domain.clone(),
          codomain: field_codomain.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn injection(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Injection> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("typeName"), name, field_map.clone(), cx.clone()), |field_type_name: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("field"), field, field_map.clone(), cx.clone()), |field_field: crate::hydra::core::Field| Right(crate::hydra::core::Injection(Rc::new(crate::hydra::core::Injection_Variant {
          type_name: field_type_name.clone(),
          field: field_field.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn integer_type(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::IntegerType> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("bigint")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Bigint)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int8")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int8)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int16")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int16)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int32")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int32)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int64")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Int64)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint8")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint8)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint16")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint16)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint32")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint32)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint64")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::core::IntegerType(Rc::new(crate::hydra::core::IntegerType_Variant::Uint64)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::IntegerType>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn integer_value(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::IntegerValue> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("bigint")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: String| crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Bigint(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Integer (v0_) => {
                  let v0_ = v0_.clone() ;
                  match &*v0_.clone().0 {
                    crate::hydra::core::IntegerValue_Variant::Bigint (v0_) => {
                      let v0_ = v0_.clone() ;
                      Right(v0_.clone())},
                    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected bigint value")))))}},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected bigint literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int8")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: i8| crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int8(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Integer (v0_) => {
                  let v0_ = v0_.clone() ;
                  match &*v0_.clone().0 {
                    crate::hydra::core::IntegerValue_Variant::Int8 (v0_) => {
                      let v0_ = v0_.clone() ;
                      Right(v0_.clone())},
                    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int8 value")))))}},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int8 literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int16")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: i16| crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int16(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Integer (v0_) => {
                  let v0_ = v0_.clone() ;
                  match &*v0_.clone().0 {
                    crate::hydra::core::IntegerValue_Variant::Int16 (v0_) => {
                      let v0_ = v0_.clone() ;
                      Right(v0_.clone())},
                    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int16 value")))))}},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int16 literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int32")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: i32| crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int32(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
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
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("int64")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: i64| crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Int64(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Integer (v0_) => {
                  let v0_ = v0_.clone() ;
                  match &*v0_.clone().0 {
                    crate::hydra::core::IntegerValue_Variant::Int64 (v0_) => {
                      let v0_ = v0_.clone() ;
                      Right(v0_.clone())},
                    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int64 value")))))}},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected int64 literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint8")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: u8| crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Uint8(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Integer (v0_) => {
                  let v0_ = v0_.clone() ;
                  match &*v0_.clone().0 {
                    crate::hydra::core::IntegerValue_Variant::Uint8 (v0_) => {
                      let v0_ = v0_.clone() ;
                      Right(v0_.clone())},
                    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected uint8 value")))))}},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected uint8 literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint16")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: u16| crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Uint16(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Integer (v0_) => {
                  let v0_ = v0_.clone() ;
                  match &*v0_.clone().0 {
                    crate::hydra::core::IntegerValue_Variant::Uint16 (v0_) => {
                      let v0_ = v0_.clone() ;
                      Right(v0_.clone())},
                    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected uint16 value")))))}},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected uint16 literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint32")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: u32| crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Uint32(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Integer (v0_) => {
                  let v0_ = v0_.clone() ;
                  match &*v0_.clone().0 {
                    crate::hydra::core::IntegerValue_Variant::Uint32 (v0_) => {
                      let v0_ = v0_.clone() ;
                      Right(v0_.clone())},
                    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected uint32 value")))))}},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected uint32 literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("uint64")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: u64| crate::hydra::core::IntegerValue(Rc::new(crate::hydra::core::IntegerValue_Variant::Uint64(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Integer (v0_) => {
                  let v0_ = v0_.clone() ;
                  match &*v0_.clone().0 {
                    crate::hydra::core::IntegerValue_Variant::Uint64 (v0_) => {
                      let v0_ = v0_.clone() ;
                      Right(v0_.clone())},
                    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected uint64 value")))))}},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected uint64 literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone()))))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::IntegerValue>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn lambda(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Lambda> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("parameter"), name, field_map.clone(), cx.clone()), |field_parameter: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("domain"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_maybe(type_, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_domain: Option<crate::hydra::core::Type>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("body"), term, field_map.clone(), cx.clone()), |field_body: crate::hydra::core::Term| Right(crate::hydra::core::Lambda(Rc::new(crate::hydra::core::Lambda_Variant {
          parameter: field_parameter.clone(),
          domain: field_domain.clone(),
          body: field_body.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn let_(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Let> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("bindings"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(binding, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_bindings: Vec<crate::hydra::core::Binding>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("body"), term, field_map.clone(), cx.clone()), |field_body: crate::hydra::core::Term| Right(crate::hydra::core::Let(Rc::new(crate::hydra::core::Let_Variant {
          bindings: field_bindings.clone(),
          body: field_body.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn literal(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Literal> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("binary")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: Vec<u8>| crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Binary(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Binary (v0_) => {
                  let v0_ = v0_.clone() ;
                  Right(v0_.clone())},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected binary literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("boolean")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: bool| crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Boolean(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::Boolean (v0_) => {
                  let v0_ = v0_.clone() ;
                  Right(v0_.clone())},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected boolean literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::FloatValue| crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Float(t.clone()))), float_value(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("integer")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::IntegerValue| crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::Integer(t.clone()))), integer_value(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("string")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: String| crate::hydra::core::Literal(Rc::new(crate::hydra::core::Literal_Variant::String(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::String (v0_) => {
                  let v0_ = v0_.clone() ;
                  Right(v0_.clone())},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone()))))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Literal>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn literal_type(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::LiteralType> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("binary")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Binary)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("boolean")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Boolean)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("float")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::FloatType| crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Float(t.clone()))), float_type(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("integer")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::IntegerType| crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::Integer(t.clone()))), integer_type(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("string")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::core::LiteralType(Rc::new(crate::hydra::core::LiteralType_Variant::String)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::LiteralType>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn map_type(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::MapType> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("keys"), type_, field_map.clone(), cx.clone()), |field_keys: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("values"), type_, field_map.clone(), cx.clone()), |field_values: crate::hydra::core::Type| Right(crate::hydra::core::MapType(Rc::new(crate::hydra::core::MapType_Variant {
          keys: field_keys.clone(),
          values: field_values.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn name(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Name> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: String| crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(b.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
        crate::hydra::core::Term_Variant::Literal (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::Literal_Variant::String (v0_) => {
              let v0_ = v0_.clone() ;
              Right(v0_.clone())},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
        _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), v0_.clone().0.body.clone())))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn projection(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Projection> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("typeName"), name, field_map.clone(), cx.clone()), |field_type_name: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("field"), name, field_map.clone(), cx.clone()), |field_field: crate::hydra::core::Name| Right(crate::hydra::core::Projection(Rc::new(crate::hydra::core::Projection_Variant {
          type_name: field_type_name.clone(),
          field: field_field.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn record(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Record> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("typeName"), name, field_map.clone(), cx.clone()), |field_type_name: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("fields"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(field, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_fields: Vec<crate::hydra::core::Field>| Right(crate::hydra::core::Record(Rc::new(crate::hydra::core::Record_Variant {
          type_name: field_type_name.clone(),
          fields: field_fields.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn term(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Term> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("annotated")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::AnnotatedTerm| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Annotated(t.clone()))), annotated_term(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("application")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Application| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Application(t.clone()))), application(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("either")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: Either<crate::hydra::core::Term, crate::hydra::core::Term>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Either(t.clone()))), crate::hydra::extract::helpers::decode_either(term, term, cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("function")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Function| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Function(t.clone()))), function(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("let")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Let| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Let(t.clone()))), let_(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("list")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: Vec<crate::hydra::core::Term>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::List(t.clone()))), crate::hydra::extract::helpers::decode_list(term, cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Literal| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Literal(t.clone()))), literal(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("map")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: BTreeMap<crate::hydra::core::Term, crate::hydra::core::Term>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Map(t.clone()))), crate::hydra::extract::helpers::decode_map(term, term, cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("maybe")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: Option<crate::hydra::core::Term>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Maybe(t.clone()))), crate::hydra::extract::helpers::decode_maybe(term, cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("pair")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: (crate::hydra::core::Term, crate::hydra::core::Term)| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Pair(t.clone()))), crate::hydra::extract::helpers::decode_pair(term, term, cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("record")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Record| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Record(t.clone()))), record(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("set")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: BTreeSet<crate::hydra::core::Term>| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Set(t.clone()))), crate::hydra::extract::helpers::decode_set(term, cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeApplication")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::TypeApplicationTerm| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeApplication(t.clone()))), type_application_term(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("typeLambda")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::TypeLambda| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::TypeLambda(t.clone()))), type_lambda(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("union")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Injection| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Union(t.clone()))), injection(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unit")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Unit)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("variable")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Name| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Variable(t.clone()))), name(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("wrap")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::WrappedTerm| crate::hydra::core::Term(Rc::new(crate::hydra::core::Term_Variant::Wrap(t.clone()))), wrapped_term(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Term>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn type_(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Type> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("annotated")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::AnnotatedType| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Annotated(t.clone()))), annotated_type(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("application")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::ApplicationType| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Application(t.clone()))), application_type(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("either")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::EitherType| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Either(t.clone()))), either_type(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("forall")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::ForallType| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Forall(t.clone()))), forall_type(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("function")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::FunctionType| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Function(t.clone()))), function_type(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("list")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Type| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::List(t.clone()))), type_(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("literal")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::LiteralType| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Literal(t.clone()))), literal_type(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("map")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::MapType| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Map(t.clone()))), map_type(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("maybe")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Type| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Maybe(t.clone()))), type_(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("pair")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::PairType| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Pair(t.clone()))), pair_type(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("record")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: Vec<crate::hydra::core::FieldType>| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Record(t.clone()))), crate::hydra::extract::helpers::decode_list(field_type, cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("set")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Type| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Set(t.clone()))), type_(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("union")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: Vec<crate::hydra::core::FieldType>| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Union(t.clone()))), crate::hydra::extract::helpers::decode_list(field_type, cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("unit")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Unit)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("variable")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Name| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Variable(t.clone()))), name(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("wrap")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Type| crate::hydra::core::Type(Rc::new(crate::hydra::core::Type_Variant::Wrap(t.clone()))), type_(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::Type>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn type_application_term(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::TypeApplicationTerm> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("body"), term, field_map.clone(), cx.clone()), |field_body: crate::hydra::core::Term| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("type"), type_, field_map.clone(), cx.clone()), |field_type: crate::hydra::core::Type| Right(crate::hydra::core::TypeApplicationTerm(Rc::new(crate::hydra::core::TypeApplicationTerm_Variant {
          body: field_body.clone(),
          type_: field_type.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn type_lambda(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::TypeLambda> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("parameter"), name, field_map.clone(), cx.clone()), |field_parameter: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("body"), term, field_map.clone(), cx.clone()), |field_body: crate::hydra::core::Term| Right(crate::hydra::core::TypeLambda(Rc::new(crate::hydra::core::TypeLambda_Variant {
          parameter: field_parameter.clone(),
          body: field_body.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn type_scheme(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::TypeScheme> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("variables"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(name, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_variables: Vec<crate::hydra::core::Name>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("type"), type_, field_map.clone(), cx.clone()), |field_type: crate::hydra::core::Type| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("constraints"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_maybe(|v12: crate::hydra::graph::Graph, v22: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_map(name, type_variable_metadata, v12.clone(), v22.clone()), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_constraints: Option<BTreeMap<crate::hydra::core::Name, crate::hydra::core::TypeVariableMetadata>>| Right(crate::hydra::core::TypeScheme(Rc::new(crate::hydra::core::TypeScheme_Variant {
          variables: field_variables.clone(),
          type_: field_type.clone(),
          constraints: field_constraints.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn type_variable_metadata(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::TypeVariableMetadata> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("classes"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_set(name, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_classes: BTreeSet<crate::hydra::core::Name>| Right(crate::hydra::core::TypeVariableMetadata(Rc::new(crate::hydra::core::TypeVariableMetadata_Variant {
          classes: field_classes.clone()}))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn wrapped_term(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::core::WrappedTerm> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("typeName"), name, field_map.clone(), cx.clone()), |field_type_name: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("body"), term, field_map.clone(), cx.clone()), |field_body: crate::hydra::core::Term| Right(crate::hydra::core::WrappedTerm(Rc::new(crate::hydra::core::WrappedTerm_Variant {
          type_name: field_type_name.clone(),
          body: field_body.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}
