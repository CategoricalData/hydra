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
use crate::hydra::query::*;
use crate::hydra::util::*;

pub fn comparison_constraint(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::ComparisonConstraint> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("equal")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::query::ComparisonConstraint(Rc::new(crate::hydra::query::ComparisonConstraint_Variant::Equal)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("notEqual")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::query::ComparisonConstraint(Rc::new(crate::hydra::query::ComparisonConstraint_Variant::NotEqual)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("lessThan")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::query::ComparisonConstraint(Rc::new(crate::hydra::query::ComparisonConstraint_Variant::LessThan)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("greaterThan")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::query::ComparisonConstraint(Rc::new(crate::hydra::query::ComparisonConstraint_Variant::GreaterThan)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("lessThanOrEqual")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::query::ComparisonConstraint(Rc::new(crate::hydra::query::ComparisonConstraint_Variant::LessThanOrEqual)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("greaterThanOrEqual")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::query::ComparisonConstraint(Rc::new(crate::hydra::query::ComparisonConstraint_Variant::GreaterThanOrEqual)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::ComparisonConstraint>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn edge(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::Edge> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("type"), crate::hydra::decode::core::name, field_map.clone(), cx.clone()), |field_type: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("out"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_maybe(crate::hydra::decode::core::name, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_out: Option<crate::hydra::core::Name>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("in"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_maybe(crate::hydra::decode::core::name, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_in: Option<crate::hydra::core::Name>| Right(crate::hydra::query::Edge(Rc::new(crate::hydra::query::Edge_Variant {
          type_: field_type.clone(),
          out: field_out.clone(),
          in_: field_in.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn graph_pattern(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::GraphPattern> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("graph"), crate::hydra::decode::core::name, field_map.clone(), cx.clone()), |field_graph: crate::hydra::core::Name| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("patterns"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(pattern, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_patterns: Vec<crate::hydra::query::Pattern>| Right(crate::hydra::query::GraphPattern(Rc::new(crate::hydra::query::GraphPattern_Variant {
          graph: field_graph.clone(),
          patterns: field_patterns.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn node(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::Node> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("term")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Term| crate::hydra::query::Node(Rc::new(crate::hydra::query::Node_Variant::Term(t.clone()))), crate::hydra::decode::core::term(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("variable")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::query::Variable| crate::hydra::query::Node(Rc::new(crate::hydra::query::Node_Variant::Variable(t.clone()))), variable(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("wildcard")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::query::Node(Rc::new(crate::hydra::query::Node_Variant::Wildcard)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::Node>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn path(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::Path> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("step")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::query::Step| crate::hydra::query::Path(Rc::new(crate::hydra::query::Path_Variant::Step(t.clone()))), step(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("regex")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::query::RegexSequence| crate::hydra::query::Path(Rc::new(crate::hydra::query::Path_Variant::Regex(t.clone()))), regex_sequence(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("inverse")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::query::Path| crate::hydra::query::Path(Rc::new(crate::hydra::query::Path_Variant::Inverse(t.clone()))), path(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::Path>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn path_equation(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::PathEquation> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("left"), path, field_map.clone(), cx.clone()), |field_left: crate::hydra::query::Path| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("right"), path, field_map.clone(), cx.clone()), |field_right: crate::hydra::query::Path| Right(crate::hydra::query::PathEquation(Rc::new(crate::hydra::query::PathEquation_Variant {
          left: field_left.clone(),
          right: field_right.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn pattern(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::Pattern> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("triple")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::query::TriplePattern| crate::hydra::query::Pattern(Rc::new(crate::hydra::query::Pattern_Variant::Triple(t.clone()))), triple_pattern(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("negation")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::query::Pattern| crate::hydra::query::Pattern(Rc::new(crate::hydra::query::Pattern_Variant::Negation(t.clone()))), pattern(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("conjunction")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: Vec<crate::hydra::query::Pattern>| crate::hydra::query::Pattern(Rc::new(crate::hydra::query::Pattern_Variant::Conjunction(t.clone()))), crate::hydra::extract::helpers::decode_list(pattern, cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("disjunction")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: Vec<crate::hydra::query::Pattern>| crate::hydra::query::Pattern(Rc::new(crate::hydra::query::Pattern_Variant::Disjunction(t.clone()))), crate::hydra::extract::helpers::decode_list(pattern, cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("graph")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::query::GraphPattern| crate::hydra::query::Pattern(Rc::new(crate::hydra::query::Pattern_Variant::Graph(t.clone()))), graph_pattern(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::Pattern>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn pattern_implication(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::PatternImplication> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("antecedent"), pattern, field_map.clone(), cx.clone()), |field_antecedent: crate::hydra::query::Pattern| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("consequent"), pattern, field_map.clone(), cx.clone()), |field_consequent: crate::hydra::query::Pattern| Right(crate::hydra::query::PatternImplication(Rc::new(crate::hydra::query::PatternImplication_Variant {
          antecedent: field_antecedent.clone(),
          consequent: field_consequent.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn query(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::Query> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("variables"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(variable, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_variables: Vec<crate::hydra::query::Variable>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("patterns"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(pattern, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_patterns: Vec<crate::hydra::query::Pattern>| Right(crate::hydra::query::Query(Rc::new(crate::hydra::query::Query_Variant {
          variables: field_variables.clone(),
          patterns: field_patterns.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn range(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::Range> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("min"), |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
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
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), field_map.clone(), cx.clone()), |field_min: i32| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("max"), |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
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
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), field_map.clone(), cx.clone()), |field_max: i32| Right(crate::hydra::query::Range(Rc::new(crate::hydra::query::Range_Variant {
          min: field_min.clone(),
          max: field_max.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn regex_quantifier(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::RegexQuantifier> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("one")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::query::RegexQuantifier(Rc::new(crate::hydra::query::RegexQuantifier_Variant::One)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("zeroOrOne")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::query::RegexQuantifier(Rc::new(crate::hydra::query::RegexQuantifier_Variant::ZeroOrOne)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("zeroOrMore")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::query::RegexQuantifier(Rc::new(crate::hydra::query::RegexQuantifier_Variant::ZeroOrMore)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("oneOrMore")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::query::RegexQuantifier(Rc::new(crate::hydra::query::RegexQuantifier_Variant::OneOrMore)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("exactly")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: i32| crate::hydra::query::RegexQuantifier(Rc::new(crate::hydra::query::RegexQuantifier_Variant::Exactly(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
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
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("atLeast")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: i32| crate::hydra::query::RegexQuantifier(Rc::new(crate::hydra::query::RegexQuantifier_Variant::AtLeast(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
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
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("range")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::query::Range| crate::hydra::query::RegexQuantifier(Rc::new(crate::hydra::query::RegexQuantifier_Variant::Range(t.clone()))), range(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::RegexQuantifier>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn regex_sequence(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::RegexSequence> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("path"), path, field_map.clone(), cx.clone()), |field_path: crate::hydra::query::Path| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("quantifier"), regex_quantifier, field_map.clone(), cx.clone()), |field_quantifier: crate::hydra::query::RegexQuantifier| Right(crate::hydra::query::RegexSequence(Rc::new(crate::hydra::query::RegexSequence_Variant {
          path: field_path.clone(),
          quantifier: field_quantifier.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn step(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::Step> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("edge")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::query::Edge| crate::hydra::query::Step(Rc::new(crate::hydra::query::Step_Variant::Edge(t.clone()))), edge(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("project")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::core::Projection| crate::hydra::query::Step(Rc::new(crate::hydra::query::Step_Variant::Project(t.clone()))), crate::hydra::decode::core::projection(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("compare")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::query::ComparisonConstraint| crate::hydra::query::Step(Rc::new(crate::hydra::query::Step_Variant::Compare(t.clone()))), comparison_constraint(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::Step>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn triple_pattern(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::TriplePattern> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("subject"), node, field_map.clone(), cx.clone()), |field_subject: crate::hydra::query::Node| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("predicate"), path, field_map.clone(), cx.clone()), |field_predicate: crate::hydra::query::Path| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("object"), node, field_map.clone(), cx.clone()), |field_object: crate::hydra::query::Node| Right(crate::hydra::query::TriplePattern(Rc::new(crate::hydra::query::TriplePattern_Variant {
          subject: field_subject.clone(),
          predicate: field_predicate.clone(),
          object: field_object.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn variable(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::query::Variable> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: String| crate::hydra::query::Variable(Rc::new(crate::hydra::query::Variable_Variant(b.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
        crate::hydra::core::Term_Variant::Literal (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::Literal_Variant::String (v0_) => {
              let v0_ = v0_.clone() ;
              Right(v0_.clone())},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
        _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), v0_.clone().0.body.clone())))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}
