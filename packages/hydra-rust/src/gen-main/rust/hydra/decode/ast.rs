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
use crate::hydra::ast::*;
use crate::hydra::util::*;

pub fn associativity(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::Associativity> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("none")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::ast::Associativity(Rc::new(crate::hydra::ast::Associativity_Variant::None)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("left")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::ast::Associativity(Rc::new(crate::hydra::ast::Associativity_Variant::Left)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("right")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::ast::Associativity(Rc::new(crate::hydra::ast::Associativity_Variant::Right)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("both")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::ast::Associativity(Rc::new(crate::hydra::ast::Associativity_Variant::Both)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::Associativity>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn block_style(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::BlockStyle> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("indent"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_maybe(|cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::String (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_indent: Option<String>| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("newlineBeforeContent"), |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::Boolean (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected boolean literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), field_map.clone(), cx.clone()), |field_newline_before_content: bool| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("newlineAfterContent"), |cx2: crate::hydra::graph::Graph, raw2: crate::hydra::core::Term| crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
          crate::hydra::core::Term_Variant::Literal (v0_) => {
            let v0_ = v0_.clone() ;
            match &*v0_.clone().0 {
              crate::hydra::core::Literal_Variant::Boolean (v0_) => {
                let v0_ = v0_.clone() ;
                Right(v0_.clone())},
              _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected boolean literal")))))}},
          _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx2.clone(), raw2.clone())), field_map.clone(), cx.clone()), |field_newline_after_content: bool| Right(crate::hydra::ast::BlockStyle(Rc::new(crate::hydra::ast::BlockStyle_Variant {
          indent: field_indent.clone(),
          newline_before_content: field_newline_before_content.clone(),
          newline_after_content: field_newline_after_content.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn bracket_expr(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::BracketExpr> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("brackets"), brackets, field_map.clone(), cx.clone()), |field_brackets: crate::hydra::ast::Brackets| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("enclosed"), expr, field_map.clone(), cx.clone()), |field_enclosed: crate::hydra::ast::Expr| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("style"), block_style, field_map.clone(), cx.clone()), |field_style: crate::hydra::ast::BlockStyle| Right(crate::hydra::ast::BracketExpr(Rc::new(crate::hydra::ast::BracketExpr_Variant {
          brackets: field_brackets.clone(),
          enclosed: field_enclosed.clone(),
          style: field_style.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn brackets(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::Brackets> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("open"), symbol, field_map.clone(), cx.clone()), |field_open: crate::hydra::ast::Symbol| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("close"), symbol, field_map.clone(), cx.clone()), |field_close: crate::hydra::ast::Symbol| Right(crate::hydra::ast::Brackets(Rc::new(crate::hydra::ast::Brackets_Variant {
          open: field_open.clone(),
          close: field_close.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn expr(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::Expr> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("const")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::ast::Symbol| crate::hydra::ast::Expr(Rc::new(crate::hydra::ast::Expr_Variant::Const(t.clone()))), symbol(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("indent")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::ast::IndentedExpression| crate::hydra::ast::Expr(Rc::new(crate::hydra::ast::Expr_Variant::Indent(t.clone()))), indented_expression(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("op")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::ast::OpExpr| crate::hydra::ast::Expr(Rc::new(crate::hydra::ast::Expr_Variant::Op(t.clone()))), op_expr(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("brackets")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::ast::BracketExpr| crate::hydra::ast::Expr(Rc::new(crate::hydra::ast::Expr_Variant::Brackets(t.clone()))), bracket_expr(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("seq")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: crate::hydra::ast::SeqExpr| crate::hydra::ast::Expr(Rc::new(crate::hydra::ast::Expr_Variant::Seq(t.clone()))), seq_expr(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::Expr>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn indented_expression(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::IndentedExpression> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("style"), indent_style, field_map.clone(), cx.clone()), |field_style: crate::hydra::ast::IndentStyle| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("expr"), expr, field_map.clone(), cx.clone()), |field_expr: crate::hydra::ast::Expr| Right(crate::hydra::ast::IndentedExpression(Rc::new(crate::hydra::ast::IndentedExpression_Variant {
          style: field_style.clone(),
          expr: field_expr.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn indent_style(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::IndentStyle> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("allLines")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: String| crate::hydra::ast::IndentStyle(Rc::new(crate::hydra::ast::IndentStyle_Variant::AllLines(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::String (v0_) => {
                  let v0_ = v0_.clone() ;
                  Right(v0_.clone())},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("subsequentLines")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: String| crate::hydra::ast::IndentStyle(Rc::new(crate::hydra::ast::IndentStyle_Variant::SubsequentLines(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
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
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::IndentStyle>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn op(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::Op> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("symbol"), symbol, field_map.clone(), cx.clone()), |field_symbol: crate::hydra::ast::Symbol| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("padding"), padding, field_map.clone(), cx.clone()), |field_padding: crate::hydra::ast::Padding| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("precedence"), precedence, field_map.clone(), cx.clone()), |field_precedence: crate::hydra::ast::Precedence| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("associativity"), associativity, field_map.clone(), cx.clone()), |field_associativity: crate::hydra::ast::Associativity| Right(crate::hydra::ast::Op(Rc::new(crate::hydra::ast::Op_Variant {
          symbol: field_symbol.clone(),
          padding: field_padding.clone(),
          precedence: field_precedence.clone(),
          associativity: field_associativity.clone()})))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn op_expr(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::OpExpr> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("op"), op, field_map.clone(), cx.clone()), |field_op: crate::hydra::ast::Op| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("lhs"), expr, field_map.clone(), cx.clone()), |field_lhs: crate::hydra::ast::Expr| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("rhs"), expr, field_map.clone(), cx.clone()), |field_rhs: crate::hydra::ast::Expr| Right(crate::hydra::ast::OpExpr(Rc::new(crate::hydra::ast::OpExpr_Variant {
          op: field_op.clone(),
          lhs: field_lhs.clone(),
          rhs: field_rhs.clone()}))))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn padding(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::Padding> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("left"), ws, field_map.clone(), cx.clone()), |field_left: crate::hydra::ast::Ws| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("right"), ws, field_map.clone(), cx.clone()), |field_right: crate::hydra::ast::Ws| Right(crate::hydra::ast::Padding(Rc::new(crate::hydra::ast::Padding_Variant {
          left: field_left.clone(),
          right: field_right.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn precedence(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::Precedence> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: i32| crate::hydra::ast::Precedence(Rc::new(crate::hydra::ast::Precedence_Variant(b.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
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
        _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), v0_.clone().0.body.clone())))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn seq_expr(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::SeqExpr> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Record (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field_map = crate::hydra::extract::helpers::to_field_map(v0_.clone()) ;
        crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("op"), op, field_map.clone(), cx.clone()), |field_op: crate::hydra::ast::Op| crate::hydra::lib::eithers::bind(crate::hydra::extract::helpers::require_field(String::from("elements"), |v1: crate::hydra::graph::Graph, v2: crate::hydra::core::Term| crate::hydra::extract::helpers::decode_list(expr, v1.clone(), v2.clone()), field_map.clone(), cx.clone()), |field_elements: Vec<crate::hydra::ast::Expr>| Right(crate::hydra::ast::SeqExpr(Rc::new(crate::hydra::ast::SeqExpr_Variant {
          op: field_op.clone(),
          elements: field_elements.clone()})))))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected record")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn symbol(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::Symbol> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Wrap (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::lib::eithers::map(|b: String| crate::hydra::ast::Symbol(Rc::new(crate::hydra::ast::Symbol_Variant(b.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
        crate::hydra::core::Term_Variant::Literal (v0_) => {
          let v0_ = v0_.clone() ;
          match &*v0_.clone().0 {
            crate::hydra::core::Literal_Variant::String (v0_) => {
              let v0_ = v0_.clone() ;
              Right(v0_.clone())},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
        _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), v0_.clone().0.body.clone())))},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected wrapped type")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}

pub fn ws(cx: crate::hydra::graph::Graph, raw: crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::Ws> {
  crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped: crate::hydra::core::Term| match &*stripped.clone().0 {
    crate::hydra::core::Term_Variant::Union (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let field = v0_.clone().0.field.clone() ;
        let fname = field.clone().0.name.clone() ;
        let fterm = field.clone().0.term.clone() ;
        let variant_map = crate::hydra::lib::maps::from_list(Vec::from([
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("none")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::None)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("space")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::Space)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("break")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::Break)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone()))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("breakAndIndent")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: String| crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::BreakAndIndent(t.clone()))), crate::hydra::lib::eithers::either(|err: String| Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(err.clone())))), |stripped2: crate::hydra::core::Term| match &*stripped2.clone().0 {
            crate::hydra::core::Term_Variant::Literal (v0_) => {
              let v0_ = v0_.clone() ;
              match &*v0_.clone().0 {
                crate::hydra::core::Literal_Variant::String (v0_) => {
                  let v0_ = v0_.clone() ;
                  Right(v0_.clone())},
                _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected string literal")))))}},
            _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected literal")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), input.clone())))),
          (crate::hydra::core::Name(Rc::new(crate::hydra::core::Name_Variant(String::from("doubleBreak")))), |input: crate::hydra::core::Term| crate::hydra::lib::eithers::map(|t: ()| crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::DoubleBreak)), crate::hydra::extract::helpers::decode_unit(cx.clone(), input.clone())))])) ;
        crate::hydra::lib::maybes::maybe(Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(crate::hydra::lib::strings::cat(Vec::from([
          String::from("no such field "),
          fname.clone().0.0.clone(),
          String::from(" in union")])))))), |f: Rc<dyn Fn(crate::hydra::core::Term) -> Either<crate::hydra::error::DecodingError, crate::hydra::ast::Ws>>| f.clone()(fterm.clone()), crate::hydra::lib::maps::lookup(fname.clone(), variant_map.clone()))}},
    _ => Left(crate::hydra::error::DecodingError(Rc::new(crate::hydra::error::DecodingError_Variant(String::from("expected union")))))}, crate::hydra::lexical::strip_and_dereference_term_either(cx.clone(), raw.clone()))}
