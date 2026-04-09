#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
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

pub fn angle_braces() -> crate::hydra::ast::Brackets {
  crate::hydra::ast::Brackets(Rc::new(crate::hydra::ast::Brackets_Variant {
    open: crate::hydra::ast::Symbol(Rc::new(crate::hydra::ast::Symbol_Variant(String::from("<")))),
    close: crate::hydra::ast::Symbol(Rc::new(crate::hydra::ast::Symbol_Variant(String::from(">"))))}))}

pub fn angle_braces_list(style: crate::hydra::ast::BlockStyle, els: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(els.clone()), cst(String::from("<>")), brackets(angle_braces, style.clone(), comma_sep(style.clone(), els.clone())))}

pub fn braces_list_adaptive(els: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  let inline_list = curly_braces_list(None, inline_style, els.clone()) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::gt(expression_length(inline_list.clone()), 70i32), curly_braces_list(None, half_block_style, els.clone()), inline_list.clone())}

pub fn bracket_list(style: crate::hydra::ast::BlockStyle, els: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(els.clone()), cst(String::from("[]")), brackets(square_brackets, style.clone(), comma_sep(style.clone(), els.clone())))}

pub fn bracket_list_adaptive(els: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  let inline_list = bracket_list(inline_style, els.clone()) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::gt(expression_length(inline_list.clone()), 70i32), bracket_list(half_block_style, els.clone()), inline_list.clone())}

pub fn brackets(br: crate::hydra::ast::Brackets, style: crate::hydra::ast::BlockStyle, e: crate::hydra::ast::Expr) -> crate::hydra::ast::Expr {
  crate::hydra::ast::Expr(Rc::new(crate::hydra::ast::Expr_Variant::Brackets(crate::hydra::ast::BracketExpr(Rc::new(crate::hydra::ast::BracketExpr_Variant {
    brackets: br.clone(),
    enclosed: e.clone(),
    style: style.clone()})))))}

pub fn comma_sep(v1: crate::hydra::ast::BlockStyle, v2: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  symbol_sep(String::from(","), v1.clone(), v2.clone())}

pub fn curly_block(style: crate::hydra::ast::BlockStyle, e: crate::hydra::ast::Expr) -> crate::hydra::ast::Expr {
  curly_braces_list(None, style.clone(), Vec::from([
    e.clone()]))}

pub fn curly_braces() -> crate::hydra::ast::Brackets {
  crate::hydra::ast::Brackets(Rc::new(crate::hydra::ast::Brackets_Variant {
    open: crate::hydra::ast::Symbol(Rc::new(crate::hydra::ast::Symbol_Variant(String::from("{")))),
    close: crate::hydra::ast::Symbol(Rc::new(crate::hydra::ast::Symbol_Variant(String::from("}"))))}))}

pub fn curly_braces_list(msymb: Option<String>, style: crate::hydra::ast::BlockStyle, els: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(els.clone()), cst(String::from("{}")), brackets(curly_braces, style.clone(), symbol_sep(crate::hydra::lib::maybes::from_maybe(String::from(","), msymb.clone()), style.clone(), els.clone())))}

pub fn cst(s: String) -> crate::hydra::ast::Expr {
  crate::hydra::ast::Expr(Rc::new(crate::hydra::ast::Expr_Variant::Const(sym(s.clone()))))}

pub fn custom_indent(idt: String, s: String) -> String {
  crate::hydra::lib::strings::cat(crate::hydra::lib::lists::intersperse(String::from("\n"), crate::hydra::lib::lists::map(|line: String| crate::hydra::lib::strings::cat2(idt.clone(), line.clone()), crate::hydra::lib::strings::lines(s.clone()))))}

pub fn custom_indent_block(idt: String, els: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  let idt_op = crate::hydra::ast::Op(Rc::new(crate::hydra::ast::Op_Variant {
    symbol: sym(String::from("")),
    padding: crate::hydra::ast::Padding(Rc::new(crate::hydra::ast::Padding_Variant {
      left: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::Space)),
      right: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::BreakAndIndent(idt.clone())))})),
    precedence: crate::hydra::ast::Precedence(Rc::new(crate::hydra::ast::Precedence_Variant(0i32))),
    associativity: crate::hydra::ast::Associativity(Rc::new(crate::hydra::ast::Associativity_Variant::None))})) ;
  crate::hydra::lib::maybes::maybe(cst(String::from("")), |head: crate::hydra::ast::Expr| crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(crate::hydra::lib::lists::length(els.clone()), 1i32), head.clone(), ifx(idt_op.clone(), head.clone(), newline_sep(crate::hydra::lib::lists::drop(1i32, els.clone())))), crate::hydra::lib::lists::safe_head(els.clone()))}

pub fn dot_sep(v1: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  sep(crate::hydra::ast::Op(Rc::new(crate::hydra::ast::Op_Variant {
    symbol: sym(String::from(".")),
    padding: crate::hydra::ast::Padding(Rc::new(crate::hydra::ast::Padding_Variant {
      left: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::None)),
      right: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::None))})),
    precedence: crate::hydra::ast::Precedence(Rc::new(crate::hydra::ast::Precedence_Variant(0i32))),
    associativity: crate::hydra::ast::Associativity(Rc::new(crate::hydra::ast::Associativity_Variant::None))})), v1.clone())}

pub fn double_newline_sep(v1: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  sep(crate::hydra::ast::Op(Rc::new(crate::hydra::ast::Op_Variant {
    symbol: sym(String::from("")),
    padding: crate::hydra::ast::Padding(Rc::new(crate::hydra::ast::Padding_Variant {
      left: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::Break)),
      right: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::Break))})),
    precedence: crate::hydra::ast::Precedence(Rc::new(crate::hydra::ast::Precedence_Variant(0i32))),
    associativity: crate::hydra::ast::Associativity(Rc::new(crate::hydra::ast::Associativity_Variant::None))})), v1.clone())}

pub fn double_space() -> String {
  String::from("  ")}

pub fn expression_length(e: crate::hydra::ast::Expr) -> i32 {
  let symbol_length = |s: crate::hydra::ast::Symbol| crate::hydra::lib::strings::length(s.clone().0.0.clone()) ;
  let ws_length = |ws: crate::hydra::ast::Ws| match &*ws.clone().0 {
    crate::hydra::ast::Ws_Variant::None (v0_) => {
      let v0_ = v0_.clone() ;
      0i32},
    crate::hydra::ast::Ws_Variant::Space (v0_) => {
      let v0_ = v0_.clone() ;
      1i32},
    crate::hydra::ast::Ws_Variant::Break (v0_) => {
      let v0_ = v0_.clone() ;
      10000i32},
    crate::hydra::ast::Ws_Variant::BreakAndIndent (v0_) => {
      let v0_ = v0_.clone() ;
      10000i32},
    crate::hydra::ast::Ws_Variant::DoubleBreak (v0_) => {
      let v0_ = v0_.clone() ;
      10000i32}} ;
  let block_style_length = |style: crate::hydra::ast::BlockStyle| {
    let mindent_len = crate::hydra::lib::maybes::maybe(0i32, crate::hydra::lib::strings::length, style.clone().0.indent.clone()) ;
    {
      let nl_before_len = crate::hydra::lib::logic::if_else(style.clone().0.newline_before_content.clone(), 1i32, 0i32) ;
      {
        let nl_after_len = crate::hydra::lib::logic::if_else(style.clone().0.newline_after_content.clone(), 1i32, 0i32) ;
        crate::hydra::lib::math::add(mindent_len.clone(), crate::hydra::lib::math::add(nl_before_len.clone(), nl_after_len.clone()))}}} ;
  let brackets_length = |brackets: crate::hydra::ast::Brackets| crate::hydra::lib::math::add(symbol_length.clone()(brackets.clone().0.open.clone()), symbol_length.clone()(brackets.clone().0.close.clone())) ;
  let bracket_expr_length = |be: crate::hydra::ast::BracketExpr| crate::hydra::lib::math::add(brackets_length.clone()(be.clone().0.brackets.clone()), crate::hydra::lib::math::add(expression_length(be.clone().0.enclosed.clone()), block_style_length.clone()(be.clone().0.style.clone()))) ;
  let indented_expression_length = |ie: crate::hydra::ast::IndentedExpression| {
    let base_len = expression_length(ie.clone().0.expr.clone()) ;
    {
      let indent_len = match &*ie.clone().0.style.clone().0 {
        crate::hydra::ast::IndentStyle_Variant::AllLines (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::lib::strings::length(v0_.clone())},
        crate::hydra::ast::IndentStyle_Variant::SubsequentLines (v0_) => {
          let v0_ = v0_.clone() ;
          crate::hydra::lib::strings::length(v0_.clone())}} ;
      crate::hydra::lib::math::add(base_len.clone(), indent_len.clone())}} ;
  let op_length = |op: crate::hydra::ast::Op| {
    let sym_len = symbol_length.clone()(op.clone().0.symbol.clone()) ;
    {
      let padding = op.clone().0.padding.clone() ;
      {
        let left_len = ws_length.clone()(padding.clone().0.left.clone()) ;
        {
          let right_len = ws_length.clone()(padding.clone().0.right.clone()) ;
          crate::hydra::lib::math::add(sym_len.clone(), crate::hydra::lib::math::add(left_len.clone(), right_len.clone()))}}}} ;
  let op_expr_length = |oe: crate::hydra::ast::OpExpr| {
    let op_len = op_length.clone()(oe.clone().0.op.clone()) ;
    {
      let left_len = expression_length(oe.clone().0.lhs.clone()) ;
      {
        let right_len = expression_length(oe.clone().0.rhs.clone()) ;
        crate::hydra::lib::math::add(op_len.clone(), crate::hydra::lib::math::add(left_len.clone(), right_len.clone()))}}} ;
  let seq_expr_length = |se: crate::hydra::ast::SeqExpr| {
    let sop_len = op_length.clone()(se.clone().0.op.clone()) ;
    {
      let element_lens = crate::hydra::lib::lists::map(expression_length, se.clone().0.elements.clone()) ;
      {
        let total_el_len = crate::hydra::lib::lists::foldl(crate::hydra::lib::math::add, 0i32, element_lens.clone()) ;
        {
          let num_seps = crate::hydra::lib::math::sub(crate::hydra::lib::lists::length(se.clone().0.elements.clone()), 1i32) ;
          crate::hydra::lib::math::add(total_el_len.clone(), crate::hydra::lib::math::mul(sop_len.clone(), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::gt(num_seps.clone(), 0i32), num_seps.clone(), 0i32)))}}}} ;
  match &*e.clone().0 {
    crate::hydra::ast::Expr_Variant::Const (v0_) => {
      let v0_ = v0_.clone() ;
      symbol_length.clone()(v0_.clone())},
    crate::hydra::ast::Expr_Variant::Indent (v0_) => {
      let v0_ = v0_.clone() ;
      indented_expression_length.clone()(v0_.clone())},
    crate::hydra::ast::Expr_Variant::Op (v0_) => {
      let v0_ = v0_.clone() ;
      op_expr_length.clone()(v0_.clone())},
    crate::hydra::ast::Expr_Variant::Brackets (v0_) => {
      let v0_ = v0_.clone() ;
      bracket_expr_length.clone()(v0_.clone())},
    crate::hydra::ast::Expr_Variant::Seq (v0_) => {
      let v0_ = v0_.clone() ;
      seq_expr_length.clone()(v0_.clone())}}}

pub fn full_block_style() -> crate::hydra::ast::BlockStyle {
  crate::hydra::ast::BlockStyle(Rc::new(crate::hydra::ast::BlockStyle_Variant {
    indent: Some(double_space),
    newline_before_content: true,
    newline_after_content: true}))}

pub fn half_block_style() -> crate::hydra::ast::BlockStyle {
  crate::hydra::ast::BlockStyle(Rc::new(crate::hydra::ast::BlockStyle_Variant {
    indent: Some(double_space),
    newline_before_content: true,
    newline_after_content: false}))}

pub fn ifx(op: crate::hydra::ast::Op, lhs: crate::hydra::ast::Expr, rhs: crate::hydra::ast::Expr) -> crate::hydra::ast::Expr {
  crate::hydra::ast::Expr(Rc::new(crate::hydra::ast::Expr_Variant::Op(crate::hydra::ast::OpExpr(Rc::new(crate::hydra::ast::OpExpr_Variant {
    op: op.clone(),
    lhs: lhs.clone(),
    rhs: rhs.clone()})))))}

pub fn indent(v1: String) -> String {
  custom_indent(double_space, v1.clone())}

pub fn indent_block(v1: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  custom_indent_block(double_space, v1.clone())}

pub fn indent_subsequent_lines(idt: String, e: crate::hydra::ast::Expr) -> crate::hydra::ast::Expr {
  crate::hydra::ast::Expr(Rc::new(crate::hydra::ast::Expr_Variant::Indent(crate::hydra::ast::IndentedExpression(Rc::new(crate::hydra::ast::IndentedExpression_Variant {
    style: crate::hydra::ast::IndentStyle(Rc::new(crate::hydra::ast::IndentStyle_Variant::SubsequentLines(idt.clone()))),
    expr: e.clone()})))))}

pub fn infix_ws(op: String, l: crate::hydra::ast::Expr, r: crate::hydra::ast::Expr) -> crate::hydra::ast::Expr {
  space_sep(Vec::from([
    l.clone(),
    cst(op.clone()),
    r.clone()]))}

pub fn infix_ws_list(op: String, opers: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  let op_expr = cst(op.clone()) ;
  let fold_fun = |e: Vec<crate::hydra::ast::Expr>, r: crate::hydra::ast::Expr| crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(e.clone()), Vec::from([
    r.clone()]), crate::hydra::lib::lists::cons(r.clone(), crate::hydra::lib::lists::cons(op_expr.clone(), e.clone()))) ;
  space_sep(crate::hydra::lib::lists::foldl(fold_fun.clone(), Vec::from([]), crate::hydra::lib::lists::reverse(opers.clone())))}

pub fn inline_style() -> crate::hydra::ast::BlockStyle {
  crate::hydra::ast::BlockStyle(Rc::new(crate::hydra::ast::BlockStyle_Variant {
    indent: None,
    newline_before_content: false,
    newline_after_content: false}))}

pub fn newline_sep(v1: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  sep(crate::hydra::ast::Op(Rc::new(crate::hydra::ast::Op_Variant {
    symbol: sym(String::from("")),
    padding: crate::hydra::ast::Padding(Rc::new(crate::hydra::ast::Padding_Variant {
      left: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::None)),
      right: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::Break))})),
    precedence: crate::hydra::ast::Precedence(Rc::new(crate::hydra::ast::Precedence_Variant(0i32))),
    associativity: crate::hydra::ast::Associativity(Rc::new(crate::hydra::ast::Associativity_Variant::None))})), v1.clone())}

pub fn no_padding() -> crate::hydra::ast::Padding {
  crate::hydra::ast::Padding(Rc::new(crate::hydra::ast::Padding_Variant {
    left: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::None)),
    right: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::None))}))}

pub fn no_sep(v1: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  sep(crate::hydra::ast::Op(Rc::new(crate::hydra::ast::Op_Variant {
    symbol: sym(String::from("")),
    padding: crate::hydra::ast::Padding(Rc::new(crate::hydra::ast::Padding_Variant {
      left: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::None)),
      right: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::None))})),
    precedence: crate::hydra::ast::Precedence(Rc::new(crate::hydra::ast::Precedence_Variant(0i32))),
    associativity: crate::hydra::ast::Associativity(Rc::new(crate::hydra::ast::Associativity_Variant::None))})), v1.clone())}

pub fn num(i: i32) -> crate::hydra::ast::Expr {
  cst(crate::hydra::lib::literals::show_int32(i.clone()))}

pub fn op(s: String, p: i32, assoc: crate::hydra::ast::Associativity) -> crate::hydra::ast::Op {
  crate::hydra::ast::Op(Rc::new(crate::hydra::ast::Op_Variant {
    symbol: sym(s.clone()),
    padding: crate::hydra::ast::Padding(Rc::new(crate::hydra::ast::Padding_Variant {
      left: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::Space)),
      right: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::Space))})),
    precedence: crate::hydra::ast::Precedence(Rc::new(crate::hydra::ast::Precedence_Variant(p.clone()))),
    associativity: assoc.clone()}))}

pub fn or_op(newlines: bool) -> crate::hydra::ast::Op {
  crate::hydra::ast::Op(Rc::new(crate::hydra::ast::Op_Variant {
    symbol: sym(String::from("|")),
    padding: crate::hydra::ast::Padding(Rc::new(crate::hydra::ast::Padding_Variant {
      left: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::Space)),
      right: crate::hydra::lib::logic::if_else(newlines.clone(), crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::Break)), crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::Space)))})),
    precedence: crate::hydra::ast::Precedence(Rc::new(crate::hydra::ast::Precedence_Variant(0i32))),
    associativity: crate::hydra::ast::Associativity(Rc::new(crate::hydra::ast::Associativity_Variant::None))}))}

pub fn or_sep(style: crate::hydra::ast::BlockStyle, l: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  let newlines = style.clone().0.newline_before_content.clone() ;
  crate::hydra::lib::maybes::maybe(cst(String::from("")), |h: crate::hydra::ast::Expr| crate::hydra::lib::lists::foldl(|acc: crate::hydra::ast::Expr, el: crate::hydra::ast::Expr| ifx(or_op(newlines.clone()), acc.clone(), el.clone()), h.clone(), crate::hydra::lib::lists::drop(1i32, l.clone())), crate::hydra::lib::lists::safe_head(l.clone()))}

pub fn paren_list(newlines: bool, els: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  let style = crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::and(newlines.clone(), crate::hydra::lib::equality::gt(crate::hydra::lib::lists::length(els.clone()), 1i32)), half_block_style, inline_style) ;
  crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(els.clone()), cst(String::from("()")), brackets(parentheses, style.clone(), comma_sep(style.clone(), els.clone())))}

pub fn parens(v1: crate::hydra::ast::Expr) -> crate::hydra::ast::Expr {
  brackets(parentheses, inline_style, v1.clone())}

pub fn parentheses() -> crate::hydra::ast::Brackets {
  crate::hydra::ast::Brackets(Rc::new(crate::hydra::ast::Brackets_Variant {
    open: crate::hydra::ast::Symbol(Rc::new(crate::hydra::ast::Symbol_Variant(String::from("(")))),
    close: crate::hydra::ast::Symbol(Rc::new(crate::hydra::ast::Symbol_Variant(String::from(")"))))}))}

pub fn parenthesize(exp: crate::hydra::ast::Expr) -> crate::hydra::ast::Expr {
  let assoc_left = |a: crate::hydra::ast::Associativity| match &*a.clone().0 {
    crate::hydra::ast::Associativity_Variant::Right (v0_) => {
      let v0_ = v0_.clone() ;
      false},
    _ => true} ;
  let assoc_right = |a: crate::hydra::ast::Associativity| match &*a.clone().0 {
    crate::hydra::ast::Associativity_Variant::Left (v0_) => {
      let v0_ = v0_.clone() ;
      false},
    _ => true} ;
  match &*exp.clone().0 {
    crate::hydra::ast::Expr_Variant::Brackets (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::ast::Expr(Rc::new(crate::hydra::ast::Expr_Variant::Brackets(crate::hydra::ast::BracketExpr(Rc::new(crate::hydra::ast::BracketExpr_Variant {
        brackets: v0_.clone().0.brackets.clone(),
        enclosed: parenthesize(v0_.clone().0.enclosed.clone()),
        style: v0_.clone().0.style.clone()})))))},
    crate::hydra::ast::Expr_Variant::Const (v0_) => {
      let v0_ = v0_.clone() ;
      exp.clone()},
    crate::hydra::ast::Expr_Variant::Indent (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::ast::Expr(Rc::new(crate::hydra::ast::Expr_Variant::Indent(crate::hydra::ast::IndentedExpression(Rc::new(crate::hydra::ast::IndentedExpression_Variant {
        style: v0_.clone().0.style.clone(),
        expr: parenthesize(v0_.clone().0.expr.clone())})))))},
    crate::hydra::ast::Expr_Variant::Seq (v0_) => {
      let v0_ = v0_.clone() ;
      crate::hydra::ast::Expr(Rc::new(crate::hydra::ast::Expr_Variant::Seq(crate::hydra::ast::SeqExpr(Rc::new(crate::hydra::ast::SeqExpr_Variant {
        op: v0_.clone().0.op.clone(),
        elements: crate::hydra::lib::lists::map(parenthesize, v0_.clone().0.elements.clone())})))))},
    crate::hydra::ast::Expr_Variant::Op (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let op = v0_.clone().0.op.clone() ;
        {
          let prec = op.clone().0.precedence.clone().0.0.clone() ;
          {
            let assoc = op.clone().0.associativity.clone() ;
            {
              let lhs = v0_.clone().0.lhs.clone() ;
              {
                let rhs = v0_.clone().0.rhs.clone() ;
                {
                  let lhs_ = parenthesize(lhs.clone()) ;
                  {
                    let rhs_ = parenthesize(rhs.clone()) ;
                    {
                      let lhs2 = match &*lhs_.clone().0 {
                        crate::hydra::ast::Expr_Variant::Op (v0_) => {
                          let v0_ = v0_.clone() ;
                          {
                            let lop = v0_.clone().0.op.clone() ;
                            {
                              let lprec = lop.clone().0.precedence.clone().0.0.clone() ;
                              {
                                let lassoc = lop.clone().0.associativity.clone() ;
                                {
                                  let comparison = crate::hydra::lib::equality::compare(prec.clone(), lprec.clone()) ;
                                  match &*comparison.clone().0 {
                                    crate::hydra::util::Comparison_Variant::LessThan (v0_) => {
                                      let v0_ = v0_.clone() ;
                                      lhs_.clone()},
                                    crate::hydra::util::Comparison_Variant::GreaterThan (v0_) => {
                                      let v0_ = v0_.clone() ;
                                      parens(lhs_.clone())},
                                    crate::hydra::util::Comparison_Variant::EqualTo (v0_) => {
                                      let v0_ = v0_.clone() ;
                                      crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::and(assoc_left.clone()(assoc.clone()), assoc_left.clone()(lassoc.clone())), lhs_.clone(), parens(lhs_.clone()))}}}}}}},
                        _ => lhs_.clone()} ;
                      {
                        let rhs2 = match &*rhs_.clone().0 {
                          crate::hydra::ast::Expr_Variant::Op (v0_) => {
                            let v0_ = v0_.clone() ;
                            {
                              let rop = v0_.clone().0.op.clone() ;
                              {
                                let rprec = rop.clone().0.precedence.clone().0.0.clone() ;
                                {
                                  let rassoc = rop.clone().0.associativity.clone() ;
                                  {
                                    let comparison = crate::hydra::lib::equality::compare(prec.clone(), rprec.clone()) ;
                                    match &*comparison.clone().0 {
                                      crate::hydra::util::Comparison_Variant::LessThan (v0_) => {
                                        let v0_ = v0_.clone() ;
                                        rhs_.clone()},
                                      crate::hydra::util::Comparison_Variant::GreaterThan (v0_) => {
                                        let v0_ = v0_.clone() ;
                                        parens(rhs_.clone())},
                                      crate::hydra::util::Comparison_Variant::EqualTo (v0_) => {
                                        let v0_ = v0_.clone() ;
                                        crate::hydra::lib::logic::if_else(crate::hydra::lib::logic::and(assoc_right.clone()(assoc.clone()), assoc_right.clone()(rassoc.clone())), rhs_.clone(), parens(rhs_.clone()))}}}}}}},
                          _ => rhs_.clone()} ;
                        crate::hydra::ast::Expr(Rc::new(crate::hydra::ast::Expr_Variant::Op(crate::hydra::ast::OpExpr(Rc::new(crate::hydra::ast::OpExpr_Variant {
                          op: op.clone(),
                          lhs: lhs2.clone(),
                          rhs: rhs2.clone()})))))}}}}}}}}}}}}

pub fn prefix(p: String, expr: crate::hydra::ast::Expr) -> crate::hydra::ast::Expr {
  let pre_op = crate::hydra::ast::Op(Rc::new(crate::hydra::ast::Op_Variant {
    symbol: sym(p.clone()),
    padding: crate::hydra::ast::Padding(Rc::new(crate::hydra::ast::Padding_Variant {
      left: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::None)),
      right: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::None))})),
    precedence: crate::hydra::ast::Precedence(Rc::new(crate::hydra::ast::Precedence_Variant(0i32))),
    associativity: crate::hydra::ast::Associativity(Rc::new(crate::hydra::ast::Associativity_Variant::None))})) ;
  ifx(pre_op.clone(), cst(String::from("")), expr.clone())}

pub fn print_expr(e: crate::hydra::ast::Expr) -> String {
  let pad = |ws: crate::hydra::ast::Ws| match &*ws.clone().0 {
    crate::hydra::ast::Ws_Variant::None (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("")},
    crate::hydra::ast::Ws_Variant::Space (v0_) => {
      let v0_ = v0_.clone() ;
      String::from(" ")},
    crate::hydra::ast::Ws_Variant::Break (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("\n")},
    crate::hydra::ast::Ws_Variant::BreakAndIndent (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("\n")},
    crate::hydra::ast::Ws_Variant::DoubleBreak (v0_) => {
      let v0_ = v0_.clone() ;
      String::from("\n\n")}} ;
  let idt = |ws: crate::hydra::ast::Ws, s: String| match &*ws.clone().0 {
    crate::hydra::ast::Ws_Variant::BreakAndIndent (v0_) => {
      let v0_ = v0_.clone() ;
      custom_indent(v0_.clone(), s.clone())},
    _ => s.clone()} ;
  match &*e.clone().0 {
    crate::hydra::ast::Expr_Variant::Const (v0_) => {
      let v0_ = v0_.clone() ;
      v0_.clone().0.0.clone()},
    crate::hydra::ast::Expr_Variant::Indent (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let style = v0_.clone().0.style.clone() ;
        {
          let expr = v0_.clone().0.expr.clone() ;
          {
            let lns = crate::hydra::lib::strings::lines(print_expr(expr.clone())) ;
            {
              let ilns = match &*style.clone().0 {
                crate::hydra::ast::IndentStyle_Variant::AllLines (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::lib::lists::map(|line: String| crate::hydra::lib::strings::cat2(v0_.clone(), line.clone()), lns.clone())},
                crate::hydra::ast::IndentStyle_Variant::SubsequentLines (v0_) => {
                  let v0_ = v0_.clone() ;
                  crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(crate::hydra::lib::lists::length(lns.clone()), 1i32), lns.clone(), crate::hydra::lib::lists::cons(crate::hydra::lib::lists::head(lns.clone()), crate::hydra::lib::lists::map(|line: String| crate::hydra::lib::strings::cat2(v0_.clone(), line.clone()), crate::hydra::lib::lists::tail(lns.clone()))))}} ;
              crate::hydra::lib::strings::intercalate(String::from("\n"), ilns.clone())}}}}},
    crate::hydra::ast::Expr_Variant::Seq (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let sop = v0_.clone().0.op.clone() ;
        {
          let ssym = sop.clone().0.symbol.clone().0.0.clone() ;
          {
            let spadding = sop.clone().0.padding.clone() ;
            {
              let spadl = spadding.clone().0.left.clone() ;
              {
                let spadr = spadding.clone().0.right.clone() ;
                {
                  let selements = v0_.clone().0.elements.clone() ;
                  {
                    let separator = crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(pad.clone()(spadl.clone()), ssym.clone()), pad.clone()(spadr.clone())) ;
                    {
                      let printed_elements = crate::hydra::lib::lists::map(|el: crate::hydra::ast::Expr| idt.clone()(spadr.clone(), print_expr(el.clone())), selements.clone()) ;
                      crate::hydra::lib::strings::intercalate(separator.clone(), printed_elements.clone())}}}}}}}}},
    crate::hydra::ast::Expr_Variant::Op (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let op = v0_.clone().0.op.clone() ;
        {
          let sym = op.clone().0.symbol.clone().0.0.clone() ;
          {
            let padding = op.clone().0.padding.clone() ;
            {
              let padl = padding.clone().0.left.clone() ;
              {
                let padr = padding.clone().0.right.clone() ;
                {
                  let l = v0_.clone().0.lhs.clone() ;
                  {
                    let r = v0_.clone().0.rhs.clone() ;
                    {
                      let lhs = idt.clone()(padl.clone(), print_expr(l.clone())) ;
                      {
                        let rhs = idt.clone()(padr.clone(), print_expr(r.clone())) ;
                        crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(lhs.clone(), pad.clone()(padl.clone())), sym.clone()), pad.clone()(padr.clone())), rhs.clone())}}}}}}}}}},
    crate::hydra::ast::Expr_Variant::Brackets (v0_) => {
      let v0_ = v0_.clone() ;
      {
        let brs = v0_.clone().0.brackets.clone() ;
        {
          let l = brs.clone().0.open.clone().0.0.clone() ;
          {
            let r = brs.clone().0.close.clone().0.0.clone() ;
            {
              let e = v0_.clone().0.enclosed.clone() ;
              {
                let style = v0_.clone().0.style.clone() ;
                {
                  let body = print_expr(e.clone()) ;
                  {
                    let do_indent = style.clone().0.indent.clone() ;
                    {
                      let nl_before = style.clone().0.newline_before_content.clone() ;
                      {
                        let nl_after = style.clone().0.newline_after_content.clone() ;
                        {
                          let ibody = crate::hydra::lib::maybes::maybe(body.clone(), |idt2: String| custom_indent(idt2.clone(), body.clone()), do_indent.clone()) ;
                          {
                            let pre = crate::hydra::lib::logic::if_else(nl_before.clone(), String::from("\n"), String::from("")) ;
                            {
                              let suf = crate::hydra::lib::logic::if_else(nl_after.clone(), String::from("\n"), String::from("")) ;
                              crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(l.clone(), pre.clone()), ibody.clone()), suf.clone()), r.clone())}}}}}}}}}}}}}}}

pub fn semicolon_sep(v1: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  symbol_sep(String::from(";"), inline_style, v1.clone())}

pub fn sep(op: crate::hydra::ast::Op, els: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  crate::hydra::lib::maybes::maybe(cst(String::from("")), |h: crate::hydra::ast::Expr| crate::hydra::lib::lists::foldl(|acc: crate::hydra::ast::Expr, el: crate::hydra::ast::Expr| ifx(op.clone(), acc.clone(), el.clone()), h.clone(), crate::hydra::lib::lists::drop(1i32, els.clone())), crate::hydra::lib::lists::safe_head(els.clone()))}

pub fn space_sep(v1: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  sep(crate::hydra::ast::Op(Rc::new(crate::hydra::ast::Op_Variant {
    symbol: sym(String::from("")),
    padding: crate::hydra::ast::Padding(Rc::new(crate::hydra::ast::Padding_Variant {
      left: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::Space)),
      right: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::None))})),
    precedence: crate::hydra::ast::Precedence(Rc::new(crate::hydra::ast::Precedence_Variant(0i32))),
    associativity: crate::hydra::ast::Associativity(Rc::new(crate::hydra::ast::Associativity_Variant::None))})), v1.clone())}

pub fn structural_sep(op: crate::hydra::ast::Op, els: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  crate::hydra::lib::logic::if_else(crate::hydra::lib::lists::null(els.clone()), cst(String::from("")), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(crate::hydra::lib::lists::length(els.clone()), 1i32), crate::hydra::lib::lists::head(els.clone()), crate::hydra::ast::Expr(Rc::new(crate::hydra::ast::Expr_Variant::Seq(crate::hydra::ast::SeqExpr(Rc::new(crate::hydra::ast::SeqExpr_Variant {
    op: op.clone(),
    elements: els.clone()})))))))}

pub fn structural_space_sep(v1: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  structural_sep(crate::hydra::ast::Op(Rc::new(crate::hydra::ast::Op_Variant {
    symbol: sym(String::from("")),
    padding: crate::hydra::ast::Padding(Rc::new(crate::hydra::ast::Padding_Variant {
      left: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::Space)),
      right: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::None))})),
    precedence: crate::hydra::ast::Precedence(Rc::new(crate::hydra::ast::Precedence_Variant(0i32))),
    associativity: crate::hydra::ast::Associativity(Rc::new(crate::hydra::ast::Associativity_Variant::None))})), v1.clone())}

pub fn square_brackets() -> crate::hydra::ast::Brackets {
  crate::hydra::ast::Brackets(Rc::new(crate::hydra::ast::Brackets_Variant {
    open: crate::hydra::ast::Symbol(Rc::new(crate::hydra::ast::Symbol_Variant(String::from("[")))),
    close: crate::hydra::ast::Symbol(Rc::new(crate::hydra::ast::Symbol_Variant(String::from("]"))))}))}

pub fn suffix(s: String, expr: crate::hydra::ast::Expr) -> crate::hydra::ast::Expr {
  let suf_op = crate::hydra::ast::Op(Rc::new(crate::hydra::ast::Op_Variant {
    symbol: sym(s.clone()),
    padding: crate::hydra::ast::Padding(Rc::new(crate::hydra::ast::Padding_Variant {
      left: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::None)),
      right: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::None))})),
    precedence: crate::hydra::ast::Precedence(Rc::new(crate::hydra::ast::Precedence_Variant(0i32))),
    associativity: crate::hydra::ast::Associativity(Rc::new(crate::hydra::ast::Associativity_Variant::None))})) ;
  ifx(suf_op.clone(), expr.clone(), cst(String::from("")))}

pub fn sym(s: String) -> crate::hydra::ast::Symbol {
  crate::hydra::ast::Symbol(Rc::new(crate::hydra::ast::Symbol_Variant(s.clone())))}

pub fn symbol_sep(symb: String, style: crate::hydra::ast::BlockStyle, l: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  let break_count = crate::hydra::lib::lists::length(crate::hydra::lib::lists::filter(|x_: bool| x_.clone(), Vec::from([
    style.clone().0.newline_before_content.clone(),
    style.clone().0.newline_after_content.clone()]))) ;
  let break_ = crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(break_count.clone(), 0i32), crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::Space)), crate::hydra::lib::logic::if_else(crate::hydra::lib::equality::equal(break_count.clone(), 1i32), crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::Break)), crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::DoubleBreak)))) ;
  let comma_op = crate::hydra::ast::Op(Rc::new(crate::hydra::ast::Op_Variant {
    symbol: sym(symb.clone()),
    padding: crate::hydra::ast::Padding(Rc::new(crate::hydra::ast::Padding_Variant {
      left: crate::hydra::ast::Ws(Rc::new(crate::hydra::ast::Ws_Variant::None)),
      right: break_.clone()})),
    precedence: crate::hydra::ast::Precedence(Rc::new(crate::hydra::ast::Precedence_Variant(0i32))),
    associativity: crate::hydra::ast::Associativity(Rc::new(crate::hydra::ast::Associativity_Variant::None))})) ;
  crate::hydra::lib::maybes::maybe(cst(String::from("")), |h: crate::hydra::ast::Expr| crate::hydra::lib::lists::foldl(|acc: crate::hydra::ast::Expr, el: crate::hydra::ast::Expr| ifx(comma_op.clone(), acc.clone(), el.clone()), h.clone(), crate::hydra::lib::lists::drop(1i32, l.clone())), crate::hydra::lib::lists::safe_head(l.clone()))}

pub fn tab_indent(e: crate::hydra::ast::Expr) -> crate::hydra::ast::Expr {
  crate::hydra::ast::Expr(Rc::new(crate::hydra::ast::Expr_Variant::Indent(crate::hydra::ast::IndentedExpression(Rc::new(crate::hydra::ast::IndentedExpression_Variant {
    style: crate::hydra::ast::IndentStyle(Rc::new(crate::hydra::ast::IndentStyle_Variant::AllLines(String::from("    ")))),
    expr: e.clone()})))))}

pub fn tab_indent_double_space(exprs: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  tab_indent(double_newline_sep(exprs.clone()))}

pub fn tab_indent_single_space(exprs: Vec<crate::hydra::ast::Expr>) -> crate::hydra::ast::Expr {
  tab_indent(newline_sep(exprs.clone()))}

pub fn unsupported_type(label: String) -> crate::hydra::ast::Expr {
  cst(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("["), label.clone()), String::from("]")))}

pub fn unsupported_variant(label: String, obj: String) -> crate::hydra::ast::Expr {
  cst(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(crate::hydra::lib::strings::cat2(String::from("[unsupported "), label.clone()), String::from(": ")), crate::hydra::lib::literals::show_string(obj.clone())), String::from("]")))}

pub fn with_comma(e: crate::hydra::ast::Expr) -> crate::hydra::ast::Expr {
  no_sep(Vec::from([
    e.clone(),
    cst(String::from(","))]))}

pub fn with_semi(e: crate::hydra::ast::Expr) -> crate::hydra::ast::Expr {
  no_sep(Vec::from([
    e.clone(),
    cst(String::from(";"))]))}
