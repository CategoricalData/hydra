#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::core::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Associativity_Variant {
  None,
  Left,
  Right,
  Both}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Associativity (pub Rc<Associativity_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockStyle_Variant {
  pub indent: Option<String>,
  pub newline_before_content: bool,
  pub newline_after_content: bool}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockStyle (pub Rc<BlockStyle_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BracketExpr_Variant {
  pub brackets: Brackets,
  pub enclosed: Expr,
  pub style: BlockStyle}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BracketExpr (pub Rc<BracketExpr_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Brackets_Variant {
  pub open: Symbol,
  pub close: Symbol}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Brackets (pub Rc<Brackets_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr_Variant {
  Const(Symbol),
  Indent(IndentedExpression),
  Op(OpExpr),
  Brackets(BracketExpr),
  Seq(SeqExpr)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Expr (pub Rc<Expr_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct IndentedExpression_Variant {
  pub style: IndentStyle,
  pub expr: Expr}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct IndentedExpression (pub Rc<IndentedExpression_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum IndentStyle_Variant {
  AllLines(String),
  SubsequentLines(String)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct IndentStyle (pub Rc<IndentStyle_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Op_Variant {
  pub symbol: Symbol,
  pub padding: Padding,
  pub precedence: Precedence,
  pub associativity: Associativity}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Op (pub Rc<Op_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct OpExpr_Variant {
  pub op: Op,
  pub lhs: Expr,
  pub rhs: Expr}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct OpExpr (pub Rc<OpExpr_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Padding_Variant {
  pub left: Ws,
  pub right: Ws}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Padding (pub Rc<Padding_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Precedence_Variant (pub i32);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Precedence (pub Rc<Precedence_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SeqExpr_Variant {
  pub op: Op,
  pub elements: Vec<Expr>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SeqExpr (pub Rc<SeqExpr_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol_Variant (pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol (pub Rc<Symbol_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Ws_Variant {
  None,
  Space,
  Break,
  BreakAndIndent(String),
  DoubleBreak}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ws (pub Rc<Ws_Variant>);
