(* A model which provides a common syntax tree for Hydra serializers *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.
Inductive Ws : Type :=
| Ws_None : unit -> Ws
| Ws_Space : unit -> Ws
| Ws_Break : unit -> Ws
| Ws_BreakAndIndent : string -> Ws
| Ws_DoubleBreak : unit -> Ws.

Definition Symbol : Type :=
  string.

Definition Precedence : Type :=
  Z.

Record Padding : Type := Build_Padding {
  padding_left : Ws ;
  padding_right : Ws
}.

Inductive IndentStyle : Type :=
| IndentStyle_AllLines : string -> IndentStyle
| IndentStyle_SubsequentLines : string -> IndentStyle.

Record Brackets : Type := Build_Brackets {
  brackets_open : Symbol ;
  brackets_close : Symbol
}.

Record BlockStyle : Type := Build_BlockStyle {
  blockStyle_indent : (option) (string) ;
  blockStyle_newlineBeforeContent : bool ;
  blockStyle_newlineAfterContent : bool
}.

Inductive Associativity : Type :=
| Associativity_None : unit -> Associativity
| Associativity_Left : unit -> Associativity
| Associativity_Right : unit -> Associativity
| Associativity_Both : unit -> Associativity.

Record Op : Type := Build_Op {
  op_symbol : Symbol ;
  op_padding : Padding ;
  op_precedence : Precedence ;
  op_associativity : Associativity
}.

Inductive SeqExpr : Type :=
| Build_SeqExpr : Op -> (list) (Expr) -> SeqExpr
with Expr : Type :=
| Expr_Const : Symbol -> Expr
| Expr_Indent : IndentedExpression -> Expr
| Expr_Op : OpExpr -> Expr
| Expr_Brackets : BracketExpr -> Expr
| Expr_Seq : SeqExpr -> Expr
with BracketExpr : Type :=
| Build_BracketExpr : Brackets -> Expr -> BlockStyle -> BracketExpr
with IndentedExpression : Type :=
| Build_IndentedExpression : IndentStyle -> Expr -> IndentedExpression
with OpExpr : Type :=
| Build_OpExpr : Op -> Expr -> Expr -> OpExpr.

Definition seqExpr_op (r_ : SeqExpr) :=
  match r_ with
| Build_SeqExpr f0 f1 => f0
end.

Definition seqExpr_elements (r_ : SeqExpr) :=
  match r_ with
| Build_SeqExpr f0 f1 => f1
end.

Definition bracketExpr_brackets (r_ : BracketExpr) :=
  match r_ with
| Build_BracketExpr f0 f1 f2 => f0
end.

Definition bracketExpr_enclosed (r_ : BracketExpr) :=
  match r_ with
| Build_BracketExpr f0 f1 f2 => f1
end.

Definition bracketExpr_style (r_ : BracketExpr) :=
  match r_ with
| Build_BracketExpr f0 f1 f2 => f2
end.

Definition indentedExpression_style (r_ : IndentedExpression) :=
  match r_ with
| Build_IndentedExpression f0 f1 => f0
end.

Definition indentedExpression_expr (r_ : IndentedExpression) :=
  match r_ with
| Build_IndentedExpression f0 f1 => f1
end.

Definition opExpr_op (r_ : OpExpr) :=
  match r_ with
| Build_OpExpr f0 f1 f2 => f0
end.

Definition opExpr_lhs (r_ : OpExpr) :=
  match r_ with
| Build_OpExpr f0 f1 f2 => f1
end.

Definition opExpr_rhs (r_ : OpExpr) :=
  match r_ with
| Build_OpExpr f0 f1 f2 => f2
end.

