(* A model which provides a common syntax tree for Hydra serializers *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.
Inductive Ws : Type :=
| Ws_None : forall (_ : unit) , Ws
| Ws_Space : forall (_ : unit) , Ws
| Ws_Break : forall (_ : unit) , Ws
| Ws_BreakAndIndent : forall (_ : string) , Ws
| Ws_DoubleBreak : forall (_ : unit) , Ws.

Definition Symbol : Type := string.

Definition Precedence : Type := Z.

Record Padding : Type := Build_Padding {
padding_left : Ws ;
padding_right : Ws ;
}.

Inductive IndentStyle : Type :=
| IndentStyle_AllLines : forall (_ : string) , IndentStyle
| IndentStyle_SubsequentLines : forall (_ : string) , IndentStyle.

Record Brackets : Type := Build_Brackets {
brackets_open : Symbol ;
brackets_close : Symbol ;
}.

Record BlockStyle : Type := Build_BlockStyle {
blockStyle_indent : (option) (string) ;
blockStyle_newlineBeforeContent : bool ;
blockStyle_newlineAfterContent : bool ;
}.

Inductive Associativity : Type :=
| Associativity_None : forall (_ : unit) , Associativity
| Associativity_Left : forall (_ : unit) , Associativity
| Associativity_Right : forall (_ : unit) , Associativity
| Associativity_Both : forall (_ : unit) , Associativity.

Record Op : Type := Build_Op {
op_symbol : Symbol ;
op_padding : Padding ;
op_precedence : Precedence ;
op_associativity : Associativity ;
}.

Inductive SeqExpr : Type :=
| Build_SeqExpr : forall (_ : Op) , forall (_ : (list) (Expr)) , SeqExpr
with Expr : Type :=
| Expr_Const : forall (_ : Symbol) , Expr
| Expr_Indent : forall (_ : IndentedExpression) , Expr
| Expr_Op : forall (_ : OpExpr) , Expr
| Expr_Brackets : forall (_ : BracketExpr) , Expr
| Expr_Seq : forall (_ : SeqExpr) , Expr
with BracketExpr : Type :=
| Build_BracketExpr : forall (_ : Brackets) , forall (_ : Expr) , forall (_ : BlockStyle) , BracketExpr
with IndentedExpression : Type :=
| Build_IndentedExpression : forall (_ : IndentStyle) , forall (_ : Expr) , IndentedExpression
with OpExpr : Type :=
| Build_OpExpr : forall (_ : Op) , forall (_ : Expr) , forall (_ : Expr) , OpExpr.

Definition seqExpr_op (r_ : SeqExpr) := match r_ with
| Build_SeqExpr f0 f1 => f0
end.

Definition seqExpr_elements (r_ : SeqExpr) := match r_ with
| Build_SeqExpr f0 f1 => f1
end.

Definition bracketExpr_brackets (r_ : BracketExpr) := match r_ with
| Build_BracketExpr f0 f1 f2 => f0
end.

Definition bracketExpr_enclosed (r_ : BracketExpr) := match r_ with
| Build_BracketExpr f0 f1 f2 => f1
end.

Definition bracketExpr_style (r_ : BracketExpr) := match r_ with
| Build_BracketExpr f0 f1 f2 => f2
end.

Definition indentedExpression_style (r_ : IndentedExpression) := match r_ with
| Build_IndentedExpression f0 f1 => f0
end.

Definition indentedExpression_expr (r_ : IndentedExpression) := match r_ with
| Build_IndentedExpression f0 f1 => f1
end.

Definition opExpr_op (r_ : OpExpr) := match r_ with
| Build_OpExpr f0 f1 f2 => f0
end.

Definition opExpr_lhs (r_ : OpExpr) := match r_ with
| Build_OpExpr f0 f1 f2 => f1
end.

Definition opExpr_rhs (r_ : OpExpr) := match r_ with
| Build_OpExpr f0 f1 f2 => f2
end.

