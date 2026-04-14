(* Term encoders for hydra.ast *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.ast hydra.core hydra.lib.maybes hydra.lib.lists.

Definition ws : forall (_ : Ws) , Term := fun x_ => match x_ with
| Ws_None v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("Ws"%string) ((Build_Field) ("none"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| Ws_Space v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("Ws"%string) ((Build_Field) ("space"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| Ws_Break v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("Ws"%string) ((Build_Field) ("break"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| Ws_BreakAndIndent v_ => (fun (y : string) => (Term_Inject) ((Build_Injection) ("Ws"%string) ((Build_Field) ("breakAndIndent"%string) ((fun (x : string) => (Term_Literal) ((Literal_String) (x))) (y))))) (v_)
| Ws_DoubleBreak v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("Ws"%string) ((Build_Field) ("doubleBreak"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.
Definition symbol : forall (_ : Symbol) , Term := fun (x : Symbol) => (Term_Wrap) ((Build_WrappedTerm) ("Symbol"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun w_ => w_) (x)))).
Definition precedence : forall (_ : Precedence) , Term := fun (x : Precedence) => (Term_Wrap) ((Build_WrappedTerm) ("Precedence"%string) ((fun (x2 : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x2)))) ((fun w_ => w_) (x)))).
Definition padding : forall (_ : Padding) , Term := fun (x : Padding) => (Term_Record) ((Build_Record_) ("Padding"%string) ((cons) ((Build_Field) ("left"%string) ((ws) ((fun r_ => (padding_left) (r_)) (x)))) ((cons) ((Build_Field) ("right"%string) ((ws) ((fun r_ => (padding_right) (r_)) (x)))) (nil)))).
Definition indentStyle : forall (_ : IndentStyle) , Term := fun x_ => match x_ with
| IndentStyle_AllLines v_ => (fun (y : string) => (Term_Inject) ((Build_Injection) ("IndentStyle"%string) ((Build_Field) ("allLines"%string) ((fun (x : string) => (Term_Literal) ((Literal_String) (x))) (y))))) (v_)
| IndentStyle_SubsequentLines v_ => (fun (y : string) => (Term_Inject) ((Build_Injection) ("IndentStyle"%string) ((Build_Field) ("subsequentLines"%string) ((fun (x : string) => (Term_Literal) ((Literal_String) (x))) (y))))) (v_)
end.
Definition brackets : forall (_ : Brackets) , Term := fun (x : Brackets) => (Term_Record) ((Build_Record_) ("Brackets"%string) ((cons) ((Build_Field) ("open"%string) ((symbol) ((fun r_ => (brackets_open) (r_)) (x)))) ((cons) ((Build_Field) ("close"%string) ((symbol) ((fun r_ => (brackets_close) (r_)) (x)))) (nil)))).
Definition blockStyle : forall (_ : BlockStyle) , Term := fun (x : BlockStyle) => (Term_Record) ((Build_Record_) ("BlockStyle"%string) ((cons) ((Build_Field) ("indent"%string) ((fun (opt : (option) (string)) => (Term_Maybe) (((maybes.map) (fun (x2 : string) => (Term_Literal) ((Literal_String) (x2)))) (opt))) ((fun r_ => (blockStyle_indent) (r_)) (x)))) ((cons) ((Build_Field) ("newlineBeforeContent"%string) ((fun (x2 : bool) => (Term_Literal) ((Literal_Boolean) (x2))) ((fun r_ => (blockStyle_newlineBeforeContent) (r_)) (x)))) ((cons) ((Build_Field) ("newlineAfterContent"%string) ((fun (x2 : bool) => (Term_Literal) ((Literal_Boolean) (x2))) ((fun r_ => (blockStyle_newlineAfterContent) (r_)) (x)))) (nil))))).
Definition associativity : forall (_ : Associativity) , Term := fun x_ => match x_ with
| Associativity_None v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("Associativity"%string) ((Build_Field) ("none"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| Associativity_Left v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("Associativity"%string) ((Build_Field) ("left"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| Associativity_Right v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("Associativity"%string) ((Build_Field) ("right"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| Associativity_Both v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("Associativity"%string) ((Build_Field) ("both"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.
Definition op : forall (_ : Op) , Term := fun (x : Op) => (Term_Record) ((Build_Record_) ("Op"%string) ((cons) ((Build_Field) ("symbol"%string) ((symbol) ((fun r_ => (op_symbol) (r_)) (x)))) ((cons) ((Build_Field) ("padding"%string) ((padding) ((fun r_ => (op_padding) (r_)) (x)))) ((cons) ((Build_Field) ("precedence"%string) ((precedence) ((fun r_ => (op_precedence) (r_)) (x)))) ((cons) ((Build_Field) ("associativity"%string) ((associativity) ((fun r_ => (op_associativity) (r_)) (x)))) (nil)))))).
Definition seqExpr_expr_bundle :=
  hydra_fix (fun (bundle_ : prod (forall (_ : SeqExpr) , Term) (prod (forall (_ : Expr) , Term) (prod (forall (_ : BracketExpr) , Term) (prod (forall (_ : IndentedExpression) , Term) (forall (_ : OpExpr) , Term))))) =>
    let seqExpr := (fst bundle_) in
    let expr := (fst (snd bundle_)) in
    let bracketExpr := (fst (snd (snd bundle_))) in
    let indentedExpression := (fst (snd (snd (snd bundle_)))) in
    let opExpr := (snd (snd (snd (snd bundle_)))) in
    (pair (fun (x : SeqExpr) => (Term_Record) ((Build_Record_) ("SeqExpr"%string) ((cons) ((Build_Field) ("op"%string) ((op) ((fun r_ => (seqExpr_op) (r_)) (x)))) ((cons) ((Build_Field) ("elements"%string) ((fun (xs : (list) (Expr)) => (Term_List) (((lists.map) (expr)) (xs))) ((fun r_ => (seqExpr_elements) (r_)) (x)))) (nil))))) ((pair (fun x_ => match x_ with
| Expr_Const v_ => (fun (y : Symbol) => (Term_Inject) ((Build_Injection) ("Expr"%string) ((Build_Field) ("const"%string) ((symbol) (y))))) (v_)
| Expr_Indent v_ => (fun (y : IndentedExpression) => (Term_Inject) ((Build_Injection) ("Expr"%string) ((Build_Field) ("indent"%string) ((indentedExpression) (y))))) (v_)
| Expr_Op v_ => (fun (y : OpExpr) => (Term_Inject) ((Build_Injection) ("Expr"%string) ((Build_Field) ("op"%string) ((opExpr) (y))))) (v_)
| Expr_Brackets v_ => (fun (y : BracketExpr) => (Term_Inject) ((Build_Injection) ("Expr"%string) ((Build_Field) ("brackets"%string) ((bracketExpr) (y))))) (v_)
| Expr_Seq v_ => (fun (y : SeqExpr) => (Term_Inject) ((Build_Injection) ("Expr"%string) ((Build_Field) ("seq"%string) ((seqExpr) (y))))) (v_)
end) ((pair (fun (x : BracketExpr) => (Term_Record) ((Build_Record_) ("BracketExpr"%string) ((cons) ((Build_Field) ("brackets"%string) ((brackets) ((fun r_ => (bracketExpr_brackets) (r_)) (x)))) ((cons) ((Build_Field) ("enclosed"%string) ((expr) ((fun r_ => (bracketExpr_enclosed) (r_)) (x)))) ((cons) ((Build_Field) ("style"%string) ((blockStyle) ((fun r_ => (bracketExpr_style) (r_)) (x)))) (nil)))))) ((pair (fun (x : IndentedExpression) => (Term_Record) ((Build_Record_) ("IndentedExpression"%string) ((cons) ((Build_Field) ("style"%string) ((indentStyle) ((fun r_ => (indentedExpression_style) (r_)) (x)))) ((cons) ((Build_Field) ("expr"%string) ((expr) ((fun r_ => (indentedExpression_expr) (r_)) (x)))) (nil))))) (fun (x : OpExpr) => (Term_Record) ((Build_Record_) ("OpExpr"%string) ((cons) ((Build_Field) ("op"%string) ((op) ((fun r_ => (opExpr_op) (r_)) (x)))) ((cons) ((Build_Field) ("lhs"%string) ((expr) ((fun r_ => (opExpr_lhs) (r_)) (x)))) ((cons) ((Build_Field) ("rhs"%string) ((expr) ((fun r_ => (opExpr_rhs) (r_)) (x)))) (nil)))))))))))))).

Definition seqExpr : forall (_ : SeqExpr) , Term :=
  (fst seqExpr_expr_bundle).
Definition expr : forall (_ : Expr) , Term :=
  (fst (snd seqExpr_expr_bundle)).
Definition bracketExpr : forall (_ : BracketExpr) , Term :=
  (fst (snd (snd seqExpr_expr_bundle))).
Definition indentedExpression : forall (_ : IndentedExpression) , Term :=
  (fst (snd (snd (snd seqExpr_expr_bundle)))).
Definition opExpr : forall (_ : OpExpr) , Term :=
  (snd (snd (snd (snd seqExpr_expr_bundle)))).

