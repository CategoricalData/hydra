(* Utilities for constructing generic program code ASTs, used for the serialization phase of source code generation. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.ast hydra.lib.equality hydra.lib.lists hydra.lib.literals hydra.lib.logic hydra.lib.math hydra.lib.maybes hydra.lib.strings hydra.util.

Definition angleBraces : Brackets := (Build_Brackets) ("<"%string) (">"%string).
Definition brackets : forall (_ : Brackets) , forall (_ : BlockStyle) , forall (_ : Expr) , Expr := fun (br : Brackets) => fun (style : BlockStyle) => fun (e : Expr) => (Expr_Brackets) ((Build_BracketExpr) (br) (e) (style)).
Definition sym : forall (_ : string) , Symbol := fun (s : string) => s.
Definition cst : forall (_ : string) , Expr := fun (s : string) => (Expr_Const) ((sym) (s)).
Definition op : forall (_ : string) , forall (_ : Z) , forall (_ : Associativity) , Op := fun (s : string) => fun (p : Z) => fun (assoc : Associativity) => (Build_Op) ((sym) (s)) ((Build_Padding) ((Ws_Space) (tt)) ((Ws_Space) (tt))) (p) (assoc).
Definition ifx : forall (_ : Op) , forall (_ : Expr) , forall (_ : Expr) , Expr := fun (op : Op) => fun (lhs : Expr) => fun (rhs : Expr) => (Expr_Op) ((Build_OpExpr) (op) (lhs) (rhs)).
Definition symbolSep : forall (_ : string) , forall (_ : BlockStyle) , forall (_ : (list) (Expr)) , Expr := fun (symb : string) => fun (style : BlockStyle) => fun (l : (list) (Expr)) => let breakCount := (lists.length) (((lists.filter) (fun (x_ : bool) => x_)) ((cons) ((fun r_ => (blockStyle_newlineBeforeContent) (r_)) (style)) ((cons) ((fun r_ => (blockStyle_newlineAfterContent) (r_)) (style)) (nil)))) in let break := (((logic.ifElse) (((equality.equal) (breakCount)) ((0)%Z))) ((Ws_Space) (tt))) ((((logic.ifElse) (((equality.equal) (breakCount)) ((1)%Z))) ((Ws_Break) (tt))) ((Ws_DoubleBreak) (tt))) in let commaOp := (Build_Op) ((sym) (symb)) ((Build_Padding) ((Ws_None) (tt)) (break)) ((0)%Z) ((Associativity_None) (tt)) in (((maybes.maybe) ((cst) (""%string))) (fun (h : Expr) => (((lists.foldl) (fun (acc : Expr) => fun (el : Expr) => (((ifx) (commaOp)) (acc)) (el))) (h)) (((lists.drop) ((1)%Z)) (l)))) ((lists.safeHead) (l)).
Definition commaSep : forall (_ : BlockStyle) , forall (_ : (list) (Expr)) , Expr := (symbolSep) (","%string).
Definition angleBracesList : forall (_ : BlockStyle) , forall (_ : (list) (Expr)) , Expr := fun (style : BlockStyle) => fun (els : (list) (Expr)) => (((logic.ifElse) ((lists.null) (els))) ((cst) ("<>"%string))) ((((brackets) (angleBraces)) (style)) (((commaSep) (style)) (els))).
Definition curlyBraces : Brackets := (Build_Brackets) ("{"%string) ("}"%string).
Definition curlyBracesList : forall (_ : (option) (string)) , forall (_ : BlockStyle) , forall (_ : (list) (Expr)) , Expr := fun (msymb : (option) (string)) => fun (style : BlockStyle) => fun (els : (list) (Expr)) => (((logic.ifElse) ((lists.null) (els))) ((cst) ("{}"%string))) ((((brackets) (curlyBraces)) (style)) ((((symbolSep) (((maybes.fromMaybe) (","%string)) (msymb))) (style)) (els))).
Definition expressionLength_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Expr) , Z) =>
    let expressionLength := bundle_ in
    fun (e : Expr) => let blockStyleLength := fun (style : BlockStyle) => let mindentLen := (((maybes.maybe) ((0)%Z)) (strings.length)) ((fun r_ => (blockStyle_indent) (r_)) (style)) in let nlAfterLen := (((logic.ifElse) ((fun r_ => (blockStyle_newlineAfterContent) (r_)) (style))) ((1)%Z)) ((0)%Z) in let nlBeforeLen := (((logic.ifElse) ((fun r_ => (blockStyle_newlineBeforeContent) (r_)) (style))) ((1)%Z)) ((0)%Z) in ((math.add) (mindentLen)) (((math.add) (nlBeforeLen)) (nlAfterLen)) in let symbolLength := fun (s : Symbol) => (strings.length) ((fun w_ => w_) (s)) in let bracketsLength := fun (brackets : Brackets) => ((math.add) ((symbolLength) ((fun r_ => (brackets_open) (r_)) (brackets)))) ((symbolLength) ((fun r_ => (brackets_close) (r_)) (brackets))) in let bracketExprLength := fun (be : BracketExpr) => ((math.add) ((bracketsLength) ((fun r_ => (bracketExpr_brackets) (r_)) (be)))) (((math.add) ((expressionLength) ((fun r_ => (bracketExpr_enclosed) (r_)) (be)))) ((blockStyleLength) ((fun r_ => (bracketExpr_style) (r_)) (be)))) in let indentedExpressionLength := fun (ie : IndentedExpression) => let baseLen := (expressionLength) ((fun r_ => (indentedExpression_expr) (r_)) (ie)) in let indentLen := (fun x_ => match x_ with
| IndentStyle_AllLines v_ => (fun (s : string) => (strings.length) (s)) (v_)
| IndentStyle_SubsequentLines v_ => (fun (s : string) => (strings.length) (s)) (v_)
end) ((fun r_ => (indentedExpression_style) (r_)) (ie)) in ((math.add) (baseLen)) (indentLen) in let wsLength := fun (ws : Ws) => (fun x_ => match x_ with
| Ws_None _ => (0)%Z
| Ws_Space _ => (1)%Z
| Ws_Break _ => (10000)%Z
| Ws_BreakAndIndent v_ => (fun (s : string) => (10000)%Z) (v_)
| Ws_DoubleBreak _ => (10000)%Z
end) (ws) in let opLength := fun (op : Op) => let padding := (fun r_ => (op_padding) (r_)) (op) in let leftLen := (wsLength) ((fun r_ => (padding_left) (r_)) (padding)) in let rightLen := (wsLength) ((fun r_ => (padding_right) (r_)) (padding)) in let symLen := (symbolLength) ((fun r_ => (op_symbol) (r_)) (op)) in ((math.add) (symLen)) (((math.add) (leftLen)) (rightLen)) in let opExprLength := fun (oe : OpExpr) => let leftLen := (expressionLength) ((fun r_ => (opExpr_lhs) (r_)) (oe)) in let opLen := (opLength) ((fun r_ => (opExpr_op) (r_)) (oe)) in let rightLen := (expressionLength) ((fun r_ => (opExpr_rhs) (r_)) (oe)) in ((math.add) (opLen)) (((math.add) (leftLen)) (rightLen)) in let seqExprLength := fun (se : SeqExpr) => let elementLens := ((lists.map) (expressionLength)) ((fun r_ => (seqExpr_elements) (r_)) (se)) in let numSeps := ((math.sub) ((lists.length) ((fun r_ => (seqExpr_elements) (r_)) (se)))) ((1)%Z) in let sopLen := (opLength) ((fun r_ => (seqExpr_op) (r_)) (se)) in let totalElLen := (((lists.foldl) (math.add)) ((0)%Z)) (elementLens) in ((math.add) (totalElLen)) (((math.mul) (sopLen)) ((((logic.ifElse) (((equality.gt) (numSeps)) ((0)%Z))) (numSeps)) ((0)%Z))) in (fun x_ => match x_ with
| Expr_Const v_ => (fun (s : Symbol) => (symbolLength) (s)) (v_)
| Expr_Indent v_ => (fun (ie : IndentedExpression) => (indentedExpressionLength) (ie)) (v_)
| Expr_Op v_ => (fun (oe : OpExpr) => (opExprLength) (oe)) (v_)
| Expr_Brackets v_ => (fun (be : BracketExpr) => (bracketExprLength) (be)) (v_)
| Expr_Seq v_ => (fun (se : SeqExpr) => (seqExprLength) (se)) (v_)
end) (e)).

Definition expressionLength : forall (_ : Expr) , Z :=
  expressionLength_bundle.
Definition doubleSpace : string := "  "%string.
Definition halfBlockStyle : BlockStyle := (Build_BlockStyle) ((Some) (doubleSpace)) (true) (false).
Definition inlineStyle : BlockStyle := (Build_BlockStyle) (None) (false) (false).
Definition bracesListAdaptive : forall (_ : (list) (Expr)) , Expr := fun (els : (list) (Expr)) => let inlineList := (((curlyBracesList) (None)) (inlineStyle)) (els) in (((logic.ifElse) (((equality.gt) ((expressionLength) (inlineList))) ((70)%Z))) ((((curlyBracesList) (None)) (halfBlockStyle)) (els))) (inlineList).
Definition squareBrackets : Brackets := (Build_Brackets) ("["%string) ("]"%string).
Definition bracketList : forall (_ : BlockStyle) , forall (_ : (list) (Expr)) , Expr := fun (style : BlockStyle) => fun (els : (list) (Expr)) => (((logic.ifElse) ((lists.null) (els))) ((cst) ("[]"%string))) ((((brackets) (squareBrackets)) (style)) (((commaSep) (style)) (els))).
Definition bracketListAdaptive : forall (_ : (list) (Expr)) , Expr := fun (els : (list) (Expr)) => let inlineList := ((bracketList) (inlineStyle)) (els) in (((logic.ifElse) (((equality.gt) ((expressionLength) (inlineList))) ((70)%Z))) (((bracketList) (halfBlockStyle)) (els))) (inlineList).
Definition curlyBlock : forall (_ : BlockStyle) , forall (_ : Expr) , Expr := fun (style : BlockStyle) => fun (e : Expr) => (((curlyBracesList) (None)) (style)) ((cons) (e) (nil)).
Definition customIndent : forall (_ : string) , forall (_ : string) , string := fun (idt : string) => fun (s : string) => (strings.cat) (((lists.intersperse) ("
"%string)) (((lists.map) (fun (line : string) => ((strings.cat2) (idt)) (line))) ((strings.lines) (s)))).
Definition sep : forall (_ : Op) , forall (_ : (list) (Expr)) , Expr := fun (op : Op) => fun (els : (list) (Expr)) => (((maybes.maybe) ((cst) (""%string))) (fun (h : Expr) => (((lists.foldl) (fun (acc : Expr) => fun (el : Expr) => (((ifx) (op)) (acc)) (el))) (h)) (((lists.drop) ((1)%Z)) (els)))) ((lists.safeHead) (els)).
Definition newlineSep : forall (_ : (list) (Expr)) , Expr := (sep) ((Build_Op) ((sym) (""%string)) ((Build_Padding) ((Ws_None) (tt)) ((Ws_Break) (tt))) ((0)%Z) ((Associativity_None) (tt))).
Definition customIndentBlock : forall (_ : string) , forall (_ : (list) (Expr)) , Expr := fun (idt : string) => fun (els : (list) (Expr)) => let idtOp := (Build_Op) ((sym) (""%string)) ((Build_Padding) ((Ws_Space) (tt)) ((Ws_BreakAndIndent) (idt))) ((0)%Z) ((Associativity_None) (tt)) in (((maybes.maybe) ((cst) (""%string))) (fun (head : Expr) => (((logic.ifElse) (((equality.equal) ((lists.length) (els))) ((1)%Z))) (head)) ((((ifx) (idtOp)) (head)) ((newlineSep) (((lists.drop) ((1)%Z)) (els)))))) ((lists.safeHead) (els)).
Definition dotSep : forall (_ : (list) (Expr)) , Expr := (sep) ((Build_Op) ((sym) ("."%string)) ((Build_Padding) ((Ws_None) (tt)) ((Ws_None) (tt))) ((0)%Z) ((Associativity_None) (tt))).
Definition doubleNewlineSep : forall (_ : (list) (Expr)) , Expr := (sep) ((Build_Op) ((sym) (""%string)) ((Build_Padding) ((Ws_Break) (tt)) ((Ws_Break) (tt))) ((0)%Z) ((Associativity_None) (tt))).
Definition fullBlockStyle : BlockStyle := (Build_BlockStyle) ((Some) (doubleSpace)) (true) (true).
Definition indent : forall (_ : string) , string := (customIndent) (doubleSpace).
Definition indentBlock : forall (_ : (list) (Expr)) , Expr := (customIndentBlock) (doubleSpace).
Definition indentSubsequentLines : forall (_ : string) , forall (_ : Expr) , Expr := fun (idt : string) => fun (e : Expr) => (Expr_Indent) ((Build_IndentedExpression) ((IndentStyle_SubsequentLines) (idt)) (e)).
Definition spaceSep : forall (_ : (list) (Expr)) , Expr := (sep) ((Build_Op) ((sym) (""%string)) ((Build_Padding) ((Ws_Space) (tt)) ((Ws_None) (tt))) ((0)%Z) ((Associativity_None) (tt))).
Definition infixWs : forall (_ : string) , forall (_ : Expr) , forall (_ : Expr) , Expr := fun (op : string) => fun (l : Expr) => fun (r : Expr) => (spaceSep) ((cons) (l) ((cons) ((cst) (op)) ((cons) (r) (nil)))).
Definition infixWsList : forall (_ : string) , forall (_ : (list) (Expr)) , Expr := fun (op : string) => fun (opers : (list) (Expr)) => let opExpr := (cst) (op) in let foldFun := fun (e : (list) (Expr)) => fun (r : Expr) => (((logic.ifElse) ((lists.null) (e))) ((cons) (r) (nil))) (((lists.cons) (r)) (((lists.cons) (opExpr)) (e))) in (spaceSep) ((((lists.foldl) (foldFun)) (nil)) ((lists.reverse) (opers))).
Definition noPadding : Padding := (Build_Padding) ((Ws_None) (tt)) ((Ws_None) (tt)).
Definition noSep : forall (_ : (list) (Expr)) , Expr := (sep) ((Build_Op) ((sym) (""%string)) ((Build_Padding) ((Ws_None) (tt)) ((Ws_None) (tt))) ((0)%Z) ((Associativity_None) (tt))).
Definition num : forall (_ : Z) , Expr := fun (i : Z) => (cst) ((literals.showInt32) (i)).
Definition orOp : forall (_ : bool) , Op := fun (newlines : bool) => (Build_Op) ((sym) ("|"%string)) ((Build_Padding) ((Ws_Space) (tt)) ((((logic.ifElse) (newlines)) ((Ws_Break) (tt))) ((Ws_Space) (tt)))) ((0)%Z) ((Associativity_None) (tt)).
Definition orSep : forall (_ : BlockStyle) , forall (_ : (list) (Expr)) , Expr := fun (style : BlockStyle) => fun (l : (list) (Expr)) => let newlines := (fun r_ => (blockStyle_newlineBeforeContent) (r_)) (style) in (((maybes.maybe) ((cst) (""%string))) (fun (h : Expr) => (((lists.foldl) (fun (acc : Expr) => fun (el : Expr) => (((ifx) ((orOp) (newlines))) (acc)) (el))) (h)) (((lists.drop) ((1)%Z)) (l)))) ((lists.safeHead) (l)).
Definition parentheses : Brackets := (Build_Brackets) ("("%string) (")"%string).
Definition parenList : forall (_ : bool) , forall (_ : (list) (Expr)) , Expr := fun (newlines : bool) => fun (els : (list) (Expr)) => let style := (((logic.ifElse) (((logic.and) (newlines)) (((equality.gt) ((lists.length) (els))) ((1)%Z)))) (halfBlockStyle)) (inlineStyle) in (((logic.ifElse) ((lists.null) (els))) ((cst) ("()"%string))) ((((brackets) (parentheses)) (style)) (((commaSep) (style)) (els))).
Definition parens : forall (_ : Expr) , Expr := ((brackets) (parentheses)) (inlineStyle).
Definition parenthesize_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Expr) , Expr) =>
    let parenthesize := bundle_ in
    fun (exp : Expr) => let assocLeft := fun (a : Associativity) => (fun x_ => match x_ with
| Associativity_Right _ => false
| _ => true
end) (a) in let assocRight := fun (a : Associativity) => (fun x_ => match x_ with
| Associativity_Left _ => false
| _ => true
end) (a) in (fun x_ => match x_ with
| Expr_Brackets v_ => (fun (bracketExpr : BracketExpr) => (Expr_Brackets) ((Build_BracketExpr) ((fun r_ => (bracketExpr_brackets) (r_)) (bracketExpr)) ((parenthesize) ((fun r_ => (bracketExpr_enclosed) (r_)) (bracketExpr))) ((fun r_ => (bracketExpr_style) (r_)) (bracketExpr)))) (v_)
| Expr_Const v_ => (fun (ignored : Symbol) => exp) (v_)
| Expr_Indent v_ => (fun (indentExpr : IndentedExpression) => (Expr_Indent) ((Build_IndentedExpression) ((fun r_ => (indentedExpression_style) (r_)) (indentExpr)) ((parenthesize) ((fun r_ => (indentedExpression_expr) (r_)) (indentExpr))))) (v_)
| Expr_Seq v_ => (fun (seqExpr : SeqExpr) => (Expr_Seq) ((Build_SeqExpr) ((fun r_ => (seqExpr_op) (r_)) (seqExpr)) (((lists.map) (parenthesize)) ((fun r_ => (seqExpr_elements) (r_)) (seqExpr))))) (v_)
| Expr_Op v_ => (fun (opExpr : OpExpr) => let op := (fun r_ => (opExpr_op) (r_)) (opExpr) in let assoc := (fun r_ => (op_associativity) (r_)) (op) in let lhs := (fun r_ => (opExpr_lhs) (r_)) (opExpr) in let lhs' := (parenthesize) (lhs) in let prec := (fun w_ => w_) ((fun r_ => (op_precedence) (r_)) (op)) in let lhs2 := (fun x_ => match x_ with
| Expr_Op v_ => (fun (lopExpr : OpExpr) => let lop := (fun r_ => (opExpr_op) (r_)) (lopExpr) in let lprec := (fun w_ => w_) ((fun r_ => (op_precedence) (r_)) (lop)) in let comparison := ((equality.compare) (prec)) (lprec) in let lassoc := (fun r_ => (op_associativity) (r_)) (lop) in (fun x_ => match x_ with
| Comparison_LessThan _ => lhs'
| Comparison_GreaterThan _ => (parens) (lhs')
| Comparison_EqualTo _ => (((logic.ifElse) (((logic.and) ((assocLeft) (assoc))) ((assocLeft) (lassoc)))) (lhs')) ((parens) (lhs'))
end) (comparison)) (v_)
| _ => lhs'
end) (lhs') in let rhs := (fun r_ => (opExpr_rhs) (r_)) (opExpr) in let rhs' := (parenthesize) (rhs) in let rhs2 := (fun x_ => match x_ with
| Expr_Op v_ => (fun (ropExpr : OpExpr) => let rop := (fun r_ => (opExpr_op) (r_)) (ropExpr) in let rprec := (fun w_ => w_) ((fun r_ => (op_precedence) (r_)) (rop)) in let comparison := ((equality.compare) (prec)) (rprec) in let rassoc := (fun r_ => (op_associativity) (r_)) (rop) in (fun x_ => match x_ with
| Comparison_LessThan _ => rhs'
| Comparison_GreaterThan _ => (parens) (rhs')
| Comparison_EqualTo _ => (((logic.ifElse) (((logic.and) ((assocRight) (assoc))) ((assocRight) (rassoc)))) (rhs')) ((parens) (rhs'))
end) (comparison)) (v_)
| _ => rhs'
end) (rhs') in (Expr_Op) ((Build_OpExpr) (op) (lhs2) (rhs2))) (v_)
end) (exp)).

Definition parenthesize : forall (_ : Expr) , Expr :=
  parenthesize_bundle.
Definition prefix : forall (_ : string) , forall (_ : Expr) , Expr := fun (p : string) => fun (expr : Expr) => let preOp := (Build_Op) ((sym) (p)) ((Build_Padding) ((Ws_None) (tt)) ((Ws_None) (tt))) ((0)%Z) ((Associativity_None) (tt)) in (((ifx) (preOp)) ((cst) (""%string))) (expr).
Definition printExpr_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Expr) , string) =>
    let printExpr := bundle_ in
    fun (e : Expr) => let idt := fun (ws : Ws) => fun (s : string) => (fun x_ => match x_ with
| Ws_BreakAndIndent v_ => (fun (indentStr : string) => ((customIndent) (indentStr)) (s)) (v_)
| _ => s
end) (ws) in let pad := fun (ws : Ws) => (fun x_ => match x_ with
| Ws_None _ => ""%string
| Ws_Space _ => " "%string
| Ws_Break _ => "
"%string
| Ws_BreakAndIndent v_ => (fun (ignored : string) => "
"%string) (v_)
| Ws_DoubleBreak _ => "

"%string
end) (ws) in (fun x_ => match x_ with
| Expr_Const v_ => (fun (symbol : Symbol) => (fun w_ => w_) (symbol)) (v_)
| Expr_Indent v_ => (fun (indentExpr : IndentedExpression) => let expr := (fun r_ => (indentedExpression_expr) (r_)) (indentExpr) in let lns := (strings.lines) ((printExpr) (expr)) in let style := (fun r_ => (indentedExpression_style) (r_)) (indentExpr) in let ilns := (fun x_ => match x_ with
| IndentStyle_AllLines v_ => (fun (idt2 : string) => ((lists.map) (fun (line : string) => ((strings.cat2) (idt2)) (line))) (lns)) (v_)
| IndentStyle_SubsequentLines v_ => (fun (idt2 : string) => (((logic.ifElse) (((equality.equal) ((lists.length) (lns))) ((1)%Z))) (lns)) (((lists.cons) ((lists.head) (lns))) (((lists.map) (fun (line : string) => ((strings.cat2) (idt2)) (line))) ((lists.tail) (lns))))) (v_)
end) (style) in ((strings.intercalate) ("
"%string)) (ilns)) (v_)
| Expr_Seq v_ => (fun (seqExpr : SeqExpr) => let selements := (fun r_ => (seqExpr_elements) (r_)) (seqExpr) in let sop := (fun r_ => (seqExpr_op) (r_)) (seqExpr) in let spadding := (fun r_ => (op_padding) (r_)) (sop) in let spadr := (fun r_ => (padding_right) (r_)) (spadding) in let printedElements := ((lists.map) (fun (el : Expr) => ((idt) (spadr)) ((printExpr) (el)))) (selements) in let spadl := (fun r_ => (padding_left) (r_)) (spadding) in let ssym := (fun w_ => w_) ((fun r_ => (op_symbol) (r_)) (sop)) in let separator := ((strings.cat2) (((strings.cat2) ((pad) (spadl))) (ssym))) ((pad) (spadr)) in ((strings.intercalate) (separator)) (printedElements)) (v_)
| Expr_Op v_ => (fun (opExpr : OpExpr) => let l := (fun r_ => (opExpr_lhs) (r_)) (opExpr) in let op := (fun r_ => (opExpr_op) (r_)) (opExpr) in let padding := (fun r_ => (op_padding) (r_)) (op) in let padl := (fun r_ => (padding_left) (r_)) (padding) in let lhs := ((idt) (padl)) ((printExpr) (l)) in let padr := (fun r_ => (padding_right) (r_)) (padding) in let r := (fun r_ => (opExpr_rhs) (r_)) (opExpr) in let rhs := ((idt) (padr)) ((printExpr) (r)) in let sym := (fun w_ => w_) ((fun r_ => (op_symbol) (r_)) (op)) in ((strings.cat2) (((strings.cat2) (((strings.cat2) (((strings.cat2) (lhs)) ((pad) (padl)))) (sym))) ((pad) (padr)))) (rhs)) (v_)
| Expr_Brackets v_ => (fun (bracketExpr : BracketExpr) => let e := (fun r_ => (bracketExpr_enclosed) (r_)) (bracketExpr) in let body := (printExpr) (e) in let brs := (fun r_ => (bracketExpr_brackets) (r_)) (bracketExpr) in let style := (fun r_ => (bracketExpr_style) (r_)) (bracketExpr) in let doIndent := (fun r_ => (blockStyle_indent) (r_)) (style) in let ibody := (((maybes.maybe) (body)) (fun (idt2 : string) => ((customIndent) (idt2)) (body))) (doIndent) in let l := (fun w_ => w_) ((fun r_ => (brackets_open) (r_)) (brs)) in let nlAfter := (fun r_ => (blockStyle_newlineAfterContent) (r_)) (style) in let nlBefore := (fun r_ => (blockStyle_newlineBeforeContent) (r_)) (style) in let pre := (((logic.ifElse) (nlBefore)) ("
"%string)) (""%string) in let r := (fun w_ => w_) ((fun r_ => (brackets_close) (r_)) (brs)) in let suf := (((logic.ifElse) (nlAfter)) ("
"%string)) (""%string) in ((strings.cat2) (((strings.cat2) (((strings.cat2) (((strings.cat2) (l)) (pre))) (ibody))) (suf))) (r)) (v_)
end) (e)).

Definition printExpr : forall (_ : Expr) , string :=
  printExpr_bundle.
Definition semicolonSep : forall (_ : (list) (Expr)) , Expr := ((symbolSep) (";"%string)) (inlineStyle).
Definition structuralSep : forall (_ : Op) , forall (_ : (list) (Expr)) , Expr := fun (op : Op) => fun (els : (list) (Expr)) => (((logic.ifElse) ((lists.null) (els))) ((cst) (""%string))) ((((logic.ifElse) (((equality.equal) ((lists.length) (els))) ((1)%Z))) ((lists.head) (els))) ((Expr_Seq) ((Build_SeqExpr) (op) (els)))).
Definition structuralSpaceSep : forall (_ : (list) (Expr)) , Expr := (structuralSep) ((Build_Op) ((sym) (""%string)) ((Build_Padding) ((Ws_Space) (tt)) ((Ws_None) (tt))) ((0)%Z) ((Associativity_None) (tt))).
Definition suffix : forall (_ : string) , forall (_ : Expr) , Expr := fun (s : string) => fun (expr : Expr) => let sufOp := (Build_Op) ((sym) (s)) ((Build_Padding) ((Ws_None) (tt)) ((Ws_None) (tt))) ((0)%Z) ((Associativity_None) (tt)) in (((ifx) (sufOp)) (expr)) ((cst) (""%string)).
Definition tabIndent : forall (_ : Expr) , Expr := fun (e : Expr) => (Expr_Indent) ((Build_IndentedExpression) ((IndentStyle_AllLines) ("    "%string)) (e)).
Definition tabIndentDoubleSpace : forall (_ : (list) (Expr)) , Expr := fun (exprs : (list) (Expr)) => (tabIndent) ((doubleNewlineSep) (exprs)).
Definition tabIndentSingleSpace : forall (_ : (list) (Expr)) , Expr := fun (exprs : (list) (Expr)) => (tabIndent) ((newlineSep) (exprs)).
Definition unsupportedType : forall (_ : string) , Expr := fun (label : string) => (cst) (((strings.cat2) (((strings.cat2) ("["%string)) (label))) ("]"%string)).
Definition unsupportedVariant : forall (_ : string) , forall (_ : string) , Expr := fun (label : string) => fun (obj : string) => (cst) (((strings.cat2) (((strings.cat2) (((strings.cat2) (((strings.cat2) ("[unsupported "%string)) (label))) (": "%string))) ((literals.showString) (obj)))) ("]"%string)).
Definition withComma : forall (_ : Expr) , Expr := fun (e : Expr) => (noSep) ((cons) (e) ((cons) ((cst) (","%string)) (nil))).
Definition withSemi : forall (_ : Expr) , Expr := fun (e : Expr) => (noSep) ((cons) (e) ((cons) ((cst) (";"%string)) (nil))).

