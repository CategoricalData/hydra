(* Term decoders for hydra.ast *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.ast hydra.core hydra.errors hydra.extract.core hydra.graph hydra.lib.eithers hydra.lib.maps hydra.lib.maybes hydra.lib.strings.

Definition associativity : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Associativity) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("none"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (Associativity_None) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("left"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (Associativity_Left) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("right"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (Associativity_Right) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("both"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (Associativity_Both) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) (nil))))) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (Associativity)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition blockStyle : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (BlockStyle) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("indent"%string)) ((decodeMaybe) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))))) (fieldMap)) (cx))) (fun (field_indent : (option) (string)) => ((eithers.bind) (((((requireField) ("newlineBeforeContent"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Boolean v_ => (fun (b : bool) => (inr) (b)) (v_)
| _ => (inl) ("expected boolean literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_newlineBeforeContent : bool) => ((eithers.bind) (((((requireField) ("newlineAfterContent"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Boolean v_ => (fun (b : bool) => (inr) (b)) (v_)
| _ => (inl) ("expected boolean literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_newlineAfterContent : bool) => (inr) ((Build_BlockStyle) (field_indent) (field_newlineBeforeContent) (field_newlineAfterContent)))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition symbol : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Symbol) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : string) => b)) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => (inl) ("expected wrapped type"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition brackets : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Brackets) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("open"%string)) (symbol)) (fieldMap)) (cx))) (fun (field_open : Symbol) => ((eithers.bind) (((((requireField) ("close"%string)) (symbol)) (fieldMap)) (cx))) (fun (field_close : Symbol) => (inr) ((Build_Brackets) (field_open) (field_close))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition indentStyle : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (IndentStyle) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("allLines"%string) (fun (input : Term) => ((eithers.map) (fun (t : string) => (IndentStyle_AllLines) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("subsequentLines"%string) (fun (input : Term) => ((eithers.map) (fun (t : string) => (IndentStyle_SubsequentLines) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) (nil))) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (IndentStyle)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition ws : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Ws) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("none"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (Ws_None) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("space"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (Ws_Space) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("break"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (Ws_Break) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("breakAndIndent"%string) (fun (input : Term) => ((eithers.map) (fun (t : string) => (Ws_BreakAndIndent) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("doubleBreak"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (Ws_DoubleBreak) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) (nil)))))) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (Ws)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition padding : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Padding) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("left"%string)) (ws)) (fieldMap)) (cx))) (fun (field_left : Ws) => ((eithers.bind) (((((requireField) ("right"%string)) (ws)) (fieldMap)) (cx))) (fun (field_right : Ws) => (inr) ((Build_Padding) (field_left) (field_right))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition precedence : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Precedence) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : Z) => b)) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => (inr) (i)) (v_)
| _ => (inl) ("expected int32 value"%string)
end) (v_)
| _ => (inl) ("expected int32 literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => (inl) ("expected wrapped type"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition op : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Op) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("symbol"%string)) (symbol)) (fieldMap)) (cx))) (fun (field_symbol : Symbol) => ((eithers.bind) (((((requireField) ("padding"%string)) (padding)) (fieldMap)) (cx))) (fun (field_padding : Padding) => ((eithers.bind) (((((requireField) ("precedence"%string)) (precedence)) (fieldMap)) (cx))) (fun (field_precedence : Precedence) => ((eithers.bind) (((((requireField) ("associativity"%string)) (associativity)) (fieldMap)) (cx))) (fun (field_associativity : Associativity) => (inr) ((Build_Op) (field_symbol) (field_padding) (field_precedence) (field_associativity))))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition bracketExpr_expr_bundle :=
  hydra_fix (fun (bundle_ : prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (BracketExpr)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Expr)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (IndentedExpression)) (prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (OpExpr)) (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (SeqExpr)))))) =>
    let bracketExpr := (fst bundle_) in
    let expr := (fst (snd bundle_)) in
    let indentedExpression := (fst (snd (snd bundle_))) in
    let opExpr := (fst (snd (snd (snd bundle_)))) in
    let seqExpr := (snd (snd (snd (snd bundle_)))) in
    (pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("brackets"%string)) (brackets)) (fieldMap)) (cx))) (fun (field_brackets : Brackets) => ((eithers.bind) (((((requireField) ("enclosed"%string)) (expr)) (fieldMap)) (cx))) (fun (field_enclosed : Expr) => ((eithers.bind) (((((requireField) ("style"%string)) (blockStyle)) (fieldMap)) (cx))) (fun (field_style : BlockStyle) => (inr) ((Build_BracketExpr) (field_brackets) (field_enclosed) (field_style)))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("const"%string) (fun (input : Term) => ((eithers.map) (fun (t : Symbol) => (Expr_Const) (t))) (((symbol) (cx)) (input)))) ((cons) ((pair) ("indent"%string) (fun (input : Term) => ((eithers.map) (fun (t : IndentedExpression) => (Expr_Indent) (t))) (((indentedExpression) (cx)) (input)))) ((cons) ((pair) ("op"%string) (fun (input : Term) => ((eithers.map) (fun (t : OpExpr) => (Expr_Op) (t))) (((opExpr) (cx)) (input)))) ((cons) ((pair) ("brackets"%string) (fun (input : Term) => ((eithers.map) (fun (t : BracketExpr) => (Expr_Brackets) (t))) (((bracketExpr) (cx)) (input)))) ((cons) ((pair) ("seq"%string) (fun (input : Term) => ((eithers.map) (fun (t : SeqExpr) => (Expr_Seq) (t))) (((seqExpr) (cx)) (input)))) (nil)))))) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (Expr)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("style"%string)) (indentStyle)) (fieldMap)) (cx))) (fun (field_style : IndentStyle) => ((eithers.bind) (((((requireField) ("expr"%string)) (expr)) (fieldMap)) (cx))) (fun (field_expr : Expr) => (inr) ((Build_IndentedExpression) (field_style) (field_expr))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) ((pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("op"%string)) (op)) (fieldMap)) (cx))) (fun (field_op : Op) => ((eithers.bind) (((((requireField) ("lhs"%string)) (expr)) (fieldMap)) (cx))) (fun (field_lhs : Expr) => ((eithers.bind) (((((requireField) ("rhs"%string)) (expr)) (fieldMap)) (cx))) (fun (field_rhs : Expr) => (inr) ((Build_OpExpr) (field_op) (field_lhs) (field_rhs)))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("op"%string)) (op)) (fieldMap)) (cx))) (fun (field_op : Op) => ((eithers.bind) (((((requireField) ("elements"%string)) ((decodeList) (expr))) (fieldMap)) (cx))) (fun (field_elements : (list) (Expr)) => (inr) ((Build_SeqExpr) (field_op) (field_elements))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))))))))))).

Definition bracketExpr : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (BracketExpr) :=
  (fst bracketExpr_expr_bundle).
Definition expr : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Expr) :=
  (fst (snd bracketExpr_expr_bundle)).
Definition indentedExpression : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (IndentedExpression) :=
  (fst (snd (snd bracketExpr_expr_bundle))).
Definition opExpr : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (OpExpr) :=
  (fst (snd (snd (snd bracketExpr_expr_bundle)))).
Definition seqExpr : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (SeqExpr) :=
  (snd (snd (snd (snd bracketExpr_expr_bundle)))).

