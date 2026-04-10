(* JSON serialization functions using the Hydra AST *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.lib.strings hydra.lib.lists hydra.lib.math hydra.lib.logic hydra.lib.equality hydra.ast hydra.json.model hydra.lib.pairs hydra.serialization hydra.lib.literals hydra.lib.maps.

Definition jsonString : string -> string :=
  fun (s : string) => let hexEscape := fun (c : Z) => let lo := (strings.fromList) ((lists.pure) (((strings.charAt) (((math.mod_) (c)) ((16)%Z))) ("0123456789abcdef"%string))) in let hi := (strings.fromList) ((lists.pure) (((strings.charAt) (((math.div) (c)) ((16)%Z))) ("0123456789abcdef"%string))) in ((strings.cat2) (((strings.cat2) ("\u00"%string)) (hi))) (lo) in let escape := fun (c : Z) => (((logic.ifElse) (((equality.equal) (c)) ((34)%Z))) ("\"""%string)) ((((logic.ifElse) (((equality.equal) (c)) ((92)%Z))) ("\\"%string)) ((((logic.ifElse) (((equality.equal) (c)) ((8)%Z))) ("\b"%string)) ((((logic.ifElse) (((equality.equal) (c)) ((12)%Z))) ("\f"%string)) ((((logic.ifElse) (((equality.equal) (c)) ((10)%Z))) ("\n"%string)) ((((logic.ifElse) (((equality.equal) (c)) ((13)%Z))) ("\r"%string)) ((((logic.ifElse) (((equality.equal) (c)) ((9)%Z))) ("\t"%string)) ((((logic.ifElse) (((equality.lt) (c)) ((32)%Z))) ((hexEscape) (c))) ((strings.fromList) ((lists.pure) (c)))))))))) in let escaped := (strings.cat) (((lists.map) (escape)) ((strings.toList) (s))) in ((strings.cat2) (((strings.cat2) (""""%string)) (escaped))) (""""%string).
Definition colonOp : Op :=
  (Build_Op) (":"%string) ((Build_Padding) ((Ws_None) (tt)) ((Ws_Space) (tt))) ((0)%Z) ((Associativity_None) (tt)).
Definition keyValueToExpr_valueToExpr_bundle :=
  hydra_fix (fun (bundle_ : prod ((prod) (string) (Value) -> Expr) (Value -> Expr)) =>
    let keyValueToExpr := (fst bundle_) in
    let valueToExpr := (snd bundle_) in
    (pair (fun (pair_ : (prod) (string) (Value)) => let value := (pairs.second) (pair_) in let key := (pairs.first) (pair_) in (((ifx) (colonOp)) ((cst) ((jsonString) (key)))) ((valueToExpr) (value))) (fun (value : Value) => (fun x_ => match x_ with
| Value_Array v_ => (fun (arr : (list) (Value)) => (bracketListAdaptive) (((lists.map) (valueToExpr)) (arr))) (v_)
| Value_Boolean v_ => (fun (b : bool) => (cst) ((((logic.ifElse) (b)) ("true"%string)) ("false"%string))) (v_)
| Value_Null _ => (cst) ("null"%string)
| Value_Number v_ => (fun (n : Q) => let rounded := (literals.bigfloatToBigint) (n) in (cst) ((((logic.ifElse) (((equality.equal) (n)) ((literals.bigintToBigfloat) (rounded)))) ((literals.showBigint) (rounded))) ((literals.showBigfloat) (n)))) (v_)
| Value_Object v_ => (fun (obj : (list) ((prod) (string) (Value))) => (bracesListAdaptive) (((lists.map) (keyValueToExpr)) ((maps.toList) (obj)))) (v_)
| Value_String v_ => (fun (s : string) => (cst) ((jsonString) (s))) (v_)
end) (value)))).

Definition keyValueToExpr : (prod) (string) (Value) -> Expr :=
  (fst keyValueToExpr_valueToExpr_bundle).
Definition valueToExpr : Value -> Expr :=
  (snd keyValueToExpr_valueToExpr_bundle).
Definition printJson : Value -> string :=
  fun (value : Value) => (printExpr) ((valueToExpr) (value)).

