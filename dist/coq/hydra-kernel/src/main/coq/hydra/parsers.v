(* General-purpose parser combinators *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.lib.equality hydra.lib.lists hydra.lib.logic hydra.lib.maybes hydra.lib.strings hydra.parsing.

Definition alt (t0 : Type) : forall (_ : (Parser) (t0)) , forall (_ : (Parser) (t0)) , (Parser) (t0) := fun (p1 : (Parser) (t0)) => fun (p2 : (Parser) (t0)) => let parse := fun (input : string) => (fun x_ => match x_ with
| ParseResult_Success v_ => (fun (s : (ParseSuccess) (t0)) => (ParseResult_Success) (s)) (v_)
| ParseResult_Failure v_ => (fun (e : ParseError) => (((logic.ifElse) (((equality.equal) ((fun r_ => (parseError_remainder) (r_)) (e))) (input))) (((fun w_ => w_) (p2)) (input))) ((ParseResult_Failure) (e))) (v_)
end) (((fun w_ => w_) (p1)) (input)) in parse.
Arguments alt {t0}.
Definition satisfy : forall (_ : forall (_ : Z) , bool) , (Parser) (Z) := fun (pred : forall (_ : Z) , bool) => let parse := fun (input : string) => let codes := (strings.toList) (input) in (((maybes.maybe) ((ParseResult_Failure) ((Build_ParseError) ("unexpected end of input"%string) (input)))) (fun (c : Z) => let rest := (strings.fromList) (((lists.drop) ((1)%Z)) (codes)) in (((logic.ifElse) ((pred) (c))) ((ParseResult_Success) ((Build_ParseSuccess) (c) (rest)))) ((ParseResult_Failure) ((Build_ParseError) ("character did not satisfy predicate"%string) (input))))) ((lists.safeHead) (codes)) in parse.
Definition anyChar : (Parser) (Z) := (satisfy) (fun (_ : Z) => true).
Definition apply (t0 : Type) (t1 : Type) : forall (_ : (Parser) (forall (_ : t0) , t1)) , forall (_ : (Parser) (t0)) , (Parser) (t1) := fun (pf : (Parser) (forall (_ : t0) , t1)) => fun (pa : (Parser) (t0)) => let parse := fun (input : string) => (fun x_ => match x_ with
| ParseResult_Success v_ => (fun (sf : (ParseSuccess) (forall (_ : t0) , t1)) => (fun x_ => match x_ with
| ParseResult_Success v_ => (fun (sa : (ParseSuccess) (t0)) => (ParseResult_Success) ((Build_ParseSuccess) (((fun r_ => (parseSuccess_value) (r_)) (sf)) ((fun r_ => (parseSuccess_value) (r_)) (sa))) ((fun r_ => (parseSuccess_remainder) (r_)) (sa)))) (v_)
| ParseResult_Failure v_ => (fun (e : ParseError) => (ParseResult_Failure) (e)) (v_)
end) (((fun w_ => w_) (pa)) ((fun r_ => (parseSuccess_remainder) (r_)) (sf)))) (v_)
| ParseResult_Failure v_ => (fun (e : ParseError) => (ParseResult_Failure) (e)) (v_)
end) (((fun w_ => w_) (pf)) (input)) in parse.
Arguments apply {t0} {t1}.
Definition bind (t0 : Type) (t1 : Type) : forall (_ : (Parser) (t0)) , forall (_ : forall (_ : t0) , (Parser) (t1)) , (Parser) (t1) := fun (pa : (Parser) (t0)) => fun (f : forall (_ : t0) , (Parser) (t1)) => let parse := fun (input : string) => (fun x_ => match x_ with
| ParseResult_Success v_ => (fun (s : (ParseSuccess) (t0)) => ((fun w_ => w_) ((f) ((fun r_ => (parseSuccess_value) (r_)) (s)))) ((fun r_ => (parseSuccess_remainder) (r_)) (s))) (v_)
| ParseResult_Failure v_ => (fun (e : ParseError) => (ParseResult_Failure) (e)) (v_)
end) (((fun w_ => w_) (pa)) (input)) in parse.
Arguments bind {t0} {t1}.
Definition pure (t0 : Type) : forall (_ : t0) , (Parser) (t0) := fun (a : t0) => fun (input : string) => (ParseResult_Success) ((Build_ParseSuccess) (a) (input)).
Arguments pure {t0}.
Definition between (t0 : Type) (t1 : Type) (t2 : Type) : forall (_ : (Parser) (t0)) , forall (_ : (Parser) (t1)) , forall (_ : (Parser) (t2)) , (Parser) (t2) := fun (open_ : (Parser) (t0)) => fun (close : (Parser) (t1)) => fun (p : (Parser) (t2)) => ((bind) (open_)) (fun (_ : t0) => ((bind) (p)) (fun (x : t2) => ((bind) (close)) (fun (_2 : t1) => (pure) (x)))).
Arguments between {t0} {t1} {t2}.
Definition char : forall (_ : Z) , (Parser) (Z) := fun (c : Z) => (satisfy) (fun (x : Z) => ((equality.equal) (x)) (c)).
Definition fail (t0 : Type) : forall (_ : string) , (Parser) (t0) := fun (msg : string) => fun (input : string) => (ParseResult_Failure) ((Build_ParseError) (msg) (input)).
Arguments fail {t0}.
Definition choice (t0 : Type) : forall (_ : (list) ((Parser) (t0))) , (Parser) (t0) := fun (ps : (list) ((Parser) (t0))) => (((lists.foldl) (alt)) ((fail) ("no choice matched"%string))) (ps).
Arguments choice {t0}.
Definition eof : (Parser) (unit) := fun (input : string) => (((logic.ifElse) (((equality.equal) (input)) (""%string))) ((ParseResult_Success) ((Build_ParseSuccess) (tt) (""%string)))) ((ParseResult_Failure) ((Build_ParseError) ("expected end of input"%string) (input))).
Definition lazy (t0 : Type) : forall (_ : forall (_ : unit) , (Parser) (t0)) , (Parser) (t0) := fun (f : forall (_ : unit) , (Parser) (t0)) => fun (input : string) => ((fun w_ => w_) ((f) (tt))) (input).
Arguments lazy {t0}.
Definition many_some_bundle (t0 : Type) :=
  hydra_fix (fun (bundle_ : prod (forall (_ : (Parser) (t0)) , (Parser) ((list) (t0))) (forall (_ : (Parser) (t0)) , (Parser) ((list) (t0)))) =>
    let many := (fst bundle_) in
    let some := (snd bundle_) in
    (pair (fun (p : (Parser) (t0)) => ((alt) ((some) (p))) ((pure) (nil))) (fun (p : (Parser) (t0)) => ((bind) (p)) (fun (x : t0) => ((bind) ((many) (p))) (fun (xs : (list) (t0)) => (pure) (((lists.cons) (x)) (xs))))))).
Arguments many_some_bundle {t0}.

Definition many (t0 : Type) : forall (_ : (Parser) (t0)) , (Parser) ((list) (t0)) :=
  (fst many_some_bundle).
Arguments many {t0}.
Definition some (t0 : Type) : forall (_ : (Parser) (t0)) , (Parser) ((list) (t0)) :=
  (snd many_some_bundle).
Arguments some {t0}.
Definition map (t0 : Type) (t1 : Type) : forall (_ : forall (_ : t0) , t1) , forall (_ : (Parser) (t0)) , (Parser) (t1) := fun (f : forall (_ : t0) , t1) => fun (pa : (Parser) (t0)) => let parse := fun (input : string) => (fun x_ => match x_ with
| ParseResult_Success v_ => (fun (s : (ParseSuccess) (t0)) => (ParseResult_Success) ((Build_ParseSuccess) ((f) ((fun r_ => (parseSuccess_value) (r_)) (s))) ((fun r_ => (parseSuccess_remainder) (r_)) (s)))) (v_)
| ParseResult_Failure v_ => (fun (e : ParseError) => (ParseResult_Failure) (e)) (v_)
end) (((fun w_ => w_) (pa)) (input)) in parse.
Arguments map {t0} {t1}.
Definition optional (t0 : Type) : forall (_ : (Parser) (t0)) , (Parser) ((option) (t0)) := fun (p : (Parser) (t0)) => ((alt) (((map) (maybes.pure)) (p))) ((pure) (None)).
Arguments optional {t0}.
Definition runParser (t0 : Type) : forall (_ : (Parser) (t0)) , forall (_ : string) , (ParseResult) (t0) := fun (p : (Parser) (t0)) => fun (input : string) => ((fun w_ => w_) (p)) (input).
Arguments runParser {t0}.
Definition sepBy1 (t0 : Type) (t1 : Type) : forall (_ : (Parser) (t0)) , forall (_ : (Parser) (t1)) , (Parser) ((list) (t0)) := fun (p : (Parser) (t0)) => fun (sep : (Parser) (t1)) => ((bind) (p)) (fun (x : t0) => ((bind) ((many) (((bind) (sep)) (fun (_ : t1) => p)))) (fun (xs : (list) (t0)) => (pure) (((lists.cons) (x)) (xs)))).
Arguments sepBy1 {t0} {t1}.
Definition sepBy (t0 : Type) (t1 : Type) : forall (_ : (Parser) (t0)) , forall (_ : (Parser) (t1)) , (Parser) ((list) (t0)) := fun (p : (Parser) (t0)) => fun (sep : (Parser) (t1)) => ((alt) (((sepBy1) (p)) (sep))) ((pure) (nil)).
Arguments sepBy {t0} {t1}.
Definition string_ : forall (_ : string) , (Parser) (string) := fun (str : string) => fun (input : string) => let inputCodes := (strings.toList) (input) in let strCodes := (strings.toList) (str) in let strLen := (lists.length) (strCodes) in let inputPrefix := ((lists.take) (strLen)) (inputCodes) in (((logic.ifElse) (((equality.equal) (strCodes)) (inputPrefix))) ((ParseResult_Success) ((Build_ParseSuccess) (str) ((strings.fromList) (((lists.drop) (strLen)) (inputCodes)))))) ((ParseResult_Failure) ((Build_ParseError) (((strings.cat2) ("expected: "%string)) (str)) (input))).

