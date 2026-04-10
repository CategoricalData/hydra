(* JSON parser using Hydra parser combinators *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.parsing hydra.parsers hydra.lib.lists hydra.lib.logic hydra.lib.equality hydra.json.model hydra.lib.strings hydra.lib.maybes hydra.lib.literals hydra.lib.maps.

Definition whitespace : (Parser) (unit) :=
  ((hydra.parsers.map) (fun (_ : (list) (Z)) => tt)) ((parsers.many) ((parsers.satisfy) (fun (c : Z) => (((lists.foldl) (logic.or)) (false)) ((cons) (((equality.equal) (c)) ((32)%Z)) ((cons) (((equality.equal) (c)) ((9)%Z)) ((cons) (((equality.equal) (c)) ((10)%Z)) ((cons) (((equality.equal) (c)) ((13)%Z)) (nil)))))))).
Definition token (t0 : Type) : (Parser) (t0) -> (Parser) (t0) :=
  fun (p : (Parser) (t0)) => ((parsers.bind) (p)) (fun (x : t0) => ((parsers.bind) (whitespace)) (fun (_ : unit) => (parsers.pure) (x))).
Arguments token {t0}.
Definition jsonNull : (Parser) (Value) :=
  ((hydra.parsers.map) (fun (_ : string) => (Value_Null) (tt))) ((token) ((hydra.parsers.string_) ("null"%string))).
Definition jsonEscapeChar : (Parser) (Z) :=
  (parsers.choice) ((cons) (((hydra.parsers.map) (fun (_ : Z) => (34)%Z)) ((parsers.char) ((34)%Z))) ((cons) (((hydra.parsers.map) (fun (_ : Z) => (92)%Z)) ((parsers.char) ((92)%Z))) ((cons) (((hydra.parsers.map) (fun (_ : Z) => (47)%Z)) ((parsers.char) ((47)%Z))) ((cons) (((hydra.parsers.map) (fun (_ : Z) => (8)%Z)) ((parsers.char) ((98)%Z))) ((cons) (((hydra.parsers.map) (fun (_ : Z) => (12)%Z)) ((parsers.char) ((102)%Z))) ((cons) (((hydra.parsers.map) (fun (_ : Z) => (10)%Z)) ((parsers.char) ((110)%Z))) ((cons) (((hydra.parsers.map) (fun (_ : Z) => (13)%Z)) ((parsers.char) ((114)%Z))) ((cons) (((hydra.parsers.map) (fun (_ : Z) => (9)%Z)) ((parsers.char) ((116)%Z))) (nil))))))))).
Definition jsonStringChar : (Parser) (Z) :=
  ((parsers.alt) (((parsers.bind) ((parsers.char) ((92)%Z))) (fun (_ : Z) => jsonEscapeChar))) ((parsers.satisfy) (fun (c : Z) => ((logic.and) ((logic.not) (((equality.equal) (c)) ((34)%Z)))) ((logic.not) (((equality.equal) (c)) ((92)%Z))))).
Definition jsonString : (Parser) (Value) :=
  (token) (((parsers.bind) ((parsers.char) ((34)%Z))) (fun (_ : Z) => ((parsers.bind) ((parsers.many) (jsonStringChar))) (fun (chars : (list) (Z)) => ((parsers.bind) ((parsers.char) ((34)%Z))) (fun (_2 : Z) => (parsers.pure) ((Value_String) ((strings.fromList) (chars))))))).
Definition jsonBool : (Parser) (Value) :=
  ((parsers.alt) (((hydra.parsers.map) (fun (_ : string) => (Value_Boolean) (true))) ((token) ((hydra.parsers.string_) ("true"%string))))) (((hydra.parsers.map) (fun (_ : string) => (Value_Boolean) (false))) ((token) ((hydra.parsers.string_) ("false"%string)))).
Definition digit : (Parser) (Z) :=
  (parsers.satisfy) (fun (c : Z) => ((logic.and) (((equality.gte) (c)) ((48)%Z))) (((equality.lte) (c)) ((57)%Z))).
Definition digits : (Parser) (string) :=
  ((hydra.parsers.map) (strings.fromList)) ((parsers.some) (digit)).
Definition jsonExponentPart : (Parser) ((option) (string)) :=
  (parsers.optional) (((parsers.bind) ((parsers.satisfy) (fun (c : Z) => ((logic.or) (((equality.equal) (c)) ((101)%Z))) (((equality.equal) (c)) ((69)%Z))))) (fun (_ : Z) => ((parsers.bind) ((parsers.optional) ((parsers.satisfy) (fun (c : Z) => ((logic.or) (((equality.equal) (c)) ((43)%Z))) (((equality.equal) (c)) ((45)%Z)))))) (fun (sign : (option) (Z)) => ((hydra.parsers.map) (fun (digits : string) => ((strings.cat2) (((strings.cat2) ("e"%string)) ((((maybes.maybe) (""%string)) (fun (arg_ : Z) => (strings.fromList) ((lists.pure) (arg_)))) (sign)))) (digits))) (digits)))).
Definition jsonFractionPart : (Parser) ((option) (string)) :=
  (parsers.optional) (((parsers.bind) ((parsers.char) ((46)%Z))) (fun (_ : Z) => ((hydra.parsers.map) (fun (d : string) => ((strings.cat2) ("."%string)) (d))) (digits))).
Definition jsonIntegerPart : (Parser) (string) :=
  ((parsers.bind) ((parsers.optional) ((parsers.char) ((45)%Z)))) (fun (sign : (option) (Z)) => ((parsers.bind) (digits)) (fun (digits : string) => (parsers.pure) ((((maybes.maybe) (digits)) (fun (_ : Z) => ((strings.cat2) ("-"%string)) (digits))) (sign)))).
Definition jsonNumber : (Parser) (Value) :=
  (token) (((parsers.bind) (jsonIntegerPart)) (fun (intPart : string) => ((parsers.bind) (jsonFractionPart)) (fun (fracPart : (option) (string)) => ((parsers.bind) (jsonExponentPart)) (fun (expPart : (option) (string)) => let numStr := ((strings.cat2) (((strings.cat2) (intPart)) ((((maybes.maybe) (""%string)) (equality.identity)) (fracPart)))) ((((maybes.maybe) (""%string)) (equality.identity)) (expPart)) in (parsers.pure) ((Value_Number) ((((maybes.maybe) ((0.0))) (equality.identity)) ((literals.readBigfloat) (numStr)))))))).
Definition jsonValue_jsonArray_bundle :=
  hydra_fix (fun (bundle_ : prod ((Parser) (Value)) (prod ((Parser) (Value)) (prod ((Parser) (Value)) ((Parser) ((prod) (string) (Value)))))) =>
    let jsonValue := (fst bundle_) in
    let jsonArray := (fst (snd bundle_)) in
    let jsonObject := (fst (snd (snd bundle_))) in
    let jsonKeyValue := (snd (snd (snd bundle_))) in
    (pair ((parsers.choice) ((cons) (jsonNull) ((cons) (jsonBool) ((cons) (jsonNumber) ((cons) (jsonString) ((cons) (jsonArray) ((cons) (jsonObject) (nil)))))))) ((pair (((hydra.parsers.map) (fun (x : (list) (Value)) => (Value_Array) (x))) ((((parsers.between) ((token) ((parsers.char) ((91)%Z)))) ((token) ((parsers.char) ((93)%Z)))) (((parsers.sepBy) ((parsers.lazy) (fun (_ : unit) => jsonValue))) ((token) ((parsers.char) ((44)%Z)))))) ((pair (((hydra.parsers.map) (fun (arg_ : (list) ((prod) (string) (Value))) => (fun (x : (list) ((prod) (string) (Value))) => (Value_Object) (x)) ((maps.fromList) (arg_)))) ((((parsers.between) ((token) ((parsers.char) ((123)%Z)))) ((token) ((parsers.char) ((125)%Z)))) (((parsers.sepBy) (jsonKeyValue)) ((token) ((parsers.char) ((44)%Z)))))) (((parsers.bind) ((token) (((parsers.bind) ((parsers.char) ((34)%Z))) (fun (_ : Z) => ((parsers.bind) ((parsers.many) (jsonStringChar))) (fun (chars : (list) (Z)) => ((parsers.bind) ((parsers.char) ((34)%Z))) (fun (_2 : Z) => (parsers.pure) ((strings.fromList) (chars)))))))) (fun (key : string) => ((parsers.bind) ((token) ((parsers.char) ((58)%Z)))) (fun (_ : Z) => ((hydra.parsers.map) (fun (v : Value) => (pair) (key) (v))) ((parsers.lazy) (fun (_2 : unit) => jsonValue))))))))))).

Definition jsonValue : (Parser) (Value) :=
  (fst jsonValue_jsonArray_bundle).
Definition jsonArray : (Parser) (Value) :=
  (fst (snd jsonValue_jsonArray_bundle)).
Definition jsonObject : (Parser) (Value) :=
  (fst (snd (snd jsonValue_jsonArray_bundle))).
Definition jsonKeyValue : (Parser) ((prod) (string) (Value)) :=
  (snd (snd (snd jsonValue_jsonArray_bundle))).
Definition parseJson : string -> (ParseResult) (Value) :=
  fun (input : string) => ((fun w_ => w_) (((parsers.bind) (whitespace)) (fun (_ : unit) => ((parsers.bind) (jsonValue)) (fun (v : Value) => ((parsers.bind) (whitespace)) (fun (_2 : unit) => ((parsers.bind) (parsers.eof)) (fun (_3 : unit) => (parsers.pure) (v))))))) (input).

