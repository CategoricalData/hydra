(* Parser combinator types for text parsing *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.
Record ParseSuccess (a : Type) : Type := Build_ParseSuccess {
  parseSuccess_value : a ;
  parseSuccess_remainder : string
}.

Record ParseError : Type := Build_ParseError {
  parseError_message : string ;
  parseError_remainder : string
}.

Inductive ParseResult (a : Type) : Type :=
| ParseResult_Success : (ParseSuccess) (a) -> (ParseResult) (a)
| ParseResult_Failure : ParseError -> (ParseResult) (a).

Definition Parser (a : Type) : Type :=
  string -> (ParseResult) (a).

Arguments ParseResult_Success {a}.
Arguments ParseResult_Failure {a}.
Arguments Build_ParseSuccess {a}.
Arguments parseSuccess_value {a}.
Arguments parseSuccess_remainder {a}.

