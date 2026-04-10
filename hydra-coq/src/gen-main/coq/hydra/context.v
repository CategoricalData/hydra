(* Execution context for tracing and diagnostics *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core.
Record Context_ : Type := Build_Context_ {
  context__trace : (list) (string) ;
  context__messages : (list) (string) ;
  context__other : (list) ((prod) (Name) (Term))
}.

Record InContext (e : Type) : Type := Build_InContext {
  inContext_object : e ;
  inContext_context : Context_
}.

Arguments Build_InContext {e}.
Arguments inContext_object {e}.
Arguments inContext_context {e}.

