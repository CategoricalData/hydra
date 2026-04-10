(* Utilities for type unification. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.lib.logic hydra.lib.equality hydra.rewriting hydra.coders hydra.errors hydra.typing hydra.strip hydra.lib.strings hydra.show.core hydra.lib.lists hydra.substitution hydra.lib.eithers hydra.lib.maybes hydra.lib.maps.

Definition variableOccursInType : Name -> Type_ -> bool :=
  fun (var : Name) => fun (typ0 : Type_) => let tryType := fun (b : bool) => fun (typ : Type_) => (fun x_ => match x_ with
| Type__Variable v_ => (fun (v : Name) => ((logic.or) (b)) (((equality.equal) ((fun w_ => w_) (v))) ((fun w_ => w_) (var)))) (v_)
| _ => b
end) (typ) in ((((foldOverType) ((TraversalOrder_Pre) (tt))) (tryType)) (false)) (typ0).
Definition joinTypes (t0 : Type) : t0 -> Type_ -> Type_ -> string -> (sum) (UnificationError) ((list) (TypeConstraint)) :=
  fun (cx : t0) => fun (left : Type_) => fun (right : Type_) => fun (comment : string) => let sright := (deannotateType) (right) in let sleft := (deannotateType) (left) in let joinOne := fun (l : Type_) => fun (r : Type_) => (Build_TypeConstraint) (l) (r) (((strings.cat2) ("join types; "%string)) (comment)) in let cannotUnify := fun (t1 : Type) => (inl) ((Build_UnificationError) (sleft) (sright) (((strings.cat2) (((strings.cat2) (((strings.cat2) ("cannot unify "%string)) ((hydra.show.core.type) (sleft)))) (" with "%string))) ((hydra.show.core.type) (sright)))) in let joinList := fun (lefts : (list) (Type_)) => fun (rights : (list) (Type_)) => (((logic.ifElse) (((equality.equal) ((lists.length) (lefts))) ((lists.length) (rights)))) ((inr) ((((lists.zipWith) (joinOne)) (lefts)) (rights)))) ((cannotUnify) ((list) (TypeConstraint))) in let joinRowTypes := fun (left2 : (list) (FieldType)) => fun (right2 : (list) (FieldType)) => (((logic.ifElse) (((logic.and) (((equality.equal) ((lists.length) (((lists.map) (fun r_ => (fieldType_name) (r_))) (left2)))) ((lists.length) (((lists.map) (fun r_ => (fieldType_name) (r_))) (right2))))) ((((lists.foldl) (logic.and)) (true)) ((((lists.zipWith) (fun (left3 : Name) => fun (right3 : Name) => ((equality.equal) ((fun w_ => w_) (left3))) ((fun w_ => w_) (right3)))) (((lists.map) (fun r_ => (fieldType_name) (r_))) (left2))) (((lists.map) (fun r_ => (fieldType_name) (r_))) (right2)))))) (((joinList) (((lists.map) (fun r_ => (fieldType_type) (r_))) (left2))) (((lists.map) (fun r_ => (fieldType_type) (r_))) (right2)))) ((cannotUnify) ((list) (TypeConstraint))) in let assertEqual := fun (t1 : Type) => (((logic.ifElse) (((equality.equal) (sleft)) (sright))) ((inr) (nil))) ((cannotUnify) ((list) (t1))) in (fun x_ => match x_ with
| Type__Application v_ => (fun (l : ApplicationType) => (fun x_ => match x_ with
| Type__Application v_ => (fun (r : ApplicationType) => (inr) ((cons) (((joinOne) ((fun r_ => (applicationType_function) (r_)) (l))) ((fun r_ => (applicationType_function) (r_)) (r))) ((cons) (((joinOne) ((fun r_ => (applicationType_argument) (r_)) (l))) ((fun r_ => (applicationType_argument) (r_)) (r))) (nil)))) (v_)
| _ => (cannotUnify) ((list) (TypeConstraint))
end) (sright)) (v_)
| Type__Either v_ => (fun (l : EitherType) => (fun x_ => match x_ with
| Type__Either v_ => (fun (r : EitherType) => (inr) ((cons) (((joinOne) ((fun r_ => (eitherType_left) (r_)) (l))) ((fun r_ => (eitherType_left) (r_)) (r))) ((cons) (((joinOne) ((fun r_ => (eitherType_right) (r_)) (l))) ((fun r_ => (eitherType_right) (r_)) (r))) (nil)))) (v_)
| _ => (cannotUnify) ((list) (TypeConstraint))
end) (sright)) (v_)
| Type__Function v_ => (fun (l : FunctionType) => (fun x_ => match x_ with
| Type__Function v_ => (fun (r : FunctionType) => (inr) ((cons) (((joinOne) ((fun r_ => (functionType_domain) (r_)) (l))) ((fun r_ => (functionType_domain) (r_)) (r))) ((cons) (((joinOne) ((fun r_ => (functionType_codomain) (r_)) (l))) ((fun r_ => (functionType_codomain) (r_)) (r))) (nil)))) (v_)
| _ => (cannotUnify) ((list) (TypeConstraint))
end) (sright)) (v_)
| Type__List v_ => (fun (l : Type_) => (fun x_ => match x_ with
| Type__List v_ => (fun (r : Type_) => (inr) ((cons) (((joinOne) (l)) (r)) (nil))) (v_)
| _ => (cannotUnify) ((list) (TypeConstraint))
end) (sright)) (v_)
| Type__Literal v_ => (fun (_ : LiteralType) => (assertEqual) (TypeConstraint)) (v_)
| Type__Map v_ => (fun (l : MapType) => (fun x_ => match x_ with
| Type__Map v_ => (fun (r : MapType) => (inr) ((cons) (((joinOne) ((fun r_ => (mapType_keys) (r_)) (l))) ((fun r_ => (mapType_keys) (r_)) (r))) ((cons) (((joinOne) ((fun r_ => (mapType_values) (r_)) (l))) ((fun r_ => (mapType_values) (r_)) (r))) (nil)))) (v_)
| _ => (cannotUnify) ((list) (TypeConstraint))
end) (sright)) (v_)
| Type__Maybe v_ => (fun (l : Type_) => (fun x_ => match x_ with
| Type__Maybe v_ => (fun (r : Type_) => (inr) ((cons) (((joinOne) (l)) (r)) (nil))) (v_)
| _ => (cannotUnify) ((list) (TypeConstraint))
end) (sright)) (v_)
| Type__Pair v_ => (fun (l : PairType) => (fun x_ => match x_ with
| Type__Pair v_ => (fun (r : PairType) => (inr) ((cons) (((joinOne) ((fun r_ => (pairType_first) (r_)) (l))) ((fun r_ => (pairType_first) (r_)) (r))) ((cons) (((joinOne) ((fun r_ => (pairType_second) (r_)) (l))) ((fun r_ => (pairType_second) (r_)) (r))) (nil)))) (v_)
| _ => (cannotUnify) ((list) (TypeConstraint))
end) (sright)) (v_)
| Type__Record v_ => (fun (l : (list) (FieldType)) => (fun x_ => match x_ with
| Type__Record v_ => (fun (r : (list) (FieldType)) => ((joinRowTypes) (l)) (r)) (v_)
| _ => (cannotUnify) ((list) (TypeConstraint))
end) (sright)) (v_)
| Type__Set v_ => (fun (l : Type_) => (fun x_ => match x_ with
| Type__Set v_ => (fun (r : Type_) => (inr) ((cons) (((joinOne) (l)) (r)) (nil))) (v_)
| _ => (cannotUnify) ((list) (TypeConstraint))
end) (sright)) (v_)
| Type__Union v_ => (fun (l : (list) (FieldType)) => (fun x_ => match x_ with
| Type__Union v_ => (fun (r : (list) (FieldType)) => ((joinRowTypes) (l)) (r)) (v_)
| _ => (cannotUnify) ((list) (TypeConstraint))
end) (sright)) (v_)
| Type__Unit _ => (fun x_ => match x_ with
| Type__Unit _ => (inr) (nil)
| _ => (cannotUnify) ((list) (TypeConstraint))
end) (sright)
| Type__Void _ => (fun x_ => match x_ with
| Type__Void _ => (inr) (nil)
| _ => (cannotUnify) ((list) (TypeConstraint))
end) (sright)
| Type__Wrap v_ => (fun (l : Type_) => (fun x_ => match x_ with
| Type__Wrap v_ => (fun (r : Type_) => (inr) ((cons) (((joinOne) (l)) (r)) (nil))) (v_)
| _ => (cannotUnify) ((list) (TypeConstraint))
end) (sright)) (v_)
| _ => (cannotUnify) ((list) (TypeConstraint))
end) (sleft).
Arguments joinTypes {t0}.
Definition unifyTypeConstraints_bundle (t0 : Type) (t1 : Type) :=
  hydra_fix (fun (bundle_ : t0 -> (list) ((prod) (Name) (t1)) -> (list) (TypeConstraint) -> (sum) (UnificationError) (TypeSubst)) =>
    let unifyTypeConstraints := bundle_ in
    fun (cx : t0) => fun (schemaTypes : (list) ((prod) (Name) (t1))) => fun (constraints : (list) (TypeConstraint)) => let withConstraint := fun (c : TypeConstraint) => fun (rest : (list) (TypeConstraint)) => let sright := (deannotateType) ((fun r_ => (typeConstraint_right) (r_)) (c)) in let sleft := (deannotateType) ((fun r_ => (typeConstraint_left) (r_)) (c)) in let comment := (fun r_ => (typeConstraint_comment) (r_)) (c) in let bind := fun (v : Name) => fun (t : Type_) => let subst := ((singletonTypeSubst) (v)) (t) in let withResult := fun (s : TypeSubst) => ((composeTypeSubst) (subst)) (s) in ((eithers.map) (withResult)) ((((unifyTypeConstraints) (cx)) (schemaTypes)) (((substituteInConstraints) (subst)) (rest))) in let noVars := let withConstraints := fun (constraints2 : (list) (TypeConstraint)) => (((unifyTypeConstraints) (cx)) (schemaTypes)) (((lists.concat2) (constraints2)) (rest)) in ((eithers.bind) (((((joinTypes) (cx)) (sleft)) (sright)) (comment))) (withConstraints) in let tryBinding := fun (v : Name) => fun (t : Type_) => (((logic.ifElse) (((variableOccursInType) (v)) (t))) ((inl) ((Build_UnificationError) (sleft) (sright) (((strings.cat2) (((strings.cat2) (((strings.cat2) (((strings.cat2) (((strings.cat2) (((strings.cat2) ("Variable "%string)) ((fun w_ => w_) (v)))) (" appears free in type "%string))) ((hydra.show.core.type) (t)))) (" ("%string))) (comment))) (")"%string))))) (((bind) (v)) (t)) in let dflt := (fun x_ => match x_ with
| Type__Variable v_ => (fun (name : Name) => ((tryBinding) (name)) (sleft)) (v_)
| _ => noVars
end) (sright) in (fun x_ => match x_ with
| Type__Variable v_ => (fun (name : Name) => (fun x_ => match x_ with
| Type__Variable v_ => (fun (name2 : Name) => (((logic.ifElse) (((equality.equal) ((fun w_ => w_) (name))) ((fun w_ => w_) (name2)))) ((((unifyTypeConstraints) (cx)) (schemaTypes)) (rest))) ((((logic.ifElse) ((maybes.isJust) (((maps.lookup) (name)) (schemaTypes)))) ((((logic.ifElse) ((maybes.isJust) (((maps.lookup) (name2)) (schemaTypes)))) ((inl) ((Build_UnificationError) (sleft) (sright) (((strings.cat2) (((strings.cat2) (((strings.cat2) (((strings.cat2) (((strings.cat2) (((strings.cat2) ("Attempted to unify schema names "%string)) ((fun w_ => w_) (name)))) (" and "%string))) ((fun w_ => w_) (name2)))) (" ("%string))) (comment))) (")"%string))))) (((bind) (name2)) (sleft)))) (((bind) (name)) (sright)))) (v_)
| _ => ((tryBinding) (name)) (sright)
end) (sright)) (v_)
| _ => dflt
end) (sleft) in (((logic.ifElse) ((lists.null) (constraints))) ((inr) (idTypeSubst))) (((withConstraint) ((lists.head) (constraints))) ((lists.tail) (constraints)))).
Arguments unifyTypeConstraints_bundle {t0} {t1}.

Definition unifyTypeConstraints (t0 : Type) (t1 : Type) : t0 -> (list) ((prod) (Name) (t1)) -> (list) (TypeConstraint) -> (sum) (UnificationError) (TypeSubst) :=
  unifyTypeConstraints_bundle.
Arguments unifyTypeConstraints {t0} {t1}.
Definition unifyTypeLists (t0 : Type) (t1 : Type) : t0 -> (list) ((prod) (Name) (t1)) -> (list) (Type_) -> (list) (Type_) -> string -> (sum) (UnificationError) (TypeSubst) :=
  fun (cx : t0) => fun (schemaTypes : (list) ((prod) (Name) (t1))) => fun (l : (list) (Type_)) => fun (r : (list) (Type_)) => fun (comment : string) => let toConstraint := fun (l2 : Type_) => fun (r2 : Type_) => (Build_TypeConstraint) (l2) (r2) (comment) in (((unifyTypeConstraints) (cx)) (schemaTypes)) ((((lists.zipWith) (toConstraint)) (l)) (r)).
Arguments unifyTypeLists {t0} {t1}.
Definition unifyTypes (t0 : Type) (t1 : Type) : t0 -> (list) ((prod) (Name) (t1)) -> Type_ -> Type_ -> string -> (sum) (UnificationError) (TypeSubst) :=
  fun (cx : t0) => fun (schemaTypes : (list) ((prod) (Name) (t1))) => fun (l : Type_) => fun (r : Type_) => fun (comment : string) => (((unifyTypeConstraints) (cx)) (schemaTypes)) ((cons) ((Build_TypeConstraint) (l) (r) (comment)) (nil)).
Arguments unifyTypes {t0} {t1}.

