(* Free variable analysis, term-level substitution, and unshadowing *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.lib.strings hydra.lib.literals hydra.lib.logic hydra.lib.maps hydra.lib.math hydra.lib.lists hydra.lib.maybes hydra.rewriting hydra.lib.equality hydra.lib.pairs hydra.lib.sets hydra.coders.

Definition unshadowVariables : Term -> Term :=
  fun (term0 : Term) => let freshName := (hydra_fix) (fun freshName => fun (base : Name) => fun (i : Z) => fun m => let candidate := ((strings.cat2) ((fun w_ => w_) (base))) ((literals.showInt32) (i)) in (((logic.ifElse) (((maps.member) (candidate)) (m))) ((((freshName) (base)) (((math.add) (i)) ((1)%Z))) (m))) (candidate)) in let f := (hydra_fix) (fun f => fun (recurse : (list) ((prod) (Name) (Name)) -> Term -> Term) => fun (m : (list) ((prod) (Name) (Name))) => fun (term_ : Term) => (fun x_ => match x_ with
| Term_Function v_ => (fun (fn : Function) => (fun x_ => match x_ with
| Function_Lambda v_ => (fun (l : Lambda) => let v := (fun r_ => (lambda_parameter) (r_)) (l) in let domain := (fun r_ => (lambda_domain) (r_)) (l) in let body := (fun r_ => (lambda_body) (r_)) (l) in (((logic.ifElse) (((maps.member) (v)) (m))) (let v2 := (((freshName) (v)) ((2)%Z)) (m) in let m2 := (((maps.insert) (v)) (v2)) ((((maps.insert) (v2)) (v2)) (m)) in (Term_Function) ((Function_Lambda) ((Build_Lambda) (v2) (domain) ((((f) (recurse)) (m2)) (body)))))) ((Term_Function) ((Function_Lambda) ((Build_Lambda) (v) (domain) ((((f) (recurse)) ((((maps.insert) (v)) (v)) (m))) (body)))))) (v_)
| _ => ((recurse) (m)) (term_)
end) (fn)) (v_)
| Term_Let v_ => (fun (lt : Let) => let m2 := (((lists.foldl) (fun (acc : (list) ((prod) (Name) (Name))) => fun (b : Binding) => let bname := (fun r_ => (binding_name) (r_)) (b) in (((logic.ifElse) (((maps.member) (bname)) (acc))) (acc)) ((((maps.insert) (bname)) (bname)) (acc)))) (m)) ((fun r_ => (let_bindings) (r_)) (lt)) in ((recurse) (m2)) (term_)) (v_)
| Term_Variable v_ => (fun (v : Name) => (Term_Variable) ((((maybes.maybe) (v)) (fun (renamed : Name) => renamed)) (((maps.lookup) (v)) (m)))) (v_)
| _ => ((recurse) (m)) (term_)
end) (term_)) in (((rewriteTermWithContext) (f)) (maps.empty)) (term0).
Definition substituteVariables : (list) ((prod) (Name) (Name)) -> Term -> Term :=
  fun (subst : (list) ((prod) (Name) (Name))) => fun (term_ : Term) => let replace := fun (recurse : Term -> Term) => fun (term2 : Term) => (fun x_ => match x_ with
| Term_Variable v_ => (fun (n : Name) => (Term_Variable) (((maybes.fromMaybe) (n)) (((maps.lookup) (n)) (subst)))) (v_)
| Term_Function v_ => (fun x_ => match x_ with
| Function_Lambda v_ => (fun (l : Lambda) => (((maybes.maybe) ((recurse) (term2))) (fun (_ : Name) => term2)) (((maps.lookup) ((fun r_ => (lambda_parameter) (r_)) (l))) (subst))) (v_)
| _ => (recurse) (term2)
end) (v_)
| _ => (recurse) (term2)
end) (term2) in ((rewriteTerm) (replace)) (term_).
Definition substituteVariable : Name -> Name -> Term -> Term :=
  fun (from : Name) => fun (to : Name) => fun (term_ : Term) => let replace := fun (recurse : Term -> Term) => fun (term2 : Term) => (fun x_ => match x_ with
| Term_Variable v_ => (fun (x : Name) => (Term_Variable) ((((logic.ifElse) (((equality.equal) (x)) (from))) (to)) (x))) (v_)
| Term_Function v_ => (fun x_ => match x_ with
| Function_Lambda v_ => (fun (l : Lambda) => (((logic.ifElse) (((equality.equal) ((fun r_ => (lambda_parameter) (r_)) (l))) (from))) (term2)) ((recurse) (term2))) (v_)
| _ => (recurse) (term2)
end) (v_)
| _ => (recurse) (term2)
end) (term2) in ((rewriteTerm) (replace)) (term_).
Definition substituteTypeVariables : (list) ((prod) (Name) (Name)) -> Type_ -> Type_ :=
  fun (subst : (list) ((prod) (Name) (Name))) => fun (typ : Type_) => let replace := fun (recurse : Type_ -> Type_) => fun (typ2 : Type_) => (fun x_ => match x_ with
| Type__Variable v_ => (fun (n : Name) => (Type__Variable) (((maybes.fromMaybe) (n)) (((maps.lookup) (n)) (subst)))) (v_)
| _ => (recurse) (typ2)
end) (typ2) in ((rewriteType) (replace)) (typ).
Definition substituteTypeVariablesInTerm : (list) ((prod) (Name) (Name)) -> Term -> Term :=
  fun (subst : (list) ((prod) (Name) (Name))) => fun (term_ : Term) => let st := (substituteTypeVariables) (subst) in let stOpt := fun (mt : (option) (Type_)) => ((maybes.map) (st)) (mt) in let stScheme := fun (ts : TypeScheme) => (Build_TypeScheme) ((fun r_ => (typeScheme_variables) (r_)) (ts)) ((st) ((fun r_ => (typeScheme_type) (r_)) (ts))) ((fun r_ => (typeScheme_constraints) (r_)) (ts)) in let stSchemeOpt := fun (mts : (option) (TypeScheme)) => ((maybes.map) (stScheme)) (mts) in let replace := fun (recurse : Term -> Term) => fun (t : Term) => (fun x_ => match x_ with
| Term_Function v_ => (fun x_ => match x_ with
| Function_Lambda v_ => (fun (l : Lambda) => (Term_Function) ((Function_Lambda) ((Build_Lambda) ((fun r_ => (lambda_parameter) (r_)) (l)) ((stOpt) ((fun r_ => (lambda_domain) (r_)) (l))) ((recurse) ((fun r_ => (lambda_body) (r_)) (l)))))) (v_)
| _ => (recurse) (t)
end) (v_)
| Term_Let v_ => (fun (lt : Let) => let mapBinding := fun (b : Binding) => (Build_Binding) ((fun r_ => (binding_name) (r_)) (b)) ((recurse) ((fun r_ => (binding_term) (r_)) (b))) ((stSchemeOpt) ((fun r_ => (binding_type) (r_)) (b))) in (Term_Let) ((Build_Let) (((lists.map) (mapBinding)) ((fun r_ => (let_bindings) (r_)) (lt))) ((recurse) ((fun r_ => (let_body) (r_)) (lt))))) (v_)
| Term_TypeApplication v_ => (fun (tt : TypeApplicationTerm) => (Term_TypeApplication) ((Build_TypeApplicationTerm) ((recurse) ((fun r_ => (typeApplicationTerm_body) (r_)) (tt))) ((st) ((fun r_ => (typeApplicationTerm_type) (r_)) (tt))))) (v_)
| Term_TypeLambda v_ => (fun (tl : TypeLambda) => (Term_TypeLambda) ((Build_TypeLambda) (((maybes.fromMaybe) ((fun r_ => (typeLambda_parameter) (r_)) (tl))) (((maps.lookup) ((fun r_ => (typeLambda_parameter) (r_)) (tl))) (subst))) ((recurse) ((fun r_ => (typeLambda_body) (r_)) (tl))))) (v_)
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => (Term_Annotated) ((Build_AnnotatedTerm) ((recurse) ((fun r_ => (annotatedTerm_body) (r_)) (at_))) ((fun r_ => (annotatedTerm_annotation) (r_)) (at_)))) (v_)
| _ => (recurse) (t)
end) (t) in ((rewriteTerm) (replace)) (term_).
Definition replaceFreeTypeVariable : Name -> Type_ -> Type_ -> Type_ :=
  fun (v : Name) => fun (rep : Type_) => fun (typ : Type_) => let mapExpr := fun (recurse : Type_ -> Type_) => fun (t : Type_) => (fun x_ => match x_ with
| Type__Forall v_ => (fun (ft : ForallType) => (((logic.ifElse) (((equality.equal) (v)) ((fun r_ => (forallType_parameter) (r_)) (ft)))) (t)) ((Type__Forall) ((Build_ForallType) ((fun r_ => (forallType_parameter) (r_)) (ft)) ((recurse) ((fun r_ => (forallType_body) (r_)) (ft)))))) (v_)
| Type__Variable v_ => (fun (v' : Name) => (((logic.ifElse) (((equality.equal) (v)) (v'))) (rep)) (t)) (v_)
| _ => (recurse) (t)
end) (t) in ((rewriteType) (mapExpr)) (typ).
Definition replaceFreeTermVariable : Name -> Term -> Term -> Term :=
  fun (vold : Name) => fun (tnew : Term) => fun (term_ : Term) => let rewrite := fun (recurse : Term -> Term) => fun (t : Term) => (fun x_ => match x_ with
| Term_Function v_ => (fun (f : Function) => (fun x_ => match x_ with
| Function_Lambda v_ => (fun (l : Lambda) => let v := (fun r_ => (lambda_parameter) (r_)) (l) in (((logic.ifElse) (((equality.equal) (v)) (vold))) (t)) ((recurse) (t))) (v_)
| _ => (recurse) (t)
end) (f)) (v_)
| Term_Variable v_ => (fun (v : Name) => (((logic.ifElse) (((equality.equal) (v)) (vold))) (tnew)) ((Term_Variable) (v))) (v_)
| _ => (recurse) (t)
end) (t) in ((rewriteTerm) (rewrite)) (term_).
Definition normalizeTypeVariablesInTerm : Term -> Term :=
  fun (term_ : Term) => let replaceName := fun subst => fun v => ((maybes.fromMaybe) (v)) (((maps.lookup) (v)) (subst)) in let substType := fun (subst : (list) ((prod) (Name) (Name))) => fun (typ : Type_) => let rewrite := fun (recurse : Type_ -> Type_) => fun (typ2 : Type_) => (fun x_ => match x_ with
| Type__Variable v_ => (fun (v : Name) => (Type__Variable) (((replaceName) (subst)) (v))) (v_)
| _ => (recurse) (typ2)
end) (typ2) in ((rewriteType) (rewrite)) (typ) in let rewriteWithSubst := (hydra_fix) (fun rewriteWithSubst => fun (state : (prod) ((prod) ((list) ((prod) (Name) (Name))) ((list) (Name))) (Z)) => fun (term0 : Term) => let sb := (pairs.first) (state) in let subst := (pairs.first) (sb) in let next := (pairs.second) (state) in let boundVars := (pairs.second) (sb) in let rewrite := fun (recurse : Term -> Term) => fun (term2 : Term) => (fun x_ => match x_ with
| Term_Function v_ => (fun x_ => match x_ with
| Function_Elimination v_ => (fun (_ : Elimination) => (recurse) (term2)) (v_)
| Function_Lambda v_ => (fun (l : Lambda) => let domain := (fun r_ => (lambda_domain) (r_)) (l) in (Term_Function) ((Function_Lambda) ((Build_Lambda) ((fun r_ => (lambda_parameter) (r_)) (l)) (((maybes.map) ((substType) (subst))) (domain)) (((rewriteWithSubst) ((pair) ((pair) (subst) (boundVars)) (next))) ((fun r_ => (lambda_body) (r_)) (l)))))) (v_)
end) (v_)
| Term_Let v_ => (fun (lt : Let) => let step := (hydra_fix) (fun step => fun (acc : (list) (Binding)) => fun (bs : (list) (Binding)) => (((logic.ifElse) ((lists.null) (bs))) ((lists.reverse) (acc))) (let tl := (lists.tail) (bs) in let b := (lists.head) (bs) in let noType := let newVal := ((rewriteWithSubst) ((pair) ((pair) (subst) (boundVars)) (next))) ((fun r_ => (binding_term) (r_)) (b)) in let b1 := (Build_Binding) ((fun r_ => (binding_name) (r_)) (b)) (newVal) (None) in ((step) (((lists.cons) (b1)) (acc))) (tl) in let withType := fun (ts : TypeScheme) => let vars := (fun r_ => (typeScheme_variables) (r_)) (ts) in let typ := (fun r_ => (typeScheme_type) (r_)) (ts) in let oldConstraints := (fun r_ => (typeScheme_constraints) (r_)) (ts) in let k := (lists.length) (vars) in let gen := (hydra_fix) (fun gen => fun (i : Z) => fun (rem : Z) => fun (acc2 : (list) (Name)) => let ti := ((strings.cat2) ("t"%string)) ((literals.showInt32) (((math.add) (next)) (i))) in (((logic.ifElse) (((equality.equal) (rem)) ((0)%Z))) ((lists.reverse) (acc2))) ((((gen) (((math.add) (i)) ((1)%Z))) (((math.sub) (rem)) ((1)%Z))) (((lists.cons) (ti)) (acc2)))) in let newVars := (((gen) ((0)%Z)) (k)) (nil) in let newBound := ((sets.union) (boundVars)) ((sets.fromList) (newVars)) in let newSubst := ((maps.union) ((maps.fromList) (((lists.zip) (vars)) (newVars)))) (subst) in let newVal := ((rewriteWithSubst) ((pair) ((pair) (newSubst) (newBound)) (((math.add) (next)) (k)))) ((fun r_ => (binding_term) (r_)) (b)) in let renameConstraintKeys := fun constraintMap => (maps.fromList) (((lists.map) (fun p => let oldName := (pairs.first) (p) in let newName := ((maybes.fromMaybe) (oldName)) (((maps.lookup) (oldName)) (newSubst)) in let meta := (pairs.second) (p) in (pair) (newName) (meta))) ((maps.toList) (constraintMap))) in let newConstraints := ((maybes.map) (renameConstraintKeys)) (oldConstraints) in let b1 := (Build_Binding) ((fun r_ => (binding_name) (r_)) (b)) (newVal) ((Some) ((Build_TypeScheme) (newVars) (((substType) (newSubst)) (typ)) (newConstraints))) in ((step) (((lists.cons) (b1)) (acc))) (tl) in (((maybes.maybe) (noType)) (fun (ts : TypeScheme) => (withType) (ts))) ((fun r_ => (binding_type) (r_)) (b)))) in let body0 := (fun r_ => (let_body) (r_)) (lt) in let bindings0 := (fun r_ => (let_bindings) (r_)) (lt) in let bindings1 := ((step) (nil)) (bindings0) in (Term_Let) ((Build_Let) (bindings1) (((rewriteWithSubst) ((pair) ((pair) (subst) (boundVars)) (next))) (body0)))) (v_)
| Term_TypeApplication v_ => (fun (tt : TypeApplicationTerm) => (Term_TypeApplication) ((Build_TypeApplicationTerm) (((rewriteWithSubst) ((pair) ((pair) (subst) (boundVars)) (next))) ((fun r_ => (typeApplicationTerm_body) (r_)) (tt))) (((substType) (subst)) ((fun r_ => (typeApplicationTerm_type) (r_)) (tt))))) (v_)
| Term_TypeLambda v_ => (fun (ta : TypeLambda) => (Term_TypeLambda) ((Build_TypeLambda) (((replaceName) (subst)) ((fun r_ => (typeLambda_parameter) (r_)) (ta))) (((rewriteWithSubst) ((pair) ((pair) (subst) (boundVars)) (next))) ((fun r_ => (typeLambda_body) (r_)) (ta))))) (v_)
| _ => (recurse) (term2)
end) (term2) in ((rewriteTerm) (rewrite)) (term0)) in ((rewriteWithSubst) ((pair) ((pair) (maps.empty) (sets.empty)) ((0)%Z))) (term_).
Definition freeVariablesInTypeSimple : Type_ -> (list) (Name) :=
  fun (typ : Type_) => let helper := fun (types : (list) (Name)) => fun (typ2 : Type_) => (fun x_ => match x_ with
| Type__Variable v_ => (fun (v : Name) => ((sets.insert) (v)) (types)) (v_)
| _ => types
end) (typ2) in ((((foldOverType) ((TraversalOrder_Pre) (tt))) (helper)) (sets.empty)) (typ).
Definition freeVariablesInTypeSchemeSimple : TypeScheme -> (list) (Name) :=
  fun (ts : TypeScheme) => let vars := (fun r_ => (typeScheme_variables) (r_)) (ts) in let t := (fun r_ => (typeScheme_type) (r_)) (ts) in ((sets.difference) ((freeVariablesInTypeSimple) (t))) ((sets.fromList) (vars)).
Definition freeVariablesInTypeOrdered : Type_ -> (list) (Name) :=
  fun (typ : Type_) => let collectVars := (hydra_fix) (fun collectVars => fun (boundVars : (list) (Name)) => fun (t : Type_) => (fun x_ => match x_ with
| Type__Variable v_ => (fun (v : Name) => (((logic.ifElse) (((sets.member) (v)) (boundVars))) (nil)) ((cons) (v) (nil))) (v_)
| Type__Forall v_ => (fun (ft : ForallType) => ((collectVars) (((sets.insert) ((fun r_ => (forallType_parameter) (r_)) (ft))) (boundVars))) ((fun r_ => (forallType_body) (r_)) (ft))) (v_)
| _ => (lists.concat) (((lists.map) ((collectVars) (boundVars))) ((subtypes) (t)))
end) (t)) in (lists.nub) (((collectVars) (sets.empty)) (typ)).
Definition freeVariablesInType_bundle :=
  hydra_fix (fun (bundle_ : Type_ -> (list) (Name)) =>
    let freeVariablesInType := bundle_ in
    fun (typ : Type_) => let dfltVars := (((lists.foldl) (fun (s : (list) (Name)) => fun (t : Type_) => ((sets.union) (s)) ((freeVariablesInType) (t)))) (sets.empty)) ((subtypes) (typ)) in (fun x_ => match x_ with
| Type__Forall v_ => (fun (lt : ForallType) => ((sets.delete) ((fun r_ => (forallType_parameter) (r_)) (lt))) ((freeVariablesInType) ((fun r_ => (forallType_body) (r_)) (lt)))) (v_)
| Type__Variable v_ => (fun (v : Name) => (sets.singleton) (v)) (v_)
| _ => dfltVars
end) (typ)).

Definition freeVariablesInType : Type_ -> (list) (Name) :=
  freeVariablesInType_bundle.
Definition freeVariablesInTypeScheme : TypeScheme -> (list) (Name) :=
  fun (ts : TypeScheme) => let vars := (fun r_ => (typeScheme_variables) (r_)) (ts) in let t := (fun r_ => (typeScheme_type) (r_)) (ts) in ((sets.difference) ((freeVariablesInType) (t))) ((sets.fromList) (vars)).
Definition freeVariablesInTerm_bundle :=
  hydra_fix (fun (bundle_ : Term -> (list) (Name)) =>
    let freeVariablesInTerm := bundle_ in
    fun (term_ : Term) => let dfltVars := fun _ => (((lists.foldl) (fun (s : (list) (Name)) => fun (t : Term) => ((sets.union) (s)) ((freeVariablesInTerm) (t)))) (sets.empty)) ((subterms) (term_)) in (fun x_ => match x_ with
| Term_Function v_ => (fun x_ => match x_ with
| Function_Lambda v_ => (fun (l : Lambda) => ((sets.delete) ((fun r_ => (lambda_parameter) (r_)) (l))) ((freeVariablesInTerm) ((fun r_ => (lambda_body) (r_)) (l)))) (v_)
| _ => (dfltVars) (tt)
end) (v_)
| Term_Let v_ => (fun (l : Let) => ((sets.difference) ((dfltVars) (tt))) ((sets.fromList) (((lists.map) (fun r_ => (binding_name) (r_))) ((fun r_ => (let_bindings) (r_)) (l))))) (v_)
| Term_Variable v_ => (fun (v : Name) => (sets.singleton) (v)) (v_)
| _ => (dfltVars) (tt)
end) (term_)).

Definition freeVariablesInTerm : Term -> (list) (Name) :=
  freeVariablesInTerm_bundle.
Definition isFreeVariableInTerm : Name -> Term -> bool :=
  fun (v : Name) => fun (term_ : Term) => (logic.not) (((sets.member) (v)) ((freeVariablesInTerm) (term_))).
Definition freeTypeVariablesInTerm : Term -> (list) (Name) :=
  fun (term0 : Term) => let tryType := fun (tvars : (list) (Name)) => fun (typ : Type_) => ((sets.difference) ((freeVariablesInType) (typ))) (tvars) in let allOf := fun sets => (((lists.foldl) (sets.union)) (sets.empty)) (sets) in let getAll := (hydra_fix) (fun getAll => fun (vars : (list) (Name)) => fun (term_ : Term) => let recurse := (getAll) (vars) in let dflt := (allOf) (((lists.map) (recurse)) ((subterms) (term_))) in (fun x_ => match x_ with
| Term_Function v_ => (fun (f : Function) => (fun x_ => match x_ with
| Function_Elimination v_ => (fun (e : Elimination) => dflt) (v_)
| Function_Lambda v_ => (fun (l : Lambda) => let domt := (((maybes.maybe) (sets.empty)) ((tryType) (vars))) ((fun r_ => (lambda_domain) (r_)) (l)) in ((sets.union) (domt)) ((recurse) ((fun r_ => (lambda_body) (r_)) (l)))) (v_)
end) (f)) (v_)
| Term_Let v_ => (fun (l : Let) => let forBinding := fun (b : Binding) => let newVars := (((maybes.maybe) (vars)) (fun (ts : TypeScheme) => ((sets.union) (vars)) ((sets.fromList) ((fun r_ => (typeScheme_variables) (r_)) (ts))))) ((fun r_ => (binding_type) (r_)) (b)) in ((sets.union) (((getAll) (newVars)) ((fun r_ => (binding_term) (r_)) (b)))) ((((maybes.maybe) (sets.empty)) (fun (ts : TypeScheme) => ((tryType) (newVars)) ((fun r_ => (typeScheme_type) (r_)) (ts)))) ((fun r_ => (binding_type) (r_)) (b))) in ((sets.union) ((allOf) (((lists.map) (forBinding)) ((fun r_ => (let_bindings) (r_)) (l))))) ((recurse) ((fun r_ => (let_body) (r_)) (l)))) (v_)
| Term_TypeApplication v_ => (fun (tt : TypeApplicationTerm) => ((sets.union) (((tryType) (vars)) ((fun r_ => (typeApplicationTerm_type) (r_)) (tt)))) ((recurse) ((fun r_ => (typeApplicationTerm_body) (r_)) (tt)))) (v_)
| Term_TypeLambda v_ => (fun (tl : TypeLambda) => ((sets.union) (((tryType) (vars)) ((Type__Variable) ((fun r_ => (typeLambda_parameter) (r_)) (tl))))) ((recurse) ((fun r_ => (typeLambda_body) (r_)) (tl)))) (v_)
| _ => dflt
end) (term_)) in ((getAll) (sets.empty)) (term0).

