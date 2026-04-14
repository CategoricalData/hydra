(* Functions for working with qualified names. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.lib.logic hydra.lib.sets hydra.lib.strings hydra.core hydra.packaging hydra.lib.lists hydra.lib.equality hydra.lib.maybes hydra.lib.literals hydra.util hydra.formatting hydra.context hydra.annotations hydra.constants hydra.lib.math hydra.lib.pairs hydra.lib.maps.

Definition uniqueLabel_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : (list) (string)) , forall (_ : string) , string) =>
    let uniqueLabel := bundle_ in
    fun (visited : (list) (string)) => fun (l : string) => (((logic.ifElse) (((sets.member) (l)) (visited))) (((uniqueLabel) (visited)) (((strings.cat2) (l)) ("'"%string)))) (l)).

Definition uniqueLabel : forall (_ : (list) (string)) , forall (_ : string) , string :=
  uniqueLabel_bundle.
Definition qualifyName : forall (_ : Name) , QualifiedName := fun (name : Name) => let parts := (lists.reverse) (((strings.splitOn) ("."%string)) ((fun w_ => w_) (name))) in (((logic.ifElse) (((equality.equal) ((1)%Z)) ((lists.length) (parts)))) ((Build_QualifiedName) (None) ((fun w_ => w_) (name)))) ((Build_QualifiedName) ((Some) (((strings.intercalate) ("."%string)) ((lists.reverse) ((lists.tail) (parts))))) ((lists.head) (parts))).
Definition qname : forall (_ : Namespace) , forall (_ : string) , Name := fun (ns : Namespace) => fun (name : string) => (strings.cat) ((cons) ((fun w_ => w_) (ns)) ((cons) ("."%string) ((cons) (name) (nil)))).
Definition unqualifyName : forall (_ : QualifiedName) , Name := fun (qname : QualifiedName) => let prefix := (((maybes.maybe) (""%string)) (fun (n : Namespace) => ((strings.cat2) ((fun w_ => w_) (n))) ("."%string))) ((fun r_ => (qualifiedName_namespace) (r_)) (qname)) in ((strings.cat2) (prefix)) ((fun r_ => (qualifiedName_local) (r_)) (qname)).
Definition normalTypeVariable : forall (_ : Z) , Name := fun (i : Z) => ((strings.cat2) ("t"%string)) ((literals.showInt32) (i)).
Definition namespaceToFilePath : forall (_ : CaseConvention) , forall (_ : FileExtension) , forall (_ : Namespace) , string := fun (caseConv : CaseConvention) => fun (ext : FileExtension) => fun (ns : Namespace) => let parts := ((lists.map) (((convertCase) ((CaseConvention_Camel) (tt))) (caseConv))) (((strings.splitOn) ("."%string)) ((fun w_ => w_) (ns))) in ((strings.cat2) (((strings.cat2) (((strings.intercalate) ("/"%string)) (parts))) ("."%string))) ((fun w_ => w_) (ext)).
Definition namespaceOf : forall (_ : Name) , (option) (Namespace) := fun (arg_ : Name) => (fun r_ => (qualifiedName_namespace) (r_)) ((qualifyName) (arg_)).
Definition nameToFilePath : forall (_ : CaseConvention) , forall (_ : CaseConvention) , forall (_ : FileExtension) , forall (_ : Name) , string := fun (nsConv : CaseConvention) => fun (localConv : CaseConvention) => fun (ext : FileExtension) => fun (name : Name) => let qualName := (qualifyName) (name) in let nsToFilePath := fun (ns2 : Namespace) => ((strings.intercalate) ("/"%string)) (((lists.map) (fun (part : string) => (((convertCase) ((CaseConvention_Camel) (tt))) (nsConv)) (part))) (((strings.splitOn) ("."%string)) ((fun w_ => w_) (ns2)))) in let ns := (fun r_ => (qualifiedName_namespace) (r_)) (qualName) in let prefix := (((maybes.maybe) (""%string)) (fun (n : Namespace) => ((strings.cat2) ((nsToFilePath) (n))) ("/"%string))) (ns) in let local := (fun r_ => (qualifiedName_local) (r_)) (qualName) in let suffix := (((convertCase) ((CaseConvention_Pascal) (tt))) (localConv)) (local) in (strings.cat) ((cons) (prefix) ((cons) (suffix) ((cons) ("."%string) ((cons) ((fun w_ => w_) (ext)) (nil))))).
Definition localNameOf : forall (_ : Name) , string := fun (arg_ : Name) => (fun r_ => (qualifiedName_local) (r_)) ((qualifyName) (arg_)).
Definition freshName : forall (_ : Context_) , (prod) (Name) (Context_) := fun (cx : Context_) => let count := ((getCount) (key_freshTypeVariableCount)) (cx) in (pair) ((normalTypeVariable) (count)) ((((putCount) (key_freshTypeVariableCount)) (((math.add) (count)) ((1)%Z))) (cx)).
Definition freshNames : forall (_ : Z) , forall (_ : Context_) , (prod) ((list) (Name)) (Context_) := fun (n : Z) => fun (cx : Context_) => let go := fun (acc : (prod) ((list) (Name)) (Context_)) => fun _ => let names := (pairs.first) (acc) in let cx0 := (pairs.second) (acc) in let result := (freshName) (cx0) in let cx1 := (pairs.second) (result) in let name := (pairs.first) (result) in (pair) (((lists.concat2) (names)) ((lists.pure) (name))) (cx1) in (((lists.foldl) (go)) ((pair) (nil) (cx))) (((lists.replicate) (n)) (tt)).
Definition compactName : forall (_ : (list) ((prod) (Namespace) (string))) , forall (_ : Name) , string := fun (namespaces : (list) ((prod) (Namespace) (string))) => fun (name : Name) => let qualName := (qualifyName) (name) in let mns := (fun r_ => (qualifiedName_namespace) (r_)) (qualName) in let local := (fun r_ => (qualifiedName_local) (r_)) (qualName) in (((maybes.maybe) ((fun w_ => w_) (name))) (fun (ns : Namespace) => (((maybes.maybe) (local)) (fun (pre : string) => (strings.cat) ((cons) (pre) ((cons) (":"%string) ((cons) (local) (nil)))))) (((maps.lookup) (ns)) (namespaces)))) (mns).

