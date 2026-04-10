(* Validation functions for modules and packages *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.packaging hydra.core hydra.error.packaging hydra.lib.lists hydra.lib.pairs hydra.lib.maybes hydra.lib.logic hydra.lib.sets hydra.lib.strings hydra.lib.equality hydra.names hydra.formatting hydra.lib.maps.

Definition definitionName : Definition_ -> Name :=
  fun (def : Definition_) => (fun x_ => match x_ with
| Definition__Term v_ => (fun (td : TermDefinition) => (fun r_ => (termDefinition_name) (r_)) (td)) (v_)
| Definition__Type v_ => (fun (td : TypeDefinition) => (fun r_ => (typeDefinition_name) (r_)) (td)) (v_)
end) (def).
Definition checkDuplicateModuleNamespaces : Package -> (option) (InvalidPackageError) :=
  fun (pkg : Package) => let result := (((lists.foldl) (fun (acc : (prod) ((list) (Namespace)) ((option) (InvalidPackageError))) => fun (mod_ : Module_) => let seen := (pairs.first) (acc) in let err := (pairs.second) (acc) in (((maybes.cases) (err)) (let ns := (fun r_ => (module__namespace) (r_)) (mod_) in (((logic.ifElse) (((sets.member) (ns)) (seen))) ((pair) (seen) ((Some) ((InvalidPackageError_DuplicateModuleNamespace) ((Build_DuplicateModuleNamespaceError) (ns)))))) ((pair) (((sets.insert) (ns)) (seen)) (None)))) (fun (_ : InvalidPackageError) => acc))) ((pair) (sets.empty) (None))) ((fun r_ => (package_modules) (r_)) (pkg)) in (pairs.second) (result).
Definition checkDuplicateDefinitionNames : Module_ -> (option) (InvalidModuleError) :=
  fun (mod_ : Module_) => let ns := (fun r_ => (module__namespace) (r_)) (mod_) in let result := (((lists.foldl) (fun (acc : (prod) ((list) (Name)) ((option) (InvalidModuleError))) => fun (def : Definition_) => let seen := (pairs.first) (acc) in let err := (pairs.second) (acc) in (((maybes.cases) (err)) (let name := (definitionName) (def) in (((logic.ifElse) (((sets.member) (name)) (seen))) ((pair) (seen) ((Some) ((InvalidModuleError_DuplicateDefinitionName) ((Build_DuplicateDefinitionNameError) (ns) (name)))))) ((pair) (((sets.insert) (name)) (seen)) (None)))) (fun (_ : InvalidModuleError) => acc))) ((pair) (sets.empty) (None))) ((fun r_ => (module__definitions) (r_)) (mod_)) in (pairs.second) (result).
Definition checkDefinitionNamespaces : Module_ -> (option) (InvalidModuleError) :=
  fun (mod_ : Module_) => let ns := (fun r_ => (module__namespace) (r_)) (mod_) in let prefix := ((strings.cat2) ((fun w_ => w_) (ns))) ("."%string) in let prefixLen := (strings.length) (prefix) in (((lists.foldl) (fun (acc : (option) (InvalidModuleError)) => fun (def : Definition_) => (((maybes.cases) (acc)) (let name := (definitionName) (def) in let nameStr := (fun w_ => w_) (name) in let namePrefix := ((lists.take) (prefixLen)) ((strings.toList) (nameStr)) in (((logic.ifElse) (((equality.equal) ((strings.fromList) (namePrefix))) (prefix))) (None)) ((Some) ((InvalidModuleError_DefinitionNotInModuleNamespace) ((Build_DefinitionNotInModuleNamespaceError) (ns) (name)))))) (fun (_ : InvalidModuleError) => acc))) (None)) ((fun r_ => (module__definitions) (r_)) (mod_)).
Definition checkConflictingVariantNames : Module_ -> (option) (InvalidModuleError) :=
  fun (mod_ : Module_) => let ns := (fun r_ => (module__namespace) (r_)) (mod_) in let defs := (fun r_ => (module__definitions) (r_)) (mod_) in let defNames := (((lists.foldl) (fun (acc : (list) (string)) => fun (def : Definition_) => ((sets.insert) ((localNameOf) ((definitionName) (def)))) (acc))) (sets.empty)) (defs) in (((lists.foldl) (fun (acc : (option) (InvalidModuleError)) => fun (def : Definition_) => (((maybes.cases) (acc)) ((fun x_ => match x_ with
| Definition__Type v_ => (fun (td : TypeDefinition) => let typeName := (fun r_ => (typeDefinition_name) (r_)) (td) in let typ := (fun r_ => (typeScheme_type) (r_)) ((fun r_ => (typeDefinition_type) (r_)) (td)) in let localTypeName := (localNameOf) (typeName) in (fun x_ => match x_ with
| Type__Union v_ => (fun (fields : (list) (FieldType)) => (((lists.foldl) (fun (innerAcc : (option) (InvalidModuleError)) => fun (field : FieldType) => (((maybes.cases) (innerAcc)) (let fieldName := (fun r_ => (fieldType_name) (r_)) (field) in let localFieldName := (localNameOf) (fieldName) in let constructorName := ((strings.cat2) ((capitalize) (localTypeName))) ((capitalize) (localFieldName)) in (((logic.ifElse) (((sets.member) (constructorName)) (defNames))) ((Some) ((InvalidModuleError_ConflictingVariantName) ((Build_ConflictingVariantNameError) (ns) (typeName) (fieldName) (constructorName))))) (None))) (fun (_ : InvalidModuleError) => innerAcc))) (None)) (fields)) (v_)
| _ => None
end) (typ)) (v_)
| _ => None
end) (def))) (fun (_ : InvalidModuleError) => acc))) (None)) (defs).
Definition module : Module_ -> (option) (InvalidModuleError) :=
  fun (mod_ : Module_) => let r1 := (checkDefinitionNamespaces) (mod_) in (((maybes.cases) (r1)) (let r2 := (checkDuplicateDefinitionNames) (mod_) in (((maybes.cases) (r2)) ((checkConflictingVariantNames) (mod_))) (fun (_ : InvalidModuleError) => r2))) (fun (_ : InvalidModuleError) => r1).
Definition checkConflictingModuleNamespaces : Package -> (option) (InvalidPackageError) :=
  fun (pkg : Package) => let result := (((lists.foldl) (fun (acc : (prod) ((list) ((prod) (string) (Namespace))) ((option) (InvalidPackageError))) => fun (mod_ : Module_) => let seen := (pairs.first) (acc) in let err := (pairs.second) (acc) in (((maybes.cases) (err)) (let ns := (fun r_ => (module__namespace) (r_)) (mod_) in let key := (strings.toLower) ((fun w_ => w_) (ns)) in let existing := ((maps.lookup) (key)) (seen) in (((maybes.cases) (existing)) ((pair) ((((maps.insert) (key)) (ns)) (seen)) (None))) (fun (first : Namespace) => (pair) (seen) ((Some) ((InvalidPackageError_ConflictingModuleNamespace) ((Build_ConflictingModuleNamespaceError) (first) (ns))))))) (fun (_ : InvalidPackageError) => acc))) ((pair) (maps.empty) (None))) ((fun r_ => (package_modules) (r_)) (pkg)) in (pairs.second) (result).
Definition package : Package -> (option) (InvalidPackageError) :=
  fun (pkg : Package) => let r1 := (checkDuplicateModuleNamespaces) (pkg) in (((maybes.cases) (r1)) (let r2 := (checkConflictingModuleNamespaces) (pkg) in (((maybes.cases) (r2)) ((((lists.foldl) (fun (acc : (option) (InvalidPackageError)) => fun (mod_ : Module_) => (((maybes.cases) (acc)) (((maybes.map) (fun (err : InvalidModuleError) => (InvalidPackageError_InvalidModule) (err))) ((module) (mod_)))) (fun (_ : InvalidPackageError) => acc))) (None)) ((fun r_ => (package_modules) (r_)) (pkg)))) (fun (_ : InvalidPackageError) => r2))) (fun (_ : InvalidPackageError) => r1).

