(* A model for Hydra namespaces, modules, and packages *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.graph.
Record TypeDefinition : Type := Build_TypeDefinition {
typeDefinition_name : Name ;
typeDefinition_type : TypeScheme ;
}.

Record TermDefinition : Type := Build_TermDefinition {
termDefinition_name : Name ;
termDefinition_term : Term ;
termDefinition_type : (option) (TypeScheme) ;
}.

Definition PackageName : Type := string.

Definition Namespace : Type := string.

Record Namespaces (n : Type) : Type := Build_Namespaces {
namespaces_focus : (prod) (Namespace) (n) ;
namespaces_mapping : (list) ((prod) (Namespace) (n)) ;
}.

Record QualifiedName : Type := Build_QualifiedName {
qualifiedName_namespace : (option) (Namespace) ;
qualifiedName_local : string ;
}.

Record Library : Type := Build_Library {
library_namespace : Namespace ;
library_prefix : string ;
library_primitives : (list) (Primitive) ;
}.

Definition FileExtension : Type := string.

Inductive Definition_ : Type :=
| Definition__Term : forall (_ : TermDefinition) , Definition_
| Definition__Type : forall (_ : TypeDefinition) , Definition_.

Record Module_ : Type := Build_Module_ {
module__namespace : Namespace ;
module__definitions : (list) (Definition_) ;
module__termDependencies : (list) (Namespace) ;
module__typeDependencies : (list) (Namespace) ;
module__description : (option) (string) ;
}.

Record Package : Type := Build_Package {
package_name : PackageName ;
package_modules : (list) (Module_) ;
package_dependencies : (list) (PackageName) ;
package_description : (option) (string) ;
}.

Arguments Build_Namespaces {n}.
Arguments namespaces_focus {n}.
Arguments namespaces_mapping {n}.

