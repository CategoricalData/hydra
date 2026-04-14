(* Term encoders for hydra.relational *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.relational hydra.lib.lists hydra.lib.maps hydra.lib.sets.

Definition row (t0 : Type) : forall (_ : forall (_ : t0) , Term) , forall (_ : (Row) (t0)) , Term := fun (v : forall (_ : t0) , Term) => fun (x : (Row) (t0)) => (Term_Wrap) ((Build_WrappedTerm) ("Row"%string) ((fun (xs : (list) (t0)) => (Term_List) (((lists.map) (v)) (xs))) ((fun w_ => w_) (x)))).
Arguments row {t0}.
Definition relationName : forall (_ : RelationName) , Term := fun (x : RelationName) => (Term_Wrap) ((Build_WrappedTerm) ("RelationName"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun w_ => w_) (x)))).
Definition relation (t0 : Type) : forall (_ : forall (_ : t0) , Term) , forall (_ : (Relation) (t0)) , Term := fun (v : forall (_ : t0) , Term) => fun (x : (Relation) (t0)) => (Term_Wrap) ((Build_WrappedTerm) ("Relation"%string) ((fun (xs : (list) ((Row) (t0))) => (Term_List) (((lists.map) ((row) (v))) (xs))) ((fun w_ => w_) (x)))).
Arguments relation {t0}.
Definition columnName : forall (_ : ColumnName) , Term := fun (x : ColumnName) => (Term_Wrap) ((Build_WrappedTerm) ("ColumnName"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun w_ => w_) (x)))).
Definition columnSchema (t0 : Type) : forall (_ : forall (_ : t0) , Term) , forall (_ : (ColumnSchema) (t0)) , Term := fun (t : forall (_ : t0) , Term) => fun (x : (ColumnSchema) (t0)) => (Term_Record) ((Build_Record_) ("ColumnSchema"%string) ((cons) ((Build_Field) ("name"%string) ((columnName) ((fun r_ => (columnSchema_name) (r_)) (x)))) ((cons) ((Build_Field) ("domain"%string) ((t) ((fun r_ => (columnSchema_domain) (r_)) (x)))) (nil)))).
Arguments columnSchema {t0}.
Definition foreignKey : forall (_ : ForeignKey) , Term := fun (x : ForeignKey) => (Term_Record) ((Build_Record_) ("ForeignKey"%string) ((cons) ((Build_Field) ("foreignRelation"%string) ((relationName) ((fun r_ => (foreignKey_foreignRelation) (r_)) (x)))) ((cons) ((Build_Field) ("keys"%string) ((fun (m : (list) ((prod) (ColumnName) (ColumnName))) => (Term_Map) ((((maps.bimap) (columnName)) (columnName)) (m))) ((fun r_ => (foreignKey_keys) (r_)) (x)))) (nil)))).
Definition primaryKey : forall (_ : PrimaryKey) , Term := fun (x : PrimaryKey) => (Term_Wrap) ((Build_WrappedTerm) ("PrimaryKey"%string) ((fun (xs : (list) (ColumnName)) => (Term_List) (((lists.map) (columnName)) (xs))) ((fun w_ => w_) (x)))).
Definition relationSchema (t0 : Type) : forall (_ : forall (_ : t0) , Term) , forall (_ : (RelationSchema) (t0)) , Term := fun (t : forall (_ : t0) , Term) => fun (x : (RelationSchema) (t0)) => (Term_Record) ((Build_Record_) ("RelationSchema"%string) ((cons) ((Build_Field) ("name"%string) ((relationName) ((fun r_ => (relationSchema_name) (r_)) (x)))) ((cons) ((Build_Field) ("columns"%string) ((fun (xs : (list) ((ColumnSchema) (t0))) => (Term_List) (((lists.map) ((columnSchema) (t))) (xs))) ((fun r_ => (relationSchema_columns) (r_)) (x)))) ((cons) ((Build_Field) ("primaryKeys"%string) ((fun (xs : (list) (PrimaryKey)) => (Term_List) (((lists.map) (primaryKey)) (xs))) ((fun r_ => (relationSchema_primaryKeys) (r_)) (x)))) ((cons) ((Build_Field) ("foreignKeys"%string) ((fun (xs : (list) (ForeignKey)) => (Term_List) (((lists.map) (foreignKey)) (xs))) ((fun r_ => (relationSchema_foreignKeys) (r_)) (x)))) (nil)))))).
Arguments relationSchema {t0}.
Definition relationship (t0 : Type) : forall (_ : forall (_ : t0) , Term) , forall (_ : (Relationship) (t0)) , Term := fun (v : forall (_ : t0) , Term) => fun (x : (Relationship) (t0)) => (Term_Wrap) ((Build_WrappedTerm) ("Relationship"%string) ((fun (s : (list) ((list) ((prod) (ColumnName) (t0)))) => (Term_Set) (((sets.map) (fun (m : (list) ((prod) (ColumnName) (t0))) => (Term_Map) ((((maps.bimap) (columnName)) (v)) (m)))) (s))) ((fun w_ => w_) (x)))).
Arguments relationship {t0}.

