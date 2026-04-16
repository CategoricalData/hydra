(* An interpretation of Codd's Relational Model, as described in 'A Relational Model of Data for Large Shared Data Banks' (1970). Types ('domains') and values are parameterized so as to allow for application-specific implementations. No special support is provided for 'nonsimple' domains; i.e. relations are assumed to be normalized. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.
Definition ColumnName : Type := string.

Record ColumnSchema (t : Type) : Type := Build_ColumnSchema {
columnSchema_name : ColumnName ;
columnSchema_domain : t ;
}.

Definition RelationName : Type := string.

Record ForeignKey : Type := Build_ForeignKey {
foreignKey_foreignRelation : RelationName ;
foreignKey_keys : (list) ((prod) (ColumnName) (ColumnName)) ;
}.

Definition PrimaryKey : Type := (list) (ColumnName).

Definition Row (v : Type) : Type := (list) (v).

Definition Relation (v : Type) : Type := (list) ((Row) (v)).

Record RelationSchema (t : Type) : Type := Build_RelationSchema {
relationSchema_name : RelationName ;
relationSchema_columns : (list) ((ColumnSchema) (t)) ;
relationSchema_primaryKeys : (list) (PrimaryKey) ;
relationSchema_foreignKeys : (list) (ForeignKey) ;
}.

Definition Relationship (v : Type) : Type := (list) ((list) ((prod) (ColumnName) (v))).

Arguments Build_ColumnSchema {t}.
Arguments columnSchema_name {t}.
Arguments columnSchema_domain {t}.
Arguments Build_RelationSchema {t}.
Arguments relationSchema_name {t}.
Arguments relationSchema_columns {t}.
Arguments relationSchema_primaryKeys {t}.
Arguments relationSchema_foreignKeys {t}.

