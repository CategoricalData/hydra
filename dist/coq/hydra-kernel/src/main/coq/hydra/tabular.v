(* A simple, untyped tabular data model, suitable for CSVs and TSVs *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.relational hydra.core.
Definition HeaderRow : Type := (list) (string).

Definition DataRow (v : Type) : Type := (list) ((option) (v)).

Record Table (v : Type) : Type := Build_Table {
table_header : (option) (HeaderRow) ;
table_data : (list) ((DataRow) (v)) ;
}.

Record ColumnType : Type := Build_ColumnType {
columnType_name : ColumnName ;
columnType_type : Type_ ;
}.

Record TableType : Type := Build_TableType {
tableType_name : RelationName ;
tableType_columns : (list) (ColumnType) ;
}.

Arguments Build_Table {v}.
Arguments table_header {v}.
Arguments table_data {v}.

