(* Term encoders for hydra.tabular *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.encode.core hydra.encode.relational hydra.lib.lists hydra.lib.maybes hydra.tabular.

Definition columnType : forall (_ : ColumnType) , Term := fun (x : ColumnType) => (Term_Record) ((Build_Record_) ("hydra.tabular.ColumnType"%string) ((cons) ((Build_Field) ("name"%string) ((hydra.encode.relational.columnName) ((fun r_ => (columnType_name) (r_)) (x)))) ((cons) ((Build_Field) ("type"%string) ((hydra.encode.core.type) ((fun r_ => (columnType_type) (r_)) (x)))) (nil)))).
Definition dataRow (t0 : Type) : forall (_ : forall (_ : t0) , Term) , forall (_ : (DataRow) (t0)) , Term := fun (v : forall (_ : t0) , Term) => fun (x : (DataRow) (t0)) => (Term_Wrap) ((Build_WrappedTerm) ("hydra.tabular.DataRow"%string) ((fun (xs : (list) ((option) (t0))) => (Term_List) (((lists.map) (fun (opt : (option) (t0)) => (Term_Maybe) (((maybes.map) (v)) (opt)))) (xs))) ((fun w_ => w_) (x)))).
Arguments dataRow {t0}.
Definition headerRow : forall (_ : HeaderRow) , Term := fun (x : HeaderRow) => (Term_Wrap) ((Build_WrappedTerm) ("hydra.tabular.HeaderRow"%string) ((fun (xs : (list) (string)) => (Term_List) (((lists.map) (fun (x2 : string) => (Term_Literal) ((Literal_String) (x2)))) (xs))) ((fun w_ => w_) (x)))).
Definition table (t0 : Type) : forall (_ : forall (_ : t0) , Term) , forall (_ : (Table) (t0)) , Term := fun (v : forall (_ : t0) , Term) => fun (x : (Table) (t0)) => (Term_Record) ((Build_Record_) ("hydra.tabular.Table"%string) ((cons) ((Build_Field) ("header"%string) ((fun (opt : (option) (HeaderRow)) => (Term_Maybe) (((maybes.map) (headerRow)) (opt))) ((fun r_ => (table_header) (r_)) (x)))) ((cons) ((Build_Field) ("data"%string) ((fun (xs : (list) ((DataRow) (t0))) => (Term_List) (((lists.map) ((dataRow) (v))) (xs))) ((fun r_ => (table_data) (r_)) (x)))) (nil)))).
Arguments table {t0}.
Definition tableType : forall (_ : TableType) , Term := fun (x : TableType) => (Term_Record) ((Build_Record_) ("hydra.tabular.TableType"%string) ((cons) ((Build_Field) ("name"%string) ((hydra.encode.relational.relationName) ((fun r_ => (tableType_name) (r_)) (x)))) ((cons) ((Build_Field) ("columns"%string) ((fun (xs : (list) (ColumnType)) => (Term_List) (((lists.map) (columnType)) (xs))) ((fun r_ => (tableType_columns) (r_)) (x)))) (nil)))).

