(* Term decoders for hydra.tabular *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.graph hydra.core hydra.errors hydra.tabular hydra.lib.eithers hydra.extract.core hydra.decode.relational hydra.relational hydra.decode.core.

Definition headerRow : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (HeaderRow) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : (list) (string)) => b)) ((((decodeList) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => (inl) ("expected wrapped type"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition dataRow (t0 : Type) : forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((DataRow) (t0)) := fun (v : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) => fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : (list) ((option) (t0))) => b)) ((((decodeList) ((decodeMaybe) (v))) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => (inl) ("expected wrapped type"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Arguments dataRow {t0}.
Definition table (t0 : Type) : forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((Table) (t0)) := fun (v : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) => fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("header"%string)) ((decodeMaybe) (headerRow))) (fieldMap)) (cx))) (fun (field_header : (option) (HeaderRow)) => ((eithers.bind) (((((requireField) ("data"%string)) ((decodeList) ((dataRow) (v)))) (fieldMap)) (cx))) (fun (field_data : (list) ((DataRow) (t0))) => (inr) ((Build_Table) (field_header) (field_data))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Arguments table {t0}.
Definition columnType : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (ColumnType) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.relational.columnName)) (fieldMap)) (cx))) (fun (field_name : ColumnName) => ((eithers.bind) (((((requireField) ("type"%string)) (hydra.decode.core.type)) (fieldMap)) (cx))) (fun (field_type : Type_) => (inr) ((Build_ColumnType) (field_name) (field_type))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition tableType : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TableType) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.relational.relationName)) (fieldMap)) (cx))) (fun (field_name : RelationName) => ((eithers.bind) (((((requireField) ("columns"%string)) ((decodeList) (columnType))) (fieldMap)) (cx))) (fun (field_columns : (list) (ColumnType)) => (inr) ((Build_TableType) (field_name) (field_columns))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).

