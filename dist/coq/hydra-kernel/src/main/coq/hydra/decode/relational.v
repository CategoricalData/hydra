(* Term decoders for hydra.relational *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.errors hydra.extract.core hydra.graph hydra.lib.eithers hydra.relational.

Definition columnName : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (ColumnName) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (ColumnName))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : string) => b)) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (string))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (DecodingError) (string)) (v_)
| _ => ((inl) ("expected string literal"%string)) : (sum) (DecodingError) (string)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => ((inl) ("expected wrapped type"%string)) : (sum) (DecodingError) (ColumnName)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition columnSchema (t0 : Type) : forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((ColumnSchema) (t0)) := fun (t : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) => fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (columnName)) (fieldMap)) (cx))) (fun (field_name : ColumnName) => ((eithers.bind) (((((requireField) ("domain"%string)) (t)) (fieldMap)) (cx))) (fun (field_domain : t0) => (inr) ((Build_ColumnSchema) (field_name) (field_domain))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Arguments columnSchema {t0}.
Definition relationName : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (RelationName) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (RelationName))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : string) => b)) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (string))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (DecodingError) (string)) (v_)
| _ => ((inl) ("expected string literal"%string)) : (sum) (DecodingError) (string)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => ((inl) ("expected wrapped type"%string)) : (sum) (DecodingError) (RelationName)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition foreignKey : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (ForeignKey) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (ForeignKey))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("foreignRelation"%string)) (relationName)) (fieldMap)) (cx))) (fun (field_foreignRelation : RelationName) => ((eithers.bind) (((((requireField) ("keys"%string)) (((decodeMap) (columnName)) (columnName))) (fieldMap)) (cx))) (fun (field_keys : (list) ((prod) (ColumnName) (ColumnName))) => ((inr) ((Build_ForeignKey) (field_foreignRelation) (field_keys))) : (sum) (DecodingError) (ForeignKey)))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (ForeignKey)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition primaryKey : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (PrimaryKey) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (PrimaryKey))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : (list) (ColumnName)) => b)) ((((decodeList) (columnName)) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => ((inl) ("expected wrapped type"%string)) : (sum) (DecodingError) (PrimaryKey)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition row (t0 : Type) : forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((Row) (t0)) := fun (v : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) => fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : (list) (t0)) => b)) ((((decodeList) (v)) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => (inl) ("expected wrapped type"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Arguments row {t0}.
Definition relation (t0 : Type) : forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((Relation) (t0)) := fun (v : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) => fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : (list) ((Row) (t0))) => b)) ((((decodeList) ((row) (v))) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => (inl) ("expected wrapped type"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Arguments relation {t0}.
Definition relationSchema (t0 : Type) : forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((RelationSchema) (t0)) := fun (t : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) => fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (relationName)) (fieldMap)) (cx))) (fun (field_name : RelationName) => ((eithers.bind) (((((requireField) ("columns"%string)) ((decodeList) ((columnSchema) (t)))) (fieldMap)) (cx))) (fun (field_columns : (list) ((ColumnSchema) (t0))) => ((eithers.bind) (((((requireField) ("primaryKeys"%string)) ((decodeList) (primaryKey))) (fieldMap)) (cx))) (fun (field_primaryKeys : (list) (PrimaryKey)) => ((eithers.bind) (((((requireField) ("foreignKeys"%string)) ((decodeList) (foreignKey))) (fieldMap)) (cx))) (fun (field_foreignKeys : (list) (ForeignKey)) => (inr) ((Build_RelationSchema) (field_name) (field_columns) (field_primaryKeys) (field_foreignKeys))))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Arguments relationSchema {t0}.
Definition relationship (t0 : Type) : forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((Relationship) (t0)) := fun (v : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) => fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : (list) ((list) ((prod) (ColumnName) (t0)))) => b)) ((((decodeSet) (((decodeMap) (columnName)) (v))) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => (inl) ("expected wrapped type"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Arguments relationship {t0}.

