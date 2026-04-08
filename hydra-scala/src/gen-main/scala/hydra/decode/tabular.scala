package hydra.decode.tabular

import hydra.core.*

import hydra.errors.*

import hydra.tabular.*

def columnType(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.tabular.ColumnType] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.tabular.ColumnType]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.relational.ColumnName, hydra.tabular.ColumnType](hydra.extract.core.requireField("name")(hydra.decode.relational.columnName)(fieldMap)(cx))((field_name: hydra.relational.ColumnName) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.tabular.ColumnType](hydra.extract.core.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) => Right(hydra.tabular.ColumnType(field_name,
         field_type))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def dataRow[T0](v: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.tabular.DataRow[T0]] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.tabular.DataRow[T0]]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[Seq[Option[T0]], hydra.tabular.DataRow[T0],
     hydra.errors.DecodingError]((b: Seq[Option[T0]]) => b)(hydra.extract.core.decodeList((v1: hydra.graph.Graph) =>
    (v2: hydra.core.Term) => hydra.extract.core.decodeMaybe(v)(v1)(v2))(cx)(v_Term_wrap_wrappedTerm.body))
  case _ => Left("expected wrapped type"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def headerRow(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.tabular.HeaderRow] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.tabular.HeaderRow]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[Seq[scala.Predef.String],
     hydra.tabular.HeaderRow, hydra.errors.DecodingError]((b: Seq[scala.Predef.String]) => b)(hydra.extract.core.decodeList((cx2: hydra.graph.Graph) =>
    (raw2: hydra.core.Term) =>
    hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
       scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(cx)(v_Term_wrap_wrappedTerm.body))
  case _ => Left("expected wrapped type"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def table[T0](v: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.tabular.Table[T0]] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.tabular.Table[T0]]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Option[hydra.tabular.HeaderRow], hydra.tabular.Table[T0]](hydra.extract.core.requireField("header")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeMaybe(hydra.decode.tabular.headerRow)(v1)(v2))(fieldMap)(cx))((field_header: Option[hydra.tabular.HeaderRow]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.tabular.DataRow[T0]], hydra.tabular.Table[T0]](hydra.extract.core.requireField("data")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList((v12: hydra.graph.Graph) =>
      (v22: hydra.core.Term) => hydra.decode.tabular.dataRow(v)(v12)(v22))(v1)(v2))(fieldMap)(cx))((field_data: Seq[hydra.tabular.DataRow[T0]]) => Right(hydra.tabular.Table(field_header,
         field_data))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def tableType(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.tabular.TableType] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.tabular.TableType]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.relational.RelationName, hydra.tabular.TableType](hydra.extract.core.requireField("name")(hydra.decode.relational.relationName)(fieldMap)(cx))((field_name: hydra.relational.RelationName) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.tabular.ColumnType], hydra.tabular.TableType](hydra.extract.core.requireField("columns")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.tabular.columnType)(v1)(v2))(fieldMap)(cx))((field_columns: Seq[hydra.tabular.ColumnType]) => Right(hydra.tabular.TableType(field_name,
         field_columns))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))
