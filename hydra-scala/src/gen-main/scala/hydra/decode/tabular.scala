package hydra.decode.tabular

import hydra.core.*

import hydra.errors.*

import hydra.tabular.*

import hydra.lib.eithers

def columnType(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.tabular.ColumnType] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.tabular.ColumnType]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.relational.ColumnName, hydra.tabular.ColumnType](hydra.extract.helpers.requireField("name")(hydra.decode.relational.columnName)(fieldMap)(cx))((field_name: hydra.relational.ColumnName) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, hydra.tabular.ColumnType](hydra.extract.helpers.requireField("type")(hydra.decode.core.`type`)(fieldMap)(cx))((field_type: hydra.core.Type) => Right(hydra.tabular.ColumnType(field_name,
         field_type))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def dataRow[T0](v: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.tabular.DataRow[T0]] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.tabular.DataRow[T0]]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[Seq[Option[T0]], hydra.tabular.DataRow[T0],
     hydra.errors.DecodingError]((b: Seq[Option[T0]]) => b)(hydra.extract.helpers.decodeList((v1: hydra.graph.Graph) =>
    (v2: hydra.core.Term) => hydra.extract.helpers.decodeMaybe(v)(v1)(v2))(cx)(v_Term_wrap_wrappedTerm.body))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def headerRow(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.tabular.HeaderRow] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.tabular.HeaderRow]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[Seq[scala.Predef.String],
     hydra.tabular.HeaderRow, hydra.errors.DecodingError]((b: Seq[scala.Predef.String]) => b)(hydra.extract.helpers.decodeList((cx2: hydra.graph.Graph) =>
    (raw2: hydra.core.Term) =>
    hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError,
       scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(cx)(v_Term_wrap_wrappedTerm.body))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def table[T0](v: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError, T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.tabular.Table[T0]] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.tabular.Table[T0]]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Option[hydra.tabular.HeaderRow], hydra.tabular.Table[T0]](hydra.extract.helpers.requireField("header")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMaybe(hydra.decode.tabular.headerRow)(v1)(v2))(fieldMap)(cx))((field_header: Option[hydra.tabular.HeaderRow]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.tabular.DataRow[T0]], hydra.tabular.Table[T0]](hydra.extract.helpers.requireField("data")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList((v12: hydra.graph.Graph) =>
      (v22: hydra.core.Term) => hydra.decode.tabular.dataRow(v)(v12)(v22))(v1)(v2))(fieldMap)(cx))((field_data: Seq[hydra.tabular.DataRow[T0]]) => Right(hydra.tabular.Table(field_header,
         field_data))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def tableType(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.tabular.TableType] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.tabular.TableType]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.relational.RelationName, hydra.tabular.TableType](hydra.extract.helpers.requireField("name")(hydra.decode.relational.relationName)(fieldMap)(cx))((field_name: hydra.relational.RelationName) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.tabular.ColumnType], hydra.tabular.TableType](hydra.extract.helpers.requireField("columns")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.tabular.columnType)(v1)(v2))(fieldMap)(cx))((field_columns: Seq[hydra.tabular.ColumnType]) => Right(hydra.tabular.TableType(field_name,
         field_columns))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
