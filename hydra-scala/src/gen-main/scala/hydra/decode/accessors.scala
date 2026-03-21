package hydra.decode.accessors

import hydra.accessors.*

import hydra.core.*

import hydra.error.*

import hydra.lib.eithers

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.strings

def accessorEdge(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.accessors.AccessorEdge] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.accessors.AccessorEdge]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.accessors.AccessorNode, hydra.accessors.AccessorEdge](hydra.extract.helpers.requireField("source")(hydra.decode.accessors.accessorNode)(fieldMap)(cx))((field_source: hydra.accessors.AccessorNode) =>
      eithers.bind[hydra.error.DecodingError, hydra.accessors.AccessorPath, hydra.accessors.AccessorEdge](hydra.extract.helpers.requireField("path")(hydra.decode.accessors.accessorPath)(fieldMap)(cx))((field_path: hydra.accessors.AccessorPath) =>
      eithers.bind[hydra.error.DecodingError, hydra.accessors.AccessorNode, hydra.accessors.AccessorEdge](hydra.extract.helpers.requireField("target")(hydra.decode.accessors.accessorNode)(fieldMap)(cx))((field_target: hydra.accessors.AccessorNode) =>
      Right(hydra.accessors.AccessorEdge(field_source, field_path, field_target)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def accessorGraph(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.accessors.AccessorGraph] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.accessors.AccessorGraph]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, Seq[hydra.accessors.AccessorNode], hydra.accessors.AccessorGraph](hydra.extract.helpers.requireField("nodes")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.accessors.accessorNode)(v1)(v2))(fieldMap)(cx))((field_nodes: Seq[hydra.accessors.AccessorNode]) =>
      eithers.bind[hydra.error.DecodingError, Seq[hydra.accessors.AccessorEdge], hydra.accessors.AccessorGraph](hydra.extract.helpers.requireField("edges")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.accessors.accessorEdge)(v1)(v2))(fieldMap)(cx))((field_edges: Seq[hydra.accessors.AccessorEdge]) =>
      Right(hydra.accessors.AccessorGraph(field_nodes, field_edges))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def accessorNode(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.accessors.AccessorNode] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.accessors.AccessorNode]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Name, hydra.accessors.AccessorNode](hydra.extract.helpers.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      eithers.bind[hydra.error.DecodingError, scala.Predef.String, hydra.accessors.AccessorNode](hydra.extract.helpers.requireField("label")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_label: scala.Predef.String) =>
      eithers.bind[hydra.error.DecodingError, scala.Predef.String, hydra.accessors.AccessorNode](hydra.extract.helpers.requireField("id")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_id: scala.Predef.String) =>
      Right(hydra.accessors.AccessorNode(field_name, field_label, field_id)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def accessorPath(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.accessors.AccessorPath] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.accessors.AccessorPath]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => eithers.map[Seq[hydra.accessors.TermAccessor],
     hydra.accessors.AccessorPath, hydra.error.DecodingError]((b: Seq[hydra.accessors.TermAccessor]) => b)(hydra.extract.helpers.decodeList(hydra.decode.accessors.termAccessor)(cx)(v_Term_wrap_wrappedTerm.body))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def termAccessor(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.accessors.TermAccessor] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.accessors.TermAccessor]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.accessors.TermAccessor])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.accessors.TermAccessor]](Seq(Tuple2("annotatedBody",
       (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: Unit) => hydra.accessors.TermAccessor.annotatedBody)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("applicationFunction", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: Unit) => hydra.accessors.TermAccessor.applicationFunction)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("applicationArgument", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: Unit) => hydra.accessors.TermAccessor.applicationArgument)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("lambdaBody", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: Unit) => hydra.accessors.TermAccessor.lambdaBody)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("unionCasesDefault", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: Unit) => hydra.accessors.TermAccessor.unionCasesDefault)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("unionCasesBranch", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Name, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: hydra.core.Name) => hydra.accessors.TermAccessor.unionCasesBranch(t))(hydra.decode.core.name(cx)(input))),
         Tuple2("letBody", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: Unit) => hydra.accessors.TermAccessor.letBody)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("letBinding", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Name, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: hydra.core.Name) => hydra.accessors.TermAccessor.letBinding(t))(hydra.decode.core.name(cx)(input))),
         Tuple2("listElement", (input: hydra.core.Term) =>
      eithers.map[Int, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: Int) => hydra.accessors.TermAccessor.listElement(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("mapKey", (input: hydra.core.Term) =>
      eithers.map[Int, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: Int) => hydra.accessors.TermAccessor.mapKey(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("mapValue", (input: hydra.core.Term) =>
      eithers.map[Int, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: Int) => hydra.accessors.TermAccessor.mapValue(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("maybeTerm", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: Unit) => hydra.accessors.TermAccessor.maybeTerm)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("productTerm", (input: hydra.core.Term) =>
      eithers.map[Int, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: Int) => hydra.accessors.TermAccessor.productTerm(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("recordField", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Name, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: hydra.core.Name) => hydra.accessors.TermAccessor.recordField(t))(hydra.decode.core.name(cx)(input))),
         Tuple2("setElement", (input: hydra.core.Term) =>
      eithers.map[Int, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: Int) => hydra.accessors.TermAccessor.setElement(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("sumTerm", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: Unit) => hydra.accessors.TermAccessor.sumTerm)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("typeLambdaBody", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: Unit) => hydra.accessors.TermAccessor.typeLambdaBody)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("typeApplicationTerm", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: Unit) => hydra.accessors.TermAccessor.typeApplicationTerm)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("injectionTerm", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: Unit) => hydra.accessors.TermAccessor.injectionTerm)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("wrappedTerm", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.accessors.TermAccessor, hydra.error.DecodingError]((t: Unit) => hydra.accessors.TermAccessor.wrappedTerm)(hydra.extract.helpers.decodeUnit(cx)(input)))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.accessors.TermAccessor], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.accessors.TermAccessor]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.accessors.TermAccessor])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.accessors.TermAccessor]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
