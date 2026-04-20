package hydra.decode.paths

import hydra.core.*

import hydra.errors.*

import hydra.paths.*

def subtermEdge(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.paths.SubtermEdge] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.paths.SubtermEdge]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.paths.SubtermNode, hydra.paths.SubtermEdge](hydra.extract.core.requireField("source")(hydra.decode.paths.subtermNode)(fieldMap)(cx))((field_source: hydra.paths.SubtermNode) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.paths.SubtermPath,
         hydra.paths.SubtermEdge](hydra.extract.core.requireField("path")(hydra.decode.paths.subtermPath)(fieldMap)(cx))((field_path: hydra.paths.SubtermPath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.paths.SubtermNode,
         hydra.paths.SubtermEdge](hydra.extract.core.requireField("target")(hydra.decode.paths.subtermNode)(fieldMap)(cx))((field_target: hydra.paths.SubtermNode) =>
      Right(hydra.paths.SubtermEdge(field_source, field_path, field_target)))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def subtermGraph(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.paths.SubtermGraph] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.paths.SubtermGraph]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.paths.SubtermNode],
       hydra.paths.SubtermGraph](hydra.extract.core.requireField("nodes")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.paths.subtermNode)(v1)(v2))(fieldMap)(cx))((field_nodes: Seq[hydra.paths.SubtermNode]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.paths.SubtermEdge],
         hydra.paths.SubtermGraph](hydra.extract.core.requireField("edges")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.paths.subtermEdge)(v1)(v2))(fieldMap)(cx))((field_edges: Seq[hydra.paths.SubtermEdge]) => Right(hydra.paths.SubtermGraph(field_nodes,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         field_edges))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def subtermNode(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.paths.SubtermNode] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.paths.SubtermNode]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.paths.SubtermNode](hydra.extract.core.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.paths.SubtermNode](hydra.extract.core.requireField("label")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_label: scala.Predef.String) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.paths.SubtermNode](hydra.extract.core.requireField("id")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_id: scala.Predef.String) =>
      Right(hydra.paths.SubtermNode(field_name, field_label, field_id)))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def subtermPath(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.paths.SubtermPath] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.paths.SubtermPath]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[Seq[hydra.paths.SubtermStep],
     hydra.paths.SubtermPath, hydra.errors.DecodingError]((b: Seq[hydra.paths.SubtermStep]) => b)(hydra.extract.core.decodeList(hydra.decode.paths.subtermStep)(cx)(v_Term_wrap_wrappedTerm.body))
  case _ => Left("expected wrapped type"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def subtermStep(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.paths.SubtermStep] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.paths.SubtermStep]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.inject(v_Term_inject_inj) => {
    lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.paths.SubtermStep])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       hydra.paths.SubtermStep]](Seq(Tuple2("annotatedBody", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtermStep.annotatedBody)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("applicationFunction", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtermStep.applicationFunction)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("applicationArgument", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtermStep.applicationArgument)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("lambdaBody", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtermStep.lambdaBody)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("unionCasesDefault", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtermStep.unionCasesDefault)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("unionCasesBranch", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.core.Name, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: hydra.core.Name) => hydra.paths.SubtermStep.unionCasesBranch(t))(hydra.decode.core.name(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("letBody", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtermStep.letBody)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("letBinding", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.core.Name, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: hydra.core.Name) => hydra.paths.SubtermStep.letBinding(t))(hydra.decode.core.name(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("listElement", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Int, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: Int) => hydra.paths.SubtermStep.listElement(t))(hydra.lib.eithers.either[hydra.errors.DecodingError,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Term, Either[hydra.errors.DecodingError, Int]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(input)))),
         Tuple2("mapKey", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Int, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: Int) => hydra.paths.SubtermStep.mapKey(t))(hydra.lib.eithers.either[hydra.errors.DecodingError,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Term, Either[hydra.errors.DecodingError, Int]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(input)))),
         Tuple2("mapValue", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Int, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: Int) => hydra.paths.SubtermStep.mapValue(t))(hydra.lib.eithers.either[hydra.errors.DecodingError,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Term, Either[hydra.errors.DecodingError, Int]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(input)))),
         Tuple2("maybeTerm", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtermStep.maybeTerm)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("productTerm", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Int, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: Int) => hydra.paths.SubtermStep.productTerm(t))(hydra.lib.eithers.either[hydra.errors.DecodingError,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Term, Either[hydra.errors.DecodingError, Int]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(input)))),
         Tuple2("recordField", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.core.Name, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: hydra.core.Name) => hydra.paths.SubtermStep.recordField(t))(hydra.decode.core.name(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("setElement", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Int, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: Int) => hydra.paths.SubtermStep.setElement(t))(hydra.lib.eithers.either[hydra.errors.DecodingError,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Term, Either[hydra.errors.DecodingError, Int]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(input)))),
         Tuple2("sumTerm", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtermStep.sumTerm)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("typeLambdaBody", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtermStep.typeLambdaBody)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("typeApplicationTerm", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtermStep.typeApplicationTerm)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("injectionTerm", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtermStep.injectionTerm)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("wrappedTerm", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtermStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtermStep.wrappedTerm)(hydra.extract.core.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.paths.SubtermStep],
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.paths.SubtermStep]](Left(hydra.lib.strings.cat(Seq("no such field ",
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.paths.SubtermStep])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.paths.SubtermStep]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def subtypeEdge(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.paths.SubtypeEdge] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.paths.SubtypeEdge]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.paths.SubtypeNode, hydra.paths.SubtypeEdge](hydra.extract.core.requireField("source")(hydra.decode.paths.subtypeNode)(fieldMap)(cx))((field_source: hydra.paths.SubtypeNode) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.paths.SubtypePath,
         hydra.paths.SubtypeEdge](hydra.extract.core.requireField("path")(hydra.decode.paths.subtypePath)(fieldMap)(cx))((field_path: hydra.paths.SubtypePath) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.paths.SubtypeNode,
         hydra.paths.SubtypeEdge](hydra.extract.core.requireField("target")(hydra.decode.paths.subtypeNode)(fieldMap)(cx))((field_target: hydra.paths.SubtypeNode) =>
      Right(hydra.paths.SubtypeEdge(field_source, field_path, field_target)))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def subtypeGraph(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.paths.SubtypeGraph] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.paths.SubtypeGraph]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.paths.SubtypeNode],
       hydra.paths.SubtypeGraph](hydra.extract.core.requireField("nodes")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.paths.subtypeNode)(v1)(v2))(fieldMap)(cx))((field_nodes: Seq[hydra.paths.SubtypeNode]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.paths.SubtypeEdge],
         hydra.paths.SubtypeGraph](hydra.extract.core.requireField("edges")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.paths.subtypeEdge)(v1)(v2))(fieldMap)(cx))((field_edges: Seq[hydra.paths.SubtypeEdge]) => Right(hydra.paths.SubtypeGraph(field_nodes,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         field_edges))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def subtypeNode(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.paths.SubtypeNode] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.paths.SubtypeNode]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.paths.SubtypeNode](hydra.extract.core.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.paths.SubtypeNode](hydra.extract.core.requireField("label")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_label: scala.Predef.String) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.paths.SubtypeNode](hydra.extract.core.requireField("id")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_id: scala.Predef.String) =>
      Right(hydra.paths.SubtypeNode(field_name, field_label, field_id)))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def subtypePath(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.paths.SubtypePath] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.paths.SubtypePath]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[Seq[hydra.paths.SubtypeStep],
     hydra.paths.SubtypePath, hydra.errors.DecodingError]((b: Seq[hydra.paths.SubtypeStep]) => b)(hydra.extract.core.decodeList(hydra.decode.paths.subtypeStep)(cx)(v_Term_wrap_wrappedTerm.body))
  case _ => Left("expected wrapped type"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def subtypeStep(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.paths.SubtypeStep] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.paths.SubtypeStep]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.inject(v_Term_inject_inj) => {
    lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.paths.SubtypeStep])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       hydra.paths.SubtypeStep]](Seq(Tuple2("annotatedBody", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtypeStep.annotatedBody)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("applicationFunction", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtypeStep.applicationFunction)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("applicationArgument", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtypeStep.applicationArgument)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("eitherLeft", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtypeStep.eitherLeft)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("eitherRight", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtypeStep.eitherRight)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("forallBody", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtypeStep.forallBody)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("functionDomain", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtypeStep.functionDomain)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("functionCodomain", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtypeStep.functionCodomain)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("listElement", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtypeStep.listElement)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("mapKeys", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtypeStep.mapKeys)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("mapValues", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtypeStep.mapValues)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("maybeElement", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtypeStep.maybeElement)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("pairFirst", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtypeStep.pairFirst)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("pairSecond", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtypeStep.pairSecond)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("recordField", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.core.Name, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: hydra.core.Name) => hydra.paths.SubtypeStep.recordField(t))(hydra.decode.core.name(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("setElement", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtypeStep.setElement)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("unionField", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.core.Name, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: hydra.core.Name) => hydra.paths.SubtypeStep.unionField(t))(hydra.decode.core.name(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("wrappedType", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.paths.SubtypeStep, hydra.errors.DecodingError]((t: Unit) => hydra.paths.SubtypeStep.wrappedType)(hydra.extract.core.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.paths.SubtypeStep],
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.paths.SubtypeStep]](Left(hydra.lib.strings.cat(Seq("no such field ",
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.paths.SubtypeStep])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.paths.SubtypeStep]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))
