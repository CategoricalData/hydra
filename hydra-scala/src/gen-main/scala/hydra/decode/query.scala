package hydra.decode.query

import hydra.core.*

import hydra.error.*

import hydra.query.*

import hydra.lib.eithers

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.strings

def comparisonConstraint(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.query.ComparisonConstraint] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.query.ComparisonConstraint]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.query.ComparisonConstraint])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.query.ComparisonConstraint]](Seq(Tuple2("equal",
       (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.query.ComparisonConstraint, hydra.error.DecodingError]((t: Unit) => hydra.query.ComparisonConstraint.equal)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("notEqual", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.query.ComparisonConstraint, hydra.error.DecodingError]((t: Unit) => hydra.query.ComparisonConstraint.notEqual)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("lessThan", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.query.ComparisonConstraint, hydra.error.DecodingError]((t: Unit) => hydra.query.ComparisonConstraint.lessThan)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("greaterThan", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.query.ComparisonConstraint, hydra.error.DecodingError]((t: Unit) => hydra.query.ComparisonConstraint.greaterThan)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("lessThanOrEqual", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.query.ComparisonConstraint, hydra.error.DecodingError]((t: Unit) => hydra.query.ComparisonConstraint.lessThanOrEqual)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("greaterThanOrEqual", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.query.ComparisonConstraint, hydra.error.DecodingError]((t: Unit) => hydra.query.ComparisonConstraint.greaterThanOrEqual)(hydra.extract.helpers.decodeUnit(cx)(input)))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.query.ComparisonConstraint], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.query.ComparisonConstraint]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.query.ComparisonConstraint])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.query.ComparisonConstraint]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def edge(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.query.Edge] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.query.Edge]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Name, hydra.query.Edge](hydra.extract.helpers.requireField("type")(hydra.decode.core.name)(fieldMap)(cx))((field_type: hydra.core.Name) =>
      eithers.bind[hydra.error.DecodingError, Option[hydra.core.Name], hydra.query.Edge](hydra.extract.helpers.requireField("out")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMaybe(hydra.decode.core.name)(v1)(v2))(fieldMap)(cx))((field_out: Option[hydra.core.Name]) =>
      eithers.bind[hydra.error.DecodingError, Option[hydra.core.Name], hydra.query.Edge](hydra.extract.helpers.requireField("in")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeMaybe(hydra.decode.core.name)(v1)(v2))(fieldMap)(cx))((field_in: Option[hydra.core.Name]) => Right(hydra.query.Edge(field_type,
         field_out, field_in)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def graphPattern(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.query.GraphPattern] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.query.GraphPattern]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.core.Name, hydra.query.GraphPattern](hydra.extract.helpers.requireField("graph")(hydra.decode.core.name)(fieldMap)(cx))((field_graph: hydra.core.Name) =>
      eithers.bind[hydra.error.DecodingError, Seq[hydra.query.Pattern], hydra.query.GraphPattern](hydra.extract.helpers.requireField("patterns")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.query.pattern)(v1)(v2))(fieldMap)(cx))((field_patterns: Seq[hydra.query.Pattern]) => Right(hydra.query.GraphPattern(field_graph,
         field_patterns))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def node(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.query.Node] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.query.Node]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.query.Node])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.query.Node]](Seq(Tuple2("term", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Term, hydra.query.Node, hydra.error.DecodingError]((t: hydra.core.Term) => hydra.query.Node.term(t))(hydra.decode.core.term(cx)(input))),
         Tuple2("variable", (input: hydra.core.Term) =>
      eithers.map[hydra.query.Variable, hydra.query.Node, hydra.error.DecodingError]((t: hydra.query.Variable) => hydra.query.Node.variable(t))(hydra.decode.query.variable(cx)(input))),
         Tuple2("wildcard", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.query.Node, hydra.error.DecodingError]((t: Unit) => hydra.query.Node.wildcard)(hydra.extract.helpers.decodeUnit(cx)(input)))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.query.Node], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.query.Node]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.query.Node])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.query.Node]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def path(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.query.Path] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.query.Path]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.query.Path])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.query.Path]](Seq(Tuple2("step", (input: hydra.core.Term) =>
      eithers.map[hydra.query.Step, hydra.query.Path, hydra.error.DecodingError]((t: hydra.query.Step) => hydra.query.Path.step(t))(hydra.decode.query.step(cx)(input))),
         Tuple2("regex", (input: hydra.core.Term) =>
      eithers.map[hydra.query.RegexSequence, hydra.query.Path, hydra.error.DecodingError]((t: hydra.query.RegexSequence) => hydra.query.Path.regex(t))(hydra.decode.query.regexSequence(cx)(input))),
         Tuple2("inverse", (input: hydra.core.Term) =>
      eithers.map[hydra.query.Path, hydra.query.Path, hydra.error.DecodingError]((t: hydra.query.Path) => hydra.query.Path.inverse(t))(hydra.decode.query.path(cx)(input)))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.query.Path], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.query.Path]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.query.Path])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.query.Path]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def pathEquation(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.query.PathEquation] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.query.PathEquation]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.query.Path, hydra.query.PathEquation](hydra.extract.helpers.requireField("left")(hydra.decode.query.path)(fieldMap)(cx))((field_left: hydra.query.Path) =>
      eithers.bind[hydra.error.DecodingError, hydra.query.Path, hydra.query.PathEquation](hydra.extract.helpers.requireField("right")(hydra.decode.query.path)(fieldMap)(cx))((field_right: hydra.query.Path) => Right(hydra.query.PathEquation(field_left,
         field_right))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def pattern(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.query.Pattern] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.query.Pattern]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.query.Pattern])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.query.Pattern]](Seq(Tuple2("triple",
       (input: hydra.core.Term) =>
      eithers.map[hydra.query.TriplePattern, hydra.query.Pattern, hydra.error.DecodingError]((t: hydra.query.TriplePattern) => hydra.query.Pattern.triple(t))(hydra.decode.query.triplePattern(cx)(input))),
         Tuple2("negation", (input: hydra.core.Term) =>
      eithers.map[hydra.query.Pattern, hydra.query.Pattern, hydra.error.DecodingError]((t: hydra.query.Pattern) => hydra.query.Pattern.negation(t))(hydra.decode.query.pattern(cx)(input))),
         Tuple2("conjunction", (input: hydra.core.Term) =>
      eithers.map[Seq[hydra.query.Pattern], hydra.query.Pattern, hydra.error.DecodingError]((t: Seq[hydra.query.Pattern]) => hydra.query.Pattern.conjunction(t))(hydra.extract.helpers.decodeList(hydra.decode.query.pattern)(cx)(input))),
         Tuple2("disjunction", (input: hydra.core.Term) =>
      eithers.map[Seq[hydra.query.Pattern], hydra.query.Pattern, hydra.error.DecodingError]((t: Seq[hydra.query.Pattern]) => hydra.query.Pattern.disjunction(t))(hydra.extract.helpers.decodeList(hydra.decode.query.pattern)(cx)(input))),
         Tuple2("graph", (input: hydra.core.Term) =>
      eithers.map[hydra.query.GraphPattern, hydra.query.Pattern, hydra.error.DecodingError]((t: hydra.query.GraphPattern) => hydra.query.Pattern.graph(t))(hydra.decode.query.graphPattern(cx)(input)))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.query.Pattern], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.query.Pattern]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.query.Pattern])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.query.Pattern]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def patternImplication(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.query.PatternImplication] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.query.PatternImplication]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.query.Pattern, hydra.query.PatternImplication](hydra.extract.helpers.requireField("antecedent")(hydra.decode.query.pattern)(fieldMap)(cx))((field_antecedent: hydra.query.Pattern) =>
      eithers.bind[hydra.error.DecodingError, hydra.query.Pattern, hydra.query.PatternImplication](hydra.extract.helpers.requireField("consequent")(hydra.decode.query.pattern)(fieldMap)(cx))((field_consequent: hydra.query.Pattern) =>
      Right(hydra.query.PatternImplication(field_antecedent, field_consequent))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def query(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.query.Query] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.query.Query]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, Seq[hydra.query.Variable], hydra.query.Query](hydra.extract.helpers.requireField("variables")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.query.variable)(v1)(v2))(fieldMap)(cx))((field_variables: Seq[hydra.query.Variable]) =>
      eithers.bind[hydra.error.DecodingError, Seq[hydra.query.Pattern], hydra.query.Query](hydra.extract.helpers.requireField("patterns")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.helpers.decodeList(hydra.decode.query.pattern)(v1)(v2))(fieldMap)(cx))((field_patterns: Seq[hydra.query.Pattern]) => Right(hydra.query.Query(field_variables,
         field_patterns))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def range(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.query.Range] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.query.Range]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, Int, hydra.query.Range](hydra.extract.helpers.requireField("min")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_min: Int) =>
      eithers.bind[hydra.error.DecodingError, Int, hydra.query.Range](hydra.extract.helpers.requireField("max")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_max: Int) => Right(hydra.query.Range(field_min,
         field_max))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def regexQuantifier(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.query.RegexQuantifier] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.query.RegexQuantifier]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.query.RegexQuantifier])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.query.RegexQuantifier]](Seq(Tuple2("one",
       (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.query.RegexQuantifier, hydra.error.DecodingError]((t: Unit) => hydra.query.RegexQuantifier.one)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("zeroOrOne", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.query.RegexQuantifier, hydra.error.DecodingError]((t: Unit) => hydra.query.RegexQuantifier.zeroOrOne)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("zeroOrMore", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.query.RegexQuantifier, hydra.error.DecodingError]((t: Unit) => hydra.query.RegexQuantifier.zeroOrMore)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("oneOrMore", (input: hydra.core.Term) =>
      eithers.map[Unit, hydra.query.RegexQuantifier, hydra.error.DecodingError]((t: Unit) => hydra.query.RegexQuantifier.oneOrMore)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("exactly", (input: hydra.core.Term) =>
      eithers.map[Int, hydra.query.RegexQuantifier, hydra.error.DecodingError]((t: Int) => hydra.query.RegexQuantifier.exactly(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("atLeast", (input: hydra.core.Term) =>
      eithers.map[Int, hydra.query.RegexQuantifier, hydra.error.DecodingError]((t: Int) => hydra.query.RegexQuantifier.atLeast(t))(eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.error.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("range", (input: hydra.core.Term) =>
      eithers.map[hydra.query.Range, hydra.query.RegexQuantifier, hydra.error.DecodingError]((t: hydra.query.Range) => hydra.query.RegexQuantifier.range(t))(hydra.decode.query.range(cx)(input)))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.query.RegexQuantifier], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.query.RegexQuantifier]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.query.RegexQuantifier])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.query.RegexQuantifier]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def regexSequence(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.query.RegexSequence] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.query.RegexSequence]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.query.Path, hydra.query.RegexSequence](hydra.extract.helpers.requireField("path")(hydra.decode.query.path)(fieldMap)(cx))((field_path: hydra.query.Path) =>
      eithers.bind[hydra.error.DecodingError, hydra.query.RegexQuantifier, hydra.query.RegexSequence](hydra.extract.helpers.requireField("quantifier")(hydra.decode.query.regexQuantifier)(fieldMap)(cx))((field_quantifier: hydra.query.RegexQuantifier) =>
      Right(hydra.query.RegexSequence(field_path, field_quantifier))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def step(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.query.Step] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.query.Step]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.query.Step])] = maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.query.Step]](Seq(Tuple2("edge", (input: hydra.core.Term) =>
      eithers.map[hydra.query.Edge, hydra.query.Step, hydra.error.DecodingError]((t: hydra.query.Edge) => hydra.query.Step.edge(t))(hydra.decode.query.edge(cx)(input))),
         Tuple2("project", (input: hydra.core.Term) =>
      eithers.map[hydra.core.Projection, hydra.query.Step, hydra.error.DecodingError]((t: hydra.core.Projection) => hydra.query.Step.project(t))(hydra.decode.core.projection(cx)(input))),
         Tuple2("compare", (input: hydra.core.Term) =>
      eithers.map[hydra.query.ComparisonConstraint, hydra.query.Step, hydra.error.DecodingError]((t: hydra.query.ComparisonConstraint) => hydra.query.Step.compare(t))(hydra.decode.query.comparisonConstraint(cx)(input)))))
    maybes.maybe[Either[hydra.error.DecodingError, hydra.query.Step], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.query.Step]](Left(strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.query.Step])) => f(fterm))(maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.query.Step]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def triplePattern(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.query.TriplePattern] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.query.TriplePattern]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    eithers.bind[hydra.error.DecodingError, hydra.query.Node, hydra.query.TriplePattern](hydra.extract.helpers.requireField("subject")(hydra.decode.query.node)(fieldMap)(cx))((field_subject: hydra.query.Node) =>
      eithers.bind[hydra.error.DecodingError, hydra.query.Path, hydra.query.TriplePattern](hydra.extract.helpers.requireField("predicate")(hydra.decode.query.path)(fieldMap)(cx))((field_predicate: hydra.query.Path) =>
      eithers.bind[hydra.error.DecodingError, hydra.query.Node, hydra.query.TriplePattern](hydra.extract.helpers.requireField("object")(hydra.decode.query.node)(fieldMap)(cx))((field_object: hydra.query.Node) =>
      Right(hydra.query.TriplePattern(field_subject, field_predicate, field_object)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def variable(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.query.Variable] =
  eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.query.Variable]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => eithers.map[scala.Predef.String, hydra.query.Variable,
     hydra.error.DecodingError]((b: scala.Predef.String) => b)(eithers.either[scala.Predef.String, hydra.core.Term,
     Either[hydra.error.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
