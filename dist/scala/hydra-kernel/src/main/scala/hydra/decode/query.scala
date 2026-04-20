package hydra.decode.query

import hydra.core.*

import hydra.errors.*

import hydra.query.*

def comparisonConstraint(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.query.ComparisonConstraint] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.query.ComparisonConstraint]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.inject(v_Term_inject_inj) => {
    lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.query.ComparisonConstraint])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.query.ComparisonConstraint]](Seq(Tuple2("equal",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.query.ComparisonConstraint, hydra.errors.DecodingError]((t: Unit) => hydra.query.ComparisonConstraint.equal)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("notEqual", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.query.ComparisonConstraint, hydra.errors.DecodingError]((t: Unit) => hydra.query.ComparisonConstraint.notEqual)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("lessThan", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.query.ComparisonConstraint, hydra.errors.DecodingError]((t: Unit) => hydra.query.ComparisonConstraint.lessThan)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("greaterThan", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.query.ComparisonConstraint, hydra.errors.DecodingError]((t: Unit) => hydra.query.ComparisonConstraint.greaterThan)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("lessThanOrEqual", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.query.ComparisonConstraint, hydra.errors.DecodingError]((t: Unit) => hydra.query.ComparisonConstraint.lessThanOrEqual)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("greaterThanOrEqual", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.query.ComparisonConstraint, hydra.errors.DecodingError]((t: Unit) => hydra.query.ComparisonConstraint.greaterThanOrEqual)(hydra.extract.core.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.query.ComparisonConstraint],
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.query.ComparisonConstraint]](Left(hydra.lib.strings.cat(Seq("no such field ",
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.query.ComparisonConstraint])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.query.ComparisonConstraint]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def edge(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.query.Edge] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.query.Edge]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.query.Edge](hydra.extract.core.requireField("type")(hydra.decode.core.name)(fieldMap)(cx))((field_type: hydra.core.Name) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Option[hydra.core.Name],
         hydra.query.Edge](hydra.extract.core.requireField("out")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeMaybe(hydra.decode.core.name)(v1)(v2))(fieldMap)(cx))((field_out: Option[hydra.core.Name]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Option[hydra.core.Name],
         hydra.query.Edge](hydra.extract.core.requireField("in")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeMaybe(hydra.decode.core.name)(v1)(v2))(fieldMap)(cx))((field_in: Option[hydra.core.Name]) => Right(hydra.query.Edge(field_type,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         field_out, field_in)))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def graphPattern(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.query.GraphPattern] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.query.GraphPattern]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.query.GraphPattern](hydra.extract.core.requireField("graph")(hydra.decode.core.name)(fieldMap)(cx))((field_graph: hydra.core.Name) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.query.Pattern],
         hydra.query.GraphPattern](hydra.extract.core.requireField("patterns")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.query.pattern)(v1)(v2))(fieldMap)(cx))((field_patterns: Seq[hydra.query.Pattern]) => Right(hydra.query.GraphPattern(field_graph,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         field_patterns))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def node(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.query.Node] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.query.Node]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.inject(v_Term_inject_inj) => {
    lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.query.Node])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       hydra.query.Node]](Seq(Tuple2("term", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.core.Term, hydra.query.Node, hydra.errors.DecodingError]((t: hydra.core.Term) => hydra.query.Node.term(t))(hydra.decode.core.term(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("variable", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.query.Variable, hydra.query.Node, hydra.errors.DecodingError]((t: hydra.query.Variable) => hydra.query.Node.variable(t))(hydra.decode.query.variable(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("wildcard", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.query.Node, hydra.errors.DecodingError]((t: Unit) => hydra.query.Node.wildcard)(hydra.extract.core.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.query.Node], (hydra.core.Term) => Either[hydra.errors.DecodingError,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       hydra.query.Node]](Left(hydra.lib.strings.cat(Seq("no such field ", fname,
       " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.query.Node])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       hydra.query.Node]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def path(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.query.Path] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.query.Path]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.inject(v_Term_inject_inj) => {
    lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.query.Path])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       hydra.query.Path]](Seq(Tuple2("step", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.query.Step, hydra.query.Path, hydra.errors.DecodingError]((t: hydra.query.Step) => hydra.query.Path.step(t))(hydra.decode.query.step(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("regex", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.query.RegexSequence, hydra.query.Path, hydra.errors.DecodingError]((t: hydra.query.RegexSequence) => hydra.query.Path.regex(t))(hydra.decode.query.regexSequence(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("inverse", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.query.Path, hydra.query.Path, hydra.errors.DecodingError]((t: hydra.query.Path) => hydra.query.Path.inverse(t))(hydra.decode.query.path(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.query.Path], (hydra.core.Term) => Either[hydra.errors.DecodingError,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       hydra.query.Path]](Left(hydra.lib.strings.cat(Seq("no such field ", fname,
       " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.query.Path])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       hydra.query.Path]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def pathEquation(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.query.PathEquation] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.query.PathEquation]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.query.Path, hydra.query.PathEquation](hydra.extract.core.requireField("left")(hydra.decode.query.path)(fieldMap)(cx))((field_left: hydra.query.Path) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.query.Path, hydra.query.PathEquation](hydra.extract.core.requireField("right")(hydra.decode.query.path)(fieldMap)(cx))((field_right: hydra.query.Path) => Right(hydra.query.PathEquation(field_left,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         field_right))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def pattern(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.query.Pattern] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.query.Pattern]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.inject(v_Term_inject_inj) => {
    lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.query.Pattern])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       hydra.query.Pattern]](Seq(Tuple2("triple", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.query.TriplePattern, hydra.query.Pattern, hydra.errors.DecodingError]((t: hydra.query.TriplePattern) => hydra.query.Pattern.triple(t))(hydra.decode.query.triplePattern(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("negation", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.query.Pattern, hydra.query.Pattern, hydra.errors.DecodingError]((t: hydra.query.Pattern) => hydra.query.Pattern.negation(t))(hydra.decode.query.pattern(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("conjunction", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Seq[hydra.query.Pattern], hydra.query.Pattern, hydra.errors.DecodingError]((t: Seq[hydra.query.Pattern]) => hydra.query.Pattern.conjunction(t))(hydra.extract.core.decodeList(hydra.decode.query.pattern)(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("disjunction", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Seq[hydra.query.Pattern], hydra.query.Pattern, hydra.errors.DecodingError]((t: Seq[hydra.query.Pattern]) => hydra.query.Pattern.disjunction(t))(hydra.extract.core.decodeList(hydra.decode.query.pattern)(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("graph", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.query.GraphPattern, hydra.query.Pattern, hydra.errors.DecodingError]((t: hydra.query.GraphPattern) => hydra.query.Pattern.graph(t))(hydra.decode.query.graphPattern(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.query.Pattern],
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.query.Pattern]](Left(hydra.lib.strings.cat(Seq("no such field ",
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.query.Pattern])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.query.Pattern]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def patternImplication(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.query.PatternImplication] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.query.PatternImplication]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.query.Pattern, hydra.query.PatternImplication](hydra.extract.core.requireField("antecedent")(hydra.decode.query.pattern)(fieldMap)(cx))((field_antecedent: hydra.query.Pattern) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.query.Pattern, hydra.query.PatternImplication](hydra.extract.core.requireField("consequent")(hydra.decode.query.pattern)(fieldMap)(cx))((field_consequent: hydra.query.Pattern) =>
      Right(hydra.query.PatternImplication(field_antecedent, field_consequent))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def query(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.query.Query] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.query.Query]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.query.Variable],
       hydra.query.Query](hydra.extract.core.requireField("variables")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.query.variable)(v1)(v2))(fieldMap)(cx))((field_variables: Seq[hydra.query.Variable]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.query.Pattern],
         hydra.query.Query](hydra.extract.core.requireField("patterns")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.query.pattern)(v1)(v2))(fieldMap)(cx))((field_patterns: Seq[hydra.query.Pattern]) => Right(hydra.query.Query(field_variables,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         field_patterns))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def range(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.query.Range] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.query.Range]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Int, hydra.query.Range](hydra.extract.core.requireField("min")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         Int]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_min: Int) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Int, hydra.query.Range](hydra.extract.core.requireField("max")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         Int]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_max: Int) => Right(hydra.query.Range(field_min,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         field_max))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def regexQuantifier(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.query.RegexQuantifier] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.query.RegexQuantifier]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.inject(v_Term_inject_inj) => {
    lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.query.RegexQuantifier])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       hydra.query.RegexQuantifier]](Seq(Tuple2("one", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.query.RegexQuantifier, hydra.errors.DecodingError]((t: Unit) => hydra.query.RegexQuantifier.one)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("zeroOrOne", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.query.RegexQuantifier, hydra.errors.DecodingError]((t: Unit) => hydra.query.RegexQuantifier.zeroOrOne)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("zeroOrMore", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.query.RegexQuantifier, hydra.errors.DecodingError]((t: Unit) => hydra.query.RegexQuantifier.zeroOrMore)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("oneOrMore", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.query.RegexQuantifier, hydra.errors.DecodingError]((t: Unit) => hydra.query.RegexQuantifier.oneOrMore)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("exactly", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Int, hydra.query.RegexQuantifier, hydra.errors.DecodingError]((t: Int) => hydra.query.RegexQuantifier.exactly(t))(hydra.lib.eithers.either[hydra.errors.DecodingError,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Term, Either[hydra.errors.DecodingError, Int]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(input)))),
         Tuple2("atLeast", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Int, hydra.query.RegexQuantifier, hydra.errors.DecodingError]((t: Int) => hydra.query.RegexQuantifier.atLeast(t))(hydra.lib.eithers.either[hydra.errors.DecodingError,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Term, Either[hydra.errors.DecodingError, Int]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
          case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
          case _ => Left("expected int32 value")
        case _ => Left("expected int32 literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(input)))),
         Tuple2("range", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.query.Range, hydra.query.RegexQuantifier, hydra.errors.DecodingError]((t: hydra.query.Range) => hydra.query.RegexQuantifier.range(t))(hydra.decode.query.range(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.query.RegexQuantifier],
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.query.RegexQuantifier]](Left(hydra.lib.strings.cat(Seq("no such field ",
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.query.RegexQuantifier])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.query.RegexQuantifier]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def regexSequence(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.query.RegexSequence] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.query.RegexSequence]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.query.Path, hydra.query.RegexSequence](hydra.extract.core.requireField("path")(hydra.decode.query.path)(fieldMap)(cx))((field_path: hydra.query.Path) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.query.RegexQuantifier,
         hydra.query.RegexSequence](hydra.extract.core.requireField("quantifier")(hydra.decode.query.regexQuantifier)(fieldMap)(cx))((field_quantifier: hydra.query.RegexQuantifier) =>
      Right(hydra.query.RegexSequence(field_path, field_quantifier))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def step(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.query.Step] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.query.Step]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.inject(v_Term_inject_inj) => {
    lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.query.Step])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       hydra.query.Step]](Seq(Tuple2("edge", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.query.Edge, hydra.query.Step, hydra.errors.DecodingError]((t: hydra.query.Edge) => hydra.query.Step.edge(t))(hydra.decode.query.edge(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("project", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.core.Projection, hydra.query.Step, hydra.errors.DecodingError]((t: hydra.core.Projection) => hydra.query.Step.project(t))(hydra.decode.core.projection(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("compare", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.query.ComparisonConstraint, hydra.query.Step, hydra.errors.DecodingError]((t: hydra.query.ComparisonConstraint) => hydra.query.Step.compare(t))(hydra.decode.query.comparisonConstraint(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.query.Step], (hydra.core.Term) => Either[hydra.errors.DecodingError,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       hydra.query.Step]](Left(hydra.lib.strings.cat(Seq("no such field ", fname,
       " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.query.Step])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       hydra.query.Step]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def triplePattern(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.query.TriplePattern] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.query.TriplePattern]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.query.Node, hydra.query.TriplePattern](hydra.extract.core.requireField("subject")(hydra.decode.query.node)(fieldMap)(cx))((field_subject: hydra.query.Node) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.query.Path, hydra.query.TriplePattern](hydra.extract.core.requireField("predicate")(hydra.decode.query.path)(fieldMap)(cx))((field_predicate: hydra.query.Path) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.query.Node, hydra.query.TriplePattern](hydra.extract.core.requireField("object")(hydra.decode.query.node)(fieldMap)(cx))((field_object: hydra.query.Node) =>
      Right(hydra.query.TriplePattern(field_subject, field_predicate, field_object)))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def variable(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.query.Variable] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.query.Variable]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String,
     hydra.query.Variable, hydra.errors.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[hydra.errors.DecodingError,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.extract.core.stripWithDecodingError(cx)(raw))
