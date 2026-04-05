package hydra.decode.ast

import hydra.ast.*

import hydra.core.*

import hydra.errors.*

def associativity(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.ast.Associativity] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.ast.Associativity]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.ast.Associativity])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.ast.Associativity]](Seq(Tuple2("none",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.ast.Associativity, hydra.errors.DecodingError]((t: Unit) => hydra.ast.Associativity.none)(hydra.extract.core.decodeUnit(cx)(input))),
         Tuple2("left", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.ast.Associativity, hydra.errors.DecodingError]((t: Unit) => hydra.ast.Associativity.left)(hydra.extract.core.decodeUnit(cx)(input))),
         Tuple2("right", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.ast.Associativity, hydra.errors.DecodingError]((t: Unit) => hydra.ast.Associativity.right)(hydra.extract.core.decodeUnit(cx)(input))),
         Tuple2("both", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.ast.Associativity, hydra.errors.DecodingError]((t: Unit) => hydra.ast.Associativity.both)(hydra.extract.core.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.ast.Associativity], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.ast.Associativity]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.ast.Associativity])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.ast.Associativity]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def blockStyle(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.ast.BlockStyle] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.ast.BlockStyle]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Option[scala.Predef.String], hydra.ast.BlockStyle](hydra.extract.core.requireField("indent")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeMaybe((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(v1)(v2))(fieldMap)(cx))((field_indent: Option[scala.Predef.String]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Boolean, hydra.ast.BlockStyle](hydra.extract.core.requireField("newlineBeforeContent")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError,
         Boolean]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.boolean(v_Literal_boolean_b) => Right(v_Literal_boolean_b)
        case _ => Left("expected boolean literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_newlineBeforeContent: Boolean) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Boolean, hydra.ast.BlockStyle](hydra.extract.core.requireField("newlineAfterContent")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError,
         Boolean]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.boolean(v_Literal_boolean_b) => Right(v_Literal_boolean_b)
        case _ => Left("expected boolean literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx2)(raw2)))(fieldMap)(cx))((field_newlineAfterContent: Boolean) =>
      Right(hydra.ast.BlockStyle(field_indent, field_newlineBeforeContent, field_newlineAfterContent)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def bracketExpr(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.ast.BracketExpr] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.ast.BracketExpr]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.Brackets, hydra.ast.BracketExpr](hydra.extract.core.requireField("brackets")(hydra.decode.ast.brackets)(fieldMap)(cx))((field_brackets: hydra.ast.Brackets) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.Expr, hydra.ast.BracketExpr](hydra.extract.core.requireField("enclosed")(hydra.decode.ast.expr)(fieldMap)(cx))((field_enclosed: hydra.ast.Expr) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.BlockStyle, hydra.ast.BracketExpr](hydra.extract.core.requireField("style")(hydra.decode.ast.blockStyle)(fieldMap)(cx))((field_style: hydra.ast.BlockStyle) =>
      Right(hydra.ast.BracketExpr(field_brackets, field_enclosed, field_style)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def brackets(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.ast.Brackets] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.ast.Brackets]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.Symbol, hydra.ast.Brackets](hydra.extract.core.requireField("open")(hydra.decode.ast.symbol)(fieldMap)(cx))((field_open: hydra.ast.Symbol) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.Symbol, hydra.ast.Brackets](hydra.extract.core.requireField("close")(hydra.decode.ast.symbol)(fieldMap)(cx))((field_close: hydra.ast.Symbol) => Right(hydra.ast.Brackets(field_open,
         field_close))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def expr(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.ast.Expr] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.ast.Expr]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.ast.Expr])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.ast.Expr]](Seq(Tuple2("const", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.ast.Symbol, hydra.ast.Expr, hydra.errors.DecodingError]((t: hydra.ast.Symbol) => hydra.ast.Expr.const(t))(hydra.decode.ast.symbol(cx)(input))),
         Tuple2("indent", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.ast.IndentedExpression, hydra.ast.Expr, hydra.errors.DecodingError]((t: hydra.ast.IndentedExpression) => hydra.ast.Expr.indent(t))(hydra.decode.ast.indentedExpression(cx)(input))),
         Tuple2("op", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.ast.OpExpr, hydra.ast.Expr, hydra.errors.DecodingError]((t: hydra.ast.OpExpr) => hydra.ast.Expr.op(t))(hydra.decode.ast.opExpr(cx)(input))),
         Tuple2("brackets", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.ast.BracketExpr, hydra.ast.Expr, hydra.errors.DecodingError]((t: hydra.ast.BracketExpr) => hydra.ast.Expr.brackets(t))(hydra.decode.ast.bracketExpr(cx)(input))),
         Tuple2("seq", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.ast.SeqExpr, hydra.ast.Expr, hydra.errors.DecodingError]((t: hydra.ast.SeqExpr) => hydra.ast.Expr.seq(t))(hydra.decode.ast.seqExpr(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.ast.Expr], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.ast.Expr]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.ast.Expr])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.ast.Expr]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def indentStyle(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.ast.IndentStyle] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.ast.IndentStyle]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.ast.IndentStyle])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.ast.IndentStyle]](Seq(Tuple2("allLines",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[scala.Predef.String, hydra.ast.IndentStyle, hydra.errors.DecodingError]((t: scala.Predef.String) => hydra.ast.IndentStyle.allLines(t))(hydra.lib.eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("subsequentLines", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[scala.Predef.String, hydra.ast.IndentStyle, hydra.errors.DecodingError]((t: scala.Predef.String) => hydra.ast.IndentStyle.subsequentLines(t))(hydra.lib.eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input))))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.ast.IndentStyle], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.ast.IndentStyle]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.ast.IndentStyle])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.ast.IndentStyle]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def indentedExpression(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.ast.IndentedExpression] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.ast.IndentedExpression]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.IndentStyle, hydra.ast.IndentedExpression](hydra.extract.core.requireField("style")(hydra.decode.ast.indentStyle)(fieldMap)(cx))((field_style: hydra.ast.IndentStyle) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.Expr, hydra.ast.IndentedExpression](hydra.extract.core.requireField("expr")(hydra.decode.ast.expr)(fieldMap)(cx))((field_expr: hydra.ast.Expr) => Right(hydra.ast.IndentedExpression(field_style,
         field_expr))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def op(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.ast.Op] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.ast.Op]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.Symbol, hydra.ast.Op](hydra.extract.core.requireField("symbol")(hydra.decode.ast.symbol)(fieldMap)(cx))((field_symbol: hydra.ast.Symbol) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.Padding, hydra.ast.Op](hydra.extract.core.requireField("padding")(hydra.decode.ast.padding)(fieldMap)(cx))((field_padding: hydra.ast.Padding) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.Precedence, hydra.ast.Op](hydra.extract.core.requireField("precedence")(hydra.decode.ast.precedence)(fieldMap)(cx))((field_precedence: hydra.ast.Precedence) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.Associativity, hydra.ast.Op](hydra.extract.core.requireField("associativity")(hydra.decode.ast.associativity)(fieldMap)(cx))((field_associativity: hydra.ast.Associativity) =>
      Right(hydra.ast.Op(field_symbol, field_padding, field_precedence, field_associativity))))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def opExpr(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.ast.OpExpr] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.ast.OpExpr]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.Op, hydra.ast.OpExpr](hydra.extract.core.requireField("op")(hydra.decode.ast.op)(fieldMap)(cx))((field_op: hydra.ast.Op) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.Expr, hydra.ast.OpExpr](hydra.extract.core.requireField("lhs")(hydra.decode.ast.expr)(fieldMap)(cx))((field_lhs: hydra.ast.Expr) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.Expr, hydra.ast.OpExpr](hydra.extract.core.requireField("rhs")(hydra.decode.ast.expr)(fieldMap)(cx))((field_rhs: hydra.ast.Expr) => Right(hydra.ast.OpExpr(field_op,
         field_lhs, field_rhs)))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def padding(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.ast.Padding] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.ast.Padding]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.Ws, hydra.ast.Padding](hydra.extract.core.requireField("left")(hydra.decode.ast.ws)(fieldMap)(cx))((field_left: hydra.ast.Ws) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.Ws, hydra.ast.Padding](hydra.extract.core.requireField("right")(hydra.decode.ast.ws)(fieldMap)(cx))((field_right: hydra.ast.Ws) => Right(hydra.ast.Padding(field_left,
         field_right))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def precedence(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.ast.Precedence] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.ast.Precedence]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[Int, hydra.ast.Precedence,
     hydra.errors.DecodingError]((b: Int) => b)(hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term,
     Either[hydra.errors.DecodingError, Int]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.integer(v_Literal_integer_v1) => v_Literal_integer_v1 match
        case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
        case _ => Left("expected int32 value")
      case _ => Left("expected int32 literal")
    case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def seqExpr(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.ast.SeqExpr] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.ast.SeqExpr]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.ast.Op, hydra.ast.SeqExpr](hydra.extract.core.requireField("op")(hydra.decode.ast.op)(fieldMap)(cx))((field_op: hydra.ast.Op) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.ast.Expr], hydra.ast.SeqExpr](hydra.extract.core.requireField("elements")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) => hydra.extract.core.decodeList(hydra.decode.ast.expr)(v1)(v2))(fieldMap)(cx))((field_elements: Seq[hydra.ast.Expr]) => Right(hydra.ast.SeqExpr(field_op,
         field_elements))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def symbol(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.ast.Symbol] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.ast.Symbol]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String, hydra.ast.Symbol,
     hydra.errors.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[scala.Predef.String,
     hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def ws(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.ast.Ws] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.DecodingError, hydra.ast.Ws]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val field: hydra.core.Field = (v_Term_union_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError, hydra.ast.Ws])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.ast.Ws]](Seq(Tuple2("none", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.ast.Ws, hydra.errors.DecodingError]((t: Unit) => hydra.ast.Ws.none)(hydra.extract.core.decodeUnit(cx)(input))),
         Tuple2("space", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.ast.Ws, hydra.errors.DecodingError]((t: Unit) => hydra.ast.Ws.space)(hydra.extract.core.decodeUnit(cx)(input))),
         Tuple2("break", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.ast.Ws, hydra.errors.DecodingError]((t: Unit) => hydra.ast.Ws.break)(hydra.extract.core.decodeUnit(cx)(input))),
         Tuple2("breakAndIndent", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[scala.Predef.String, hydra.ast.Ws, hydra.errors.DecodingError]((t: scala.Predef.String) => hydra.ast.Ws.breakAndIndent(t))(hydra.lib.eithers.either[scala.Predef.String,
         hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(input)))), Tuple2("doubleBreak", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.ast.Ws, hydra.errors.DecodingError]((t: Unit) => hydra.ast.Ws.doubleBreak)(hydra.extract.core.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.ast.Ws], (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.ast.Ws]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.ast.Ws])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
       hydra.ast.Ws]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
