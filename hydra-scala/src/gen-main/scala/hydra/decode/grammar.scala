package hydra.decode.grammar

import hydra.core.*

import hydra.error.*

import hydra.grammar.*

import hydra.lib.eithers

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.strings

def constant(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.grammar.Constant] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.grammar.Constant]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String, hydra.grammar.Constant,
     hydra.error.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[scala.Predef.String,
     hydra.core.Term, Either[hydra.error.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def grammar(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.grammar.Grammar] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.grammar.Grammar]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[Seq[hydra.grammar.Production],
     hydra.grammar.Grammar, hydra.error.DecodingError]((b: Seq[hydra.grammar.Production]) => b)(hydra.extract.helpers.decodeList(hydra.decode.grammar.production)(cx)(v_Term_wrap_wrappedTerm.body))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def label(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.grammar.Label] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.grammar.Label]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String, hydra.grammar.Label,
     hydra.error.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[scala.Predef.String,
     hydra.core.Term, Either[hydra.error.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def labeledPattern(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.grammar.LabeledPattern] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.grammar.LabeledPattern]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.error.DecodingError, hydra.grammar.Label, hydra.grammar.LabeledPattern](hydra.extract.helpers.requireField("label")(hydra.decode.grammar.label)(fieldMap)(cx))((field_label: hydra.grammar.Label) =>
      hydra.lib.eithers.bind[hydra.error.DecodingError, hydra.grammar.Pattern, hydra.grammar.LabeledPattern](hydra.extract.helpers.requireField("pattern")(hydra.decode.grammar.pattern)(fieldMap)(cx))((field_pattern: hydra.grammar.Pattern) =>
      Right(hydra.grammar.LabeledPattern(field_label, field_pattern))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def pattern(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.grammar.Pattern] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.grammar.Pattern]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.union(v_Term_union_inj) => {
    val field: hydra.core.Field = (v_Term_union_inj.field)
    val fname: hydra.core.Name = (field.name)
    val fterm: hydra.core.Term = (field.term)
    val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.error.DecodingError, hydra.grammar.Pattern])] = hydra.lib.maps.fromList[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.error.DecodingError, hydra.grammar.Pattern]](Seq(Tuple2("alternatives",
       (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Seq[hydra.grammar.Pattern], hydra.grammar.Pattern, hydra.error.DecodingError]((t: Seq[hydra.grammar.Pattern]) => hydra.grammar.Pattern.alternatives(t))(hydra.extract.helpers.decodeList(hydra.decode.grammar.pattern)(cx)(input))),
         Tuple2("constant", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.grammar.Constant, hydra.grammar.Pattern, hydra.error.DecodingError]((t: hydra.grammar.Constant) => hydra.grammar.Pattern.constant(t))(hydra.decode.grammar.constant(cx)(input))),
         Tuple2("ignored", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.grammar.Pattern, hydra.grammar.Pattern, hydra.error.DecodingError]((t: hydra.grammar.Pattern) => hydra.grammar.Pattern.ignored(t))(hydra.decode.grammar.pattern(cx)(input))),
         Tuple2("labeled", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.grammar.LabeledPattern, hydra.grammar.Pattern, hydra.error.DecodingError]((t: hydra.grammar.LabeledPattern) => hydra.grammar.Pattern.labeled(t))(hydra.decode.grammar.labeledPattern(cx)(input))),
         Tuple2("nil", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.grammar.Pattern, hydra.error.DecodingError]((t: Unit) => hydra.grammar.Pattern.nil)(hydra.extract.helpers.decodeUnit(cx)(input))),
         Tuple2("nonterminal", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.grammar.Symbol, hydra.grammar.Pattern, hydra.error.DecodingError]((t: hydra.grammar.Symbol) => hydra.grammar.Pattern.nonterminal(t))(hydra.decode.grammar.symbol(cx)(input))),
         Tuple2("option", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.grammar.Pattern, hydra.grammar.Pattern, hydra.error.DecodingError]((t: hydra.grammar.Pattern) => hydra.grammar.Pattern.option(t))(hydra.decode.grammar.pattern(cx)(input))),
         Tuple2("plus", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.grammar.Pattern, hydra.grammar.Pattern, hydra.error.DecodingError]((t: hydra.grammar.Pattern) => hydra.grammar.Pattern.plus(t))(hydra.decode.grammar.pattern(cx)(input))),
         Tuple2("regex", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.grammar.Regex, hydra.grammar.Pattern, hydra.error.DecodingError]((t: hydra.grammar.Regex) => hydra.grammar.Pattern.regex(t))(hydra.decode.grammar.regex(cx)(input))),
         Tuple2("sequence", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Seq[hydra.grammar.Pattern], hydra.grammar.Pattern, hydra.error.DecodingError]((t: Seq[hydra.grammar.Pattern]) => hydra.grammar.Pattern.sequence(t))(hydra.extract.helpers.decodeList(hydra.decode.grammar.pattern)(cx)(input))),
         Tuple2("star", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.grammar.Pattern, hydra.grammar.Pattern, hydra.error.DecodingError]((t: hydra.grammar.Pattern) => hydra.grammar.Pattern.star(t))(hydra.decode.grammar.pattern(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.error.DecodingError, hydra.grammar.Pattern], (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.grammar.Pattern]](Left(hydra.lib.strings.cat(Seq("no such field ", fname, " in union"))))((f: (hydra.core.Term => Either[hydra.error.DecodingError,
       hydra.grammar.Pattern])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name, (hydra.core.Term) => Either[hydra.error.DecodingError,
       hydra.grammar.Pattern]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def production(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.grammar.Production] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.grammar.Production]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.helpers.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.error.DecodingError, hydra.grammar.Symbol, hydra.grammar.Production](hydra.extract.helpers.requireField("symbol")(hydra.decode.grammar.symbol)(fieldMap)(cx))((field_symbol: hydra.grammar.Symbol) =>
      hydra.lib.eithers.bind[hydra.error.DecodingError, hydra.grammar.Pattern, hydra.grammar.Production](hydra.extract.helpers.requireField("pattern")(hydra.decode.grammar.pattern)(fieldMap)(cx))((field_pattern: hydra.grammar.Pattern) => Right(hydra.grammar.Production(field_symbol,
         field_pattern))))
  }
  case _ => Left("expected record"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def regex(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.grammar.Regex] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.grammar.Regex]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String, hydra.grammar.Regex,
     hydra.error.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[scala.Predef.String,
     hydra.core.Term, Either[hydra.error.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))

def symbol(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.error.DecodingError, hydra.grammar.Symbol] =
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.error.DecodingError, hydra.grammar.Symbol]]((err: scala.Predef.String) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String, hydra.grammar.Symbol,
     hydra.error.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[scala.Predef.String,
     hydra.core.Term, Either[hydra.error.DecodingError, scala.Predef.String]]((err: scala.Predef.String) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.lexical.stripAndDereferenceTermEither(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.lexical.stripAndDereferenceTermEither(cx)(raw))
