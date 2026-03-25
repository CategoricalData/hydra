package hydra.extract.core

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.lib.eithers

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.literals

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.strings

def bigfloat(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], BigDecimal] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Literal, BigDecimal](hydra.extract.core.literal(cx)(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.FloatValue, BigDecimal](hydra.extract.core.floatLiteral(cx)(l))((f: hydra.core.FloatValue) => hydra.extract.core.bigfloatValue(cx)(f)))

def bigfloatValue(cx: hydra.context.Context)(v: hydra.core.FloatValue): Either[hydra.context.InContext[hydra.errors.Error], BigDecimal] =
  v match
  case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_f) => Right(v_FloatValue_bigfloat_f)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("bigfloat"))(" but found "))(hydra.show.core.float(v))),
     cx))

def bigint(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], BigInt] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Literal, BigInt](hydra.extract.core.literal(cx)(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.IntegerValue, BigInt](hydra.extract.core.integerLiteral(cx)(l))((i: hydra.core.IntegerValue) => hydra.extract.core.bigintValue(cx)(i)))

def bigintValue(cx: hydra.context.Context)(v: hydra.core.IntegerValue): Either[hydra.context.InContext[hydra.errors.Error], BigInt] =
  v match
  case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_i) => Right(v_IntegerValue_bigint_i)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("bigint"))(" but found "))(hydra.show.core.integer(v))),
     cx))

def binary(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], scala.Predef.String] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Literal, scala.Predef.String](hydra.extract.core.literal(cx)(graph)(t))((l: hydra.core.Literal) => hydra.extract.core.binaryLiteral(cx)(l))

def binaryLiteral(cx: hydra.context.Context)(v: hydra.core.Literal): Either[hydra.context.InContext[hydra.errors.Error], scala.Predef.String] =
  v match
  case hydra.core.Literal.binary(v_Literal_binary_b) => Right(v_Literal_binary_b)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("binary"))(" but found "))(hydra.show.core.literal(v))),
     cx))

def boolean(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], Boolean] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Literal, Boolean](hydra.extract.core.literal(cx)(graph)(t))((l: hydra.core.Literal) => hydra.extract.core.booleanLiteral(cx)(l))

def booleanLiteral(cx: hydra.context.Context)(v: hydra.core.Literal): Either[hydra.context.InContext[hydra.errors.Error], Boolean] =
  v match
  case hydra.core.Literal.boolean(v_Literal_boolean_b) => Right(v_Literal_boolean_b)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("boolean"))(" but found "))(hydra.show.core.literal(v))),
     cx))

def caseField(cx: hydra.context.Context)(name: hydra.core.Name)(n: scala.Predef.String)(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.core.Field] =
  {
  lazy val fieldName: hydra.core.Name = n
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.CaseStatement, hydra.core.Field](hydra.extract.core.cases(cx)(name)(graph)(term))((cs: hydra.core.CaseStatement) =>
    {
    lazy val matching: Seq[hydra.core.Field] = hydra.lib.lists.filter[hydra.core.Field]((f: hydra.core.Field) =>
      hydra.lib.equality.equal[scala.Predef.String](f.name)(fieldName))(cs.cases)
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Field]](hydra.lib.lists.`null`[hydra.core.Field](matching))(Left(hydra.context.InContext(hydra.errors.Error.other("not enough cases"),
       cx)))(Right(hydra.lib.lists.head[hydra.core.Field](matching)))
  })
}

def cases(cx: hydra.context.Context)(name: hydra.core.Name)(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.core.CaseStatement] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Term, hydra.core.CaseStatement](hydra.lexical.stripAndDereferenceTerm(cx)(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.function(v_Term_function_function) => v_Term_function_function match
    case hydra.core.Function.elimination(v_Function_elimination_elimination) => v_Function_elimination_elimination match
      case hydra.core.Elimination.union(v_Elimination_union_cs) => hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error],
         hydra.core.CaseStatement]](hydra.lib.equality.equal[scala.Predef.String](v_Elimination_union_cs.typeName)(name))(Right(v_Elimination_union_cs))(Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")(hydra.lib.strings.cat2("case statement for type ")(name)))(" but found "))(hydra.show.core.term(term))),
         cx)))
      case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("case statement"))(" but found "))(hydra.show.core.term(term))),
         cx))
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("case statement"))(" but found "))(hydra.show.core.term(term))),
       cx))
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("case statement"))(" but found "))(hydra.show.core.term(term))),
     cx)))

def eitherTerm[T0, T1](cx: hydra.context.Context)(leftFun: (hydra.core.Term => Either[hydra.context.InContext[hydra.errors.Error],
   T0]))(rightFun: (hydra.core.Term => Either[hydra.context.InContext[hydra.errors.Error], T1]))(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   Either[T0, T1]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Term, Either[T0, T1]](hydra.lexical.stripAndDereferenceTerm(cx)(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.either(v_Term_either_et) => hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term,
     Either[hydra.context.InContext[hydra.errors.Error], Either[T0, T1]]]((l: hydra.core.Term) =>
    hydra.lib.eithers.map[T0, Either[T0, T1], hydra.context.InContext[hydra.errors.Error]]((x: T0) => Left(x))(leftFun(l)))((r: hydra.core.Term) =>
    hydra.lib.eithers.map[T1, Either[T0, T1], hydra.context.InContext[hydra.errors.Error]]((x: T1) => Right(x))(rightFun(r)))(v_Term_either_et)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("either value"))(" but found "))(hydra.show.core.term(term))),
     cx)))

def eitherType(cx: hydra.context.Context)(typ: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.EitherType] =
  {
  lazy val stripped: hydra.core.Type = hydra.rewriting.deannotateType(typ)
  stripped match
    case hydra.core.Type.either(v_Type_either_et) => Right(v_Type_either_et)
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("either type"))(" but found "))(hydra.show.core.`type`(typ))),
       cx))
}

def field[T0](cx: hydra.context.Context)(fname: hydra.core.Name)(mapping: (hydra.core.Term => Either[hydra.context.InContext[hydra.errors.Error],
   T0]))(graph: hydra.graph.Graph)(fields: Seq[hydra.core.Field]): Either[hydra.context.InContext[hydra.errors.Error],
   T0] =
  {
  lazy val matchingFields: Seq[hydra.core.Field] = hydra.lib.lists.filter[hydra.core.Field]((f: hydra.core.Field) => hydra.lib.equality.equal[scala.Predef.String](f.name)(fname))(fields)
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], T0]](hydra.lib.lists.`null`[hydra.core.Field](matchingFields))(Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")(hydra.lib.strings.cat2("field ")(fname)))(" but found "))("no matching field")),
     cx)))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], T0]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Field](matchingFields))(1))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
     hydra.core.Term, T0](hydra.lexical.stripAndDereferenceTerm(cx)(graph)(hydra.lib.lists.head[hydra.core.Field](matchingFields).term))((stripped: hydra.core.Term) => mapping(stripped)))(Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("single field"))(" but found "))(hydra.lib.strings.cat2("multiple fields named ")(fname))),
     cx))))
}

def float32(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], Float] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Literal, Float](hydra.extract.core.literal(cx)(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.FloatValue, Float](hydra.extract.core.floatLiteral(cx)(l))((f: hydra.core.FloatValue) => hydra.extract.core.float32Value(cx)(f)))

def float32Value(cx: hydra.context.Context)(v: hydra.core.FloatValue): Either[hydra.context.InContext[hydra.errors.Error], Float] =
  v match
  case hydra.core.FloatValue.float32(v_FloatValue_float32_f) => Right(v_FloatValue_float32_f)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("float32"))(" but found "))(hydra.show.core.float(v))),
     cx))

def float64(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], Double] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Literal, Double](hydra.extract.core.literal(cx)(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.FloatValue, Double](hydra.extract.core.floatLiteral(cx)(l))((f: hydra.core.FloatValue) => hydra.extract.core.float64Value(cx)(f)))

def float64Value(cx: hydra.context.Context)(v: hydra.core.FloatValue): Either[hydra.context.InContext[hydra.errors.Error], Double] =
  v match
  case hydra.core.FloatValue.float64(v_FloatValue_float64_f) => Right(v_FloatValue_float64_f)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("float64"))(" but found "))(hydra.show.core.float(v))),
     cx))

def floatLiteral(cx: hydra.context.Context)(lit: hydra.core.Literal): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.FloatValue] =
  lit match
  case hydra.core.Literal.float(v_Literal_float_v) => Right(v_Literal_float_v)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("floating-point value"))(" but found "))(hydra.show.core.literal(lit))),
     cx))

def floatValue(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.core.FloatValue] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Literal, hydra.core.FloatValue](hydra.extract.core.literal(cx)(graph)(t))((l: hydra.core.Literal) => hydra.extract.core.floatLiteral(cx)(l))

def functionType(cx: hydra.context.Context)(typ: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.FunctionType] =
  {
  lazy val stripped: hydra.core.Type = hydra.rewriting.deannotateType(typ)
  stripped match
    case hydra.core.Type.function(v_Type_function_ft) => Right(v_Type_function_ft)
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("function type"))(" but found "))(hydra.show.core.`type`(typ))),
       cx))
}

def injection(cx: hydra.context.Context)(expected: hydra.core.Name)(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.core.Field] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Term, hydra.core.Field](hydra.lexical.stripAndDereferenceTerm(cx)(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.union(v_Term_union_injection) => hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error],
     hydra.core.Field]](hydra.lib.equality.equal[scala.Predef.String](v_Term_union_injection.typeName)(expected))(Right(v_Term_union_injection.field))(Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")(hydra.lib.strings.cat2("injection of type ")(expected)))(" but found "))(v_Term_union_injection.typeName)),
     cx)))
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("injection"))(" but found "))(hydra.show.core.term(term))),
     cx)))

def int16(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], Short] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Literal, Short](hydra.extract.core.literal(cx)(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.IntegerValue, Short](hydra.extract.core.integerLiteral(cx)(l))((i: hydra.core.IntegerValue) => hydra.extract.core.int16Value(cx)(i)))

def int16Value(cx: hydra.context.Context)(v: hydra.core.IntegerValue): Either[hydra.context.InContext[hydra.errors.Error], Short] =
  v match
  case hydra.core.IntegerValue.int16(v_IntegerValue_int16_i) => Right(v_IntegerValue_int16_i)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("int16"))(" but found "))(hydra.show.core.integer(v))),
     cx))

def int32(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], Int] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Literal, Int](hydra.extract.core.literal(cx)(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.IntegerValue, Int](hydra.extract.core.integerLiteral(cx)(l))((i: hydra.core.IntegerValue) => hydra.extract.core.int32Value(cx)(i)))

def int32Value(cx: hydra.context.Context)(v: hydra.core.IntegerValue): Either[hydra.context.InContext[hydra.errors.Error], Int] =
  v match
  case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(v_IntegerValue_int32_i)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("int32"))(" but found "))(hydra.show.core.integer(v))),
     cx))

def int64(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], Long] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Literal, Long](hydra.extract.core.literal(cx)(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.IntegerValue, Long](hydra.extract.core.integerLiteral(cx)(l))((i: hydra.core.IntegerValue) => hydra.extract.core.int64Value(cx)(i)))

def int64Value(cx: hydra.context.Context)(v: hydra.core.IntegerValue): Either[hydra.context.InContext[hydra.errors.Error], Long] =
  v match
  case hydra.core.IntegerValue.int64(v_IntegerValue_int64_i) => Right(v_IntegerValue_int64_i)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("int64"))(" but found "))(hydra.show.core.integer(v))),
     cx))

def int8(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], Byte] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Literal, Byte](hydra.extract.core.literal(cx)(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.IntegerValue, Byte](hydra.extract.core.integerLiteral(cx)(l))((i: hydra.core.IntegerValue) => hydra.extract.core.int8Value(cx)(i)))

def int8Value(cx: hydra.context.Context)(v: hydra.core.IntegerValue): Either[hydra.context.InContext[hydra.errors.Error], Byte] =
  v match
  case hydra.core.IntegerValue.int8(v_IntegerValue_int8_i) => Right(v_IntegerValue_int8_i)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("int8"))(" but found "))(hydra.show.core.integer(v))),
     cx))

def integerLiteral(cx: hydra.context.Context)(lit: hydra.core.Literal): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.IntegerValue] =
  lit match
  case hydra.core.Literal.integer(v_Literal_integer_v) => Right(v_Literal_integer_v)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("integer value"))(" but found "))(hydra.show.core.literal(lit))),
     cx))

def integerValue(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.core.IntegerValue] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Literal, hydra.core.IntegerValue](hydra.extract.core.literal(cx)(graph)(t))((l: hydra.core.Literal) => hydra.extract.core.integerLiteral(cx)(l))

def lambda(cx: hydra.context.Context)(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.core.Lambda] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Term, hydra.core.Lambda](hydra.lexical.stripAndDereferenceTerm(cx)(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.function(v_Term_function_function) => v_Term_function_function match
    case hydra.core.Function.lambda(v_Function_lambda_l) => Right(v_Function_lambda_l)
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("lambda"))(" but found "))(hydra.show.core.term(term))),
       cx))
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("lambda"))(" but found "))(hydra.show.core.term(term))),
     cx)))

def lambdaBody(cx: hydra.context.Context)(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.core.Term] =
  hydra.lib.eithers.map[hydra.core.Lambda, hydra.core.Term, hydra.context.InContext[hydra.errors.Error]]((x: hydra.core.Lambda) => (x.body))(hydra.extract.core.lambda(cx)(graph)(term))

def let(cx: hydra.context.Context)(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Let] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Term, hydra.core.Let](hydra.lexical.stripAndDereferenceTerm(cx)(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.let(v_Term_let_lt) => Right(v_Term_let_lt)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("let term"))(" but found "))(hydra.show.core.term(term))),
     cx)))

def letBinding(cx: hydra.context.Context)(n: scala.Predef.String)(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.core.Term] =
  {
  lazy val name: hydra.core.Name = n
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Let, hydra.core.Term](hydra.extract.core.let(cx)(graph)(term))((letExpr: hydra.core.Let) =>
    {
    lazy val matchingBindings: Seq[hydra.core.Binding] = hydra.lib.lists.filter[hydra.core.Binding]((b: hydra.core.Binding) => hydra.lib.equality.equal[scala.Predef.String](b.name)(name))(letExpr.bindings)
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Term]](hydra.lib.lists.`null`[hydra.core.Binding](matchingBindings))(Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2("no such binding: ")(n)),
       cx)))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Term]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Binding](matchingBindings))(1))(Right(hydra.lib.lists.head[hydra.core.Binding](matchingBindings).term))(Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2("multiple bindings named ")(n)),
       cx))))
  })
}

def list(cx: hydra.context.Context)(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   Seq[hydra.core.Term]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Term, Seq[hydra.core.Term]](hydra.lexical.stripAndDereferenceTerm(cx)(graph)(term))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.list(v_Term_list_l) => Right(v_Term_list_l)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("list"))(" but found "))(hydra.show.core.term(stripped))),
     cx)))

def listHead(cx: hydra.context.Context)(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(cx)(graph)(term))((l: Seq[hydra.core.Term]) =>
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Term]](hydra.lib.lists.`null`[hydra.core.Term](l))(Left(hydra.context.InContext(hydra.errors.Error.other("empty list"),
     cx)))(Right(hydra.lib.lists.head[hydra.core.Term](l))))

def listOf[T0](cx: hydra.context.Context)(f: (hydra.core.Term => Either[hydra.context.InContext[hydra.errors.Error],
   T0]))(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   Seq[T0]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.Term], Seq[T0]](hydra.extract.core.list(cx)(graph)(term))((els: Seq[hydra.core.Term]) =>
  hydra.lib.eithers.mapList[hydra.core.Term, T0, hydra.context.InContext[hydra.errors.Error]](f)(els))

def listType(cx: hydra.context.Context)(typ: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type] =
  {
  lazy val stripped: hydra.core.Type = hydra.rewriting.deannotateType(typ)
  stripped match
    case hydra.core.Type.list(v_Type_list_t) => Right(v_Type_list_t)
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("list type"))(" but found "))(hydra.show.core.`type`(typ))),
       cx))
}

def literal(cx: hydra.context.Context)(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.core.Literal] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Term, hydra.core.Literal](hydra.lexical.stripAndDereferenceTerm(cx)(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.literal(v_Term_literal_lit) => Right(v_Term_literal_lit)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("literal"))(" but found "))(hydra.show.core.term(term))),
     cx)))

def map[T0, T1](cx: hydra.context.Context)(fk: (hydra.core.Term => Either[hydra.context.InContext[hydra.errors.Error],
   T0]))(fv: (hydra.core.Term => Either[hydra.context.InContext[hydra.errors.Error], T1]))(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   Map[T0, T1]] =
  {
  def pair(kvPair: Tuple2[hydra.core.Term, hydra.core.Term]): Either[hydra.context.InContext[hydra.errors.Error], Tuple2[T0, T1]] =
    {
    lazy val kterm: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](kvPair)
    lazy val vterm: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kvPair)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], T0, Tuple2[T0, T1]](fk(kterm))((kval: T0) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], T1, Tuple2[T0, T1]](fv(vterm))((vval: T1) => Right(Tuple2(kval, vval))))
  }
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Term, Map[T0, T1]](hydra.lexical.stripAndDereferenceTerm(cx)(graph)(term0))((term: hydra.core.Term) =>
    term match
    case hydra.core.Term.map(v_Term_map_m) => hydra.lib.eithers.map[Seq[Tuple2[T0, T1]], Map[T0, T1],
       hydra.context.InContext[hydra.errors.Error]](hydra.lib.maps.fromList[T0, T1])(hydra.lib.eithers.mapList[Tuple2[hydra.core.Term,
       hydra.core.Term], Tuple2[T0, T1], hydra.context.InContext[hydra.errors.Error]](pair)(hydra.lib.maps.toList[hydra.core.Term,
       hydra.core.Term](v_Term_map_m)))
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("map"))(" but found "))(hydra.show.core.term(term))),
       cx)))
}

def mapType(cx: hydra.context.Context)(typ: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.MapType] =
  {
  lazy val stripped: hydra.core.Type = hydra.rewriting.deannotateType(typ)
  stripped match
    case hydra.core.Type.map(v_Type_map_mt) => Right(v_Type_map_mt)
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("map type"))(" but found "))(hydra.show.core.`type`(typ))),
       cx))
}

def maybeTerm[T0](cx: hydra.context.Context)(f: (hydra.core.Term => Either[hydra.context.InContext[hydra.errors.Error],
   T0]))(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   Option[T0]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Term, Option[T0]](hydra.lexical.stripAndDereferenceTerm(cx)(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.maybe(v_Term_maybe_mt) => hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error],
     Option[T0]], hydra.core.Term](Right(None))((t: hydra.core.Term) =>
    hydra.lib.eithers.map[T0, Option[T0], hydra.context.InContext[hydra.errors.Error]](hydra.lib.maybes.pure[T0])(f(t)))(v_Term_maybe_mt)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("maybe value"))(" but found "))(hydra.show.core.term(term))),
     cx)))

def maybeType(cx: hydra.context.Context)(typ: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type] =
  {
  lazy val stripped: hydra.core.Type = hydra.rewriting.deannotateType(typ)
  stripped match
    case hydra.core.Type.maybe(v_Type_maybe_t) => Right(v_Type_maybe_t)
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("maybe type"))(" but found "))(hydra.show.core.`type`(typ))),
       cx))
}

def nArgs[T0](cx: hydra.context.Context)(name: hydra.core.Name)(n: Int)(args: Seq[T0]): Either[hydra.context.InContext[hydra.errors.Error], Unit] =
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Unit]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[T0](args))(n))(Right(()))(Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")(hydra.lib.strings.cat(Seq(hydra.lib.literals.showInt32(n),
     " arguments to primitive ", hydra.lib.literals.showString(name)))))(" but found "))(hydra.lib.literals.showInt32(hydra.lib.lists.length[T0](args)))),
     cx)))

def pair[T0, T1](cx: hydra.context.Context)(kf: (hydra.core.Term => Either[hydra.context.InContext[hydra.errors.Error],
   T0]))(vf: (hydra.core.Term => Either[hydra.context.InContext[hydra.errors.Error], T1]))(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   Tuple2[T0, T1]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Term, Tuple2[T0, T1]](hydra.lexical.stripAndDereferenceTerm(cx)(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
     T0, Tuple2[T0, T1]](kf(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)))((kVal: T0) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], T1, Tuple2[T0, T1]](vf(hydra.lib.pairs.second[hydra.core.Term,
       hydra.core.Term](v_Term_pair_p)))((vVal: T1) => Right(Tuple2(kVal, vVal))))
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("pair"))(" but found "))(hydra.show.core.term(term))),
     cx)))

def record(cx: hydra.context.Context)(expected: hydra.core.Name)(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   Seq[hydra.core.Field]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Record, Seq[hydra.core.Field]](hydra.extract.core.termRecord(cx)(graph)(term0))((record: hydra.core.Record) =>
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.Field]]](hydra.lib.equality.equal[hydra.core.Name](record.typeName)(expected))(Right(record.fields))(Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")(hydra.lib.strings.cat2("record of type ")(expected)))(" but found "))(record.typeName)),
     cx))))

def recordType[T0](cx: hydra.context.Context)(ename: T0)(typ: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.FieldType]] =
  {
  lazy val stripped: hydra.core.Type = hydra.rewriting.deannotateType(typ)
  stripped match
    case hydra.core.Type.record(v_Type_record_fields) => Right(v_Type_record_fields)
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("record type"))(" but found "))(hydra.show.core.`type`(typ))),
       cx))
}

def set(cx: hydra.context.Context)(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   scala.collection.immutable.Set[hydra.core.Term]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Term, scala.collection.immutable.Set[hydra.core.Term]](hydra.lexical.stripAndDereferenceTerm(cx)(graph)(term))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.set(v_Term_set_s) => Right(v_Term_set_s)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("set"))(" but found "))(hydra.show.core.term(stripped))),
     cx)))

def setOf[T0](cx: hydra.context.Context)(f: (hydra.core.Term => Either[hydra.context.InContext[hydra.errors.Error],
   T0]))(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   scala.collection.immutable.Set[T0]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], scala.collection.immutable.Set[hydra.core.Term],
     scala.collection.immutable.Set[T0]](hydra.extract.core.set(cx)(graph)(term))((els: scala.collection.immutable.Set[hydra.core.Term]) =>
  hydra.lib.eithers.mapSet[hydra.core.Term, T0, hydra.context.InContext[hydra.errors.Error]](f)(els))

def setType(cx: hydra.context.Context)(typ: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type] =
  {
  lazy val stripped: hydra.core.Type = hydra.rewriting.deannotateType(typ)
  stripped match
    case hydra.core.Type.set(v_Type_set_t) => Right(v_Type_set_t)
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("set type"))(" but found "))(hydra.show.core.`type`(typ))),
       cx))
}

def string(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], scala.Predef.String] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Literal, scala.Predef.String](hydra.extract.core.literal(cx)(graph)(t))((l: hydra.core.Literal) => hydra.extract.core.stringLiteral(cx)(l))

def stringLiteral(cx: hydra.context.Context)(v: hydra.core.Literal): Either[hydra.context.InContext[hydra.errors.Error], scala.Predef.String] =
  v match
  case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("string"))(" but found "))(hydra.show.core.literal(v))),
     cx))

def termRecord(cx: hydra.context.Context)(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.core.Record] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Term, hydra.core.Record](hydra.lexical.stripAndDereferenceTerm(cx)(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.record(v_Term_record_record) => Right(v_Term_record_record)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("record"))(" but found "))(hydra.show.core.term(term))),
     cx)))

def uint16(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], Int] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Literal, Int](hydra.extract.core.literal(cx)(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.IntegerValue, Int](hydra.extract.core.integerLiteral(cx)(l))((i: hydra.core.IntegerValue) => hydra.extract.core.uint16Value(cx)(i)))

def uint16Value(cx: hydra.context.Context)(v: hydra.core.IntegerValue): Either[hydra.context.InContext[hydra.errors.Error], Int] =
  v match
  case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_i) => Right(v_IntegerValue_uint16_i)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("uint16"))(" but found "))(hydra.show.core.integer(v))),
     cx))

def uint32(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], Long] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Literal, Long](hydra.extract.core.literal(cx)(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.IntegerValue, Long](hydra.extract.core.integerLiteral(cx)(l))((i: hydra.core.IntegerValue) => hydra.extract.core.uint32Value(cx)(i)))

def uint32Value(cx: hydra.context.Context)(v: hydra.core.IntegerValue): Either[hydra.context.InContext[hydra.errors.Error], Long] =
  v match
  case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_i) => Right(v_IntegerValue_uint32_i)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("uint32"))(" but found "))(hydra.show.core.integer(v))),
     cx))

def uint64(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], BigInt] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Literal, BigInt](hydra.extract.core.literal(cx)(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.IntegerValue, BigInt](hydra.extract.core.integerLiteral(cx)(l))((i: hydra.core.IntegerValue) => hydra.extract.core.uint64Value(cx)(i)))

def uint64Value(cx: hydra.context.Context)(v: hydra.core.IntegerValue): Either[hydra.context.InContext[hydra.errors.Error], BigInt] =
  v match
  case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_i) => Right(v_IntegerValue_uint64_i)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("uint64"))(" but found "))(hydra.show.core.integer(v))),
     cx))

def uint8(cx: hydra.context.Context)(graph: hydra.graph.Graph)(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], Byte] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Literal, Byte](hydra.extract.core.literal(cx)(graph)(t))((l: hydra.core.Literal) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.IntegerValue, Byte](hydra.extract.core.integerLiteral(cx)(l))((i: hydra.core.IntegerValue) => hydra.extract.core.uint8Value(cx)(i)))

def uint8Value(cx: hydra.context.Context)(v: hydra.core.IntegerValue): Either[hydra.context.InContext[hydra.errors.Error], Byte] =
  v match
  case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_i) => Right(v_IntegerValue_uint8_i)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("uint8"))(" but found "))(hydra.show.core.integer(v))),
     cx))

def unionType[T0](cx: hydra.context.Context)(ename: T0)(typ: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.FieldType]] =
  {
  lazy val stripped: hydra.core.Type = hydra.rewriting.deannotateType(typ)
  stripped match
    case hydra.core.Type.union(v_Type_union_fields) => Right(v_Type_union_fields)
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("union type"))(" but found "))(hydra.show.core.`type`(typ))),
       cx))
}

def unit(cx: hydra.context.Context)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], Unit] =
  term match
  case hydra.core.Term.unit => Right(())
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("unit"))(" but found "))(hydra.show.core.term(term))),
     cx))

def unitVariant(cx: hydra.context.Context)(tname: hydra.core.Name)(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.core.Name] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Field, hydra.core.Name](hydra.extract.core.injection(cx)(tname)(graph)(term))((field: hydra.core.Field) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Unit, hydra.core.Name](hydra.extract.core.unit(cx)(field.term))((ignored: Unit) => Right(field.name)))

def wrap(cx: hydra.context.Context)(expected: hydra.core.Name)(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Term, hydra.core.Term](hydra.lexical.stripAndDereferenceTerm(cx)(graph)(term0))((term: hydra.core.Term) =>
  term match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error],
     hydra.core.Term]](hydra.lib.equality.equal[scala.Predef.String](v_Term_wrap_wrappedTerm.typeName)(expected))(Right(v_Term_wrap_wrappedTerm.body))(Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")(hydra.lib.strings.cat2("wrapper of type ")(expected)))(" but found "))(v_Term_wrap_wrappedTerm.typeName)),
     cx)))
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")(hydra.lib.strings.cat2(hydra.lib.strings.cat2("wrap(")(expected))(")")))(" but found "))(hydra.show.core.term(term))),
     cx)))

def wrappedType[T0](cx: hydra.context.Context)(ename: T0)(typ: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type] =
  {
  lazy val stripped: hydra.core.Type = hydra.rewriting.deannotateType(typ)
  stripped match
    case hydra.core.Type.wrap(v_Type_wrap_innerType) => Right(v_Type_wrap_innerType)
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ")("wrapped type"))(" but found "))(hydra.show.core.`type`(typ))),
       cx))
}
