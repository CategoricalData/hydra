package hydra.ext.python.coder

import hydra.coders.*

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.ext.python.environment.*

import hydra.ext.python.syntax.*

import hydra.graph.*

import hydra.module.*

import hydra.typing.*

import hydra.util.*

import hydra.lib.eithers

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.literals

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.math

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

def useInlineTypeParamsFor(version: hydra.ext.python.environment.PythonVersion): Boolean =
  hydra.lib.equality.equal[hydra.ext.python.environment.PythonVersion](version)(hydra.ext.python.environment.PythonVersion.python312)

lazy val useInlineTypeParams: Boolean = hydra.ext.python.coder.useInlineTypeParamsFor(hydra.ext.python.utils.targetPythonVersion)

def typeAliasStatementFor(env: hydra.ext.python.environment.PythonEnvironment)(name: hydra.ext.python.syntax.Name)(tparams: Seq[hydra.ext.python.syntax.TypeParameter])(mcomment: Option[scala.Predef.String])(tyexpr: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Statement =
  hydra.lib.logic.ifElse[hydra.ext.python.syntax.Statement](hydra.ext.python.coder.useInlineTypeParamsFor(env.version))(hydra.ext.python.utils.typeAliasStatement(name)(tparams)(mcomment)(tyexpr))(hydra.ext.python.utils.typeAliasStatement310(name)(tparams)(mcomment)(tyexpr))

def unionTypeStatementsFor(env: hydra.ext.python.environment.PythonEnvironment)(name: hydra.ext.python.syntax.Name)(tparams: Seq[hydra.ext.python.syntax.TypeParameter])(mcomment: Option[scala.Predef.String])(tyexpr: hydra.ext.python.syntax.Expression)(extraStmts: Seq[hydra.ext.python.syntax.Statement]): Seq[hydra.ext.python.syntax.Statement] =
  hydra.lib.logic.ifElse[Seq[hydra.ext.python.syntax.Statement]](hydra.ext.python.coder.useInlineTypeParamsFor(env.version))(hydra.lib.lists.concat2[hydra.ext.python.syntax.Statement](Seq(hydra.ext.python.utils.typeAliasStatement(name)(tparams)(mcomment)(tyexpr)))(extraStmts))(hydra.ext.python.utils.unionTypeClassStatements310(name)(mcomment)(tyexpr)(extraStmts))

def wrapInNullaryLambda(expr: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Expression =
  hydra.ext.python.syntax.Expression.lambda(hydra.ext.python.syntax.Lambda(hydra.ext.python.syntax.LambdaParameters(None, Seq(), Seq(), None), expr))

def wrapLazyArguments(name: hydra.core.Name)(args: Seq[hydra.ext.python.syntax.Expression]): Seq[hydra.ext.python.syntax.Expression] =
  hydra.lib.logic.ifElse[Seq[hydra.ext.python.syntax.Expression]](hydra.lib.logic.and(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.logic.ifElse"))(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ext.python.syntax.Expression](args))(3)))(Seq(hydra.lib.lists.at[hydra.ext.python.syntax.Expression](0)(args), hydra.ext.python.coder.wrapInNullaryLambda(hydra.lib.lists.at[hydra.ext.python.syntax.Expression](1)(args)), hydra.ext.python.coder.wrapInNullaryLambda(hydra.lib.lists.at[hydra.ext.python.syntax.Expression](2)(args))))(hydra.lib.logic.ifElse[Seq[hydra.ext.python.syntax.Expression]](hydra.lib.logic.and(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.cases"))(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ext.python.syntax.Expression](args))(3)))(Seq(hydra.lib.lists.at[hydra.ext.python.syntax.Expression](0)(args), hydra.ext.python.coder.wrapInNullaryLambda(hydra.lib.lists.at[hydra.ext.python.syntax.Expression](1)(args)), hydra.lib.lists.at[hydra.ext.python.syntax.Expression](2)(args)))(hydra.lib.logic.ifElse[Seq[hydra.ext.python.syntax.Expression]](hydra.lib.logic.and(hydra.lib.logic.or(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.maybe"))(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.fromMaybe")))(hydra.lib.equality.gte[Int](hydra.lib.lists.length[hydra.ext.python.syntax.Expression](args))(1)))(hydra.lib.lists.cons[hydra.ext.python.syntax.Expression](hydra.ext.python.coder.wrapInNullaryLambda(hydra.lib.lists.at[hydra.ext.python.syntax.Expression](0)(args)))(hydra.lib.lists.tail[hydra.ext.python.syntax.Expression](args)))(args)))

def pyInt(n: BigInt): hydra.ext.python.syntax.Expression =
  hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.number(hydra.ext.python.syntax.Number.integer(n)))

lazy val lruCacheDecorator: hydra.ext.python.syntax.NamedExpression = hydra.ext.python.syntax.NamedExpression.simple(hydra.ext.python.utils.functionCall(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("lru_cache")))(Seq(hydra.ext.python.coder.pyInt(BigInt(1L)))))

def makeThunk(pbody: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Expression =
  hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyExpressionToPyPrimary(hydra.ext.python.utils.functionCall(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("lru_cache")))(Seq(hydra.ext.python.coder.pyInt(BigInt(1L))))))(Seq(hydra.ext.python.coder.wrapInNullaryLambda(pbody)))

def makeCurriedLambda(params: Seq[hydra.ext.python.syntax.Name])(body: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Expression =
  hydra.lib.lists.foldl[hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Name]((acc: hydra.ext.python.syntax.Expression) =>
  (p: hydra.ext.python.syntax.Name) =>
  hydra.ext.python.syntax.Expression.lambda(hydra.ext.python.syntax.Lambda(hydra.ext.python.syntax.LambdaParameters(None, Seq(p), Seq(), None), acc)))(body)(hydra.lib.lists.reverse[hydra.ext.python.syntax.Name](params))

def genericArg(tparamList: Seq[hydra.core.Name]): Option[hydra.ext.python.syntax.Expression] =
  hydra.lib.logic.ifElse[Option[hydra.ext.python.syntax.Expression]](hydra.lib.lists.`null`[hydra.core.Name](tparamList))(None)(Some(hydra.ext.python.utils.pyPrimaryToPyExpression(hydra.ext.python.utils.primaryWithExpressionSlices(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("Generic")))(hydra.lib.lists.map[hydra.core.Name, hydra.ext.python.syntax.Expression]((n: hydra.core.Name) =>
  hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None, hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None, hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false, hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name(hydra.ext.python.names.encodeTypeVariable(n)))), None)))))))), Seq()))))))(tparamList)))))

def variantArgs(ptype: hydra.ext.python.syntax.Expression)(tparams: Seq[hydra.core.Name]): hydra.ext.python.syntax.Args =
  hydra.ext.python.utils.pyExpressionsToPyArgs(hydra.lib.maybes.cat[hydra.ext.python.syntax.Expression](Seq(Some(hydra.ext.python.utils.pyPrimaryToPyExpression(hydra.ext.python.utils.primaryWithExpressionSlices(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("Node")))(Seq(ptype)))), hydra.ext.python.coder.genericArg(tparams))))

def environmentTypeParameters(env: hydra.ext.python.environment.PythonEnvironment): Seq[hydra.ext.python.syntax.TypeParameter] =
  hydra.lib.lists.map[hydra.core.Name, hydra.ext.python.syntax.TypeParameter]((`arg_`: hydra.core.Name) =>
  hydra.ext.python.utils.pyNameToPyTypeParameter(hydra.ext.python.names.encodeTypeVariable(`arg_`)))(hydra.lib.pairs.first[Seq[hydra.core.Name], Map[hydra.core.Name, hydra.ext.python.syntax.Name]](env.boundTypeVariables))

def encodeFloatValue[T0](fv: hydra.core.FloatValue): Either[T0, hydra.ext.python.syntax.Expression] =
  fv match
  case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_f) => Right(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary("Decimal"))(Seq(hydra.ext.python.utils.singleQuotedString(hydra.lib.literals.showBigfloat(v_FloatValue_bigfloat_f)))))
  case hydra.core.FloatValue.float32(v_FloatValue_float32_f) => Right(hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.number(hydra.ext.python.syntax.Number.float(hydra.lib.literals.float32ToBigfloat(v_FloatValue_float32_f)))))
  case hydra.core.FloatValue.float64(v_FloatValue_float64_f) => Right(hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.number(hydra.ext.python.syntax.Number.float(hydra.lib.literals.float64ToBigfloat(v_FloatValue_float64_f)))))

def encodeIntegerValue[T0](iv: hydra.core.IntegerValue): Either[T0, hydra.ext.python.syntax.Expression] =
  {
  def toPyInt[T1](n: BigInt): Either[T1, hydra.ext.python.syntax.Expression] =
    Right(hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.number(hydra.ext.python.syntax.Number.integer(n))))
  iv match
    case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_i) => toPyInt(v_IntegerValue_bigint_i)
    case hydra.core.IntegerValue.int8(v_IntegerValue_int8_i) => toPyInt(hydra.lib.literals.int8ToBigint(v_IntegerValue_int8_i))
    case hydra.core.IntegerValue.int16(v_IntegerValue_int16_i) => toPyInt(hydra.lib.literals.int16ToBigint(v_IntegerValue_int16_i))
    case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => toPyInt(hydra.lib.literals.int32ToBigint(v_IntegerValue_int32_i))
    case hydra.core.IntegerValue.int64(v_IntegerValue_int64_i) => toPyInt(hydra.lib.literals.int64ToBigint(v_IntegerValue_int64_i))
    case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_i) => toPyInt(hydra.lib.literals.uint8ToBigint(v_IntegerValue_uint8_i))
    case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_i) => toPyInt(hydra.lib.literals.uint16ToBigint(v_IntegerValue_uint16_i))
    case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_i) => toPyInt(hydra.lib.literals.uint32ToBigint(v_IntegerValue_uint32_i))
    case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_i) => toPyInt(hydra.lib.literals.uint64ToBigint(v_IntegerValue_uint64_i))
}

def encodeLiteral[T0](lit: hydra.core.Literal): Either[T0, hydra.ext.python.syntax.Expression] =
  lit match
  case hydra.core.Literal.binary(v_Literal_binary_bs) => {
    lazy val byteValues: Seq[Int] = hydra.lib.literals.binaryToBytes(v_Literal_binary_bs)
    Right(hydra.ext.python.utils.functionCall(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("bytes")))(Seq(hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.list(hydra.ext.python.utils.pyList(hydra.lib.lists.map[Int, hydra.ext.python.syntax.Expression]((byteVal: Int) =>
      hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.number(hydra.ext.python.syntax.Number.integer(hydra.lib.literals.int32ToBigint(byteVal)))))(byteValues)))))))
  }
  case hydra.core.Literal.boolean(v_Literal_boolean_b) => Right(hydra.ext.python.utils.pyAtomToPyExpression(hydra.lib.logic.ifElse[hydra.ext.python.syntax.Atom](v_Literal_boolean_b)(hydra.ext.python.syntax.Atom.`true`)(hydra.ext.python.syntax.Atom.`false`)))
  case hydra.core.Literal.float(v_Literal_float_f) => hydra.ext.python.coder.encodeFloatValue(v_Literal_float_f)
  case hydra.core.Literal.integer(v_Literal_integer_i) => hydra.ext.python.coder.encodeIntegerValue(v_Literal_integer_i)
  case hydra.core.Literal.string(v_Literal_string_s) => Right(hydra.ext.python.utils.stringToPyExpression(hydra.ext.python.syntax.QuoteStyle.double)(v_Literal_string_s))

def encodeLiteralType[T0](lt: hydra.core.LiteralType): Either[T0, hydra.ext.python.syntax.Expression] =
  {
  lazy val findName: scala.Predef.String = lt match
    case hydra.core.LiteralType.binary => "bytes"
    case hydra.core.LiteralType.boolean => "bool"
    case hydra.core.LiteralType.float(v_LiteralType_float_ft) => v_LiteralType_float_ft match
      case hydra.core.FloatType.bigfloat => "Decimal"
      case hydra.core.FloatType.float32 => "float"
      case hydra.core.FloatType.float64 => "float"
    case hydra.core.LiteralType.integer(_) => "int"
    case hydra.core.LiteralType.string => "str"
  Right(hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None, hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None, hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false, hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name(findName))), None)))))))), Seq()))))))
}

def encodeApplicationType[T0](env: hydra.ext.python.environment.PythonEnvironment)(at: hydra.core.ApplicationType): Either[T0, hydra.ext.python.syntax.Expression] =
  {
  def gatherParams(t: hydra.core.Type)(ps: Seq[hydra.core.Type]): Tuple2[hydra.core.Type, Seq[hydra.core.Type]] =
    hydra.rewriting.deannotateType(t) match
    case hydra.core.Type.application(v_Type_application_appT) => gatherParams(v_Type_application_appT.function)(hydra.lib.lists.cons[hydra.core.Type](v_Type_application_appT.argument)(ps))
    case hydra.core.Type.annotated(_) => Tuple2(t, ps)
    case hydra.core.Type.function(_) => Tuple2(t, ps)
    case hydra.core.Type.forall(_) => Tuple2(t, ps)
    case hydra.core.Type.list(_) => Tuple2(t, ps)
    case hydra.core.Type.literal(_) => Tuple2(t, ps)
    case hydra.core.Type.map(_) => Tuple2(t, ps)
    case hydra.core.Type.maybe(_) => Tuple2(t, ps)
    case hydra.core.Type.either(_) => Tuple2(t, ps)
    case hydra.core.Type.pair(_) => Tuple2(t, ps)
    case hydra.core.Type.record(_) => Tuple2(t, ps)
    case hydra.core.Type.set(_) => Tuple2(t, ps)
    case hydra.core.Type.union(_) => Tuple2(t, ps)
    case hydra.core.Type.unit => Tuple2(t, ps)
    case hydra.core.Type.variable(_) => Tuple2(t, ps)
    case hydra.core.Type.void => Tuple2(t, ps)
    case hydra.core.Type.wrap(_) => Tuple2(t, ps)
  lazy val bodyAndArgs: Tuple2[hydra.core.Type, Seq[hydra.core.Type]] = gatherParams(hydra.core.Type.application(at))(Seq())
  lazy val body: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, Seq[hydra.core.Type]](bodyAndArgs)
  lazy val args: Seq[hydra.core.Type] = hydra.lib.pairs.second[hydra.core.Type, Seq[hydra.core.Type]](bodyAndArgs)
  hydra.lib.eithers.bind[T0, hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](hydra.ext.python.coder.encodeType(env)(body))((pyBody: hydra.ext.python.syntax.Expression) =>
    hydra.lib.eithers.bind[T0, Seq[hydra.ext.python.syntax.Expression], hydra.ext.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Type, hydra.ext.python.syntax.Expression, T0]((v1: hydra.core.Type) => hydra.ext.python.coder.encodeType(env)(v1))(args))((pyArgs: Seq[hydra.ext.python.syntax.Expression]) =>
    Right(hydra.ext.python.utils.primaryAndParams(hydra.ext.python.utils.pyExpressionToPyPrimary(pyBody))(pyArgs))))
}

def encodeForallType[T0](env: hydra.ext.python.environment.PythonEnvironment)(lt: hydra.core.ForallType): Either[T0, hydra.ext.python.syntax.Expression] =
  {
  def gatherParams(t: hydra.core.Type)(ps: Seq[hydra.core.Name]): Tuple2[hydra.core.Type, Seq[hydra.core.Name]] =
    hydra.rewriting.deannotateType(t) match
    case hydra.core.Type.forall(v_Type_forall_forallT) => gatherParams(v_Type_forall_forallT.body)(hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_forallT.parameter)(ps))
    case hydra.core.Type.annotated(_) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.application(_) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.function(_) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.list(_) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.literal(_) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.map(_) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.maybe(_) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.either(_) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.pair(_) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.record(_) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.set(_) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.union(_) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.unit => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.variable(_) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.void => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
    case hydra.core.Type.wrap(_) => Tuple2(t, hydra.lib.lists.reverse[hydra.core.Name](ps))
  lazy val bodyAndParams: Tuple2[hydra.core.Type, Seq[hydra.core.Name]] = gatherParams(hydra.core.Type.forall(lt))(Seq())
  lazy val body: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, Seq[hydra.core.Name]](bodyAndParams)
  lazy val params: Seq[hydra.core.Name] = hydra.lib.pairs.second[hydra.core.Type, Seq[hydra.core.Name]](bodyAndParams)
  hydra.lib.eithers.bind[T0, hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](hydra.ext.python.coder.encodeType(env)(body))((pyBody: hydra.ext.python.syntax.Expression) =>
    Right(hydra.ext.python.utils.primaryAndParams(hydra.ext.python.utils.pyExpressionToPyPrimary(pyBody))(hydra.lib.lists.map[hydra.core.Name, hydra.ext.python.syntax.Expression]((n: hydra.core.Name) =>
    hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None, hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None, hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false, hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name(n))), None)))))))), Seq()))))))(params))))
}

def encodeFunctionType[T0](env: hydra.ext.python.environment.PythonEnvironment)(ft: hydra.core.FunctionType): Either[T0, hydra.ext.python.syntax.Expression] =
  {
  def gatherParams(rdoms: Seq[hydra.core.Type])(ftype: hydra.core.FunctionType): Tuple2[Seq[hydra.core.Type], hydra.core.Type] =
    {
    lazy val innerCod: hydra.core.Type = (ftype.codomain)
    lazy val dom: hydra.core.Type = (ftype.domain)
    hydra.rewriting.deannotateType(innerCod) match
      case hydra.core.Type.function(v_Type_function_ft2) => gatherParams(hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms))(v_Type_function_ft2)
      case hydra.core.Type.annotated(_) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)), innerCod)
      case hydra.core.Type.application(_) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)), innerCod)
      case hydra.core.Type.forall(_) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)), innerCod)
      case hydra.core.Type.list(_) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)), innerCod)
      case hydra.core.Type.literal(_) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)), innerCod)
      case hydra.core.Type.map(_) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)), innerCod)
      case hydra.core.Type.maybe(_) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)), innerCod)
      case hydra.core.Type.either(_) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)), innerCod)
      case hydra.core.Type.pair(_) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)), innerCod)
      case hydra.core.Type.record(_) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)), innerCod)
      case hydra.core.Type.set(_) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)), innerCod)
      case hydra.core.Type.union(_) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)), innerCod)
      case hydra.core.Type.unit => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)), innerCod)
      case hydra.core.Type.variable(_) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)), innerCod)
      case hydra.core.Type.void => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)), innerCod)
      case hydra.core.Type.wrap(_) => Tuple2(hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](dom)(rdoms)), innerCod)
  }
  lazy val domsAndCod: Tuple2[Seq[hydra.core.Type], hydra.core.Type] = gatherParams(Seq())(ft)
  lazy val doms: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.core.Type](domsAndCod)
  lazy val cod: hydra.core.Type = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.core.Type](domsAndCod)
  hydra.lib.eithers.bind[T0, Seq[hydra.ext.python.syntax.Expression], hydra.ext.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Type, hydra.ext.python.syntax.Expression, T0]((v1: hydra.core.Type) => hydra.ext.python.coder.encodeType(env)(v1))(doms))((pydoms: Seq[hydra.ext.python.syntax.Expression]) =>
    hydra.lib.eithers.bind[T0, hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](hydra.ext.python.coder.encodeType(env)(cod))((pycod: hydra.ext.python.syntax.Expression) =>
    Right(hydra.ext.python.utils.pyPrimaryToPyExpression(hydra.ext.python.utils.primaryWithSlices(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("Callable")))(hydra.ext.python.utils.pyPrimaryToPySlice(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.list(hydra.ext.python.utils.pyList(pydoms)))))(Seq(hydra.ext.python.syntax.SliceOrStarredExpression.slice(hydra.ext.python.utils.pyExpressionToPySlice(pycod))))))))
}

def encodeType[T0](env: hydra.ext.python.environment.PythonEnvironment)(typ: hydra.core.Type): Either[T0, hydra.ext.python.syntax.Expression] =
  {
  def dflt[T1]: Either[T1, hydra.ext.python.syntax.Expression] =
    Right(hydra.ext.python.utils.doubleQuotedString(hydra.lib.strings.cat2("type = ")(hydra.show.core.`type`(hydra.rewriting.deannotateType(typ)))))
  hydra.rewriting.deannotateType(typ) match
    case hydra.core.Type.application(v_Type_application_at) => hydra.ext.python.coder.encodeApplicationType(env)(v_Type_application_at)
    case hydra.core.Type.function(v_Type_function_ft) => hydra.ext.python.coder.encodeFunctionType(env)(v_Type_function_ft)
    case hydra.core.Type.forall(v_Type_forall_lt) => hydra.ext.python.coder.encodeForallType(env)(v_Type_forall_lt)
    case hydra.core.Type.list(v_Type_list_et) => hydra.lib.eithers.bind[T0, hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](hydra.ext.python.coder.encodeType(env)(v_Type_list_et))((pyet: hydra.ext.python.syntax.Expression) =>
      Right(hydra.ext.python.utils.nameAndParams("frozenlist")(Seq(pyet))))
    case hydra.core.Type.map(v_Type_map_mt) => hydra.lib.eithers.bind[T0, hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](hydra.ext.python.coder.encodeType(env)(v_Type_map_mt.keys))((pykt: hydra.ext.python.syntax.Expression) =>
      hydra.lib.eithers.bind[T0, hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](hydra.ext.python.coder.encodeType(env)(v_Type_map_mt.values))((pyvt: hydra.ext.python.syntax.Expression) =>
      Right(hydra.ext.python.utils.nameAndParams("FrozenDict")(Seq(pykt, pyvt)))))
    case hydra.core.Type.literal(v_Type_literal_lt) => hydra.ext.python.coder.encodeLiteralType(v_Type_literal_lt)
    case hydra.core.Type.maybe(v_Type_maybe_et) => hydra.lib.eithers.bind[T0, hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](hydra.ext.python.coder.encodeType(env)(v_Type_maybe_et))((ptype: hydra.ext.python.syntax.Expression) =>
      Right(hydra.ext.python.utils.pyPrimaryToPyExpression(hydra.ext.python.utils.primaryWithExpressionSlices(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("Maybe")))(Seq(ptype)))))
    case hydra.core.Type.either(v_Type_either_eitherT) => hydra.lib.eithers.bind[T0, hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](hydra.ext.python.coder.encodeType(env)(v_Type_either_eitherT.left))((pyleft: hydra.ext.python.syntax.Expression) =>
      hydra.lib.eithers.bind[T0, hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](hydra.ext.python.coder.encodeType(env)(v_Type_either_eitherT.right))((pyright: hydra.ext.python.syntax.Expression) =>
      Right(hydra.ext.python.utils.pyPrimaryToPyExpression(hydra.ext.python.utils.primaryWithExpressionSlices(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("Either")))(Seq(pyleft, pyright))))))
    case hydra.core.Type.pair(v_Type_pair_pairT) => hydra.lib.eithers.bind[T0, hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](hydra.ext.python.coder.encodeType(env)(v_Type_pair_pairT.first))((pyFirst: hydra.ext.python.syntax.Expression) =>
      hydra.lib.eithers.bind[T0, hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](hydra.ext.python.coder.encodeType(env)(v_Type_pair_pairT.second))((pySecond: hydra.ext.python.syntax.Expression) =>
      Right(hydra.ext.python.utils.nameAndParams("tuple")(Seq(pyFirst, pySecond)))))
    case hydra.core.Type.record(_) => dflt
    case hydra.core.Type.set(v_Type_set_et) => hydra.lib.eithers.bind[T0, hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](hydra.ext.python.coder.encodeType(env)(v_Type_set_et))((pyet: hydra.ext.python.syntax.Expression) =>
      Right(hydra.ext.python.utils.nameAndParams("frozenset")(Seq(pyet))))
    case hydra.core.Type.union(_) => dflt
    case hydra.core.Type.unit => Right(hydra.ext.python.utils.pyNameToPyExpression(hydra.ext.python.utils.pyNone))
    case hydra.core.Type.void => Right(hydra.ext.python.utils.pyNameToPyExpression(hydra.ext.python.utils.pyNone))
    case hydra.core.Type.variable(v_Type_variable_name) => Right(hydra.ext.python.names.typeVariableReference(env)(v_Type_variable_name))
    case hydra.core.Type.wrap(_) => dflt
    case hydra.core.Type.annotated(_) => dflt
}

def encodeTypeQuoted[T0](env: hydra.ext.python.environment.PythonEnvironment)(typ: hydra.core.Type): Either[T0, hydra.ext.python.syntax.Expression] =
  hydra.lib.eithers.bind[T0, hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](hydra.ext.python.coder.encodeType(env)(typ))((pytype: hydra.ext.python.syntax.Expression) =>
  Right(hydra.lib.logic.ifElse[hydra.ext.python.syntax.Expression](hydra.lib.sets.`null`[hydra.core.Name](hydra.rewriting.freeVariablesInType(typ)))(pytype)(hydra.ext.python.utils.doubleQuotedString(hydra.serialization.printExpr(hydra.ext.python.serde.encodeExpression(pytype))))))

def encodeNameConstants(env: hydra.ext.python.environment.PythonEnvironment)(name: hydra.core.Name)(fields: Seq[hydra.core.FieldType]): Seq[hydra.ext.python.syntax.Statement] =
  {
  def toStmt(pair: Tuple2[hydra.ext.python.syntax.Name, hydra.core.Name]): hydra.ext.python.syntax.Statement =
    hydra.ext.python.utils.assignmentStatement(hydra.lib.pairs.first[hydra.ext.python.syntax.Name, hydra.core.Name](pair))(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary(hydra.ext.python.names.encodeName(true)(hydra.util.CaseConvention.pascal)(env)("hydra.core.Name")))(Seq(hydra.ext.python.utils.doubleQuotedString(hydra.lib.pairs.second[hydra.ext.python.syntax.Name, hydra.core.Name](pair)))))
  lazy val namePair: Tuple2[hydra.ext.python.syntax.Name, hydra.core.Name] = Tuple2(hydra.ext.python.names.encodeConstantForTypeName(env)(name), name)
  lazy val fieldPairs: Seq[Tuple2[hydra.ext.python.syntax.Name, hydra.core.Name]] = hydra.lib.lists.map[hydra.core.FieldType, Tuple2[hydra.ext.python.syntax.Name, hydra.core.Name]]((field: hydra.core.FieldType) =>
    Tuple2(hydra.ext.python.names.encodeConstantForFieldName(env)(name)(field.name), (field.name)))(fields)
  hydra.lib.lists.map[Tuple2[hydra.ext.python.syntax.Name, hydra.core.Name], hydra.ext.python.syntax.Statement](toStmt)(hydra.lib.lists.cons[Tuple2[hydra.ext.python.syntax.Name, hydra.core.Name]](namePair)(fieldPairs))
}

def findTypeParams(env: hydra.ext.python.environment.PythonEnvironment)(typ: hydra.core.Type): Seq[hydra.core.Name] =
  {
  lazy val boundVars: Map[hydra.core.Name, hydra.ext.python.syntax.Name] = hydra.lib.pairs.second[Seq[hydra.core.Name], Map[hydra.core.Name, hydra.ext.python.syntax.Name]](env.boundTypeVariables)
  def isBound(v: hydra.core.Name): Boolean =
    hydra.lib.maybes.isJust[hydra.ext.python.syntax.Name](hydra.lib.maps.lookup[hydra.core.Name, hydra.ext.python.syntax.Name](v)(boundVars))
  hydra.lib.lists.filter[hydra.core.Name](isBound)(hydra.lib.sets.toList[hydra.core.Name](hydra.rewriting.freeVariablesInType(typ)))
}

def encodeWrappedType[T0](env: hydra.ext.python.environment.PythonEnvironment)(name: hydra.core.Name)(typ: hydra.core.Type)(comment: Option[scala.Predef.String]): Either[T0, Seq[hydra.ext.python.syntax.Statement]] =
  {
  lazy val tparamList: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name], Map[hydra.core.Name, hydra.ext.python.syntax.Name]](env.boundTypeVariables)
  hydra.lib.eithers.bind[T0, hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Statement]](hydra.ext.python.coder.encodeTypeQuoted(env)(typ))((ptypeQuoted: hydra.ext.python.syntax.Expression) =>
    {
    lazy val pyName: hydra.ext.python.syntax.Name = hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.pascal)(env)(name)
    {
      lazy val body: hydra.ext.python.syntax.Block = hydra.ext.python.utils.indentedBlock(comment)(Seq())
      {
        lazy val typeConstStmt: hydra.ext.python.syntax.Statement = hydra.ext.python.utils.dottedAssignmentStatement(pyName)(hydra.ext.python.names.encodeConstantForTypeName(env)(name))(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary(hydra.ext.python.names.encodeName(true)(hydra.util.CaseConvention.pascal)(env)("hydra.core.Name")))(Seq(hydra.ext.python.utils.doubleQuotedString(name))))
        Right(Seq(hydra.ext.python.utils.pyClassDefinitionToPyStatement(hydra.ext.python.syntax.ClassDefinition(None, pyName, hydra.lib.lists.map[hydra.core.Name, hydra.ext.python.syntax.TypeParameter]((`arg_`: hydra.core.Name) =>
          hydra.ext.python.utils.pyNameToPyTypeParameter(hydra.ext.python.names.encodeTypeVariable(`arg_`)))(hydra.ext.python.coder.findTypeParams(env)(typ)), Some(hydra.ext.python.coder.variantArgs(ptypeQuoted)(tparamList)), body)), typeConstStmt))
      }
    }
  })
}

def extendEnvWithTypeVar(env: hydra.ext.python.environment.PythonEnvironment)(`var_`: hydra.core.Name): hydra.ext.python.environment.PythonEnvironment =
  {
  lazy val oldBound: Tuple2[Seq[hydra.core.Name], Map[hydra.core.Name, hydra.ext.python.syntax.Name]] = (env.boundTypeVariables)
  lazy val tparamList: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name], Map[hydra.core.Name, hydra.ext.python.syntax.Name]](oldBound)
  lazy val tparamMap: Map[hydra.core.Name, hydra.ext.python.syntax.Name] = hydra.lib.pairs.second[Seq[hydra.core.Name], Map[hydra.core.Name, hydra.ext.python.syntax.Name]](oldBound)
  lazy val newList: Seq[hydra.core.Name] = hydra.lib.lists.concat2[hydra.core.Name](tparamList)(Seq(`var_`))
  lazy val newMap: Map[hydra.core.Name, hydra.ext.python.syntax.Name] = hydra.lib.maps.insert[hydra.core.Name, hydra.ext.python.syntax.Name](`var_`)(hydra.ext.python.names.encodeTypeVariable(`var_`))(tparamMap)
  hydra.ext.python.environment.PythonEnvironment(env.namespaces, Tuple2(newList, newMap), (env.graph), (env.nullaryBindings), (env.version), (env.skipCasts), (env.inlineVariables))
}

def gatherLambdas(term: hydra.core.Term): Tuple2[Seq[hydra.core.Name], hydra.core.Term] =
  {
  def go(params: Seq[hydra.core.Name])(t: hydra.core.Term): Tuple2[Seq[hydra.core.Name], hydra.core.Term] =
    hydra.rewriting.deannotateAndDetypeTerm(t) match
    case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
      case hydra.core.Function.lambda(v_Function_lambda_l) => go(hydra.lib.lists.concat2[hydra.core.Name](params)(Seq(v_Function_lambda_l.parameter)))(v_Function_lambda_l.body)
      case _ => Tuple2(params, t)
    case _ => Tuple2(params, t)
  go(Seq())(term)
}

def extendEnvWithLambdaParams(env: hydra.ext.python.environment.PythonEnvironment)(term: hydra.core.Term): hydra.ext.python.environment.PythonEnvironment =
  {
  def go(e: hydra.ext.python.environment.PythonEnvironment)(t: hydra.core.Term): hydra.ext.python.environment.PythonEnvironment =
    hydra.rewriting.deannotateAndDetypeTerm(t) match
    case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
      case hydra.core.Function.lambda(v_Function_lambda_lam) => {
        lazy val newTc: hydra.graph.Graph = hydra.rewriting.extendGraphForLambda(hydra.ext.python.coder.pythonEnvironmentGetGraph(e))(v_Function_lambda_lam)
        {
          lazy val newEnv: hydra.ext.python.environment.PythonEnvironment = hydra.ext.python.coder.pythonEnvironmentSetGraph(newTc)(e)
          go(newEnv)(v_Function_lambda_lam.body)
        }
      }
      case _ => e
    case _ => e
  go(env)(term)
}

def makeSimpleLambda(arity: Int)(lhs: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Expression =
  {
  lazy val args: Seq[hydra.ext.python.syntax.Name] = hydra.lib.lists.map[Int, hydra.ext.python.syntax.Name]((i: Int) => hydra.lib.strings.cat2("x")(hydra.lib.literals.showInt32(i)))(hydra.lib.math.range(1)(arity))
  hydra.lib.logic.ifElse[hydra.ext.python.syntax.Expression](hydra.lib.equality.equal[Int](arity)(0))(lhs)(hydra.ext.python.syntax.Expression.lambda(hydra.ext.python.syntax.Lambda(hydra.ext.python.syntax.LambdaParameters(None, hydra.lib.lists.map[hydra.ext.python.syntax.Name, hydra.ext.python.syntax.LambdaParamNoDefault]((a: hydra.ext.python.syntax.Name) => a)(args), Seq(), None), hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyExpressionToPyPrimary(lhs))(hydra.lib.lists.map[hydra.ext.python.syntax.Name, hydra.ext.python.syntax.Expression]((a: hydra.ext.python.syntax.Name) =>
    hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None, hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None, hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false, hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name(a))), None)))))))), Seq()))))))(args)))))
}

def isCaseStatementApplication(term: hydra.core.Term): Option[Tuple2[hydra.core.Name, Tuple2[Option[hydra.core.Term], Tuple2[Seq[hydra.core.Field], hydra.core.Term]]]] =
  {
  lazy val gathered: Tuple2[Seq[hydra.core.Term], hydra.core.Term] = hydra.coderUtils.gatherApplications(term)
  lazy val args: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], hydra.core.Term](gathered)
  lazy val body: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Term], hydra.core.Term](gathered)
  hydra.lib.logic.ifElse[Option[Tuple2[hydra.core.Name, Tuple2[Option[hydra.core.Term], Tuple2[Seq[hydra.core.Field], hydra.core.Term]]]]](hydra.lib.logic.not(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Term](args))(1)))(None)({
    lazy val arg: hydra.core.Term = hydra.lib.lists.head[hydra.core.Term](args)
    hydra.rewriting.deannotateAndDetypeTerm(body) match
      case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
        case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
          case hydra.core.Elimination.union(v_Elimination_union_cs) => Some(Tuple2(v_Elimination_union_cs.typeName, Tuple2(v_Elimination_union_cs.default, Tuple2(v_Elimination_union_cs.cases, arg))))
          case _ => None
        case _ => None
      case _ => None
  })
}

def isVariantUnitType(rowType: Seq[hydra.core.FieldType])(fieldName: hydra.core.Name): Boolean =
  {
  lazy val mfield: Option[hydra.core.FieldType] = hydra.lib.lists.find[hydra.core.FieldType]((ft: hydra.core.FieldType) =>
    hydra.lib.equality.equal[hydra.core.Name](ft.name)(fieldName))(rowType)
  hydra.lib.maybes.fromMaybe[Boolean](false)(hydra.lib.maybes.map[hydra.core.FieldType, Boolean]((ft: hydra.core.FieldType) =>
    hydra.schemas.isUnitType(hydra.rewriting.deannotateType(ft.`type`)))(mfield))
}

def wildcardCaseBlock(stmt: hydra.ext.python.syntax.Statement): hydra.ext.python.syntax.CaseBlock =
  hydra.ext.python.syntax.CaseBlock(hydra.ext.python.utils.pyClosedPatternToPyPatterns(hydra.ext.python.syntax.ClosedPattern.wildcard), None, hydra.ext.python.utils.indentedBlock(None)(Seq(Seq(stmt))))

def enumVariantPattern(env: hydra.ext.python.environment.PythonEnvironment)(typeName: hydra.core.Name)(fieldName: hydra.core.Name): hydra.ext.python.syntax.ClosedPattern =
  hydra.ext.python.syntax.ClosedPattern.value(Seq(hydra.ext.python.names.encodeName(true)(hydra.util.CaseConvention.pascal)(env)(typeName), hydra.ext.python.names.encodeEnumValue(env)(fieldName)))

def classVariantPatternUnit(pyVariantName: hydra.ext.python.syntax.Name): hydra.ext.python.syntax.ClosedPattern =
  hydra.ext.python.syntax.ClosedPattern.`class`(hydra.ext.python.syntax.ClassPattern(Seq(pyVariantName), None, None))

def classVariantPatternWithCapture(env: hydra.ext.python.environment.PythonEnvironment)(pyVariantName: hydra.ext.python.syntax.Name)(varName: hydra.core.Name): hydra.ext.python.syntax.ClosedPattern =
  {
  lazy val pyVarNameAttr: hydra.ext.python.syntax.NameOrAttribute = Seq(pyVariantName)
  lazy val capturePattern: hydra.ext.python.syntax.ClosedPattern = hydra.ext.python.syntax.ClosedPattern.capture(hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env)(varName))
  lazy val keywordPattern: hydra.ext.python.syntax.KeywordPattern = hydra.ext.python.syntax.KeywordPattern("value", hydra.ext.python.syntax.Pattern.or(Seq(capturePattern)))
  hydra.ext.python.syntax.ClosedPattern.`class`(hydra.ext.python.syntax.ClassPattern(pyVarNameAttr, None, Some(Seq(keywordPattern))))
}

def isCasesFull[T0, T1](rowType: Seq[T0])(`cases_`: Seq[T1]): Boolean =
  {
  lazy val numCases: Int = hydra.lib.lists.length[T1](`cases_`)
  lazy val numFields: Int = hydra.lib.lists.length[T0](rowType)
  hydra.lib.logic.not(hydra.lib.equality.lt[Int](numCases)(numFields))
}

def variantClosedPattern[T0](env: hydra.ext.python.environment.PythonEnvironment)(typeName: hydra.core.Name)(fieldName: hydra.core.Name)(pyVariantName: hydra.ext.python.syntax.Name)(rowType: T0)(isEnum: Boolean)(varName: hydra.core.Name)(shouldCapture: Boolean): hydra.ext.python.syntax.ClosedPattern =
  hydra.lib.logic.ifElse[hydra.ext.python.syntax.ClosedPattern](isEnum)(hydra.ext.python.coder.enumVariantPattern(env)(typeName)(fieldName))(hydra.lib.logic.ifElse[hydra.ext.python.syntax.ClosedPattern](hydra.lib.logic.not(shouldCapture))(hydra.ext.python.coder.classVariantPatternUnit(pyVariantName))(hydra.ext.python.coder.classVariantPatternWithCapture(env)(pyVariantName)(varName)))

def deduplicateCaseVariables(`cases_`: Seq[hydra.core.Field]): Seq[hydra.core.Field] =
  {
  def rewriteCase(state: Tuple2[Map[hydra.core.Name, Int], Seq[hydra.core.Field]])(field: hydra.core.Field): Tuple2[Map[hydra.core.Name, Int], Seq[hydra.core.Field]] =
    {
    lazy val countByName: Map[hydra.core.Name, Int] = hydra.lib.pairs.first[Map[hydra.core.Name, Int], Seq[hydra.core.Field]](state)
    lazy val done: Seq[hydra.core.Field] = hydra.lib.pairs.second[Map[hydra.core.Name, Int], Seq[hydra.core.Field]](state)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    hydra.rewriting.deannotateAndDetypeTerm(fterm) match
      case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
        case hydra.core.Function.lambda(v_Function_lambda_lam) => {
          lazy val v: hydra.core.Name = (v_Function_lambda_lam.parameter)
          {
            lazy val mdom: Option[hydra.core.Type] = (v_Function_lambda_lam.domain)
            {
              lazy val body: hydra.core.Term = (v_Function_lambda_lam.body)
              hydra.lib.maybes.maybe[Tuple2[Map[hydra.core.Name, Int], Seq[hydra.core.Field]], Int](Tuple2(hydra.lib.maps.insert[hydra.core.Name, Int](v)(1)(countByName), hydra.lib.lists.cons[hydra.core.Field](field)(done)))((count: Int) =>
                {
                lazy val count2: Int = hydra.lib.math.add(count)(1)
                {
                  lazy val v2: hydra.core.Name = hydra.lib.strings.cat2(v)(hydra.lib.literals.showInt32(count2))
                  {
                    lazy val newBody: hydra.core.Term = hydra.reduction.alphaConvert(v)(v2)(body)
                    {
                      lazy val newLam: hydra.core.Lambda = hydra.core.Lambda(v2, mdom, newBody)
                      {
                        lazy val newTerm: hydra.core.Term = hydra.core.Term.function(hydra.core.Function.lambda(newLam))
                        {
                          lazy val newField: hydra.core.Field = hydra.core.Field(fname, newTerm)
                          Tuple2(hydra.lib.maps.insert[hydra.core.Name, Int](v)(count2)(countByName), hydra.lib.lists.cons[hydra.core.Field](newField)(done))
                        }
                      }
                    }
                  }
                }
              })(hydra.lib.maps.lookup[hydra.core.Name, Int](v)(countByName))
            }
          }
        }
        case _ => Tuple2(countByName, hydra.lib.lists.cons[hydra.core.Field](field)(done))
      case _ => Tuple2(countByName, hydra.lib.lists.cons[hydra.core.Field](field)(done))
  }
  lazy val result: Tuple2[Map[hydra.core.Name, Int], Seq[hydra.core.Field]] = hydra.lib.lists.foldl[Tuple2[Map[hydra.core.Name, Int], Seq[hydra.core.Field]], hydra.core.Field](rewriteCase)(Tuple2(hydra.lib.maps.empty[hydra.core.Name, Int], Seq()))(`cases_`)
  hydra.lib.lists.reverse[hydra.core.Field](hydra.lib.pairs.second[Map[hydra.core.Name, Int], Seq[hydra.core.Field]](result))
}

def eliminateUnitVar(v: hydra.core.Name)(term0: hydra.core.Term): hydra.core.Term =
  {
  def rewriteField(rewrite: (hydra.core.Term => hydra.core.Term))(fld: hydra.core.Field): hydra.core.Field = hydra.core.Field(fld.name, rewrite(fld.term))
  def rewriteBinding(rewrite: (hydra.core.Term => hydra.core.Term))(bnd: hydra.core.Binding): hydra.core.Binding = hydra.core.Binding(bnd.name, rewrite(bnd.term), (bnd.`type`))
  def rewrite(recurse: (hydra.core.Term => hydra.core.Term))(term: hydra.core.Term): hydra.core.Term =
    hydra.rewriting.deannotateAndDetypeTerm(term) match
    case hydra.core.Term.variable(v_Term_variable_n) => hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Name](v_Term_variable_n)(v))(hydra.core.Term.unit)(term)
    case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.core.Term.annotated(hydra.core.AnnotatedTerm(recurse(v_Term_annotated_at.body), (v_Term_annotated_at.annotation)))
    case hydra.core.Term.application(v_Term_application_app) => hydra.core.Term.application(hydra.core.Application(recurse(v_Term_application_app.function), recurse(v_Term_application_app.argument)))
    case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
      case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Name](v_Function_lambda_lam.parameter)(v))(term)(hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(v_Function_lambda_lam.parameter, (v_Function_lambda_lam.domain), recurse(v_Function_lambda_lam.body)))))
      case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
        case hydra.core.Elimination.union(v_Elimination_union_cs) => hydra.core.Term.function(hydra.core.Function.elimination(hydra.core.Elimination.union(hydra.core.CaseStatement(v_Elimination_union_cs.typeName, hydra.lib.maybes.map[hydra.core.Term, hydra.core.Term](recurse)(v_Elimination_union_cs.default), hydra.lib.lists.map[hydra.core.Field, hydra.core.Field]((v1: hydra.core.Field) => rewriteField(recurse)(v1))(v_Elimination_union_cs.cases)))))
        case _ => term
      case _ => term
    case hydra.core.Term.let(v_Term_let_lt) => hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding]((v1: hydra.core.Binding) => rewriteBinding(recurse)(v1))(v_Term_let_lt.bindings), recurse(v_Term_let_lt.body)))
    case hydra.core.Term.list(v_Term_list_ts) => hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Term, hydra.core.Term](recurse)(v_Term_list_ts))
    case hydra.core.Term.map(v_Term_map_m) => hydra.core.Term.map(hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](hydra.lib.lists.map[Tuple2[hydra.core.Term, hydra.core.Term], Tuple2[hydra.core.Term, hydra.core.Term]]((kv: Tuple2[hydra.core.Term, hydra.core.Term]) =>
      Tuple2(recurse(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](kv)), recurse(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kv))))(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m))))
    case hydra.core.Term.record(v_Term_record_rec) => hydra.core.Term.record(hydra.core.Record(v_Term_record_rec.typeName, hydra.lib.lists.map[hydra.core.Field, hydra.core.Field]((v1: hydra.core.Field) => rewriteField(recurse)(v1))(v_Term_record_rec.fields)))
    case hydra.core.Term.set(v_Term_set_s) => hydra.core.Term.set(hydra.lib.sets.map[hydra.core.Term, hydra.core.Term](recurse)(v_Term_set_s))
    case hydra.core.Term.union(v_Term_union_inj) => hydra.core.Term.union(hydra.core.Injection(v_Term_union_inj.typeName, rewriteField(recurse)(v_Term_union_inj.field)))
    case hydra.core.Term.maybe(v_Term_maybe_mt) => hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.core.Term, hydra.core.Term](recurse)(v_Term_maybe_mt))
    case hydra.core.Term.pair(v_Term_pair_p) => hydra.core.Term.pair(Tuple2(recurse(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)), recurse(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p))))
    case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.core.Term.wrap(hydra.core.WrappedTerm(v_Term_wrap_wt.typeName, recurse(v_Term_wrap_wt.body)))
    case hydra.core.Term.either(v_Term_either_e) => hydra.core.Term.either(hydra.lib.eithers.bimap[hydra.core.Term, hydra.core.Term, hydra.core.Term, hydra.core.Term](recurse)(recurse)(v_Term_either_e))
    case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(recurse(v_Term_typeApplication_ta.body), (v_Term_typeApplication_ta.`type`)))
    case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_tl.parameter, recurse(v_Term_typeLambda_tl.body)))
    case _ => term
  def go(term: hydra.core.Term): hydra.core.Term = rewrite(go)(term)
  go(term0)
}

def encodeDefaultCaseBlock[T0, T1](encodeTerm: (T0 => Either[T1, hydra.ext.python.syntax.Expression]))(isFull: Boolean)(mdflt: Option[T0])(tname: hydra.core.Name): Either[T1, Seq[hydra.ext.python.syntax.CaseBlock]] =
  hydra.lib.eithers.bind[T1, hydra.ext.python.syntax.Statement, Seq[hydra.ext.python.syntax.CaseBlock]](hydra.lib.maybes.maybe[Either[T1, hydra.ext.python.syntax.Statement], T0](Right(hydra.lib.logic.ifElse[hydra.ext.python.syntax.Statement](isFull)(hydra.ext.python.utils.raiseAssertionError("Unreachable: all variants handled"))(hydra.ext.python.utils.raiseTypeError(hydra.lib.strings.cat2("Unsupported ")(hydra.names.localNameOf(tname))))))((d: T0) =>
  hydra.lib.eithers.bind[T1, hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Statement](encodeTerm(d))((pyexpr: hydra.ext.python.syntax.Expression) => Right(hydra.ext.python.utils.returnSingle(pyexpr))))(mdflt))((stmt: hydra.ext.python.syntax.Statement) =>
  {
  lazy val patterns: hydra.ext.python.syntax.Patterns = hydra.ext.python.utils.pyClosedPatternToPyPatterns(hydra.ext.python.syntax.ClosedPattern.wildcard)
  {
    lazy val body: hydra.ext.python.syntax.Block = hydra.ext.python.utils.indentedBlock(None)(Seq(Seq(stmt)))
    Right(Seq(hydra.ext.python.syntax.CaseBlock(patterns, None, body)))
  }
})

def encodeCaseBlock[T0, T1](cx: T0)(env: hydra.ext.python.environment.PythonEnvironment)(tname: hydra.core.Name)(rowType: Seq[hydra.core.FieldType])(isEnum: Boolean)(encodeBody: (hydra.ext.python.environment.PythonEnvironment => hydra.core.Term => Either[T1, Seq[hydra.ext.python.syntax.Statement]]))(field: hydra.core.Field): Either[T1, hydra.ext.python.syntax.CaseBlock] =
  {
  lazy val fname: hydra.core.Name = (field.name)
  lazy val fterm: hydra.core.Term = (field.term)
  lazy val stripped: hydra.core.Term = hydra.rewriting.deannotateAndDetypeTerm(fterm)
  lazy val effectiveLambda: hydra.core.Lambda = stripped match
    case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
      case hydra.core.Function.lambda(v_Function_lambda_lam) => v_Function_lambda_lam
      case _ => {
        lazy val syntheticVar2: hydra.core.Name = "_matchValue"
        hydra.core.Lambda(syntheticVar2, None, hydra.core.Term.application(hydra.core.Application(stripped, hydra.core.Term.variable(syntheticVar2))))
      }
    case _ => {
      lazy val syntheticVar: hydra.core.Name = "_matchValue"
      hydra.core.Lambda(syntheticVar, None, hydra.core.Term.application(hydra.core.Application(stripped, hydra.core.Term.variable(syntheticVar))))
    }
  lazy val v: hydra.core.Name = (effectiveLambda.parameter)
  lazy val rawBody: hydra.core.Term = (effectiveLambda.body)
  lazy val isUnitVariant: Boolean = hydra.ext.python.coder.isVariantUnitType(rowType)(fname)
  lazy val effectiveBody: hydra.core.Term = hydra.lib.logic.ifElse[hydra.core.Term](isUnitVariant)(hydra.ext.python.coder.eliminateUnitVar(v)(rawBody))(rawBody)
  lazy val shouldCapture: Boolean = hydra.lib.logic.not(hydra.lib.logic.or(isUnitVariant)(hydra.lib.logic.or(hydra.rewriting.isFreeVariableInTerm(v)(rawBody))(hydra.schemas.isUnitTerm(rawBody))))
  lazy val env2: hydra.ext.python.environment.PythonEnvironment = hydra.ext.python.coder.pythonEnvironmentSetGraph(hydra.rewriting.extendGraphForLambda(hydra.ext.python.coder.pythonEnvironmentGetGraph(env))(effectiveLambda))(env)
  lazy val pyVariantName: hydra.ext.python.syntax.Name = hydra.ext.python.coder.deconflictVariantName(true)(env2)(tname)(fname)(env2.graph)
  lazy val pattern: hydra.ext.python.syntax.ClosedPattern = hydra.ext.python.coder.variantClosedPattern(env2)(tname)(fname)(pyVariantName)(rowType)(isEnum)(v)(shouldCapture)
  hydra.lib.eithers.bind[T1, Seq[hydra.ext.python.syntax.Statement], hydra.ext.python.syntax.CaseBlock](encodeBody(env2)(effectiveBody))((stmts: Seq[hydra.ext.python.syntax.Statement]) =>
    {
    lazy val pyBody: hydra.ext.python.syntax.Block = hydra.ext.python.utils.indentedBlock(None)(Seq(stmts))
    Right(hydra.ext.python.syntax.CaseBlock(hydra.ext.python.utils.pyClosedPatternToPyPatterns(pattern), None, pyBody))
  })
}

def pyGraphGraph(pyg: hydra.ext.python.environment.PyGraph): hydra.graph.Graph = (pyg.graph)

def pyGraphMetadata(pyg: hydra.ext.python.environment.PyGraph): hydra.ext.python.environment.PythonModuleMetadata = (pyg.metadata)

def makePyGraph(g: hydra.graph.Graph)(m: hydra.ext.python.environment.PythonModuleMetadata): hydra.ext.python.environment.PyGraph = hydra.ext.python.environment.PyGraph(g, m)

def encodeFieldType(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(fieldType: hydra.core.FieldType): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Statement] =
  {
  lazy val fname: hydra.core.Name = (fieldType.name)
  lazy val ftype: hydra.core.Type = (fieldType.`type`)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[scala.Predef.String], hydra.ext.python.syntax.Statement](hydra.annotations.getTypeDescription(cx)(hydra.ext.python.coder.pythonEnvironmentGetGraph(env))(ftype))((comment: Option[scala.Predef.String]) =>
    {
    lazy val pyName: hydra.ext.python.syntax.SingleTarget = hydra.ext.python.syntax.SingleTarget.name(hydra.ext.python.names.encodeFieldName(env)(fname))
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Statement](hydra.ext.python.coder.encodeType(env)(ftype))((pyType: hydra.ext.python.syntax.Expression) =>
      {
      lazy val annotatedPyType: hydra.ext.python.syntax.Expression = hydra.ext.python.utils.annotatedExpression(comment)(pyType)
      Right(hydra.ext.python.utils.pyAssignmentToPyStatement(hydra.ext.python.syntax.Assignment.typed(hydra.ext.python.syntax.TypedAssignment(pyName, annotatedPyType, None))))
    })
  })
}

lazy val dataclassDecorator: hydra.ext.python.syntax.NamedExpression = hydra.ext.python.syntax.NamedExpression.simple(hydra.ext.python.utils.pyPrimaryToPyExpression(hydra.ext.python.utils.primaryWithRhs(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("dataclass")))(hydra.ext.python.syntax.PrimaryRhs.call(hydra.ext.python.syntax.Args(Seq(), Seq(hydra.ext.python.syntax.KwargOrStarred.kwarg(hydra.ext.python.syntax.Kwarg("frozen", hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.`true`)))), Seq())))))

def encodeRecordType(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(name: hydra.core.Name)(rowType: Seq[hydra.core.FieldType])(comment: Option[scala.Predef.String]): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Statement] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement], hydra.ext.python.syntax.Statement](hydra.lib.eithers.mapList[hydra.core.FieldType, hydra.ext.python.syntax.Statement, hydra.context.InContext[hydra.errors.Error]]((v1: hydra.core.FieldType) => hydra.ext.python.coder.encodeFieldType(cx)(env)(v1))(rowType))((pyFields: Seq[hydra.ext.python.syntax.Statement]) =>
  {
  lazy val constStmts: Seq[hydra.ext.python.syntax.Statement] = hydra.ext.python.coder.encodeNameConstants(env)(name)(rowType)
  {
    lazy val body: hydra.ext.python.syntax.Block = hydra.ext.python.utils.indentedBlock(comment)(Seq(pyFields, constStmts))
    {
      lazy val boundVars: Tuple2[Seq[hydra.core.Name], Map[hydra.core.Name, hydra.ext.python.syntax.Name]] = (env.boundTypeVariables)
      {
        lazy val tparamList: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name], Map[hydra.core.Name, hydra.ext.python.syntax.Name]](boundVars)
        {
          lazy val mGenericArg: Option[hydra.ext.python.syntax.Expression] = hydra.ext.python.coder.genericArg(tparamList)
          {
            lazy val args: Option[hydra.ext.python.syntax.Args] = hydra.lib.maybes.maybe[Option[hydra.ext.python.syntax.Args], hydra.ext.python.syntax.Expression](None)((a: hydra.ext.python.syntax.Expression) => Some(hydra.ext.python.utils.pyExpressionsToPyArgs(Seq(a))))(mGenericArg)
            {
              lazy val decs: Option[hydra.ext.python.syntax.Decorators] = Some(Seq(hydra.ext.python.coder.dataclassDecorator))
              {
                lazy val pyName: hydra.ext.python.syntax.Name = hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.pascal)(env)(name)
                {
                  def noTypeParams[T0]: Seq[T0] = Seq()
                  Right(hydra.ext.python.utils.pyClassDefinitionToPyStatement(hydra.ext.python.syntax.ClassDefinition(decs, pyName, noTypeParams, args, body)))
                }
              }
            }
          }
        }
      }
    }
  }
})

def encodeEnumValueAssignment(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(fieldType: hydra.core.FieldType): Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement]] =
  {
  lazy val fname: hydra.core.Name = (fieldType.name)
  lazy val ftype: hydra.core.Type = (fieldType.`type`)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[scala.Predef.String], Seq[hydra.ext.python.syntax.Statement]](hydra.annotations.getTypeDescription(cx)(hydra.ext.python.coder.pythonEnvironmentGetGraph(env))(ftype))((mcomment: Option[scala.Predef.String]) =>
    {
    lazy val pyName: hydra.ext.python.syntax.Name = hydra.ext.python.names.encodeEnumValue(env)(fname)
    {
      lazy val fnameStr: scala.Predef.String = fname
      {
        lazy val pyValue: hydra.ext.python.syntax.Expression = hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary(hydra.ext.python.names.encodeName(true)(hydra.util.CaseConvention.pascal)(env)("hydra.core.Name")))(Seq(hydra.ext.python.utils.doubleQuotedString(fnameStr)))
        {
          lazy val assignStmt: hydra.ext.python.syntax.Statement = hydra.ext.python.utils.assignmentStatement(pyName)(pyValue)
          Right(hydra.lib.maybes.maybe[Seq[hydra.ext.python.syntax.Statement], scala.Predef.String](Seq(assignStmt))((c: scala.Predef.String) =>
            Seq(assignStmt, hydra.ext.python.utils.pyExpressionToPyStatement(hydra.ext.python.utils.tripleQuotedString(c))))(mcomment))
        }
      }
    }
  })
}

def deconflictVariantName(isQualified: Boolean)(env: hydra.ext.python.environment.PythonEnvironment)(unionName: hydra.core.Name)(fname: hydra.core.Name)(g: hydra.graph.Graph): hydra.ext.python.syntax.Name =
  {
  lazy val candidateHydraName: hydra.core.Name = hydra.lib.strings.cat2(unionName)(hydra.formatting.capitalize(fname))
  lazy val elements: Seq[hydra.core.Binding] = hydra.lexical.graphToBindings(g)
  lazy val collision: Boolean = hydra.lib.maybes.isJust[hydra.core.Binding](hydra.lib.lists.find[hydra.core.Binding]((b: hydra.core.Binding) =>
    hydra.lib.equality.equal[scala.Predef.String](b.name)(candidateHydraName))(elements))
  hydra.lib.logic.ifElse[hydra.ext.python.syntax.Name](collision)(hydra.lib.strings.cat2(hydra.ext.python.names.variantName(isQualified)(env)(unionName)(fname))("_"))(hydra.ext.python.names.variantName(isQualified)(env)(unionName)(fname))
}

def encodeUnionField(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(unionName: hydra.core.Name)(fieldType: hydra.core.FieldType): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Statement] =
  {
  lazy val fname: hydra.core.Name = (fieldType.name)
  lazy val ftype: hydra.core.Type = (fieldType.`type`)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[scala.Predef.String], hydra.ext.python.syntax.Statement](hydra.annotations.getTypeDescription(cx)(hydra.ext.python.coder.pythonEnvironmentGetGraph(env))(ftype))((fcomment: Option[scala.Predef.String]) =>
    {
    lazy val isUnit: Boolean = hydra.lib.equality.equal[hydra.core.Type](hydra.rewriting.deannotateType(ftype))(hydra.core.Type.unit)
    {
      lazy val varName: hydra.ext.python.syntax.Name = hydra.ext.python.coder.deconflictVariantName(false)(env)(unionName)(fname)(env.graph)
      {
        lazy val tparamNames: Seq[hydra.core.Name] = hydra.ext.python.coder.findTypeParams(env)(ftype)
        {
          lazy val tparamPyNames: Seq[hydra.ext.python.syntax.Name] = hydra.lib.lists.map[hydra.core.Name, hydra.ext.python.syntax.Name](hydra.ext.python.names.encodeTypeVariable)(tparamNames)
          {
            lazy val fieldParams: Seq[hydra.ext.python.syntax.TypeParameter] = hydra.lib.lists.map[hydra.ext.python.syntax.Name, hydra.ext.python.syntax.TypeParameter](hydra.ext.python.utils.pyNameToPyTypeParameter)(tparamPyNames)
            {
              lazy val body: hydra.ext.python.syntax.Block = hydra.lib.logic.ifElse[hydra.ext.python.syntax.Block](isUnit)(hydra.ext.python.utils.indentedBlock(fcomment)(Seq(hydra.ext.python.utils.unitVariantMethods(varName))))(hydra.ext.python.utils.indentedBlock(fcomment)(Seq()))
              hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[hydra.ext.python.syntax.Args], hydra.ext.python.syntax.Statement](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Option[hydra.ext.python.syntax.Args]]](isUnit)(Right(None))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Option[hydra.ext.python.syntax.Args]](hydra.ext.python.coder.encodeTypeQuoted(env)(ftype))((quotedType: hydra.ext.python.syntax.Expression) =>
                Right(Some(hydra.ext.python.coder.variantArgs(quotedType)(Seq()))))))((margs: Option[hydra.ext.python.syntax.Args]) =>
                Right(hydra.ext.python.utils.pyClassDefinitionToPyStatement(hydra.ext.python.syntax.ClassDefinition(None, varName, fieldParams, margs, body))))
            }
          }
        }
      }
    }
  })
}

def encodeUnionType(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(name: hydra.core.Name)(rowType: Seq[hydra.core.FieldType])(comment: Option[scala.Predef.String]): Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement]] =
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement]]](hydra.schemas.isEnumRowType(rowType))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[Seq[hydra.ext.python.syntax.Statement]], Seq[hydra.ext.python.syntax.Statement]](hydra.lib.eithers.mapList[hydra.core.FieldType, Seq[hydra.ext.python.syntax.Statement], hydra.context.InContext[hydra.errors.Error]]((v1: hydra.core.FieldType) =>
  hydra.ext.python.coder.encodeEnumValueAssignment(cx)(env)(v1))(rowType))((vals: Seq[Seq[hydra.ext.python.syntax.Statement]]) =>
  {
  lazy val body: hydra.ext.python.syntax.Block = hydra.ext.python.utils.indentedBlock(comment)(vals)
  {
    lazy val enumName: hydra.ext.python.syntax.Name = "Enum"
    {
      lazy val args: Option[hydra.ext.python.syntax.Args] = Some(hydra.ext.python.utils.pyExpressionsToPyArgs(Seq(hydra.ext.python.utils.pyNameToPyExpression(enumName))))
      {
        lazy val pyName: hydra.ext.python.syntax.Name = hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.pascal)(env)(name)
        {
          lazy val typeConstStmt: hydra.ext.python.syntax.Statement = hydra.ext.python.utils.dottedAssignmentStatement(pyName)(hydra.ext.python.names.encodeConstantForTypeName(env)(name))(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary(hydra.ext.python.names.encodeName(true)(hydra.util.CaseConvention.pascal)(env)("hydra.core.Name")))(Seq(hydra.ext.python.utils.doubleQuotedString(name))))
          Right(Seq(hydra.ext.python.utils.pyClassDefinitionToPyStatement(hydra.ext.python.syntax.ClassDefinition(None, pyName, Seq(), args, body)), typeConstStmt))
        }
      }
    }
  }
}))({
  lazy val constStmts: Seq[hydra.ext.python.syntax.Statement] = hydra.ext.python.coder.encodeNameConstants(env)(name)(rowType)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement], Seq[hydra.ext.python.syntax.Statement]](hydra.lib.eithers.mapList[hydra.core.FieldType, hydra.ext.python.syntax.Statement, hydra.context.InContext[hydra.errors.Error]]((v1: hydra.core.FieldType) => hydra.ext.python.coder.encodeUnionField(cx)(env)(name)(v1))(rowType))((fieldStmts: Seq[hydra.ext.python.syntax.Statement]) =>
    {
    lazy val tparams: Seq[hydra.ext.python.syntax.TypeParameter] = hydra.ext.python.coder.environmentTypeParameters(env)
    {
      lazy val unionAlts: Seq[hydra.ext.python.syntax.Primary] = hydra.lib.lists.map[hydra.core.FieldType, hydra.ext.python.syntax.Primary]((v1: hydra.core.FieldType) => hydra.ext.python.coder.encodeUnionFieldAlt(env)(name)(v1))(rowType)
      {
        lazy val unionStmts: Seq[hydra.ext.python.syntax.Statement] = hydra.ext.python.coder.unionTypeStatementsFor(env)(hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.pascal)(env)(name))(tparams)(comment)(hydra.ext.python.utils.orExpression(unionAlts))(constStmts)
        Right(hydra.lib.lists.concat2[hydra.ext.python.syntax.Statement](fieldStmts)(unionStmts))
      }
    }
  })
})

def encodeUnionFieldAlt(env: hydra.ext.python.environment.PythonEnvironment)(unionName: hydra.core.Name)(fieldType: hydra.core.FieldType): hydra.ext.python.syntax.Primary =
  {
  lazy val fname: hydra.core.Name = (fieldType.name)
  lazy val ftype: hydra.core.Type = (fieldType.`type`)
  lazy val tparamNames: Seq[hydra.core.Name] = hydra.ext.python.coder.findTypeParams(env)(ftype)
  lazy val tparams: Seq[hydra.ext.python.syntax.Name] = hydra.lib.lists.map[hydra.core.Name, hydra.ext.python.syntax.Name](hydra.ext.python.names.encodeTypeVariable)(tparamNames)
  lazy val namePrim: hydra.ext.python.syntax.Primary = hydra.ext.python.utils.pyNameToPyPrimary(hydra.ext.python.names.variantName(false)(env)(unionName)(fname))
  hydra.lib.logic.ifElse[hydra.ext.python.syntax.Primary](hydra.lib.lists.`null`[hydra.ext.python.syntax.Name](tparams))(namePrim)({
    lazy val tparamExprs: Seq[hydra.ext.python.syntax.Expression] = hydra.lib.lists.map[hydra.ext.python.syntax.Name, hydra.ext.python.syntax.Expression](hydra.ext.python.utils.pyNameToPyExpression)(tparams)
    hydra.ext.python.utils.primaryWithExpressionSlices(namePrim)(tparamExprs)
  })
}

def encodeTypeDefSingle(env: hydra.ext.python.environment.PythonEnvironment)(name: hydra.core.Name)(comment: Option[scala.Predef.String])(typeExpr: hydra.ext.python.syntax.Expression): Seq[hydra.ext.python.syntax.Statement] =
  {
  lazy val pyName: hydra.ext.python.syntax.Name = hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.pascal)(env)(name)
  lazy val tparams: Seq[hydra.ext.python.syntax.TypeParameter] = hydra.ext.python.coder.environmentTypeParameters(env)
  Seq(hydra.ext.python.coder.typeAliasStatementFor(env)(pyName)(tparams)(comment)(typeExpr))
}

def encodeTypeAssignment(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(name: hydra.core.Name)(typ: hydra.core.Type)(comment: Option[scala.Predef.String]): Either[hydra.context.InContext[hydra.errors.Error], Seq[Seq[hydra.ext.python.syntax.Statement]]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement], Seq[Seq[hydra.ext.python.syntax.Statement]]](hydra.ext.python.coder.encodeTypeAssignmentInner(cx)(env)(name)(typ)(comment))((defStmts: Seq[hydra.ext.python.syntax.Statement]) =>
  Right(hydra.lib.lists.map[hydra.ext.python.syntax.Statement, Seq[hydra.ext.python.syntax.Statement]]((s: hydra.ext.python.syntax.Statement) => Seq(s))(defStmts)))

def encodeTypeAssignmentInner(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(name: hydra.core.Name)(typ: hydra.core.Type)(comment: Option[scala.Predef.String]): Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement]] =
  {
  lazy val stripped: hydra.core.Type = hydra.rewriting.deannotateType(typ)
  def dflt[T0]: Either[T0, Seq[hydra.ext.python.syntax.Statement]] =
    hydra.lib.eithers.bind[T0, hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Statement]](hydra.ext.python.coder.encodeType(env)(typ))((typeExpr: hydra.ext.python.syntax.Expression) =>
    Right(hydra.ext.python.coder.encodeTypeDefSingle(env)(name)(comment)(typeExpr)))
  stripped match
    case hydra.core.Type.forall(v_Type_forall_ft) => {
      lazy val tvar: hydra.core.Name = (v_Type_forall_ft.parameter)
      {
        lazy val body: hydra.core.Type = (v_Type_forall_ft.body)
        {
          lazy val newEnv: hydra.ext.python.environment.PythonEnvironment = hydra.ext.python.coder.extendEnvWithTypeVar(env)(tvar)
          hydra.ext.python.coder.encodeTypeAssignmentInner(cx)(newEnv)(name)(body)(comment)
        }
      }
    }
    case hydra.core.Type.record(v_Type_record_rt) => hydra.lib.eithers.map[hydra.ext.python.syntax.Statement, Seq[hydra.ext.python.syntax.Statement], hydra.context.InContext[hydra.errors.Error]]((s: hydra.ext.python.syntax.Statement) => Seq(s))(hydra.ext.python.coder.encodeRecordType(cx)(env)(name)(v_Type_record_rt)(comment))
    case hydra.core.Type.union(v_Type_union_rt) => hydra.ext.python.coder.encodeUnionType(cx)(env)(name)(v_Type_union_rt)(comment)
    case hydra.core.Type.wrap(v_Type_wrap_wt) => hydra.ext.python.coder.encodeWrappedType(env)(name)(v_Type_wrap_wt)(comment)
    case _ => dflt
}

def unsupportedExpression(msg: scala.Predef.String): hydra.ext.python.syntax.Expression =
  hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyExpressionToPyPrimary(hydra.ext.python.utils.projectFromExpression(hydra.ext.python.utils.projectFromExpression(hydra.ext.python.utils.projectFromExpression(hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None, hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None, hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false, hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("hydra"))), None)))))))), Seq()))))))("dsl"))("python"))("unsupported")))(Seq(hydra.ext.python.utils.stringToPyExpression(hydra.ext.python.syntax.QuoteStyle.double)(msg)))

def makeUncurriedLambda(params: Seq[hydra.ext.python.syntax.Name])(body: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Expression =
  hydra.ext.python.syntax.Expression.lambda(hydra.ext.python.syntax.Lambda(hydra.ext.python.syntax.LambdaParameters(None, hydra.lib.lists.map[hydra.ext.python.syntax.Name, hydra.ext.python.syntax.LambdaParamNoDefault]((p: hydra.ext.python.syntax.Name) => p)(params), Seq(), None), body))

def encodeField[T0, T1, T2](cx: T0)(env: hydra.ext.python.environment.PythonEnvironment)(field: hydra.core.Field)(encodeTerm: (hydra.core.Term => Either[T1, T2])): Either[T1, Tuple2[hydra.ext.python.syntax.Name, T2]] =
  {
  lazy val fname: hydra.core.Name = (field.name)
  lazy val fterm: hydra.core.Term = (field.term)
  hydra.lib.eithers.bind[T1, T2, Tuple2[hydra.ext.python.syntax.Name, T2]](encodeTerm(fterm))((pterm: T2) =>
    Right(Tuple2(hydra.ext.python.names.encodeFieldName(env)(fname), pterm)))
}

def extractCaseElimination(term: hydra.core.Term): Option[hydra.core.CaseStatement] =
  hydra.rewriting.deannotateAndDetypeTerm(term) match
  case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
    case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
      case hydra.core.Elimination.union(v_Elimination_union_cs) => Some(v_Elimination_union_cs)
      case _ => None
    case _ => None
  case _ => None

def encodeBindingsAsDefs[T0, T1, T2, T3](env: T0)(encodeBinding: (T0 => T1 => Either[T2, T3]))(bindings: Seq[T1]): Either[T2, Seq[T3]] =
  hydra.lib.eithers.mapList[T1, T3, T2]((v1: T1) => encodeBinding(env)(v1))(bindings)

def encodeBindingAs(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(binding: hydra.core.Binding): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Statement] =
  {
  lazy val name1: hydra.core.Name = (binding.name)
  lazy val term1: hydra.core.Term = (binding.term)
  lazy val mts: Option[hydra.core.TypeScheme] = (binding.`type`)
  lazy val fname: hydra.ext.python.syntax.Name = hydra.ext.python.names.encodeName(true)(hydra.util.CaseConvention.lowerSnake)(env)(name1)
  hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Statement], hydra.core.TypeScheme]({
    lazy val gathered: Tuple2[Seq[hydra.core.Name], hydra.core.Term] = hydra.ext.python.coder.gatherLambdas(term1)
    {
      lazy val lambdaParams: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name], hydra.core.Term](gathered)
      {
        lazy val innerBody: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Name], hydra.core.Term](gathered)
        {
          lazy val mcsa: Option[Tuple2[hydra.core.Name, Tuple2[Option[hydra.core.Term], Tuple2[Seq[hydra.core.Field], hydra.core.Term]]]] = hydra.ext.python.coder.isCaseStatementApplication(innerBody)
          hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Statement], Tuple2[hydra.core.Name, Tuple2[Option[hydra.core.Term], Tuple2[Seq[hydra.core.Field], hydra.core.Term]]]]({
            lazy val mcs: Option[hydra.core.CaseStatement] = hydra.ext.python.coder.extractCaseElimination(term1)
            hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Statement], hydra.core.CaseStatement](hydra.lib.eithers.map[Seq[hydra.ext.python.syntax.Statement], hydra.ext.python.syntax.Statement, hydra.context.InContext[hydra.errors.Error]]((stmts: Seq[hydra.ext.python.syntax.Statement]) =>
              hydra.lib.lists.head[hydra.ext.python.syntax.Statement](stmts))(hydra.ext.python.coder.encodeTermMultiline(cx)(env)(term1)))((cs: hydra.core.CaseStatement) =>
              {
              lazy val tname: hydra.core.Name = (cs.typeName)
              {
                lazy val dflt: Option[hydra.core.Term] = (cs.default)
                {
                  lazy val `cases_`: Seq[hydra.core.Field] = (cs.cases)
                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.FieldType], hydra.ext.python.syntax.Statement](hydra.schemas.requireUnionType(cx)(hydra.ext.python.coder.pythonEnvironmentGetGraph(env))(tname))((rt: Seq[hydra.core.FieldType]) =>
                    {
                    lazy val isEnum: Boolean = hydra.schemas.isEnumRowType(rt)
                    {
                      lazy val isFull: Boolean = hydra.ext.python.coder.isCasesFull(rt)(`cases_`)
                      {
                        lazy val innerParam: hydra.ext.python.syntax.Param = hydra.ext.python.syntax.Param("x", None)
                        {
                          lazy val param: hydra.ext.python.syntax.ParamNoDefault = hydra.ext.python.syntax.ParamNoDefault(innerParam, None)
                          {
                            lazy val params: hydra.ext.python.syntax.Parameters = hydra.ext.python.syntax.Parameters.paramNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters(Seq(param), Seq(), None))
                            hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.CaseBlock], hydra.ext.python.syntax.Statement](hydra.lib.eithers.mapList[hydra.core.Field, hydra.ext.python.syntax.CaseBlock, hydra.context.InContext[hydra.errors.Error]]((v1: hydra.core.Field) =>
                              hydra.ext.python.coder.encodeCaseBlock(cx)(env)(tname)(rt)(isEnum)((e: hydra.ext.python.environment.PythonEnvironment) =>
                              (t: hydra.core.Term) => hydra.ext.python.coder.encodeTermMultiline(cx)(e)(t))(v1))(`cases_`))((pyCases: Seq[hydra.ext.python.syntax.CaseBlock]) =>
                              hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.CaseBlock], hydra.ext.python.syntax.Statement](hydra.ext.python.coder.encodeDefaultCaseBlock((t: hydra.core.Term) => hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(t))(isFull)(dflt)(tname))((pyDflt: Seq[hydra.ext.python.syntax.CaseBlock]) =>
                              {
                              lazy val subj: hydra.ext.python.syntax.SubjectExpression = hydra.ext.python.syntax.SubjectExpression.simple(hydra.ext.python.syntax.NamedExpression.simple(hydra.ext.python.utils.pyNameToPyExpression("x")))
                              {
                                lazy val allCases: Seq[hydra.ext.python.syntax.CaseBlock] = hydra.lib.lists.concat2[hydra.ext.python.syntax.CaseBlock](pyCases)(pyDflt)
                                {
                                  lazy val matchStmt: hydra.ext.python.syntax.Statement = hydra.ext.python.syntax.Statement.compound(hydra.ext.python.syntax.CompoundStatement.`match`(hydra.ext.python.syntax.MatchStatement(subj, allCases)))
                                  {
                                    lazy val body: hydra.ext.python.syntax.Block = hydra.ext.python.utils.indentedBlock(None)(Seq(Seq(matchStmt)))
                                    {
                                      lazy val funcDefRaw: hydra.ext.python.syntax.FunctionDefRaw = hydra.ext.python.syntax.FunctionDefRaw(false, fname, Seq(), Some(params), None, None, body)
                                      Right(hydra.ext.python.syntax.Statement.compound(hydra.ext.python.syntax.CompoundStatement.function(hydra.ext.python.syntax.FunctionDefinition(None, funcDefRaw))))
                                    }
                                  }
                                }
                              }
                            }))
                          }
                        }
                      }
                    }
                  })
                }
              }
            })(mcs)
          })((csa: Tuple2[hydra.core.Name, Tuple2[Option[hydra.core.Term], Tuple2[Seq[hydra.core.Field], hydra.core.Term]]]) =>
            hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Statement]](hydra.lib.lists.`null`[hydra.core.Name](lambdaParams))({
            lazy val mcs: Option[hydra.core.CaseStatement] = hydra.ext.python.coder.extractCaseElimination(term1)
            hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Statement], hydra.core.CaseStatement](hydra.lib.eithers.map[Seq[hydra.ext.python.syntax.Statement], hydra.ext.python.syntax.Statement, hydra.context.InContext[hydra.errors.Error]]((stmts: Seq[hydra.ext.python.syntax.Statement]) =>
              hydra.lib.lists.head[hydra.ext.python.syntax.Statement](stmts))(hydra.ext.python.coder.encodeTermMultiline(cx)(env)(term1)))((cs: hydra.core.CaseStatement) =>
              {
              lazy val tname: hydra.core.Name = (cs.typeName)
              {
                lazy val dflt: Option[hydra.core.Term] = (cs.default)
                {
                  lazy val `cases_`: Seq[hydra.core.Field] = (cs.cases)
                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.FieldType], hydra.ext.python.syntax.Statement](hydra.schemas.requireUnionType(cx)(hydra.ext.python.coder.pythonEnvironmentGetGraph(env))(tname))((rt: Seq[hydra.core.FieldType]) =>
                    {
                    lazy val isEnum: Boolean = hydra.schemas.isEnumRowType(rt)
                    {
                      lazy val isFull: Boolean = hydra.ext.python.coder.isCasesFull(rt)(`cases_`)
                      {
                        lazy val innerParam: hydra.ext.python.syntax.Param = hydra.ext.python.syntax.Param("x", None)
                        {
                          lazy val param: hydra.ext.python.syntax.ParamNoDefault = hydra.ext.python.syntax.ParamNoDefault(innerParam, None)
                          {
                            lazy val params: hydra.ext.python.syntax.Parameters = hydra.ext.python.syntax.Parameters.paramNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters(Seq(param), Seq(), None))
                            hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.CaseBlock], hydra.ext.python.syntax.Statement](hydra.lib.eithers.mapList[hydra.core.Field, hydra.ext.python.syntax.CaseBlock, hydra.context.InContext[hydra.errors.Error]]((v1: hydra.core.Field) =>
                              hydra.ext.python.coder.encodeCaseBlock(cx)(env)(tname)(rt)(isEnum)((e: hydra.ext.python.environment.PythonEnvironment) =>
                              (t: hydra.core.Term) => hydra.ext.python.coder.encodeTermMultiline(cx)(e)(t))(v1))(`cases_`))((pyCases: Seq[hydra.ext.python.syntax.CaseBlock]) =>
                              hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.CaseBlock], hydra.ext.python.syntax.Statement](hydra.ext.python.coder.encodeDefaultCaseBlock((t: hydra.core.Term) => hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(t))(isFull)(dflt)(tname))((pyDflt: Seq[hydra.ext.python.syntax.CaseBlock]) =>
                              {
                              lazy val subj: hydra.ext.python.syntax.SubjectExpression = hydra.ext.python.syntax.SubjectExpression.simple(hydra.ext.python.syntax.NamedExpression.simple(hydra.ext.python.utils.pyNameToPyExpression("x")))
                              {
                                lazy val allCases: Seq[hydra.ext.python.syntax.CaseBlock] = hydra.lib.lists.concat2[hydra.ext.python.syntax.CaseBlock](pyCases)(pyDflt)
                                {
                                  lazy val matchStmt: hydra.ext.python.syntax.Statement = hydra.ext.python.syntax.Statement.compound(hydra.ext.python.syntax.CompoundStatement.`match`(hydra.ext.python.syntax.MatchStatement(subj, allCases)))
                                  {
                                    lazy val body: hydra.ext.python.syntax.Block = hydra.ext.python.utils.indentedBlock(None)(Seq(Seq(matchStmt)))
                                    {
                                      lazy val funcDefRaw: hydra.ext.python.syntax.FunctionDefRaw = hydra.ext.python.syntax.FunctionDefRaw(false, fname, Seq(), Some(params), None, None, body)
                                      Right(hydra.ext.python.syntax.Statement.compound(hydra.ext.python.syntax.CompoundStatement.function(hydra.ext.python.syntax.FunctionDefinition(None, funcDefRaw))))
                                    }
                                  }
                                }
                              }
                            }))
                          }
                        }
                      }
                    }
                  })
                }
              }
            })(mcs)
          })({
            lazy val tname: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, Tuple2[Option[hydra.core.Term], Tuple2[Seq[hydra.core.Field], hydra.core.Term]]](csa)
            {
              lazy val rest1: Tuple2[Option[hydra.core.Term], Tuple2[Seq[hydra.core.Field], hydra.core.Term]] = hydra.lib.pairs.second[hydra.core.Name, Tuple2[Option[hydra.core.Term], Tuple2[Seq[hydra.core.Field], hydra.core.Term]]](csa)
              {
                lazy val dflt: Option[hydra.core.Term] = hydra.lib.pairs.first[Option[hydra.core.Term], Tuple2[Seq[hydra.core.Field], hydra.core.Term]](rest1)
                {
                  lazy val rest2: Tuple2[Seq[hydra.core.Field], hydra.core.Term] = hydra.lib.pairs.second[Option[hydra.core.Term], Tuple2[Seq[hydra.core.Field], hydra.core.Term]](rest1)
                  {
                    lazy val `cases_`: Seq[hydra.core.Field] = hydra.lib.pairs.first[Seq[hydra.core.Field], hydra.core.Term](rest2)
                    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.FieldType], hydra.ext.python.syntax.Statement](hydra.schemas.requireUnionType(cx)(hydra.ext.python.coder.pythonEnvironmentGetGraph(env))(tname))((rt: Seq[hydra.core.FieldType]) =>
                      {
                      lazy val isEnum: Boolean = hydra.schemas.isEnumRowType(rt)
                      {
                        lazy val isFull: Boolean = hydra.ext.python.coder.isCasesFull(rt)(`cases_`)
                        {
                          lazy val capturedVarNames: Seq[hydra.core.Name] = hydra.lib.lists.init[hydra.core.Name](lambdaParams)
                          {
                            lazy val matchLambdaParam: hydra.core.Name = hydra.lib.lists.last[hydra.core.Name](lambdaParams)
                            {
                              lazy val capturedParams: Seq[hydra.ext.python.syntax.ParamNoDefault] = hydra.lib.lists.map[hydra.core.Name, hydra.ext.python.syntax.ParamNoDefault]((n: hydra.core.Name) =>
                                hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param(hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env)(n), None), None))(capturedVarNames)
                              {
                                lazy val matchArgName: hydra.ext.python.syntax.Name = hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env)(matchLambdaParam)
                                {
                                  lazy val matchParam: hydra.ext.python.syntax.ParamNoDefault = hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param(matchArgName, None), None)
                                  {
                                    lazy val allParams: Seq[hydra.ext.python.syntax.ParamNoDefault] = hydra.lib.lists.concat2[hydra.ext.python.syntax.ParamNoDefault](capturedParams)(Seq(matchParam))
                                    {
                                      lazy val params: hydra.ext.python.syntax.Parameters = hydra.ext.python.syntax.Parameters.paramNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters(allParams, Seq(), None))
                                      {
                                        lazy val envWithParams: hydra.ext.python.environment.PythonEnvironment = hydra.ext.python.coder.extendEnvWithLambdaParams(env)(term1)
                                        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.CaseBlock], hydra.ext.python.syntax.Statement](hydra.lib.eithers.mapList[hydra.core.Field, hydra.ext.python.syntax.CaseBlock, hydra.context.InContext[hydra.errors.Error]]((v1: hydra.core.Field) =>
                                          hydra.ext.python.coder.encodeCaseBlock(cx)(envWithParams)(tname)(rt)(isEnum)((e: hydra.ext.python.environment.PythonEnvironment) =>
                                          (t: hydra.core.Term) => hydra.ext.python.coder.encodeTermMultiline(cx)(e)(t))(v1))(`cases_`))((pyCases: Seq[hydra.ext.python.syntax.CaseBlock]) =>
                                          hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.CaseBlock], hydra.ext.python.syntax.Statement](hydra.ext.python.coder.encodeDefaultCaseBlock((t: hydra.core.Term) =>
                                          hydra.ext.python.coder.encodeTermInline(cx)(envWithParams)(false)(t))(isFull)(dflt)(tname))((pyDflt: Seq[hydra.ext.python.syntax.CaseBlock]) =>
                                          {
                                          lazy val subj: hydra.ext.python.syntax.SubjectExpression = hydra.ext.python.syntax.SubjectExpression.simple(hydra.ext.python.syntax.NamedExpression.simple(hydra.ext.python.utils.pyNameToPyExpression(matchArgName)))
                                          {
                                            lazy val allCases: Seq[hydra.ext.python.syntax.CaseBlock] = hydra.lib.lists.concat2[hydra.ext.python.syntax.CaseBlock](pyCases)(pyDflt)
                                            {
                                              lazy val matchStmt: hydra.ext.python.syntax.Statement = hydra.ext.python.syntax.Statement.compound(hydra.ext.python.syntax.CompoundStatement.`match`(hydra.ext.python.syntax.MatchStatement(subj, allCases)))
                                              {
                                                lazy val body: hydra.ext.python.syntax.Block = hydra.ext.python.utils.indentedBlock(None)(Seq(Seq(matchStmt)))
                                                {
                                                  lazy val funcDefRaw: hydra.ext.python.syntax.FunctionDefRaw = hydra.ext.python.syntax.FunctionDefRaw(false, fname, Seq(), Some(params), None, None, body)
                                                  Right(hydra.ext.python.syntax.Statement.compound(hydra.ext.python.syntax.CompoundStatement.function(hydra.ext.python.syntax.FunctionDefinition(None, funcDefRaw))))
                                                }
                                              }
                                            }
                                          }
                                        }))
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    })
                  }
                }
              }
            }
          }))(mcsa)
        }
      }
    }
  })((ts: hydra.core.TypeScheme) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[scala.Predef.String], hydra.ext.python.syntax.Statement](hydra.annotations.getTermDescription(cx)(hydra.ext.python.coder.pythonEnvironmentGetGraph(env))(term1))((comment: Option[scala.Predef.String]) =>
    {
    lazy val normComment: Option[scala.Predef.String] = hydra.lib.maybes.map[scala.Predef.String, scala.Predef.String](hydra.coderUtils.normalizeComment)(comment)
    hydra.ext.python.coder.encodeTermAssignment(cx)(env)(name1)(term1)(ts)(normComment)
  }))(mts)
}

def encodeDefinition(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(`def_`: hydra.module.Definition): Either[hydra.context.InContext[hydra.errors.Error], Seq[Seq[hydra.ext.python.syntax.Statement]]] =
  `def_` match
  case hydra.module.Definition.term(v_Definition_term_td) => {
    lazy val name: hydra.core.Name = (v_Definition_term_td.name)
    {
      lazy val term: hydra.core.Term = (v_Definition_term_td.term)
      {
        lazy val typ: hydra.core.TypeScheme = hydra.lib.maybes.maybe[hydra.core.TypeScheme, hydra.core.TypeScheme](hydra.core.TypeScheme(Seq(), hydra.core.Type.variable("hydra.core.Unit"), None))((x: hydra.core.TypeScheme) => x)(v_Definition_term_td.`type`)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[scala.Predef.String], Seq[Seq[hydra.ext.python.syntax.Statement]]](hydra.annotations.getTermDescription(cx)(hydra.ext.python.coder.pythonEnvironmentGetGraph(env))(term))((comment: Option[scala.Predef.String]) =>
          {
          lazy val normComment: Option[scala.Predef.String] = hydra.lib.maybes.map[scala.Predef.String, scala.Predef.String](hydra.coderUtils.normalizeComment)(comment)
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Statement, Seq[Seq[hydra.ext.python.syntax.Statement]]](hydra.ext.python.coder.encodeTermAssignment(cx)(env)(name)(term)(typ)(normComment))((stmt: hydra.ext.python.syntax.Statement) => Right(Seq(Seq(stmt))))
        })
      }
    }
  }
  case hydra.module.Definition.`type`(v_Definition_type_td) => {
    lazy val name: hydra.core.Name = (v_Definition_type_td.name)
    {
      lazy val typ: hydra.core.Type = (v_Definition_type_td.`type`)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[scala.Predef.String], Seq[Seq[hydra.ext.python.syntax.Statement]]](hydra.annotations.getTypeDescription(cx)(hydra.ext.python.coder.pythonEnvironmentGetGraph(env))(typ))((comment: Option[scala.Predef.String]) =>
        {
        lazy val normComment: Option[scala.Predef.String] = hydra.lib.maybes.map[scala.Predef.String, scala.Predef.String](hydra.coderUtils.normalizeComment)(comment)
        hydra.ext.python.coder.encodeTypeAssignment(cx)(env)(name)(typ)(normComment)
      })
    }
  }

def termArityWithPrimitives(graph: hydra.graph.Graph)(term: hydra.core.Term): Int =
  hydra.rewriting.deannotateAndDetypeTerm(term) match
  case hydra.core.Term.application(v_Term_application_app) => hydra.lib.math.max(0)(hydra.lib.math.sub(hydra.ext.python.coder.termArityWithPrimitives(graph)(v_Term_application_app.function))(1))
  case hydra.core.Term.function(v_Term_function_f) => hydra.ext.python.coder.functionArityWithPrimitives(graph)(v_Term_function_f)
  case hydra.core.Term.variable(v_Term_variable_name) => hydra.lib.maybes.maybe[Int, hydra.core.Binding](0)((el: hydra.core.Binding) =>
    hydra.lib.maybes.maybe[Int, hydra.core.TypeScheme](hydra.arity.termArity(el.term))((ts: hydra.core.TypeScheme) => hydra.arity.typeSchemeArity(ts))(el.`type`))(hydra.lexical.lookupElement(graph)(v_Term_variable_name))
  case _ => 0

def functionArityWithPrimitives(graph: hydra.graph.Graph)(f: hydra.core.Function): Int =
  f match
  case hydra.core.Function.elimination(_) => 1
  case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.lib.math.add(1)(hydra.ext.python.coder.termArityWithPrimitives(graph)(v_Function_lambda_lam.body))
  case hydra.core.Function.primitive(v_Function_primitive_name) => hydra.lib.maybes.maybe[Int, hydra.graph.Primitive](0)((prim: hydra.graph.Primitive) => hydra.arity.primitiveArity(prim))(hydra.lib.maps.lookup[hydra.core.Name, hydra.graph.Primitive](v_Function_primitive_name)(graph.primitives))
  case _ => 0

def pythonEnvironmentGetGraph(env: hydra.ext.python.environment.PythonEnvironment): hydra.graph.Graph = (env.graph)

def pythonEnvironmentSetGraph(tc: hydra.graph.Graph)(env: hydra.ext.python.environment.PythonEnvironment): hydra.ext.python.environment.PythonEnvironment =
  hydra.ext.python.environment.PythonEnvironment(env.namespaces, (env.boundTypeVariables), tc, (env.nullaryBindings), (env.version), (env.skipCasts), (env.inlineVariables))

def withLambda[T0](v1: hydra.ext.python.environment.PythonEnvironment)(v2: hydra.core.Lambda)(v3: (hydra.ext.python.environment.PythonEnvironment => T0)): T0 =
  hydra.schemas.withLambdaContext(hydra.ext.python.coder.pythonEnvironmentGetGraph)(hydra.ext.python.coder.pythonEnvironmentSetGraph)(v1)(v2)(v3)

def withTypeLambda[T0](v1: hydra.ext.python.environment.PythonEnvironment)(v2: hydra.core.TypeLambda)(v3: (hydra.ext.python.environment.PythonEnvironment => T0)): T0 =
  hydra.schemas.withTypeLambdaContext(hydra.ext.python.coder.pythonEnvironmentGetGraph)(hydra.ext.python.coder.pythonEnvironmentSetGraph)(v1)(v2)(v3)

def withLet[T0](v1: hydra.ext.python.environment.PythonEnvironment)(v2: hydra.core.Let)(v3: (hydra.ext.python.environment.PythonEnvironment => T0)): T0 =
  hydra.schemas.withLetContext(hydra.ext.python.coder.pythonEnvironmentGetGraph)(hydra.ext.python.coder.pythonEnvironmentSetGraph)(hydra.ext.python.coder.pythonBindingMetadata)(v1)(v2)(v3)

def withLetInline[T0](env: hydra.ext.python.environment.PythonEnvironment)(lt: hydra.core.Let)(body: (hydra.ext.python.environment.PythonEnvironment => T0)): T0 =
  {
  lazy val bindingNames: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.Binding, hydra.core.Name]((b: hydra.core.Binding) => (b.name))(lt.bindings)
  lazy val inlineVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](bindingNames)
  def noMetadata[T1, T2, T3](tc: T1)(b: T2): Option[T3] = None
  hydra.schemas.withLetContext(hydra.ext.python.coder.pythonEnvironmentGetGraph)(hydra.ext.python.coder.pythonEnvironmentSetGraph)(noMetadata)(env)(lt)((innerEnv: hydra.ext.python.environment.PythonEnvironment) =>
    {
    lazy val updatedEnv: hydra.ext.python.environment.PythonEnvironment = hydra.ext.python.environment.PythonEnvironment(innerEnv.namespaces, (innerEnv.boundTypeVariables), (innerEnv.graph), (innerEnv.nullaryBindings), (innerEnv.version), (innerEnv.skipCasts), hydra.lib.sets.union[hydra.core.Name](inlineVars)(innerEnv.inlineVariables))
    body(updatedEnv)
  })
}

def initialMetadata(ns: hydra.module.Namespace): hydra.ext.python.environment.PythonModuleMetadata =
  {
  lazy val dottedNs: hydra.ext.python.syntax.DottedName = hydra.ext.python.names.encodeNamespace(ns)
  lazy val emptyNs: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName] = hydra.module.Namespaces(Tuple2(ns, dottedNs), hydra.lib.maps.empty[hydra.module.Namespace, hydra.ext.python.syntax.DottedName])
  hydra.ext.python.environment.PythonModuleMetadata(emptyNs, hydra.lib.sets.empty[hydra.core.Name], false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)
}

def initialEnvironment(namespaces: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName])(tcontext: hydra.graph.Graph): hydra.ext.python.environment.PythonEnvironment =
  hydra.ext.python.environment.PythonEnvironment(namespaces, Tuple2(Seq(), hydra.lib.maps.empty[hydra.core.Name, hydra.ext.python.syntax.Name]), tcontext, hydra.lib.sets.empty[hydra.core.Name], hydra.ext.python.coder.targetPythonVersion, true, hydra.lib.sets.empty[hydra.core.Name])

lazy val targetPythonVersion: hydra.ext.python.environment.PythonVersion = hydra.ext.python.utils.targetPythonVersion

def pythonBindingMetadata(g: hydra.graph.Graph)(b: hydra.core.Binding): Option[hydra.core.Term] =
  hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.ext.python.coder.shouldThunkBinding(g)(b))(hydra.coderUtils.bindingMetadata(g)(b))(None)

def shouldThunkBinding(g: hydra.graph.Graph)(b: hydra.core.Binding): Boolean =
  hydra.lib.logic.and(hydra.coderUtils.isComplexBinding(g)(b))(hydra.lib.logic.not(hydra.coderUtils.isTrivialTerm(b.term)))

def analyzePythonFunction[T0](cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(term: hydra.core.Term): Either[T0, hydra.typing.FunctionStructure[hydra.ext.python.environment.PythonEnvironment]] =
  hydra.coderUtils.analyzeFunctionTermWith(cx)(hydra.ext.python.coder.pythonBindingMetadata)(hydra.ext.python.coder.pythonEnvironmentGetGraph)(hydra.ext.python.coder.pythonEnvironmentSetGraph)(env)(term)

def withDefinitions[T0](env: hydra.ext.python.environment.PythonEnvironment)(defs: Seq[hydra.module.Definition])(body: (hydra.ext.python.environment.PythonEnvironment => T0)): T0 =
  {
  lazy val bindings: Seq[hydra.core.Binding] = hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.module.Definition, Option[hydra.core.Binding]]((`def_`: hydra.module.Definition) =>
    `def_` match
    case hydra.module.Definition.term(v_Definition_term_td) => Some(hydra.core.Binding(v_Definition_term_td.name, (v_Definition_term_td.term), (v_Definition_term_td.`type`)))
    case hydra.module.Definition.`type`(_) => None
    case _ => None)(defs))
  lazy val dummyLet: hydra.core.Let = hydra.core.Let(bindings, hydra.core.Term.literal(hydra.core.Literal.string("dummy")))
  hydra.ext.python.coder.withLet(env)(dummyLet)(body)
}

def encodeBindingAsAssignment(cx: hydra.context.Context)(allowThunking: Boolean)(env: hydra.ext.python.environment.PythonEnvironment)(binding: hydra.core.Binding): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.NamedExpression] =
  {
  lazy val name: hydra.core.Name = (binding.name)
  lazy val term: hydra.core.Term = (binding.term)
  lazy val mts: Option[hydra.core.TypeScheme] = (binding.`type`)
  lazy val pyName: hydra.ext.python.syntax.Name = hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env)(name)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.NamedExpression](hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(term))((pbody: hydra.ext.python.syntax.Expression) =>
    {
    lazy val tc: hydra.graph.Graph = (env.graph)
    {
      lazy val isComplexVar: Boolean = hydra.coderUtils.isComplexVariable(tc)(name)
      {
        lazy val termIsComplex: Boolean = hydra.coderUtils.isComplexTerm(tc)(term)
        {
          lazy val isTrivial: Boolean = hydra.coderUtils.isTrivialTerm(term)
          {
            lazy val needsThunk: Boolean = hydra.lib.logic.ifElse[Boolean](isTrivial)(false)(hydra.lib.maybes.maybe[Boolean, hydra.core.TypeScheme](hydra.lib.logic.and(allowThunking)(hydra.lib.logic.or(isComplexVar)(termIsComplex)))((ts: hydra.core.TypeScheme) =>
              hydra.lib.logic.and(allowThunking)(hydra.lib.logic.and(hydra.lib.equality.equal[Int](hydra.arity.typeSchemeArity(ts))(0))(hydra.lib.logic.or(isComplexVar)(termIsComplex))))(mts))
            {
              lazy val pterm: hydra.ext.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.ext.python.syntax.Expression](needsThunk)(hydra.ext.python.coder.makeThunk(pbody))(pbody)
              Right(hydra.ext.python.syntax.NamedExpression.assignment(hydra.ext.python.syntax.AssignmentExpression(pyName, pterm)))
            }
          }
        }
      }
    }
  })
}

def encodeTermMultilineTCO(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(funcName: hydra.core.Name)(paramNames: Seq[hydra.core.Name])(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement]] =
  {
  lazy val stripped: hydra.core.Term = hydra.rewriting.deannotateAndDetypeTerm(term)
  lazy val gathered: Tuple2[Seq[hydra.core.Term], hydra.core.Term] = hydra.coderUtils.gatherApplications(stripped)
  lazy val gatherArgs: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], hydra.core.Term](gathered)
  lazy val gatherFun: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Term], hydra.core.Term](gathered)
  lazy val strippedFun: hydra.core.Term = hydra.rewriting.deannotateAndDetypeTerm(gatherFun)
  lazy val isSelfCall: Boolean = strippedFun match
    case hydra.core.Term.variable(v_Term_variable_n) => hydra.lib.equality.equal[hydra.core.Name](v_Term_variable_n)(funcName)
    case _ => false
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement]]](hydra.lib.logic.and(isSelfCall)(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Term](gatherArgs))(hydra.lib.lists.length[hydra.core.Name](paramNames))))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Expression], Seq[hydra.ext.python.syntax.Statement]](hydra.lib.eithers.mapList[hydra.core.Term, hydra.ext.python.syntax.Expression, hydra.context.InContext[hydra.errors.Error]]((a: hydra.core.Term) => hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(a))(gatherArgs))((pyArgs: Seq[hydra.ext.python.syntax.Expression]) =>
    {
    lazy val assignments: Seq[hydra.ext.python.syntax.Statement] = hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.ext.python.syntax.Expression], hydra.ext.python.syntax.Statement]((pair: Tuple2[hydra.core.Name, hydra.ext.python.syntax.Expression]) =>
      {
      lazy val paramName: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.ext.python.syntax.Expression](pair)
      {
        lazy val pyArg: hydra.ext.python.syntax.Expression = hydra.lib.pairs.second[hydra.core.Name, hydra.ext.python.syntax.Expression](pair)
        hydra.ext.python.utils.assignmentStatement(hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env)(paramName))(pyArg)
      }
    })(hydra.lib.lists.zip[hydra.core.Name, hydra.ext.python.syntax.Expression](paramNames)(pyArgs))
    {
      lazy val continueStmt: hydra.ext.python.syntax.Statement = hydra.ext.python.syntax.Statement.simple(Seq(hydra.ext.python.syntax.SimpleStatement.continue))
      Right(hydra.lib.lists.concat2[hydra.ext.python.syntax.Statement](assignments)(Seq(continueStmt)))
    }
  }))({
    lazy val gathered2: Tuple2[Seq[hydra.core.Term], hydra.core.Term] = hydra.coderUtils.gatherApplications(term)
    {
      lazy val args2: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], hydra.core.Term](gathered2)
      {
        lazy val body2: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Term], hydra.core.Term](gathered2)
        hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement]]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Term](args2))(1))({
          lazy val arg: hydra.core.Term = hydra.lib.lists.head[hydra.core.Term](args2)
          hydra.rewriting.deannotateAndDetypeTerm(body2) match
            case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
              case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
                case hydra.core.Elimination.union(v_Elimination_union_cs) => {
                  lazy val tname: hydra.core.Name = (v_Elimination_union_cs.typeName)
                  {
                    lazy val dflt: Option[hydra.core.Term] = (v_Elimination_union_cs.default)
                    {
                      lazy val `cases_`: Seq[hydra.core.Field] = (v_Elimination_union_cs.cases)
                      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.FieldType], Seq[hydra.ext.python.syntax.Statement]](hydra.schemas.requireUnionType(cx)(hydra.ext.python.coder.pythonEnvironmentGetGraph(env))(tname))((rt: Seq[hydra.core.FieldType]) =>
                        {
                        lazy val isEnum: Boolean = hydra.schemas.isEnumRowType(rt)
                        {
                          lazy val isFull: Boolean = hydra.ext.python.coder.isCasesFull(rt)(`cases_`)
                          hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Statement]](hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(arg))((pyArg: hydra.ext.python.syntax.Expression) =>
                            hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.CaseBlock], Seq[hydra.ext.python.syntax.Statement]](hydra.lib.eithers.mapList[hydra.core.Field, hydra.ext.python.syntax.CaseBlock, hydra.context.InContext[hydra.errors.Error]]((v1: hydra.core.Field) =>
                            hydra.ext.python.coder.encodeCaseBlock(cx)(env)(tname)(rt)(isEnum)((e2: hydra.ext.python.environment.PythonEnvironment) =>
                            (t2: hydra.core.Term) =>
                            hydra.ext.python.coder.encodeTermMultilineTCO(cx)(e2)(funcName)(paramNames)(t2))(v1))(hydra.ext.python.coder.deduplicateCaseVariables(`cases_`)))((pyCases: Seq[hydra.ext.python.syntax.CaseBlock]) =>
                            hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.CaseBlock], Seq[hydra.ext.python.syntax.Statement]](hydra.ext.python.coder.encodeDefaultCaseBlock((t2: hydra.core.Term) => hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(t2))(isFull)(dflt)(tname))((pyDflt: Seq[hydra.ext.python.syntax.CaseBlock]) =>
                            {
                            lazy val subj: hydra.ext.python.syntax.SubjectExpression = hydra.ext.python.syntax.SubjectExpression.simple(hydra.ext.python.syntax.NamedExpression.simple(pyArg))
                            {
                              lazy val matchStmt: hydra.ext.python.syntax.Statement = hydra.ext.python.syntax.Statement.compound(hydra.ext.python.syntax.CompoundStatement.`match`(hydra.ext.python.syntax.MatchStatement(subj, hydra.lib.lists.concat2[hydra.ext.python.syntax.CaseBlock](pyCases)(pyDflt))))
                              Right(Seq(matchStmt))
                            }
                          })))
                        }
                      })
                    }
                  }
                }
                case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Statement]](hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(term))((expr: hydra.ext.python.syntax.Expression) => Right(Seq(hydra.ext.python.utils.returnSingle(expr))))
              case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Statement]](hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(term))((expr: hydra.ext.python.syntax.Expression) => Right(Seq(hydra.ext.python.utils.returnSingle(expr))))
            case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Statement]](hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(term))((expr: hydra.ext.python.syntax.Expression) => Right(Seq(hydra.ext.python.utils.returnSingle(expr))))
        })(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Statement]](hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(term))((expr: hydra.ext.python.syntax.Expression) => Right(Seq(hydra.ext.python.utils.returnSingle(expr)))))
      }
    }
  })
}

def encodeFunctionDefinition(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(name: hydra.core.Name)(tparams: Seq[hydra.core.Name])(args: Seq[hydra.core.Name])(body: hydra.core.Term)(doms: Seq[hydra.core.Type])(mcod: Option[hydra.core.Type])(comment: Option[scala.Predef.String])(prefixes: Seq[hydra.ext.python.syntax.Statement]): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Statement] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.ParamNoDefault], hydra.ext.python.syntax.Statement](hydra.lib.eithers.mapList[Tuple2[hydra.core.Name, hydra.core.Type], hydra.ext.python.syntax.ParamNoDefault, hydra.context.InContext[hydra.errors.Error]]((pair: Tuple2[hydra.core.Name, hydra.core.Type]) =>
  {
  lazy val argName: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.core.Type](pair)
  {
    lazy val typ: hydra.core.Type = hydra.lib.pairs.second[hydra.core.Name, hydra.core.Type](pair)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.ParamNoDefault](hydra.ext.python.coder.encodeType(env)(typ))((pyTyp: hydra.ext.python.syntax.Expression) =>
      Right(hydra.ext.python.syntax.ParamNoDefault(hydra.ext.python.syntax.Param(hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env)(argName), Some(pyTyp)), None)))
  }
})(hydra.lib.lists.zip[hydra.core.Name, hydra.core.Type](args)(doms)))((pyArgs: Seq[hydra.ext.python.syntax.ParamNoDefault]) =>
  {
  lazy val pyParams: hydra.ext.python.syntax.Parameters = hydra.ext.python.syntax.Parameters.paramNoDefault(hydra.ext.python.syntax.ParamNoDefaultParameters(pyArgs, Seq(), None))
  {
    lazy val isTCO: Boolean = hydra.lib.logic.and(hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Name](args)))(hydra.coderUtils.isSelfTailRecursive(name)(body))
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Block, hydra.ext.python.syntax.Statement](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Block]](isTCO)(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement], hydra.ext.python.syntax.Block](hydra.ext.python.coder.encodeTermMultilineTCO(cx)(env)(name)(args)(body))((tcoStmts: Seq[hydra.ext.python.syntax.Statement]) =>
      {
      lazy val trueExpr: hydra.ext.python.syntax.NamedExpression = hydra.ext.python.syntax.NamedExpression.simple(hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.`true`))
      {
        lazy val whileBody: hydra.ext.python.syntax.Block = hydra.ext.python.utils.indentedBlock(None)(Seq(hydra.lib.lists.concat2[hydra.ext.python.syntax.Statement](prefixes)(tcoStmts)))
        {
          lazy val whileStmt: hydra.ext.python.syntax.Statement = hydra.ext.python.syntax.Statement.compound(hydra.ext.python.syntax.CompoundStatement.`while`(hydra.ext.python.syntax.WhileStatement(trueExpr, whileBody, None)))
          Right(hydra.ext.python.utils.indentedBlock(comment)(Seq(Seq(whileStmt))))
        }
      }
    }))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement], hydra.ext.python.syntax.Block](hydra.ext.python.coder.encodeTermMultiline(cx)(env)(body))((stmts: Seq[hydra.ext.python.syntax.Statement]) =>
      Right(hydra.ext.python.utils.indentedBlock(comment)(Seq(hydra.lib.lists.concat2[hydra.ext.python.syntax.Statement](prefixes)(stmts)))))))((block: hydra.ext.python.syntax.Block) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[hydra.ext.python.syntax.Expression], hydra.ext.python.syntax.Statement](hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], Option[hydra.ext.python.syntax.Expression]], hydra.core.Type](Right(None))((cod: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Option[hydra.ext.python.syntax.Expression]](hydra.ext.python.coder.encodeType(env)(cod))((pytyp: hydra.ext.python.syntax.Expression) => Right(Some(pytyp))))(mcod))((mreturnType: Option[hydra.ext.python.syntax.Expression]) =>
      {
      lazy val pyTparams: Seq[hydra.ext.python.syntax.TypeParameter] = hydra.lib.logic.ifElse[Seq[hydra.ext.python.syntax.TypeParameter]](hydra.ext.python.coder.useInlineTypeParams)(hydra.lib.lists.map[hydra.core.Name, hydra.ext.python.syntax.TypeParameter]((`arg_`: hydra.core.Name) =>
        hydra.ext.python.utils.pyNameToPyTypeParameter(hydra.ext.python.names.encodeTypeVariable(`arg_`)))(tparams))(Seq())
      {
        lazy val isThunk: Boolean = hydra.lib.lists.`null`[hydra.core.Name](args)
        {
          lazy val mDecorators: Option[hydra.ext.python.syntax.Decorators] = hydra.lib.logic.ifElse[Option[hydra.ext.python.syntax.Decorators]](isThunk)(Some(Seq(hydra.ext.python.coder.lruCacheDecorator)))(None)
          {
            lazy val pyName: hydra.ext.python.syntax.Name = hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env)(name)
            Right(hydra.ext.python.syntax.Statement.compound(hydra.ext.python.syntax.CompoundStatement.function(hydra.ext.python.syntax.FunctionDefinition(mDecorators, hydra.ext.python.syntax.FunctionDefRaw(false, pyName, pyTparams, Some(pyParams), mreturnType, None, block)))))
          }
        }
      }
    }))
  }
})

def encodeTermMultiline(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement]] =
  {
  lazy val dfltLogic: Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement]] = hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.typing.FunctionStructure[hydra.ext.python.environment.PythonEnvironment], Seq[hydra.ext.python.syntax.Statement]](hydra.ext.python.coder.analyzePythonFunction(cx)(env)(term))((fs: hydra.typing.FunctionStructure[hydra.ext.python.environment.PythonEnvironment]) =>
    {
    lazy val params: Seq[hydra.core.Name] = (fs.params)
    {
      lazy val bindings: Seq[hydra.core.Binding] = (fs.bindings)
      {
        lazy val innerBody: hydra.core.Term = (fs.body)
        {
          lazy val env2: hydra.ext.python.environment.PythonEnvironment = (fs.environment)
          hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement]]](hydra.lib.lists.`null`[hydra.core.Binding](bindings))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Statement]](hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(term))((expr: hydra.ext.python.syntax.Expression) => Right(Seq(hydra.ext.python.utils.returnSingle(expr)))))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement], Seq[hydra.ext.python.syntax.Statement]](hydra.lib.eithers.mapList[hydra.core.Binding, hydra.ext.python.syntax.Statement, hydra.context.InContext[hydra.errors.Error]]((v1: hydra.core.Binding) => hydra.ext.python.coder.encodeBindingAs(cx)(env2)(v1))(bindings))((bindingStmts: Seq[hydra.ext.python.syntax.Statement]) =>
            hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement], Seq[hydra.ext.python.syntax.Statement]](hydra.ext.python.coder.encodeTermMultiline(cx)(env2)(innerBody))((bodyStmts: Seq[hydra.ext.python.syntax.Statement]) =>
            Right(hydra.lib.lists.concat2[hydra.ext.python.syntax.Statement](bindingStmts)(bodyStmts)))))
        }
      }
    }
  })
  lazy val gathered: Tuple2[Seq[hydra.core.Term], hydra.core.Term] = hydra.coderUtils.gatherApplications(term)
  lazy val args: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], hydra.core.Term](gathered)
  lazy val body: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Term], hydra.core.Term](gathered)
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement]]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Term](args))(1))({
    lazy val arg: hydra.core.Term = hydra.lib.lists.head[hydra.core.Term](args)
    hydra.rewriting.deannotateAndDetypeTerm(body) match
      case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
        case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
          case hydra.core.Elimination.union(v_Elimination_union_cs) => {
            lazy val tname: hydra.core.Name = (v_Elimination_union_cs.typeName)
            {
              lazy val dflt: Option[hydra.core.Term] = (v_Elimination_union_cs.default)
              {
                lazy val `cases_`: Seq[hydra.core.Field] = (v_Elimination_union_cs.cases)
                hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.FieldType], Seq[hydra.ext.python.syntax.Statement]](hydra.schemas.requireUnionType(cx)(hydra.ext.python.coder.pythonEnvironmentGetGraph(env))(tname))((rt: Seq[hydra.core.FieldType]) =>
                  {
                  lazy val isEnum: Boolean = hydra.schemas.isEnumRowType(rt)
                  {
                    lazy val isFull: Boolean = hydra.ext.python.coder.isCasesFull(rt)(`cases_`)
                    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Statement]](hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(arg))((pyArg: hydra.ext.python.syntax.Expression) =>
                      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.CaseBlock], Seq[hydra.ext.python.syntax.Statement]](hydra.lib.eithers.mapList[hydra.core.Field, hydra.ext.python.syntax.CaseBlock, hydra.context.InContext[hydra.errors.Error]]((v1: hydra.core.Field) =>
                      hydra.ext.python.coder.encodeCaseBlock(cx)(env)(tname)(rt)(isEnum)((e2: hydra.ext.python.environment.PythonEnvironment) =>
                      (t: hydra.core.Term) => hydra.ext.python.coder.encodeTermMultiline(cx)(e2)(t))(v1))(hydra.ext.python.coder.deduplicateCaseVariables(`cases_`)))((pyCases: Seq[hydra.ext.python.syntax.CaseBlock]) =>
                      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.CaseBlock], Seq[hydra.ext.python.syntax.Statement]](hydra.ext.python.coder.encodeDefaultCaseBlock((t: hydra.core.Term) => hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(t))(isFull)(dflt)(tname))((pyDflt: Seq[hydra.ext.python.syntax.CaseBlock]) =>
                      {
                      lazy val subj: hydra.ext.python.syntax.SubjectExpression = hydra.ext.python.syntax.SubjectExpression.simple(hydra.ext.python.syntax.NamedExpression.simple(pyArg))
                      {
                        lazy val matchStmt: hydra.ext.python.syntax.Statement = hydra.ext.python.syntax.Statement.compound(hydra.ext.python.syntax.CompoundStatement.`match`(hydra.ext.python.syntax.MatchStatement(subj, hydra.lib.lists.concat2[hydra.ext.python.syntax.CaseBlock](pyCases)(pyDflt))))
                        Right(Seq(matchStmt))
                      }
                    })))
                  }
                })
              }
            }
          }
          case _ => dfltLogic
        case _ => dfltLogic
      case _ => dfltLogic
  })(dfltLogic)
}

def encodeFunction(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(f: hydra.core.Function): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression] =
  f match
  case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.typing.FunctionStructure[hydra.ext.python.environment.PythonEnvironment], hydra.ext.python.syntax.Expression](hydra.ext.python.coder.analyzePythonFunction(cx)(env)(hydra.core.Term.function(hydra.core.Function.lambda(v_Function_lambda_lam))))((fs: hydra.typing.FunctionStructure[hydra.ext.python.environment.PythonEnvironment]) =>
    {
    lazy val params: Seq[hydra.core.Name] = (fs.params)
    {
      lazy val bindings: Seq[hydra.core.Binding] = (fs.bindings)
      {
        lazy val innerBody: hydra.core.Term = (fs.body)
        {
          lazy val innerEnv0: hydra.ext.python.environment.PythonEnvironment = (fs.environment)
          {
            lazy val bindingNames: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.Binding, hydra.core.Name]((b: hydra.core.Binding) => (b.name))(bindings)
            {
              lazy val innerEnv: hydra.ext.python.environment.PythonEnvironment = hydra.ext.python.environment.PythonEnvironment(innerEnv0.namespaces, (innerEnv0.boundTypeVariables), (innerEnv0.graph), (innerEnv0.nullaryBindings), (innerEnv0.version), (innerEnv0.skipCasts), hydra.lib.sets.union[hydra.core.Name](hydra.lib.sets.fromList[hydra.core.Name](bindingNames))(innerEnv0.inlineVariables))
              hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](hydra.ext.python.coder.encodeTermInline(cx)(innerEnv)(false)(innerBody))((pbody: hydra.ext.python.syntax.Expression) =>
                {
                lazy val pparams: Seq[hydra.ext.python.syntax.Name] = hydra.lib.lists.map[hydra.core.Name, hydra.ext.python.syntax.Name]((v1: hydra.core.Name) =>
                  hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(innerEnv)(v1))(params)
                hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression]](hydra.lib.lists.`null`[hydra.core.Binding](bindings))(Right(hydra.ext.python.coder.makeUncurriedLambda(pparams)(pbody)))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.NamedExpression], hydra.ext.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Binding, hydra.ext.python.syntax.NamedExpression, hydra.context.InContext[hydra.errors.Error]]((v1: hydra.core.Binding) =>
                  hydra.ext.python.coder.encodeBindingAsAssignment(cx)(false)(innerEnv)(v1))(bindings))((pbindingExprs: Seq[hydra.ext.python.syntax.NamedExpression]) =>
                  {
                  lazy val pbindingStarExprs: Seq[hydra.ext.python.syntax.StarNamedExpression] = hydra.lib.lists.map[hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.StarNamedExpression]((ne: hydra.ext.python.syntax.NamedExpression) => hydra.ext.python.syntax.StarNamedExpression.simple(ne))(pbindingExprs)
                  {
                    lazy val pbodyStarExpr: hydra.ext.python.syntax.StarNamedExpression = hydra.ext.python.utils.pyExpressionToPyStarNamedExpression(pbody)
                    {
                      lazy val tupleElements: Seq[hydra.ext.python.syntax.StarNamedExpression] = hydra.lib.lists.concat2[hydra.ext.python.syntax.StarNamedExpression](pbindingStarExprs)(Seq(pbodyStarExpr))
                      {
                        lazy val tupleExpr: hydra.ext.python.syntax.Expression = hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.tuple(tupleElements))
                        {
                          lazy val indexValue: hydra.ext.python.syntax.Expression = hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.number(hydra.ext.python.syntax.Number.integer(hydra.lib.literals.int32ToBigint(hydra.lib.lists.length[hydra.core.Binding](bindings)))))
                          {
                            lazy val indexedExpr: hydra.ext.python.syntax.Primary = hydra.ext.python.utils.primaryWithExpressionSlices(hydra.ext.python.utils.pyExpressionToPyPrimary(tupleExpr))(Seq(indexValue))
                            Right(hydra.ext.python.coder.makeUncurriedLambda(pparams)(hydra.ext.python.utils.pyPrimaryToPyExpression(indexedExpr)))
                          }
                        }
                      }
                    }
                  }
                }))
              })
            }
          }
        }
      }
    }
  })
  case hydra.core.Function.primitive(v_Function_primitive_name) => hydra.ext.python.coder.encodeVariable(cx)(env)(v_Function_primitive_name)(Seq())
  case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
    case hydra.core.Elimination.record(v_Elimination_record_proj) => {
      lazy val fname: hydra.core.Name = (v_Elimination_record_proj.field)
      Right(hydra.ext.python.coder.makeCurriedLambda(Seq("v1"))(hydra.ext.python.utils.projectFromExpression(hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None, hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None, hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false, hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("v1"))), None)))))))), Seq()))))))(hydra.ext.python.names.encodeFieldName(env)(fname))))
    }
    case hydra.core.Elimination.wrap(_) => Right(hydra.ext.python.coder.makeCurriedLambda(Seq("v1"))(hydra.ext.python.utils.projectFromExpression(hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None, hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None, hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false, hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("v1"))), None)))))))), Seq()))))))("value")))
    case hydra.core.Elimination.union(_) => Right(hydra.ext.python.coder.unsupportedExpression("case expressions as values are not yet supported"))

def encodeTermAssignment(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(name: hydra.core.Name)(term: hydra.core.Term)(ts: hydra.core.TypeScheme)(comment: Option[scala.Predef.String]): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Statement] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.typing.FunctionStructure[hydra.ext.python.environment.PythonEnvironment], hydra.ext.python.syntax.Statement](hydra.ext.python.coder.analyzePythonFunction(cx)(env)(term))((fs: hydra.typing.FunctionStructure[hydra.ext.python.environment.PythonEnvironment]) =>
  {
  lazy val tparams: Seq[hydra.core.Name] = (fs.typeParams)
  {
    lazy val params: Seq[hydra.core.Name] = (fs.params)
    {
      lazy val bindings: Seq[hydra.core.Binding] = (fs.bindings)
      {
        lazy val body: hydra.core.Term = (fs.body)
        {
          lazy val doms: Seq[hydra.core.Type] = (fs.domains)
          {
            lazy val mcod: Option[hydra.core.Type] = (fs.codomain)
            {
              lazy val env2: hydra.ext.python.environment.PythonEnvironment = (fs.environment)
              {
                lazy val tc: hydra.graph.Graph = (env2.graph)
                {
                  lazy val binding: hydra.core.Binding = hydra.core.Binding(name, term, Some(ts))
                  {
                    lazy val isComplex: Boolean = hydra.coderUtils.isComplexBinding(tc)(binding)
                    {
                      lazy val isTrivial: Boolean = hydra.coderUtils.isTrivialTerm(term)
                      hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Statement]](hydra.lib.logic.and(isComplex)(hydra.lib.logic.not(isTrivial)))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Statement], hydra.ext.python.syntax.Statement](hydra.lib.eithers.mapList[hydra.core.Binding, hydra.ext.python.syntax.Statement, hydra.context.InContext[hydra.errors.Error]]((v1: hydra.core.Binding) => hydra.ext.python.coder.encodeBindingAs(cx)(env2)(v1))(bindings))((bindingStmts: Seq[hydra.ext.python.syntax.Statement]) =>
                        hydra.ext.python.coder.encodeFunctionDefinition(cx)(env2)(name)(tparams)(params)(body)(doms)(mcod)(comment)(bindingStmts)))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Statement](hydra.ext.python.coder.encodeTermInline(cx)(env2)(false)(body))((bodyExpr: hydra.ext.python.syntax.Expression) =>
                        {
                        lazy val pyName: hydra.ext.python.syntax.Name = hydra.ext.python.names.encodeName(false)(hydra.util.CaseConvention.lowerSnake)(env2)(name)
                        Right(hydra.ext.python.utils.annotatedStatement(comment)(hydra.ext.python.utils.assignmentStatement(pyName)(bodyExpr)))
                      }))
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
})

def encodeVariable(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(name: hydra.core.Name)(args: Seq[hydra.ext.python.syntax.Expression]): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression] =
  {
  lazy val g: hydra.graph.Graph = hydra.ext.python.coder.pythonEnvironmentGetGraph(env)
  lazy val tc: hydra.graph.Graph = (env.graph)
  lazy val tcTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = (tc.boundTypes)
  lazy val tcLambdaVars: scala.collection.immutable.Set[hydra.core.Name] = (tc.lambdaVariables)
  lazy val tcMetadata: Map[hydra.core.Name, hydra.core.Term] = (tc.metadata)
  lazy val inlineVars: scala.collection.immutable.Set[hydra.core.Name] = (env.inlineVariables)
  lazy val mTypScheme: Option[hydra.core.TypeScheme] = hydra.lib.maps.lookup[hydra.core.Name, hydra.core.TypeScheme](name)(tcTypes)
  lazy val mTyp: Option[hydra.core.Type] = hydra.lib.maybes.map[hydra.core.TypeScheme, hydra.core.Type]((`ts_`: hydra.core.TypeScheme) => (`ts_`.`type`))(mTypScheme)
  lazy val asVariable: hydra.ext.python.syntax.Expression = hydra.ext.python.names.termVariableReference(env)(name)
  lazy val asFunctionCall: hydra.ext.python.syntax.Expression = hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary(hydra.ext.python.names.encodeName(true)(hydra.util.CaseConvention.lowerSnake)(env)(name)))(args)
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression]](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.ext.python.syntax.Expression](args)))(hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression], hydra.graph.Primitive](Right(asFunctionCall))((prim: hydra.graph.Primitive) =>
    {
    lazy val primArity: Int = hydra.arity.primitiveArity(prim)
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression]](hydra.lib.equality.equal[Int](primArity)(hydra.lib.lists.length[hydra.ext.python.syntax.Expression](args)))(Right(asFunctionCall))({
      lazy val numRemaining: Int = hydra.lib.math.sub(primArity)(hydra.lib.lists.length[hydra.ext.python.syntax.Expression](args))
      {
        lazy val remainingParams: Seq[hydra.ext.python.syntax.Name] = hydra.lib.lists.map[Int, hydra.ext.python.syntax.Name]((i: Int) => hydra.lib.strings.cat2("x")(hydra.lib.literals.showInt32(i)))(hydra.lib.math.range(1)(numRemaining))
        {
          lazy val remainingExprs: Seq[hydra.ext.python.syntax.Expression] = hydra.lib.lists.map[hydra.ext.python.syntax.Name, hydra.ext.python.syntax.Expression]((n: hydra.ext.python.syntax.Name) =>
            hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.syntax.BitwiseOr(None, hydra.ext.python.syntax.BitwiseXor(None, hydra.ext.python.syntax.BitwiseAnd(None, hydra.ext.python.syntax.ShiftExpression(None, hydra.ext.python.syntax.Sum(None, hydra.ext.python.syntax.Term(None, hydra.ext.python.syntax.Factor.simple(hydra.ext.python.syntax.Power(hydra.ext.python.syntax.AwaitPrimary(false, hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name(n))), None)))))))), Seq()))))))(remainingParams)
          {
            lazy val allArgs: Seq[hydra.ext.python.syntax.Expression] = hydra.lib.lists.concat2[hydra.ext.python.syntax.Expression](args)(remainingExprs)
            {
              lazy val fullCall: hydra.ext.python.syntax.Expression = hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary(hydra.ext.python.names.encodeName(true)(hydra.util.CaseConvention.lowerSnake)(env)(name)))(allArgs)
              Right(hydra.ext.python.coder.makeUncurriedLambda(remainingParams)(fullCall))
            }
          }
        }
      }
    })
  })(hydra.lexical.lookupPrimitive(g)(name)))(hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression], hydra.core.Type](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression]](hydra.lib.sets.member[hydra.core.Name](name)(tcLambdaVars))(Right(asVariable))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression]](hydra.lib.sets.member[hydra.core.Name](name)(inlineVars))(Right(asVariable))(hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression], hydra.graph.Primitive](hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression], hydra.core.Binding](hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression], hydra.core.Term](Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2("Unknown variable: ")(name)), cx)))((_x: hydra.core.Term) => Right(asFunctionCall))(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Term](name)(tcMetadata)))((el: hydra.core.Binding) =>
    {
    lazy val elTrivial1: Boolean = hydra.coderUtils.isTrivialTerm(el.term)
    hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression], hydra.core.TypeScheme](Right(asVariable))((ts: hydra.core.TypeScheme) =>
      hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression]](hydra.lib.logic.and(hydra.lib.logic.and(hydra.lib.equality.equal[Int](hydra.arity.typeSchemeArity(ts))(0))(hydra.coderUtils.isComplexBinding(tc)(el)))(hydra.lib.logic.not(elTrivial1)))(Right(asFunctionCall))({
      lazy val asFunctionRef: hydra.ext.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.ext.python.syntax.Expression](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Name](ts.variables)))(hydra.ext.python.coder.makeSimpleLambda(hydra.arity.typeArity(ts.`type`))(asVariable))(asVariable)
      Right(asFunctionRef)
    }))(el.`type`)
  })(hydra.lexical.lookupElement(g)(name)))((prim: hydra.graph.Primitive) =>
    {
    lazy val primArity: Int = hydra.arity.primitiveArity(prim)
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression]](hydra.lib.equality.equal[Int](primArity)(0))(Right(asFunctionCall))({
      lazy val ts: hydra.core.TypeScheme = (prim.`type`)
      {
        lazy val asFunctionRef: hydra.ext.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.ext.python.syntax.Expression](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Name](ts.variables)))(hydra.ext.python.coder.makeSimpleLambda(hydra.arity.typeArity(ts.`type`))(asVariable))(asVariable)
        Right(asFunctionRef)
      }
    })
  })(hydra.lexical.lookupPrimitive(g)(name)))))((typ: hydra.core.Type) =>
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression]](hydra.lib.sets.member[hydra.core.Name](name)(tcLambdaVars))(Right(asVariable))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression]](hydra.lib.sets.member[hydra.core.Name](name)(inlineVars))({
    lazy val asFunctionRef: hydra.ext.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.ext.python.syntax.Expression](hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](hydra.rewriting.freeVariablesInType(typ))))(hydra.ext.python.coder.makeSimpleLambda(hydra.arity.typeArity(typ))(asVariable))(asVariable)
    Right(asFunctionRef)
  })(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression]](hydra.lib.logic.not(hydra.lib.maps.member[hydra.core.Name, hydra.core.Term](name)(tcMetadata)))(hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression], hydra.core.Binding]({
    lazy val asFunctionRef: hydra.ext.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.ext.python.syntax.Expression](hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](hydra.rewriting.freeVariablesInType(typ))))(hydra.ext.python.coder.makeSimpleLambda(hydra.arity.typeArity(typ))(asVariable))(asVariable)
    Right(asFunctionRef)
  })((el: hydra.core.Binding) =>
    {
    lazy val elTrivial: Boolean = hydra.coderUtils.isTrivialTerm(el.term)
    hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression], hydra.core.TypeScheme](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression]](hydra.lib.logic.and(hydra.lib.equality.equal[Int](hydra.arity.typeArity(typ))(0))(hydra.lib.logic.not(elTrivial)))(Right(asFunctionCall))({
      lazy val asFunctionRef: hydra.ext.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.ext.python.syntax.Expression](hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](hydra.rewriting.freeVariablesInType(typ))))(hydra.ext.python.coder.makeSimpleLambda(hydra.arity.typeArity(typ))(asVariable))(asVariable)
      Right(asFunctionRef)
    }))((ts: hydra.core.TypeScheme) =>
      hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression]](hydra.lib.logic.and(hydra.lib.logic.and(hydra.lib.equality.equal[Int](hydra.arity.typeArity(typ))(0))(hydra.coderUtils.isComplexBinding(tc)(el)))(hydra.lib.logic.not(elTrivial)))(Right(asFunctionCall))({
      lazy val asFunctionRef: hydra.ext.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.ext.python.syntax.Expression](hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](hydra.rewriting.freeVariablesInType(typ))))(hydra.ext.python.coder.makeSimpleLambda(hydra.arity.typeArity(typ))(asVariable))(asVariable)
      Right(asFunctionRef)
    }))(el.`type`)
  })(hydra.lexical.lookupElement(g)(name)))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression]](hydra.lib.logic.and(hydra.lib.equality.equal[Int](hydra.arity.typeArity(typ))(0))(hydra.coderUtils.isComplexVariable(tc)(name)))(Right(asFunctionCall))({
    lazy val asFunctionRef: hydra.ext.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.ext.python.syntax.Expression](hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](hydra.rewriting.freeVariablesInType(typ))))(hydra.ext.python.coder.makeSimpleLambda(hydra.arity.typeArity(typ))(asVariable))(asVariable)
    Right(asFunctionRef)
  })))))(mTyp))
}

def encodeApplication(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(app: hydra.core.Application): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression] =
  {
  lazy val g: hydra.graph.Graph = hydra.ext.python.coder.pythonEnvironmentGetGraph(env)
  lazy val term: hydra.core.Term = hydra.core.Term.application(app)
  lazy val gathered: Tuple2[hydra.core.Term, Seq[hydra.core.Term]] = hydra.coderUtils.gatherArgs(term)(Seq())
  lazy val fun: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, Seq[hydra.core.Term]](gathered)
  lazy val args: Seq[hydra.core.Term] = hydra.lib.pairs.second[hydra.core.Term, Seq[hydra.core.Term]](gathered)
  lazy val knownArity: Int = hydra.ext.python.coder.termArityWithPrimitives(g)(fun)
  lazy val arity: Int = hydra.lib.math.max(knownArity)(hydra.lib.lists.length[hydra.core.Term](args))
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Expression], hydra.ext.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term, hydra.ext.python.syntax.Expression, hydra.context.InContext[hydra.errors.Error]]((t: hydra.core.Term) => hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(t))(args))((pargs: Seq[hydra.ext.python.syntax.Expression]) =>
    {
    lazy val hargs: Seq[hydra.ext.python.syntax.Expression] = hydra.lib.lists.take[hydra.ext.python.syntax.Expression](arity)(pargs)
    {
      lazy val rargs: Seq[hydra.ext.python.syntax.Expression] = hydra.lib.lists.drop[hydra.ext.python.syntax.Expression](arity)(pargs)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Tuple2[hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]], hydra.ext.python.syntax.Expression](hydra.ext.python.coder.encodeApplicationInner(cx)(env)(fun)(hargs)(rargs))((result: Tuple2[hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]]) =>
        {
        lazy val lhs: hydra.ext.python.syntax.Expression = hydra.lib.pairs.first[hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]](result)
        {
          lazy val remainingRargs: Seq[hydra.ext.python.syntax.Expression] = hydra.lib.pairs.second[hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]](result)
          {
            lazy val pyapp: hydra.ext.python.syntax.Expression = hydra.lib.lists.foldl[hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression]((t: hydra.ext.python.syntax.Expression) =>
              (a: hydra.ext.python.syntax.Expression) =>
              hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyExpressionToPyPrimary(t))(Seq(a)))(lhs)(remainingRargs)
            Right(pyapp)
          }
        }
      })
    }
  })
}

def encodeApplicationInner(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(fun: hydra.core.Term)(hargs: Seq[hydra.ext.python.syntax.Expression])(rargs: Seq[hydra.ext.python.syntax.Expression]): Either[hydra.context.InContext[hydra.errors.Error], Tuple2[hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]]] =
  {
  lazy val firstArg: hydra.ext.python.syntax.Expression = hydra.lib.lists.head[hydra.ext.python.syntax.Expression](hargs)
  lazy val restArgs: Seq[hydra.ext.python.syntax.Expression] = hydra.lib.lists.tail[hydra.ext.python.syntax.Expression](hargs)
  def withRest(e: hydra.ext.python.syntax.Expression): hydra.ext.python.syntax.Expression =
    hydra.lib.logic.ifElse[hydra.ext.python.syntax.Expression](hydra.lib.lists.`null`[hydra.ext.python.syntax.Expression](restArgs))(e)(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyExpressionToPyPrimary(e))(restArgs))
  lazy val defaultCase: Either[hydra.context.InContext[hydra.errors.Error], Tuple2[hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]]] = hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Tuple2[hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]]](hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(fun))((pfun: hydra.ext.python.syntax.Expression) =>
    Right(Tuple2(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyExpressionToPyPrimary(pfun))(hargs), rargs)))
  hydra.rewriting.deannotateAndDetypeTerm(fun) match
    case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
      case hydra.core.Function.elimination(v_Function_elimination_elm) => v_Function_elimination_elm match
        case hydra.core.Elimination.record(v_Elimination_record_proj) => {
          lazy val fname: hydra.core.Name = (v_Elimination_record_proj.field)
          {
            lazy val fieldExpr: hydra.ext.python.syntax.Expression = hydra.ext.python.utils.projectFromExpression(firstArg)(hydra.ext.python.names.encodeFieldName(env)(fname))
            Right(Tuple2(withRest(fieldExpr), rargs))
          }
        }
        case hydra.core.Elimination.union(v_Elimination_union_cs) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Tuple2[hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]]](hydra.ext.python.coder.encodeUnionEliminationInline(cx)(env)(v_Elimination_union_cs)(firstArg))((inlineExpr: hydra.ext.python.syntax.Expression) => Right(Tuple2(withRest(inlineExpr), rargs)))
        case hydra.core.Elimination.wrap(_) => {
          lazy val valueExpr: hydra.ext.python.syntax.Expression = hydra.ext.python.utils.projectFromExpression(firstArg)("value")
          {
            lazy val allArgs: Seq[hydra.ext.python.syntax.Expression] = hydra.lib.lists.concat2[hydra.ext.python.syntax.Expression](restArgs)(rargs)
            hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Tuple2[hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]]]](hydra.lib.lists.`null`[hydra.ext.python.syntax.Expression](allArgs))(Right(Tuple2(valueExpr, Seq())))(Right(Tuple2(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyExpressionToPyPrimary(valueExpr))(allArgs), Seq())))
          }
        }
        case _ => defaultCase
      case hydra.core.Function.primitive(v_Function_primitive_name) => {
        lazy val wrappedArgs: Seq[hydra.ext.python.syntax.Expression] = hydra.ext.python.coder.wrapLazyArguments(v_Function_primitive_name)(hargs)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Tuple2[hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]]](hydra.ext.python.coder.encodeVariable(cx)(env)(v_Function_primitive_name)(wrappedArgs))((expr: hydra.ext.python.syntax.Expression) => Right(Tuple2(expr, rargs)))
      }
      case hydra.core.Function.lambda(_) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Tuple2[hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]]](hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(fun))((pfun: hydra.ext.python.syntax.Expression) =>
        Right(Tuple2(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyExpressionToPyPrimary(pfun))(hargs), rargs)))
      case _ => defaultCase
    case hydra.core.Term.variable(v_Term_variable_name) => {
      lazy val g: hydra.graph.Graph = hydra.ext.python.coder.pythonEnvironmentGetGraph(env)
      {
        lazy val allArgs: Seq[hydra.ext.python.syntax.Expression] = hydra.lib.lists.concat2[hydra.ext.python.syntax.Expression](hargs)(rargs)
        hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], Tuple2[hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]]], hydra.core.Binding](hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Tuple2[hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]]](hydra.ext.python.coder.encodeVariable(cx)(env)(v_Term_variable_name)(hargs))((expr: hydra.ext.python.syntax.Expression) => Right(Tuple2(expr, rargs))))((el: hydra.core.Binding) =>
          hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], Tuple2[hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]]], hydra.core.TypeScheme](hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Tuple2[hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]]](hydra.ext.python.coder.encodeVariable(cx)(env)(v_Term_variable_name)(hargs))((expr: hydra.ext.python.syntax.Expression) => Right(Tuple2(expr, rargs))))((ts: hydra.core.TypeScheme) =>
          {
          lazy val elArity: Int = hydra.arity.typeSchemeArity(ts)
          {
            lazy val consumeCount: Int = hydra.lib.math.min(elArity)(hydra.lib.lists.length[hydra.ext.python.syntax.Expression](allArgs))
            {
              lazy val consumedArgs: Seq[hydra.ext.python.syntax.Expression] = hydra.lib.lists.take[hydra.ext.python.syntax.Expression](consumeCount)(allArgs)
              {
                lazy val remainingArgs: Seq[hydra.ext.python.syntax.Expression] = hydra.lib.lists.drop[hydra.ext.python.syntax.Expression](consumeCount)(allArgs)
                hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Tuple2[hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]]]](hydra.lib.lists.`null`[hydra.ext.python.syntax.Expression](consumedArgs))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Tuple2[hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]]](hydra.ext.python.coder.encodeVariable(cx)(env)(v_Term_variable_name)(Seq()))((expr: hydra.ext.python.syntax.Expression) => Right(Tuple2(expr, rargs))))(Right(Tuple2(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary(hydra.ext.python.names.encodeName(true)(hydra.util.CaseConvention.lowerSnake)(env)(v_Term_variable_name)))(consumedArgs), remainingArgs)))
              }
            }
          }
        })(el.`type`))(hydra.lexical.lookupElement(g)(v_Term_variable_name))
      }
    }
    case _ => defaultCase
}

def encodeUnionEliminationInline(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(cs: hydra.core.CaseStatement)(pyArg: hydra.ext.python.syntax.Expression): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression] =
  {
  lazy val tname: hydra.core.Name = (cs.typeName)
  lazy val mdefault: Option[hydra.core.Term] = (cs.default)
  lazy val `cases_`: Seq[hydra.core.Field] = (cs.cases)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.FieldType], hydra.ext.python.syntax.Expression](hydra.schemas.requireUnionType(cx)(hydra.ext.python.coder.pythonEnvironmentGetGraph(env))(tname))((rt: Seq[hydra.core.FieldType]) =>
    {
    lazy val isEnum: Boolean = hydra.schemas.isEnumRowType(rt)
    {
      lazy val valueExpr: hydra.ext.python.syntax.Expression = hydra.ext.python.utils.projectFromExpression(pyArg)("value")
      {
        lazy val isinstancePrimary: hydra.ext.python.syntax.Primary = hydra.ext.python.utils.pyNameToPyPrimary("isinstance")
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression], hydra.core.Term](Right(hydra.ext.python.coder.unsupportedExpression("no matching case in inline union elimination")))((dflt: hydra.core.Term) =>
          hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(dflt))(mdefault))((pyDefault: hydra.ext.python.syntax.Expression) =>
          {
          def encodeBranch(field: hydra.core.Field): Either[hydra.context.InContext[hydra.errors.Error], Tuple2[hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression]] =
            {
            lazy val fname: hydra.core.Name = (field.name)
            lazy val fterm: hydra.core.Term = (field.term)
            lazy val isUnitVariant: Boolean = hydra.ext.python.coder.isVariantUnitType(rt)(fname)
            lazy val pyVariantName: hydra.ext.python.syntax.Name = hydra.ext.python.coder.deconflictVariantName(true)(env)(tname)(fname)(env.graph)
            lazy val isinstanceCheck: hydra.ext.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.ext.python.syntax.Expression](isEnum)(hydra.ext.python.syntax.Expression.simple(Seq(Seq(hydra.ext.python.syntax.Inversion.simple(hydra.ext.python.syntax.Comparison(hydra.ext.python.utils.pyExpressionToBitwiseOr(pyArg), Seq(hydra.ext.python.syntax.CompareOpBitwiseOrPair(hydra.ext.python.syntax.CompareOp.eq, hydra.ext.python.utils.pyExpressionToBitwiseOr(hydra.ext.python.utils.pyNameToPyExpression(pyVariantName))))))))))(hydra.ext.python.utils.functionCall(isinstancePrimary)(Seq(pyArg, hydra.ext.python.utils.pyNameToPyExpression(pyVariantName))))
            hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Tuple2[hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression]](hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(fterm))((pyBranch: hydra.ext.python.syntax.Expression) =>
              {
              lazy val pyResult: hydra.ext.python.syntax.Expression = hydra.lib.logic.ifElse[hydra.ext.python.syntax.Expression](isEnum)(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyExpressionToPyPrimary(pyBranch))(Seq(pyArg)))(hydra.lib.logic.ifElse[hydra.ext.python.syntax.Expression](isUnitVariant)(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyExpressionToPyPrimary(pyBranch))(Seq(pyArg)))(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyExpressionToPyPrimary(pyBranch))(Seq(valueExpr))))
              Right(Tuple2(isinstanceCheck, pyResult))
            })
          }
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[Tuple2[hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression]], hydra.ext.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Field, Tuple2[hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression], hydra.context.InContext[hydra.errors.Error]](encodeBranch)(`cases_`))((encodedBranches: Seq[Tuple2[hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression]]) =>
            {
            def buildChain(elseExpr: hydra.ext.python.syntax.Expression)(branchPair: Tuple2[hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression]): hydra.ext.python.syntax.Expression =
              {
              lazy val checkExpr: hydra.ext.python.syntax.Expression = hydra.lib.pairs.first[hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](branchPair)
              lazy val resultExpr: hydra.ext.python.syntax.Expression = hydra.lib.pairs.second[hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](branchPair)
              hydra.ext.python.syntax.Expression.conditional(hydra.ext.python.syntax.Conditional(hydra.ext.python.utils.pyExpressionToDisjunction(resultExpr), hydra.ext.python.utils.pyExpressionToDisjunction(checkExpr), elseExpr))
            }
            Right(hydra.lib.lists.foldl[hydra.ext.python.syntax.Expression, Tuple2[hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression]](buildChain)(pyDefault)(hydra.lib.lists.reverse[Tuple2[hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression]](encodedBranches)))
          })
        })
      }
    }
  })
}

def encodeTermInline(cx: hydra.context.Context)(env: hydra.ext.python.environment.PythonEnvironment)(noCast: Boolean)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression] =
  {
  def encode(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression] = hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(t)
  def stripTypeApps(t: hydra.core.Term): hydra.core.Term =
    t match
    case hydra.core.Term.annotated(v_Term_annotated_ann) => stripTypeApps(v_Term_annotated_ann.body)
    case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => stripTypeApps(v_Term_typeApplication_ta.body)
    case _ => t
  def withCast[T1](pyexp: hydra.ext.python.syntax.Expression): Either[T1, hydra.ext.python.syntax.Expression] =
    hydra.lib.logic.ifElse[Either[T1, hydra.ext.python.syntax.Expression]](hydra.lib.logic.or(noCast)(env.skipCasts))(Right(pyexp))({
    lazy val tc: hydra.graph.Graph = (env.graph)
    {
      lazy val mtyp: Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type] = hydra.lib.eithers.map[Tuple2[hydra.core.Type, hydra.context.Context], hydra.core.Type, hydra.context.InContext[hydra.errors.Error]]((_r: Tuple2[hydra.core.Type, hydra.context.Context]) =>
        hydra.lib.pairs.first[hydra.core.Type, hydra.context.Context](_r))(hydra.checking.typeOf(cx)(tc)(Seq())(term))
      hydra.lib.eithers.either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type, Either[T1, hydra.ext.python.syntax.Expression]]((_x: hydra.context.InContext[hydra.errors.Error]) => Right(pyexp))((typ: hydra.core.Type) =>
        hydra.lib.eithers.either((_x) => Right(pyexp))((pytyp: hydra.ext.python.syntax.Expression) => Right(hydra.ext.python.utils.castTo(pytyp)(pyexp)))(hydra.ext.python.coder.encodeType(env)(typ)))(mtyp)
    }
  })
  hydra.rewriting.deannotateAndDetypeTerm(term) match
    case hydra.core.Term.application(v_Term_application_app) => hydra.ext.python.coder.encodeApplication(cx)(env)(v_Term_application_app)
    case hydra.core.Term.either(v_Term_either_et) => hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression]]((t1: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](encode(t1))((pyexp: hydra.ext.python.syntax.Expression) =>
      withCast(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary("Left"))(Seq(pyexp)))))((t1: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](encode(t1))((pyexp: hydra.ext.python.syntax.Expression) =>
      withCast(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary("Right"))(Seq(pyexp)))))(v_Term_either_et)
    case hydra.core.Term.function(v_Term_function_f) => hydra.ext.python.coder.encodeFunction(cx)(env)(v_Term_function_f)
    case hydra.core.Term.let(v_Term_let_lt) => {
      lazy val bindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
      {
        lazy val body: hydra.core.Term = (v_Term_let_lt.body)
        hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression]](hydra.lib.lists.`null`[hydra.core.Binding](bindings))(hydra.ext.python.coder.encodeTermInline(cx)(env)(false)(body))(hydra.ext.python.coder.withLetInline(env)(v_Term_let_lt)((innerEnv: hydra.ext.python.environment.PythonEnvironment) =>
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.NamedExpression], hydra.ext.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Binding, hydra.ext.python.syntax.NamedExpression, hydra.context.InContext[hydra.errors.Error]]((v1: hydra.core.Binding) =>
          hydra.ext.python.coder.encodeBindingAsAssignment(cx)(false)(innerEnv)(v1))(bindings))((pbindingExprs: Seq[hydra.ext.python.syntax.NamedExpression]) =>
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](hydra.ext.python.coder.encodeTermInline(cx)(innerEnv)(false)(body))((pbody: hydra.ext.python.syntax.Expression) =>
          {
          lazy val pbindingStarExprs: Seq[hydra.ext.python.syntax.StarNamedExpression] = hydra.lib.lists.map[hydra.ext.python.syntax.NamedExpression, hydra.ext.python.syntax.StarNamedExpression]((ne: hydra.ext.python.syntax.NamedExpression) => hydra.ext.python.syntax.StarNamedExpression.simple(ne))(pbindingExprs)
          {
            lazy val pbodyStarExpr: hydra.ext.python.syntax.StarNamedExpression = hydra.ext.python.utils.pyExpressionToPyStarNamedExpression(pbody)
            {
              lazy val tupleElements: Seq[hydra.ext.python.syntax.StarNamedExpression] = hydra.lib.lists.concat2[hydra.ext.python.syntax.StarNamedExpression](pbindingStarExprs)(Seq(pbodyStarExpr))
              {
                lazy val tupleExpr: hydra.ext.python.syntax.Expression = hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.tuple(tupleElements))
                {
                  lazy val indexValue: hydra.ext.python.syntax.Expression = hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.number(hydra.ext.python.syntax.Number.integer(hydra.lib.literals.int32ToBigint(hydra.lib.lists.length[hydra.core.Binding](bindings)))))
                  {
                    lazy val indexedExpr: hydra.ext.python.syntax.Primary = hydra.ext.python.utils.primaryWithExpressionSlices(hydra.ext.python.utils.pyExpressionToPyPrimary(tupleExpr))(Seq(indexValue))
                    Right(hydra.ext.python.utils.pyPrimaryToPyExpression(indexedExpr))
                  }
                }
              }
            }
          }
        }))))
      }
    }
    case hydra.core.Term.list(v_Term_list_terms) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Expression], hydra.ext.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term, hydra.ext.python.syntax.Expression, hydra.context.InContext[hydra.errors.Error]](encode)(v_Term_list_terms))((pyExprs: Seq[hydra.ext.python.syntax.Expression]) =>
      Right(hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.tuple(hydra.lib.lists.map[hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.StarNamedExpression](hydra.ext.python.utils.pyExpressionToPyStarNamedExpression)(pyExprs)))))
    case hydra.core.Term.literal(v_Term_literal_lit) => hydra.ext.python.coder.encodeLiteral(v_Term_literal_lit)
    case hydra.core.Term.map(v_Term_map_m) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.DoubleStarredKvpair], hydra.ext.python.syntax.Expression](hydra.lib.eithers.mapList[Tuple2[hydra.core.Term, hydra.core.Term], hydra.ext.python.syntax.DoubleStarredKvpair, hydra.context.InContext[hydra.errors.Error]]((kv: Tuple2[hydra.core.Term, hydra.core.Term]) =>
      {
      lazy val k: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](kv)
      {
        lazy val v: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kv)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.DoubleStarredKvpair](encode(k))((pyK: hydra.ext.python.syntax.Expression) =>
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.DoubleStarredKvpair](encode(v))((pyV: hydra.ext.python.syntax.Expression) =>
          Right(hydra.ext.python.syntax.DoubleStarredKvpair.pair(hydra.ext.python.syntax.Kvpair(pyK, pyV)))))
      }
    })(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m)))((pairs: Seq[hydra.ext.python.syntax.DoubleStarredKvpair]) =>
      Right(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary("FrozenDict"))(Seq(hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.dict(pairs))))))
    case hydra.core.Term.maybe(v_Term_maybe_mt) => hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression], hydra.core.Term](Right(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary("Nothing"))(Seq())))((t1: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](encode(t1))((pyexp: hydra.ext.python.syntax.Expression) =>
      withCast(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary("Just"))(Seq(pyexp)))))(v_Term_maybe_mt)
    case hydra.core.Term.pair(v_Term_pair_p) => {
      lazy val t1: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)
      {
        lazy val t2: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](encode(t1))((pyExpr1: hydra.ext.python.syntax.Expression) =>
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](encode(t2))((pyExpr2: hydra.ext.python.syntax.Expression) =>
          Right(hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.tuple(Seq(hydra.ext.python.utils.pyExpressionToPyStarNamedExpression(pyExpr1), hydra.ext.python.utils.pyExpressionToPyStarNamedExpression(pyExpr2)))))))
      }
    }
    case hydra.core.Term.record(v_Term_record_r) => {
      lazy val tname: hydra.core.Name = (v_Term_record_r.typeName)
      {
        lazy val fields: Seq[hydra.core.Field] = (v_Term_record_r.fields)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Expression], hydra.ext.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Field, hydra.ext.python.syntax.Expression, hydra.context.InContext[hydra.errors.Error]]((fld: hydra.core.Field) => encode(fld.term))(fields))((pargs: Seq[hydra.ext.python.syntax.Expression]) =>
          Right(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary(hydra.ext.python.names.encodeNameQualified(env)(tname)))(pargs)))
      }
    }
    case hydra.core.Term.set(v_Term_set_s) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Expression], hydra.ext.python.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term, hydra.ext.python.syntax.Expression, hydra.context.InContext[hydra.errors.Error]](encode)(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s)))((pyEls: Seq[hydra.ext.python.syntax.Expression]) =>
      Right(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary("frozenset"))(Seq(hydra.ext.python.utils.pyAtomToPyExpression(hydra.ext.python.syntax.Atom.set(hydra.lib.lists.map[hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.StarNamedExpression](hydra.ext.python.utils.pyExpressionToPyStarNamedExpression)(pyEls)))))))
    case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => {
      lazy val body: hydra.core.Term = (v_Term_typeApplication_ta.body)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](hydra.ext.python.coder.encodeTermInline(cx)(env)(true)(stripTypeApps(body)))((pybase: hydra.ext.python.syntax.Expression) => withCast(pybase))
    }
    case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
      lazy val body: hydra.core.Term = (v_Term_typeLambda_tl.body)
      hydra.ext.python.coder.withTypeLambda(env)(v_Term_typeLambda_tl)((env2: hydra.ext.python.environment.PythonEnvironment) =>
        hydra.ext.python.coder.encodeTermInline(cx)(env2)(noCast)(body))
    }
    case hydra.core.Term.union(v_Term_union_inj) => {
      lazy val tname: hydra.core.Name = (v_Term_union_inj.typeName)
      {
        lazy val field: hydra.core.Field = (v_Term_union_inj.field)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.FieldType], hydra.ext.python.syntax.Expression](hydra.schemas.requireUnionType(cx)(hydra.ext.python.coder.pythonEnvironmentGetGraph(env))(tname))((rt: Seq[hydra.core.FieldType]) =>
          hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression]](hydra.schemas.isEnumRowType(rt))(Right(hydra.ext.python.utils.projectFromExpression(hydra.ext.python.utils.pyNameToPyExpression(hydra.ext.python.names.encodeNameQualified(env)(tname)))(hydra.ext.python.names.encodeEnumValue(env)(field.name))))({
          lazy val fname: hydra.core.Name = (field.name)
          {
            lazy val isUnitVariant: Boolean = hydra.lib.maybes.maybe[Boolean, hydra.core.FieldType](false)((ft: hydra.core.FieldType) =>
              hydra.schemas.isUnitType(hydra.rewriting.deannotateType(ft.`type`)))(hydra.lib.lists.find[hydra.core.FieldType]((ft: hydra.core.FieldType) =>
              hydra.lib.equality.equal[scala.Predef.String](ft.name)(fname))(rt))
            hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Expression], hydra.ext.python.syntax.Expression](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.python.syntax.Expression]]](hydra.lib.logic.or(hydra.schemas.isUnitTerm(field.term))(isUnitVariant))(Right(Seq()))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, Seq[hydra.ext.python.syntax.Expression]](encode(field.term))((parg: hydra.ext.python.syntax.Expression) => Right(Seq(parg)))))((args: Seq[hydra.ext.python.syntax.Expression]) =>
              {
              lazy val deconflictedName: hydra.ext.python.syntax.Name = hydra.ext.python.coder.deconflictVariantName(true)(env)(tname)(fname)(env.graph)
              Right(hydra.ext.python.utils.castTo(hydra.ext.python.names.typeVariableReference(env)(tname))(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary(deconflictedName))(args)))
            })
          }
        }))
      }
    }
    case hydra.core.Term.unit => Right(hydra.ext.python.utils.pyNameToPyExpression(hydra.ext.python.utils.pyNone))
    case hydra.core.Term.variable(v_Term_variable_name) => hydra.ext.python.coder.encodeVariable(cx)(env)(v_Term_variable_name)(Seq())
    case hydra.core.Term.wrap(v_Term_wrap_wrapped) => {
      lazy val tname: hydra.core.Name = (v_Term_wrap_wrapped.typeName)
      {
        lazy val inner: hydra.core.Term = (v_Term_wrap_wrapped.body)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Expression, hydra.ext.python.syntax.Expression](encode(inner))((parg: hydra.ext.python.syntax.Expression) =>
          Right(hydra.ext.python.utils.functionCall(hydra.ext.python.utils.pyNameToPyPrimary(hydra.ext.python.names.encodeNameQualified(env)(tname)))(Seq(parg))))
      }
    }
}

def extendMetaForTerm(topLevel: Boolean)(meta0: hydra.ext.python.environment.PythonModuleMetadata)(term: hydra.core.Term): hydra.ext.python.environment.PythonModuleMetadata =
  {
  def step(meta: hydra.ext.python.environment.PythonModuleMetadata)(t: hydra.core.Term): hydra.ext.python.environment.PythonModuleMetadata =
    t match
    case hydra.core.Term.either(v_Term_either_e) => {
      lazy val metaWithCast: hydra.ext.python.environment.PythonModuleMetadata = hydra.ext.python.coder.setMetaUsesCast(true)(meta)
      hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, hydra.ext.python.environment.PythonModuleMetadata]((_x: hydra.core.Term) => hydra.ext.python.coder.setMetaUsesLeft(metaWithCast)(true))((_x: hydra.core.Term) => hydra.ext.python.coder.setMetaUsesRight(metaWithCast)(true))(v_Term_either_e)
    }
    case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
      case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.lib.maybes.maybe[hydra.ext.python.environment.PythonModuleMetadata, hydra.core.Type](meta)((dom: hydra.core.Type) =>
        hydra.lib.logic.ifElse[hydra.ext.python.environment.PythonModuleMetadata](topLevel)(hydra.ext.python.coder.extendMetaForType(true)(false)(dom)(meta))(meta))(v_Function_lambda_lam.domain)
      case _ => meta
    case hydra.core.Term.let(v_Term_let_lt) => {
      lazy val bindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
      hydra.lib.lists.foldl[hydra.ext.python.environment.PythonModuleMetadata, hydra.core.Binding]({
        def forBinding(m: hydra.ext.python.environment.PythonModuleMetadata)(b: hydra.core.Binding): hydra.ext.python.environment.PythonModuleMetadata =
          hydra.lib.maybes.maybe[hydra.ext.python.environment.PythonModuleMetadata, hydra.core.TypeScheme](m)((ts: hydra.core.TypeScheme) =>
          {
          lazy val term1: hydra.core.Term = (b.term)
          hydra.lib.logic.ifElse[hydra.ext.python.environment.PythonModuleMetadata](hydra.coderUtils.isSimpleAssignment(term1))(m)(hydra.ext.python.coder.extendMetaForType(true)(true)(ts.`type`)(m))
        })(b.`type`)
        forBinding
      })(meta)(bindings)
    }
    case hydra.core.Term.literal(v_Term_literal_l) => v_Term_literal_l match
      case hydra.core.Literal.float(v_Literal_float_fv) => v_Literal_float_fv match
        case hydra.core.FloatValue.bigfloat(_) => hydra.ext.python.coder.setMetaUsesDecimal(meta)(true)
        case _ => meta
      case _ => meta
    case hydra.core.Term.map(_) => hydra.ext.python.coder.setMetaUsesFrozenDict(meta)(true)
    case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.lib.maybes.maybe[hydra.ext.python.environment.PythonModuleMetadata, hydra.core.Term](hydra.ext.python.coder.setMetaUsesNothing(meta)(true))((_x: hydra.core.Term) => hydra.ext.python.coder.setMetaUsesJust(meta)(true))(v_Term_maybe_m)
    case hydra.core.Term.union(_) => hydra.ext.python.coder.setMetaUsesCast(true)(meta)
    case _ => meta
  hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)(step)(meta0)(term)
}

def extendMetaForType(topLevel: Boolean)(isTermAnnot: Boolean)(typ: hydra.core.Type)(meta: hydra.ext.python.environment.PythonModuleMetadata): hydra.ext.python.environment.PythonModuleMetadata =
  {
  lazy val currentTvars: scala.collection.immutable.Set[hydra.core.Name] = (meta.typeVariables)
  lazy val newTvars: scala.collection.immutable.Set[hydra.core.Name] = hydra.ext.python.coder.collectTypeVariables(currentTvars)(typ)
  lazy val metaWithTvars: hydra.ext.python.environment.PythonModuleMetadata = hydra.ext.python.coder.setMetaTypeVariables(meta)(newTvars)
  lazy val metaWithSubtypes: hydra.ext.python.environment.PythonModuleMetadata = hydra.lib.lists.foldl[hydra.ext.python.environment.PythonModuleMetadata, hydra.core.Type]((m: hydra.ext.python.environment.PythonModuleMetadata) =>
    (t: hydra.core.Type) =>
    hydra.ext.python.coder.extendMetaForType(false)(isTermAnnot)(t)(m))(metaWithTvars)(hydra.rewriting.subtypes(typ))
  hydra.rewriting.deannotateType(typ) match
    case hydra.core.Type.function(v_Type_function_ft) => {
      lazy val cod: hydra.core.Type = (v_Type_function_ft.codomain)
      {
        lazy val dom: hydra.core.Type = (v_Type_function_ft.domain)
        {
          lazy val meta2: hydra.ext.python.environment.PythonModuleMetadata = hydra.ext.python.coder.extendMetaForType(topLevel)(isTermAnnot)(cod)(metaWithSubtypes)
          {
            lazy val meta3: hydra.ext.python.environment.PythonModuleMetadata = hydra.ext.python.coder.extendMetaForType(false)(isTermAnnot)(dom)(meta2)
            hydra.lib.logic.ifElse[hydra.ext.python.environment.PythonModuleMetadata](hydra.lib.logic.and(isTermAnnot)(topLevel))(meta3)(hydra.ext.python.coder.setMetaUsesCallable(meta3)(true))
          }
        }
      }
    }
    case hydra.core.Type.list(_) => hydra.ext.python.coder.setMetaUsesFrozenList(metaWithSubtypes)(true)
    case hydra.core.Type.map(_) => hydra.ext.python.coder.setMetaUsesFrozenDict(metaWithSubtypes)(true)
    case hydra.core.Type.maybe(_) => hydra.ext.python.coder.setMetaUsesMaybe(metaWithSubtypes)(true)
    case hydra.core.Type.either(_) => hydra.ext.python.coder.setMetaUsesEither(metaWithSubtypes)(true)
    case hydra.core.Type.literal(v_Type_literal_lt) => v_Type_literal_lt match
      case hydra.core.LiteralType.float(v_LiteralType_float_ft) => v_LiteralType_float_ft match
        case hydra.core.FloatType.bigfloat => hydra.ext.python.coder.setMetaUsesDecimal(metaWithSubtypes)(true)
        case _ => metaWithSubtypes
      case _ => metaWithSubtypes
    case hydra.core.Type.union(v_Type_union_rt) => hydra.lib.logic.ifElse[hydra.ext.python.environment.PythonModuleMetadata](hydra.schemas.isEnumRowType(v_Type_union_rt))(hydra.ext.python.coder.setMetaUsesEnum(metaWithSubtypes)(true))(hydra.lib.logic.ifElse[hydra.ext.python.environment.PythonModuleMetadata](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.FieldType](v_Type_union_rt)))(hydra.ext.python.coder.setMetaUsesNode(metaWithSubtypes)(true))(metaWithSubtypes))
    case hydra.core.Type.forall(v_Type_forall_ft) => {
      lazy val body: hydra.core.Type = (v_Type_forall_ft.body)
      {
        lazy val metaForWrap: hydra.ext.python.environment.PythonModuleMetadata = hydra.ext.python.coder.digForWrap(isTermAnnot)(metaWithSubtypes)(body)
        hydra.rewriting.deannotateType(body) match
          case hydra.core.Type.record(_) => hydra.ext.python.coder.setMetaUsesGeneric(metaForWrap)(true)
          case _ => metaForWrap
      }
    }
    case hydra.core.Type.record(v_Type_record_rt) => {
      lazy val hasAnnotated: Boolean = hydra.lib.lists.foldl[Boolean, hydra.core.FieldType]((b: Boolean) =>
        (ft: hydra.core.FieldType) =>
        hydra.lib.logic.or(b)(hydra.annotations.hasTypeDescription(ft.`type`)))(false)(v_Type_record_rt)
      {
        lazy val meta1: hydra.ext.python.environment.PythonModuleMetadata = hydra.lib.logic.ifElse[hydra.ext.python.environment.PythonModuleMetadata](hydra.lib.lists.`null`[hydra.core.FieldType](v_Type_record_rt))(metaWithSubtypes)(hydra.ext.python.coder.setMetaUsesDataclass(metaWithSubtypes)(true))
        hydra.lib.logic.ifElse[hydra.ext.python.environment.PythonModuleMetadata](hasAnnotated)(hydra.ext.python.coder.setMetaUsesAnnotated(meta1)(true))(meta1)
      }
    }
    case hydra.core.Type.wrap(_) => hydra.lib.logic.ifElse[hydra.ext.python.environment.PythonModuleMetadata](isTermAnnot)(metaWithSubtypes)(hydra.ext.python.coder.setMetaUsesNode(metaWithSubtypes)(true))
    case _ => metaWithSubtypes
}

def digForWrap(isTermAnnot: Boolean)(meta: hydra.ext.python.environment.PythonModuleMetadata)(typ: hydra.core.Type): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.rewriting.deannotateType(typ) match
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.ext.python.coder.digForWrap(isTermAnnot)(meta)(v_Type_forall_ft.body)
  case hydra.core.Type.wrap(_) => hydra.lib.logic.ifElse[hydra.ext.python.environment.PythonModuleMetadata](isTermAnnot)(meta)(hydra.ext.python.coder.setMetaUsesNode(meta)(true))
  case _ => meta

def setMetaNamespaces(ns: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName])(m: hydra.ext.python.environment.PythonModuleMetadata): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(ns, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesLeft(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), b, (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesRight(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), b, (m.usesTypeVar))

def setMetaUsesDecimal(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), b, (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesFrozenDict(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), b, (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesNothing(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), b, (m.usesRight), (m.usesTypeVar))

def setMetaUsesJust(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), b, (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesCallable(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), b, (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesLruCache(b: Boolean)(m: hydra.ext.python.environment.PythonModuleMetadata): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), b, (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesCast(b: Boolean)(m: hydra.ext.python.environment.PythonModuleMetadata): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), b, (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesGeneric(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), b, (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesFrozenList(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), b, (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesMaybe(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), b, (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesEither(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), b, (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesNode(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), b, (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesEnum(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), b, (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesAnnotated(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), b, (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesDataclass(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), b, (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaTypeVariables(m: hydra.ext.python.environment.PythonModuleMetadata)(tvars: scala.collection.immutable.Set[hydra.core.Name]): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, tvars, (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def isTypeVariableName(name: hydra.core.Name): Boolean =
  hydra.lib.equality.equal[Int](1)(hydra.lib.lists.length[scala.Predef.String](hydra.lib.strings.splitOn(".")(name)))

def collectTypeVariables(initial: scala.collection.immutable.Set[hydra.core.Name])(typ: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
  hydra.rewriting.deannotateType(typ) match
  case hydra.core.Type.forall(v_Type_forall_ft) => {
    lazy val v: hydra.core.Name = (v_Type_forall_ft.parameter)
    {
      lazy val body: hydra.core.Type = (v_Type_forall_ft.body)
      hydra.ext.python.coder.collectTypeVariables(hydra.lib.sets.insert[hydra.core.Name](v)(initial))(body)
    }
  }
  case _ => {
    lazy val freeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.rewriting.freeVariablesInType(typ)
    {
      def isTypeVar(n: hydra.core.Name): Boolean = hydra.ext.python.coder.isTypeVariableName(n)
      {
        lazy val filteredList: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name](isTypeVar)(hydra.lib.sets.toList[hydra.core.Name](freeVars))
        hydra.lib.sets.union[hydra.core.Name](initial)(hydra.lib.sets.fromList[hydra.core.Name](filteredList))
      }
    }
  }

def extendMetaForTypes(types: Seq[hydra.core.Type])(meta: hydra.ext.python.environment.PythonModuleMetadata): hydra.ext.python.environment.PythonModuleMetadata =
  {
  lazy val names: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.unions[hydra.core.Name](hydra.lib.lists.map[hydra.core.Type, scala.collection.immutable.Set[hydra.core.Name]]((t: hydra.core.Type) => hydra.rewriting.typeDependencyNames(false)(t))(types))
  lazy val currentNs: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName] = (meta.namespaces)
  lazy val updatedNs: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName] = hydra.schemas.addNamesToNamespaces(hydra.ext.python.names.encodeNamespace)(names)(currentNs)
  lazy val meta1: hydra.ext.python.environment.PythonModuleMetadata = hydra.ext.python.coder.setMetaNamespaces(updatedNs)(meta)
  hydra.lib.lists.foldl[hydra.ext.python.environment.PythonModuleMetadata, hydra.core.Type]((m: hydra.ext.python.environment.PythonModuleMetadata) =>
    (t: hydra.core.Type) => hydra.ext.python.coder.extendMetaForType(true)(false)(t)(m))(meta1)(types)
}

def setMetaUsesName(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), b, (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def setMetaUsesTypeVar(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), (m.usesTypeAlias), (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), b)

def emptyMetadata(ns: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(ns, hydra.lib.sets.empty[hydra.core.Name], false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false)

def gatherMetadata(focusNs: hydra.module.Namespace)(defs: Seq[hydra.module.Definition]): hydra.ext.python.environment.PythonModuleMetadata =
  {
  lazy val start: hydra.ext.python.environment.PythonModuleMetadata = hydra.ext.python.coder.emptyMetadata(hydra.ext.python.utils.findNamespaces(focusNs)(defs))
  def addDef(meta: hydra.ext.python.environment.PythonModuleMetadata)(`def`: hydra.module.Definition): hydra.ext.python.environment.PythonModuleMetadata =
    `def` match
    case hydra.module.Definition.term(v_Definition_term_termDef) => {
      lazy val term: hydra.core.Term = (v_Definition_term_termDef.term)
      {
        lazy val typ: hydra.core.Type = hydra.lib.maybes.maybe[hydra.core.Type, hydra.core.TypeScheme](hydra.core.Type.variable("hydra.core.Unit"))((x: hydra.core.TypeScheme) => (x.`type`))(v_Definition_term_termDef.`type`)
        {
          lazy val meta2: hydra.ext.python.environment.PythonModuleMetadata = hydra.ext.python.coder.extendMetaForType(true)(true)(typ)(meta)
          hydra.ext.python.coder.extendMetaForTerm(true)(meta2)(term)
        }
      }
    }
    case hydra.module.Definition.`type`(v_Definition_type_typeDef) => {
      lazy val typ: hydra.core.Type = (v_Definition_type_typeDef.`type`)
      {
        lazy val meta2: hydra.ext.python.environment.PythonModuleMetadata = hydra.ext.python.coder.setMetaUsesName(meta)(true)
        hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)((m: hydra.ext.python.environment.PythonModuleMetadata) =>
          (t: hydra.core.Type) => hydra.ext.python.coder.extendMetaForType(true)(false)(t)(m))(meta2)(typ)
      }
    }
  lazy val result: hydra.ext.python.environment.PythonModuleMetadata = hydra.lib.lists.foldl[hydra.ext.python.environment.PythonModuleMetadata, hydra.module.Definition](addDef)(start)(defs)
  lazy val tvars: scala.collection.immutable.Set[hydra.core.Name] = (result.typeVariables)
  lazy val result2: hydra.ext.python.environment.PythonModuleMetadata = hydra.ext.python.coder.setMetaUsesCast(true)(hydra.ext.python.coder.setMetaUsesLruCache(true)(result))
  hydra.ext.python.coder.setMetaUsesTypeVar(result2)(hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](tvars)))
}

def setMetaUsesTypeAlias(m: hydra.ext.python.environment.PythonModuleMetadata)(b: Boolean): hydra.ext.python.environment.PythonModuleMetadata =
  hydra.ext.python.environment.PythonModuleMetadata(m.namespaces, (m.typeVariables), (m.usesAnnotated), (m.usesCallable), (m.usesCast), (m.usesLruCache), b, (m.usesDataclass), (m.usesDecimal), (m.usesEither), (m.usesEnum), (m.usesFrozenDict), (m.usesFrozenList), (m.usesGeneric), (m.usesJust), (m.usesLeft), (m.usesMaybe), (m.usesName), (m.usesNode), (m.usesNothing), (m.usesRight), (m.usesTypeVar))

def isTypeModuleCheck(defs: Seq[hydra.module.Definition]): Boolean =
  hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.module.Definition](hydra.lib.lists.filter[hydra.module.Definition]((d: hydra.module.Definition) =>
  d match
  case hydra.module.Definition.`type`(_) => true
  case _ => false)(defs)))

def tvarStatement(name: hydra.ext.python.syntax.Name): hydra.ext.python.syntax.Statement =
  hydra.ext.python.utils.assignmentStatement(name)(hydra.ext.python.utils.functionCall(hydra.ext.python.syntax.Primary.simple(hydra.ext.python.syntax.Atom.name("TypeVar")))(Seq(hydra.ext.python.utils.doubleQuotedString(name))))

def condImportSymbol[T0](name: T0)(flag: Boolean): Option[T0] = hydra.lib.logic.ifElse[Option[T0]](flag)(Some(name))(None)

def moduleDomainImports(namespaces: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName]): Seq[hydra.ext.python.syntax.ImportStatement] =
  {
  lazy val names: Seq[hydra.ext.python.syntax.DottedName] = hydra.lib.lists.sort[hydra.ext.python.syntax.DottedName](hydra.lib.maps.elems[hydra.module.Namespace, hydra.ext.python.syntax.DottedName](namespaces.mapping))
  hydra.lib.lists.map[hydra.ext.python.syntax.DottedName, hydra.ext.python.syntax.ImportStatement]((ns: hydra.ext.python.syntax.DottedName) =>
    hydra.ext.python.syntax.ImportStatement.name(Seq(hydra.ext.python.syntax.DottedAsName(ns, None))))(names)
}

def standardImportStatement(modName: scala.Predef.String)(symbols: Seq[scala.Predef.String]): hydra.ext.python.syntax.ImportStatement =
  hydra.ext.python.syntax.ImportStatement.from(hydra.ext.python.syntax.ImportFrom(Seq(), Some(Seq(modName)), hydra.ext.python.syntax.ImportFromTargets.simple(hydra.lib.lists.map[scala.Predef.String, hydra.ext.python.syntax.ImportFromAsName]((s: scala.Predef.String) => hydra.ext.python.syntax.ImportFromAsName(s, None))(symbols))))

def moduleStandardImports(meta: hydra.ext.python.environment.PythonModuleMetadata): Seq[hydra.ext.python.syntax.ImportStatement] =
  {
  lazy val pairs: Seq[Tuple2[scala.Predef.String, Seq[Option[scala.Predef.String]]]] = Seq(Tuple2("__future__", Seq(hydra.ext.python.coder.condImportSymbol("annotations")(hydra.ext.python.names.useFutureAnnotations))), Tuple2("collections.abc", Seq(hydra.ext.python.coder.condImportSymbol("Callable")(meta.usesCallable))), Tuple2("dataclasses", Seq(hydra.ext.python.coder.condImportSymbol("dataclass")(meta.usesDataclass))), Tuple2("decimal", Seq(hydra.ext.python.coder.condImportSymbol("Decimal")(meta.usesDecimal))), Tuple2("enum", Seq(hydra.ext.python.coder.condImportSymbol("Enum")(meta.usesEnum))), Tuple2("functools", Seq(hydra.ext.python.coder.condImportSymbol("lru_cache")(meta.usesLruCache))), Tuple2("hydra.dsl.python", Seq(hydra.ext.python.coder.condImportSymbol("Either")(meta.usesEither), hydra.ext.python.coder.condImportSymbol("FrozenDict")(meta.usesFrozenDict), hydra.ext.python.coder.condImportSymbol("Just")(meta.usesJust), hydra.ext.python.coder.condImportSymbol("Left")(meta.usesLeft), hydra.ext.python.coder.condImportSymbol("Maybe")(meta.usesMaybe), hydra.ext.python.coder.condImportSymbol("Node")(meta.usesNode), hydra.ext.python.coder.condImportSymbol("Nothing")(meta.usesNothing), hydra.ext.python.coder.condImportSymbol("Right")(meta.usesRight), hydra.ext.python.coder.condImportSymbol("frozenlist")(meta.usesFrozenList))), Tuple2("typing", Seq(hydra.ext.python.coder.condImportSymbol("Annotated")(meta.usesAnnotated), hydra.ext.python.coder.condImportSymbol("Generic")(meta.usesGeneric), hydra.ext.python.coder.condImportSymbol("TypeAlias")(meta.usesTypeAlias), hydra.ext.python.coder.condImportSymbol("TypeVar")(meta.usesTypeVar), hydra.ext.python.coder.condImportSymbol("cast")(meta.usesCast))))
  lazy val simplified: Seq[Tuple2[scala.Predef.String, Seq[scala.Predef.String]]] = hydra.lib.maybes.cat[Tuple2[scala.Predef.String, Seq[scala.Predef.String]]](hydra.lib.lists.map[Tuple2[scala.Predef.String, Seq[Option[scala.Predef.String]]], Option[Tuple2[scala.Predef.String, Seq[scala.Predef.String]]]]((p: Tuple2[scala.Predef.String, Seq[Option[scala.Predef.String]]]) =>
    {
    lazy val modName: scala.Predef.String = hydra.lib.pairs.first[scala.Predef.String, Seq[Option[scala.Predef.String]]](p)
    {
      lazy val symbols: Seq[scala.Predef.String] = hydra.lib.maybes.cat[scala.Predef.String](hydra.lib.pairs.second[scala.Predef.String, Seq[Option[scala.Predef.String]]](p))
      hydra.lib.logic.ifElse[Option[Tuple2[scala.Predef.String, Seq[scala.Predef.String]]]](hydra.lib.lists.`null`[scala.Predef.String](symbols))(None)(Some(Tuple2(modName, symbols)))
    }
  })(pairs))
  hydra.lib.lists.map[Tuple2[scala.Predef.String, Seq[scala.Predef.String]], hydra.ext.python.syntax.ImportStatement]((p: Tuple2[scala.Predef.String, Seq[scala.Predef.String]]) =>
    hydra.ext.python.coder.standardImportStatement(hydra.lib.pairs.first[scala.Predef.String, Seq[scala.Predef.String]](p))(hydra.lib.pairs.second[scala.Predef.String, Seq[scala.Predef.String]](p)))(simplified)
}

def moduleImports(namespaces: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName])(meta: hydra.ext.python.environment.PythonModuleMetadata): Seq[hydra.ext.python.syntax.Statement] =
  hydra.lib.lists.map[hydra.ext.python.syntax.ImportStatement, hydra.ext.python.syntax.Statement]((imp: hydra.ext.python.syntax.ImportStatement) =>
  hydra.ext.python.utils.pySimpleStatementToPyStatement(hydra.ext.python.syntax.SimpleStatement.`import`(imp)))(hydra.lib.lists.concat[hydra.ext.python.syntax.ImportStatement](Seq(hydra.ext.python.coder.moduleStandardImports(meta), hydra.ext.python.coder.moduleDomainImports(namespaces))))

def encodePythonModule(cx: hydra.context.Context)(g: hydra.graph.Graph)(mod: hydra.module.Module)(defs0: Seq[hydra.module.Definition]): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Module] =
  {
  lazy val defs: Seq[hydra.module.Definition] = hydra.coderUtils.reorderDefs(defs0)
  lazy val meta0: hydra.ext.python.environment.PythonModuleMetadata = hydra.ext.python.coder.gatherMetadata(mod.namespace)(defs)
  lazy val namespaces0: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName] = (meta0.namespaces)
  lazy val env0: hydra.ext.python.environment.PythonEnvironment = hydra.ext.python.coder.initialEnvironment(namespaces0)(g)
  lazy val isTypeMod: Boolean = hydra.ext.python.coder.isTypeModuleCheck(defs0)
  hydra.ext.python.coder.withDefinitions(env0)(defs)((env: hydra.ext.python.environment.PythonEnvironment) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[Seq[hydra.ext.python.syntax.Statement]], hydra.ext.python.syntax.Module](hydra.lib.eithers.map[Seq[Seq[Seq[hydra.ext.python.syntax.Statement]]], Seq[Seq[hydra.ext.python.syntax.Statement]], hydra.context.InContext[hydra.errors.Error]]((xs: Seq[Seq[Seq[hydra.ext.python.syntax.Statement]]]) =>
    hydra.lib.lists.concat[Seq[hydra.ext.python.syntax.Statement]](xs))(hydra.lib.eithers.mapList[hydra.module.Definition, Seq[Seq[hydra.ext.python.syntax.Statement]], hydra.context.InContext[hydra.errors.Error]]((d: hydra.module.Definition) => hydra.ext.python.coder.encodeDefinition(cx)(env)(d))(defs)))((defStmts: Seq[Seq[hydra.ext.python.syntax.Statement]]) =>
    {
    lazy val meta2: hydra.ext.python.environment.PythonModuleMetadata = hydra.lib.logic.ifElse[hydra.ext.python.environment.PythonModuleMetadata](hydra.lib.logic.and(hydra.lib.logic.not(isTypeMod))(hydra.ext.python.coder.useInlineTypeParams))(hydra.ext.python.coder.setMetaUsesTypeVar(meta0)(false))(meta0)
    {
      lazy val meta: hydra.ext.python.environment.PythonModuleMetadata = hydra.lib.logic.ifElse[hydra.ext.python.environment.PythonModuleMetadata](hydra.lib.logic.and(isTypeMod)(hydra.lib.equality.equal[hydra.ext.python.environment.PythonVersion](hydra.ext.python.coder.targetPythonVersion)(hydra.ext.python.environment.PythonVersion.python310)))(hydra.ext.python.coder.setMetaUsesTypeAlias(meta2)(true))(meta2)
      {
        lazy val namespaces: hydra.module.Namespaces[hydra.ext.python.syntax.DottedName] = (meta0.namespaces)
        {
          lazy val commentStmts: Seq[hydra.ext.python.syntax.Statement] = hydra.lib.maybes.maybe[Seq[hydra.ext.python.syntax.Statement], scala.Predef.String](Seq())((c: scala.Predef.String) => Seq(hydra.ext.python.utils.commentStatement(c)))(hydra.lib.maybes.map[scala.Predef.String, scala.Predef.String](hydra.coderUtils.normalizeComment)(mod.description))
          {
            lazy val importStmts: Seq[hydra.ext.python.syntax.Statement] = hydra.ext.python.coder.moduleImports(namespaces)(meta)
            {
              lazy val tvars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.logic.ifElse[scala.collection.immutable.Set[hydra.core.Name]](hydra.lib.logic.or(isTypeMod)(hydra.lib.logic.not(hydra.ext.python.coder.useInlineTypeParams)))(meta.typeVariables)(hydra.lib.sets.empty[hydra.core.Name])
              {
                lazy val tvarStmts: Seq[hydra.ext.python.syntax.Statement] = hydra.lib.lists.map[hydra.core.Name, hydra.ext.python.syntax.Statement]((tv: hydra.core.Name) =>
                  hydra.ext.python.coder.tvarStatement(hydra.ext.python.names.encodeTypeVariable(tv)))(hydra.lib.sets.toList[hydra.core.Name](tvars))
                {
                  lazy val body: Seq[Seq[hydra.ext.python.syntax.Statement]] = hydra.lib.lists.filter[Seq[hydra.ext.python.syntax.Statement]]((group: Seq[hydra.ext.python.syntax.Statement]) =>
                    hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.ext.python.syntax.Statement](group)))(hydra.lib.lists.concat[Seq[hydra.ext.python.syntax.Statement]](Seq(Seq(commentStmts, importStmts, tvarStmts), defStmts)))
                  Right(body)
                }
              }
            }
          }
        }
      }
    }
  }))
}

def moduleToPython(mod: hydra.module.Module)(defs: Seq[hydra.module.Definition])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.errors.Error], Map[scala.Predef.String, scala.Predef.String]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.python.syntax.Module, Map[scala.Predef.String, scala.Predef.String]](hydra.ext.python.coder.encodePythonModule(cx)(g)(mod)(defs))((file: hydra.ext.python.syntax.Module) =>
  {
  lazy val s: scala.Predef.String = hydra.serialization.printExpr(hydra.serialization.parenthesize(hydra.ext.python.serde.encodeModule(file)))
  {
    lazy val path: scala.Predef.String = hydra.names.namespaceToFilePath(hydra.util.CaseConvention.lowerSnake)("py")(mod.namespace)
    Right(hydra.lib.maps.singleton[scala.Predef.String, scala.Predef.String](path)(s))
  }
})
