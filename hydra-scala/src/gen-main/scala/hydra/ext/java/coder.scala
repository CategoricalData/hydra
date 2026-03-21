package hydra.ext.java.coder

import hydra.context.*

import hydra.core.*

import hydra.error.*

import hydra.ext.java.helpers.*

import hydra.ext.java.syntax.*

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

val java8Features: hydra.ext.java.helpers.JavaFeatures = hydra.ext.java.helpers.JavaFeatures(false)

val java11Features: hydra.ext.java.helpers.JavaFeatures = hydra.ext.java.helpers.JavaFeatures(true)

val javaFeatures: hydra.ext.java.helpers.JavaFeatures = hydra.ext.java.coder.java11Features

val classModsPublic: Seq[hydra.ext.java.syntax.ClassModifier] = Seq(hydra.ext.java.syntax.ClassModifier.public)

def noComment(decl: hydra.ext.java.syntax.ClassBodyDeclaration): hydra.ext.java.syntax.ClassBodyDeclarationWithComments = hydra.ext.java.syntax.ClassBodyDeclarationWithComments(decl,
   None)

def typeArgsOrDiamond(args: Seq[hydra.ext.java.syntax.TypeArgument]): hydra.ext.java.syntax.TypeArgumentsOrDiamond =
  hydra.lib.logic.ifElse[hydra.ext.java.syntax.TypeArgumentsOrDiamond](hydra.ext.java.coder.javaFeatures.supportsDiamondOperator)(hydra.ext.java.syntax.TypeArgumentsOrDiamond.diamond)(hydra.ext.java.syntax.TypeArgumentsOrDiamond.arguments(args))

def bindingNameToFilePath(name: hydra.core.Name): scala.Predef.String =
  {
  val qn: hydra.module.QualifiedName = hydra.names.qualifyName(name)
  val `ns_`: Option[hydra.module.Namespace] = (qn.namespace)
  val local: scala.Predef.String = (qn.local)
  val sanitized: scala.Predef.String = hydra.formatting.sanitizeWithUnderscores(hydra.ext.java.language.reservedWords)(local)
  val unq: hydra.core.Name = hydra.names.unqualifyName(hydra.module.QualifiedName(`ns_`, sanitized))
  hydra.coderUtils.nameToFilePath(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.pascal)("java")(unq)
}

def javaIdentifierToString(id: hydra.ext.java.syntax.Identifier): scala.Predef.String = id

def boundTypeVariables(typ: hydra.core.Type): Seq[hydra.core.Name] =
  typ match
  case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.ext.java.coder.boundTypeVariables(v_Type_annotated_at.body)
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_ft.parameter)(hydra.ext.java.coder.boundTypeVariables(v_Type_forall_ft.body))
  case _ => Seq()

def extractTypeApplicationArgs(typ: hydra.core.Type): Seq[hydra.core.Type] =
  hydra.lib.lists.reverse[hydra.core.Type](hydra.ext.java.coder.extractTypeApplicationArgs_go(typ))

def extractTypeApplicationArgs_go(t: hydra.core.Type): Seq[hydra.core.Type] =
  t match
  case hydra.core.Type.application(v_Type_application_at) => hydra.lib.lists.cons[hydra.core.Type](v_Type_application_at.argument)(hydra.ext.java.coder.extractTypeApplicationArgs_go(v_Type_application_at.function))
  case _ => Seq()

def javaTypeParametersForType(typ: hydra.core.Type): Seq[hydra.ext.java.syntax.TypeParameter] =
  {
  def toParam(name: hydra.core.Name): hydra.ext.java.syntax.TypeParameter = hydra.ext.java.utils.javaTypeParameter(hydra.formatting.capitalize(name))
  val boundVars: Seq[hydra.core.Name] = hydra.ext.java.coder.javaTypeParametersForType_bvars(typ)
  val freeVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) => hydra.ext.java.coder.isLambdaBoundVariable(v))(hydra.lib.sets.toList[hydra.core.Name](hydra.rewriting.freeVariablesInType(typ)))
  val vars: Seq[hydra.core.Name] = hydra.lib.lists.nub[hydra.core.Name](hydra.lib.lists.concat2[hydra.core.Name](boundVars)(freeVars))
  hydra.lib.lists.map[hydra.core.Name, hydra.ext.java.syntax.TypeParameter](toParam)(vars)
}

def javaTypeParametersForType_bvars(t: hydra.core.Type): Seq[hydra.core.Name] =
  t match
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_ft.parameter)(hydra.ext.java.coder.javaTypeParametersForType_bvars(v_Type_forall_ft.body))
  case _ => Seq()

def javaTypeArgumentsForType(typ: hydra.core.Type): Seq[hydra.ext.java.syntax.TypeArgument] =
  hydra.lib.lists.reverse[hydra.ext.java.syntax.TypeArgument](hydra.lib.lists.map[hydra.ext.java.syntax.TypeParameter,
     hydra.ext.java.syntax.TypeArgument](hydra.ext.java.utils.typeParameterToTypeArgument)(hydra.ext.java.coder.javaTypeParametersForType(typ)))

def isLambdaBoundVariable(name: hydra.core.Name): Boolean =
  {
  val v: scala.Predef.String = name
  hydra.lib.equality.lte[Int](hydra.lib.strings.length(v))(4)
}

def isLocalVariable(name: hydra.core.Name): Boolean =
  hydra.lib.maybes.isNothing[hydra.module.Namespace](hydra.names.qualifyName(name).namespace)

def serializableTypes(isSer: Boolean): Seq[hydra.ext.java.syntax.InterfaceType] =
  {
  val javaSerializableType: hydra.ext.java.syntax.InterfaceType = hydra.ext.java.syntax.ClassType(Seq(),
     hydra.ext.java.syntax.ClassTypeQualifier.none, hydra.ext.java.utils.javaTypeIdentifier("Serializable"),
     Seq())
  hydra.lib.logic.ifElse[Seq[hydra.ext.java.syntax.InterfaceType]](isSer)(Seq(javaSerializableType))(Seq())
}

def encodeLiteralType[T0, T1, T2](lt: hydra.core.LiteralType)(cx: T0)(g: T1): Either[T2, hydra.ext.java.syntax.Type] =
  lt match
  case hydra.core.LiteralType.binary => Right(hydra.ext.java.syntax.Type.reference(hydra.ext.java.syntax.ReferenceType.array(hydra.ext.java.syntax.ArrayType(Seq(Seq()),
     hydra.ext.java.syntax.ArrayType_Variant.primitive(hydra.ext.java.syntax.PrimitiveTypeWithAnnotations(hydra.ext.java.syntax.PrimitiveType.numeric(hydra.ext.java.syntax.NumericType.integral(hydra.ext.java.syntax.IntegralType.byte)),
     Seq()))))))
  case hydra.core.LiteralType.boolean => hydra.ext.java.coder.encodeLiteralType_simple("Boolean")(cx)(g)
  case hydra.core.LiteralType.float(v_LiteralType_float_ft) => v_LiteralType_float_ft match
    case hydra.core.FloatType.bigfloat => Right(hydra.ext.java.utils.javaRefType(Seq())(Some(hydra.ext.java.names.javaPackageName(Seq("java",
       "math"))))("BigDecimal"))
    case hydra.core.FloatType.float32 => hydra.ext.java.coder.encodeLiteralType_simple("Float")(cx)(g)
    case hydra.core.FloatType.float64 => hydra.ext.java.coder.encodeLiteralType_simple("Double")(cx)(g)
  case hydra.core.LiteralType.integer(v_LiteralType_integer_it) => v_LiteralType_integer_it match
    case hydra.core.IntegerType.bigint => Right(hydra.ext.java.utils.javaRefType(Seq())(Some(hydra.ext.java.names.javaPackageName(Seq("java",
       "math"))))("BigInteger"))
    case hydra.core.IntegerType.int8 => hydra.ext.java.coder.encodeLiteralType_simple("Byte")(cx)(g)
    case hydra.core.IntegerType.int16 => hydra.ext.java.coder.encodeLiteralType_simple("Short")(cx)(g)
    case hydra.core.IntegerType.int32 => hydra.ext.java.coder.encodeLiteralType_simple("Integer")(cx)(g)
    case hydra.core.IntegerType.int64 => hydra.ext.java.coder.encodeLiteralType_simple("Long")(cx)(g)
    case hydra.core.IntegerType.uint8 => hydra.ext.java.coder.encodeLiteralType_simple("Short")(cx)(g)
    case hydra.core.IntegerType.uint16 => hydra.ext.java.coder.encodeLiteralType_simple("Character")(cx)(g)
    case hydra.core.IntegerType.uint32 => hydra.ext.java.coder.encodeLiteralType_simple("Long")(cx)(g)
    case hydra.core.IntegerType.uint64 => Right(hydra.ext.java.utils.javaRefType(Seq())(Some(hydra.ext.java.names.javaPackageName(Seq("java",
       "math"))))("BigInteger"))
  case hydra.core.LiteralType.string => hydra.ext.java.coder.encodeLiteralType_simple("String")(cx)(g)

def encodeLiteralType_simple[T0, T1, T2](n: scala.Predef.String)(cx: T0)(g: T1): Either[T2, hydra.ext.java.syntax.Type] = Right(hydra.ext.java.utils.javaRefType(Seq())(None)(n))

def elementsClassName(ns: hydra.module.Namespace): scala.Predef.String =
  {
  val nsStr: scala.Predef.String = ns
  val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(nsStr)
  hydra.formatting.sanitizeWithUnderscores(hydra.ext.java.language.reservedWords)(hydra.formatting.capitalize(hydra.lib.lists.last[scala.Predef.String](parts)))
}

def isRecursiveVariable(aliases: hydra.ext.java.helpers.Aliases)(name: hydra.core.Name): Boolean = hydra.lib.sets.member[hydra.core.Name](name)(aliases.recursiveVars)

def interfaceTypes(isSer: Boolean)(aliases: hydra.ext.java.helpers.Aliases)(tparams: Seq[hydra.ext.java.syntax.TypeParameter])(elName: hydra.core.Name): Seq[hydra.ext.java.syntax.InterfaceType] =
  {
  val javaSerializableType: hydra.ext.java.syntax.InterfaceType = hydra.ext.java.syntax.ClassType(Seq(),
     hydra.ext.java.syntax.ClassTypeQualifier.none, hydra.ext.java.utils.javaTypeIdentifier("Serializable"),
     Seq())
  val selfTypeArg: hydra.ext.java.syntax.TypeArgument = hydra.ext.java.syntax.TypeArgument.reference(hydra.ext.java.utils.nameToJavaReferenceType(aliases)(false)(hydra.lib.lists.map[hydra.ext.java.syntax.TypeParameter,
     hydra.ext.java.syntax.TypeArgument]((`tp_`: hydra.ext.java.syntax.TypeParameter) => hydra.ext.java.utils.typeParameterToTypeArgument(`tp_`))(tparams))(elName)(None))
  val javaComparableType: hydra.ext.java.syntax.InterfaceType = hydra.ext.java.syntax.ClassType(Seq(),
     hydra.ext.java.syntax.ClassTypeQualifier.none, hydra.ext.java.utils.javaTypeIdentifier("Comparable"),
     Seq(selfTypeArg))
  hydra.lib.logic.ifElse[Seq[hydra.ext.java.syntax.InterfaceType]](isSer)(Seq(javaSerializableType, javaComparableType))(Seq())
}

def isNonComparableType(typ: hydra.core.Type): Boolean =
  hydra.rewriting.deannotateType(typ) match
  case hydra.core.Type.either(v_Type_either__) => true
  case hydra.core.Type.function(v_Type_function__) => true
  case hydra.core.Type.unit => true
  case hydra.core.Type.literal(v_Type_literal_lt) => v_Type_literal_lt match
    case hydra.core.LiteralType.binary => true
    case _ => false
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.ext.java.coder.isNonComparableType(v_Type_forall_ft.body)
  case _ => false

def isBinaryType(typ: hydra.core.Type): Boolean =
  hydra.rewriting.deannotateType(typ) match
  case hydra.core.Type.literal(v_Type_literal_lt) => v_Type_literal_lt match
    case hydra.core.LiteralType.binary => true
    case _ => false
  case _ => false

def isBigNumericType(typ: hydra.core.Type): Boolean =
  hydra.rewriting.deannotateType(typ) match
  case hydra.core.Type.literal(v_Type_literal_lt) => v_Type_literal_lt match
    case hydra.core.LiteralType.float(v_LiteralType_float_ft) => v_LiteralType_float_ft match
      case hydra.core.FloatType.bigfloat => true
      case _ => false
    case hydra.core.LiteralType.integer(v_LiteralType_integer_it) => v_LiteralType_integer_it match
      case hydra.core.IntegerType.bigint => true
      case _ => false
    case _ => false
  case _ => false

def innerClassRef(aliases: hydra.ext.java.helpers.Aliases)(name: hydra.core.Name)(local: scala.Predef.String): hydra.ext.java.syntax.Identifier =
  {
  val id: scala.Predef.String = hydra.ext.java.utils.nameToJavaName(aliases)(name)
  hydra.lib.strings.cat2(hydra.lib.strings.cat2(id)("."))(local)
}

def peelExpectedTypes(subst: Map[hydra.core.Name, hydra.core.Type])(n: Int)(t: hydra.core.Type): Seq[hydra.core.Type] =
  hydra.lib.logic.ifElse[Seq[hydra.core.Type]](hydra.lib.equality.equal[Int](n)(0))(Seq())(hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => hydra.lib.lists.cons[hydra.core.Type](hydra.ext.java.coder.applySubstFull(subst)(v_Type_function_ft.domain))(hydra.ext.java.coder.peelExpectedTypes(subst)(hydra.lib.math.sub(n)(1))(v_Type_function_ft.codomain))
  case _ => Seq())

def applySubstFull(s: Map[hydra.core.Name, hydra.core.Type])(t: hydra.core.Type): hydra.core.Type =
  hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.maps.findWithDefault[hydra.core.Type, hydra.core.Name](t)(v_Type_variable_v)(s)
  case hydra.core.Type.function(v_Type_function_ft) => hydra.core.Type.function(hydra.core.FunctionType(hydra.ext.java.coder.applySubstFull(s)(v_Type_function_ft.domain),
     hydra.ext.java.coder.applySubstFull(s)(v_Type_function_ft.codomain)))
  case hydra.core.Type.application(v_Type_application_at) => hydra.core.Type.application(hydra.core.ApplicationType(hydra.ext.java.coder.applySubstFull(s)(v_Type_application_at.function),
     hydra.ext.java.coder.applySubstFull(s)(v_Type_application_at.argument)))
  case hydra.core.Type.list(v_Type_list_inner) => hydra.core.Type.list(hydra.ext.java.coder.applySubstFull(s)(v_Type_list_inner))
  case hydra.core.Type.set(v_Type_set_inner) => hydra.core.Type.set(hydra.ext.java.coder.applySubstFull(s)(v_Type_set_inner))
  case hydra.core.Type.maybe(v_Type_maybe_inner) => hydra.core.Type.maybe(hydra.ext.java.coder.applySubstFull(s)(v_Type_maybe_inner))
  case hydra.core.Type.map(v_Type_map_mt) => hydra.core.Type.map(hydra.core.MapType(hydra.ext.java.coder.applySubstFull(s)(v_Type_map_mt.keys),
     hydra.ext.java.coder.applySubstFull(s)(v_Type_map_mt.values)))
  case hydra.core.Type.pair(v_Type_pair_pt) => hydra.core.Type.pair(hydra.core.PairType(hydra.ext.java.coder.applySubstFull(s)(v_Type_pair_pt.first),
     hydra.ext.java.coder.applySubstFull(s)(v_Type_pair_pt.second)))
  case hydra.core.Type.either(v_Type_either_et) => hydra.core.Type.either(hydra.core.EitherType(hydra.ext.java.coder.applySubstFull(s)(v_Type_either_et.left),
     hydra.ext.java.coder.applySubstFull(s)(v_Type_either_et.right)))
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.core.Type.forall(hydra.core.ForallType(v_Type_forall_ft.parameter,
     hydra.ext.java.coder.applySubstFull(hydra.lib.maps.delete[hydra.core.Name, hydra.core.Type](v_Type_forall_ft.parameter)(s))(v_Type_forall_ft.body)))
  case _ => t

def collectForallParams(t: hydra.core.Type): Seq[hydra.core.Name] =
  hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.forall(v_Type_forall_fa) => hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_fa.parameter)(hydra.ext.java.coder.collectForallParams(v_Type_forall_fa.body))
  case _ => Seq()

def stripForalls(t: hydra.core.Type): hydra.core.Type =
  hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.forall(v_Type_forall_fa) => hydra.ext.java.coder.stripForalls(v_Type_forall_fa.body)
  case _ => t

def collectTypeVars(typ: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] = hydra.ext.java.coder.collectTypeVars_go(hydra.rewriting.deannotateType(typ))

def collectTypeVars_go(t: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
  t match
  case hydra.core.Type.variable(v_Type_variable_name) => hydra.lib.sets.singleton[hydra.core.Name](v_Type_variable_name)
  case hydra.core.Type.function(v_Type_function_ft) => hydra.lib.sets.union[hydra.core.Name](hydra.ext.java.coder.collectTypeVars_go(hydra.rewriting.deannotateType(v_Type_function_ft.domain)))(hydra.ext.java.coder.collectTypeVars_go(hydra.rewriting.deannotateType(v_Type_function_ft.codomain)))
  case hydra.core.Type.application(v_Type_application_at) => hydra.lib.sets.union[hydra.core.Name](hydra.ext.java.coder.collectTypeVars_go(hydra.rewriting.deannotateType(v_Type_application_at.function)))(hydra.ext.java.coder.collectTypeVars_go(hydra.rewriting.deannotateType(v_Type_application_at.argument)))
  case hydra.core.Type.list(v_Type_list_inner) => hydra.ext.java.coder.collectTypeVars_go(hydra.rewriting.deannotateType(v_Type_list_inner))
  case hydra.core.Type.set(v_Type_set_inner) => hydra.ext.java.coder.collectTypeVars_go(hydra.rewriting.deannotateType(v_Type_set_inner))
  case hydra.core.Type.maybe(v_Type_maybe_inner) => hydra.ext.java.coder.collectTypeVars_go(hydra.rewriting.deannotateType(v_Type_maybe_inner))
  case hydra.core.Type.map(v_Type_map_mt) => hydra.lib.sets.union[hydra.core.Name](hydra.ext.java.coder.collectTypeVars_go(hydra.rewriting.deannotateType(v_Type_map_mt.keys)))(hydra.ext.java.coder.collectTypeVars_go(hydra.rewriting.deannotateType(v_Type_map_mt.values)))
  case hydra.core.Type.pair(v_Type_pair_pt) => hydra.lib.sets.union[hydra.core.Name](hydra.ext.java.coder.collectTypeVars_go(hydra.rewriting.deannotateType(v_Type_pair_pt.first)))(hydra.ext.java.coder.collectTypeVars_go(hydra.rewriting.deannotateType(v_Type_pair_pt.second)))
  case hydra.core.Type.either(v_Type_either_et) => hydra.lib.sets.union[hydra.core.Name](hydra.ext.java.coder.collectTypeVars_go(hydra.rewriting.deannotateType(v_Type_either_et.left)))(hydra.ext.java.coder.collectTypeVars_go(hydra.rewriting.deannotateType(v_Type_either_et.right)))
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.ext.java.coder.collectTypeVars_go(hydra.rewriting.deannotateType(v_Type_forall_ft.body))
  case _ => hydra.lib.sets.empty[hydra.core.Name]

def substituteTypeVarsWithTypes(subst: Map[hydra.core.Name, hydra.core.Type])(t: hydra.core.Type): hydra.core.Type =
  hydra.ext.java.coder.substituteTypeVarsWithTypes_go(subst)(hydra.rewriting.deannotateType(t))

def substituteTypeVarsWithTypes_go(subst: Map[hydra.core.Name, hydra.core.Type])(t: hydra.core.Type): hydra.core.Type =
  hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.maybes.cases[hydra.core.Type, hydra.core.Type](hydra.lib.maps.lookup[hydra.core.Name,
     hydra.core.Type](v_Type_variable_v)(subst))(t)((rep: hydra.core.Type) => rep)
  case hydra.core.Type.function(v_Type_function_ft) => hydra.core.Type.function(hydra.core.FunctionType(hydra.ext.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_function_ft.domain),
     hydra.ext.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_function_ft.codomain)))
  case hydra.core.Type.application(v_Type_application_at) => hydra.core.Type.application(hydra.core.ApplicationType(hydra.ext.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_application_at.function),
     hydra.ext.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_application_at.argument)))
  case hydra.core.Type.list(v_Type_list_inner) => hydra.core.Type.list(hydra.ext.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_list_inner))
  case hydra.core.Type.set(v_Type_set_inner) => hydra.core.Type.set(hydra.ext.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_set_inner))
  case hydra.core.Type.maybe(v_Type_maybe_inner) => hydra.core.Type.maybe(hydra.ext.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_maybe_inner))
  case hydra.core.Type.map(v_Type_map_mt) => hydra.core.Type.map(hydra.core.MapType(hydra.ext.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_map_mt.keys),
     hydra.ext.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_map_mt.values)))
  case hydra.core.Type.pair(v_Type_pair_pt) => hydra.core.Type.pair(hydra.core.PairType(hydra.ext.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_pair_pt.first),
     hydra.ext.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_pair_pt.second)))
  case hydra.core.Type.either(v_Type_either_et) => hydra.core.Type.either(hydra.core.EitherType(hydra.ext.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_either_et.left),
     hydra.ext.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_either_et.right)))
  case hydra.core.Type.forall(v_Type_forall_ft) => hydra.core.Type.forall(hydra.core.ForallType(v_Type_forall_ft.parameter,
     hydra.ext.java.coder.substituteTypeVarsWithTypes_go(subst)(v_Type_forall_ft.body)))
  case _ => t

def addComment(decl: hydra.ext.java.syntax.ClassBodyDeclaration)(field: hydra.core.FieldType)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.ClassBodyDeclarationWithComments] =
  hydra.lib.eithers.map[Option[scala.Predef.String], hydra.ext.java.syntax.ClassBodyDeclarationWithComments,
     hydra.context.InContext[hydra.error.Error]]((c: Option[scala.Predef.String]) =>
  hydra.ext.java.syntax.ClassBodyDeclarationWithComments(decl, c))(hydra.coderUtils.commentsFromFieldType(cx)(g)(field))

def insertBranchVar(name: hydra.core.Name)(env: hydra.ext.java.helpers.JavaEnvironment): hydra.ext.java.helpers.JavaEnvironment =
  {
  val aliases: hydra.ext.java.helpers.Aliases = (env.aliases)
  hydra.ext.java.helpers.JavaEnvironment(hydra.ext.java.helpers.Aliases(aliases.currentNamespace, (aliases.packages),
     hydra.lib.sets.insert[hydra.core.Name](name)(aliases.branchVars), (aliases.recursiveVars), (aliases.inScopeTypeParams),
     (aliases.polymorphicLocals), (aliases.inScopeJavaVars), (aliases.varRenames), (aliases.lambdaVars),
     (aliases.typeVarSubst), (aliases.trustedTypeVars), (aliases.methodCodomain), (aliases.thunkedVars)),
     (env.graph))
}

def getCodomain(ann: Map[hydra.core.Name, hydra.core.Term])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.core.Type] =
  hydra.lib.eithers.map[hydra.core.FunctionType, hydra.core.Type, hydra.context.InContext[hydra.error.Error]]((ft: hydra.core.FunctionType) => (ft.codomain))(hydra.ext.java.coder.getFunctionType(ann)(cx)(g))

def getFunctionType(ann: Map[hydra.core.Name, hydra.core.Term])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.core.FunctionType] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type], hydra.core.FunctionType](hydra.lib.eithers.bimap[hydra.error.DecodingError,
     Option[hydra.core.Type], hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type]]((__de: hydra.error.DecodingError) => hydra.context.InContext(hydra.error.Error.other(__de),
     cx))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(ann)))((mt: Option[hydra.core.Type]) =>
  hydra.lib.maybes.cases[hydra.core.Type, Either[hydra.context.InContext[hydra.error.Error], hydra.core.FunctionType]](mt)(Left(hydra.context.InContext(hydra.error.Error.other("type annotation is required for function and elimination terms in Java"),
     cx)))((t: hydra.core.Type) =>
  t match
  case hydra.core.Type.function(v_Type_function_ft) => Right(v_Type_function_ft)
  case _ => Left(hydra.context.InContext(hydra.error.Error.other(hydra.lib.strings.cat2("expected function type, got: ")(hydra.show.core.`type`(t))), cx))))

def wrapLazyArguments(name: hydra.core.Name)(args: Seq[hydra.ext.java.syntax.Expression]): Tuple2[Seq[hydra.ext.java.syntax.Expression],
   Option[scala.Predef.String]] =
  hydra.lib.logic.ifElse[Tuple2[Seq[hydra.ext.java.syntax.Expression], Option[scala.Predef.String]]](hydra.lib.logic.and(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.logic.ifElse"))(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ext.java.syntax.Expression](args))(3)))(Tuple2(Seq(hydra.lib.lists.at[hydra.ext.java.syntax.Expression](0)(args),
     hydra.ext.java.coder.wrapInSupplierLambda(hydra.lib.lists.at[hydra.ext.java.syntax.Expression](1)(args)),
     hydra.ext.java.coder.wrapInSupplierLambda(hydra.lib.lists.at[hydra.ext.java.syntax.Expression](2)(args))),
     Some("lazy")))(hydra.lib.logic.ifElse[Tuple2[Seq[hydra.ext.java.syntax.Expression], Option[scala.Predef.String]]](hydra.lib.logic.and(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.maybe"))(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ext.java.syntax.Expression](args))(3)))(Tuple2(Seq(hydra.ext.java.coder.wrapInSupplierLambda(hydra.lib.lists.at[hydra.ext.java.syntax.Expression](0)(args)),
     hydra.lib.lists.at[hydra.ext.java.syntax.Expression](1)(args), hydra.lib.lists.at[hydra.ext.java.syntax.Expression](2)(args)),
     Some("applyLazy")))(hydra.lib.logic.ifElse[Tuple2[Seq[hydra.ext.java.syntax.Expression], Option[scala.Predef.String]]](hydra.lib.logic.and(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.cases"))(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ext.java.syntax.Expression](args))(3)))(Tuple2(Seq(hydra.lib.lists.at[hydra.ext.java.syntax.Expression](0)(args),
     hydra.ext.java.coder.wrapInSupplierLambda(hydra.lib.lists.at[hydra.ext.java.syntax.Expression](1)(args)),
     hydra.lib.lists.at[hydra.ext.java.syntax.Expression](2)(args)), Some("applyLazy")))(hydra.lib.logic.ifElse[Tuple2[Seq[hydra.ext.java.syntax.Expression],
     Option[scala.Predef.String]]](hydra.lib.logic.and(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maps.findWithDefault"))(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ext.java.syntax.Expression](args))(3)))(Tuple2(Seq(hydra.ext.java.coder.wrapInSupplierLambda(hydra.lib.lists.at[hydra.ext.java.syntax.Expression](0)(args)),
     hydra.lib.lists.at[hydra.ext.java.syntax.Expression](1)(args), hydra.lib.lists.at[hydra.ext.java.syntax.Expression](2)(args)),
     Some("applyLazy")))(hydra.lib.logic.ifElse[Tuple2[Seq[hydra.ext.java.syntax.Expression], Option[scala.Predef.String]]](hydra.lib.logic.and(hydra.lib.logic.or(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.maybes.fromMaybe"))(hydra.lib.logic.or(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.eithers.fromLeft"))(hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.eithers.fromRight"))))(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ext.java.syntax.Expression](args))(2)))(Tuple2(Seq(hydra.ext.java.coder.wrapInSupplierLambda(hydra.lib.lists.at[hydra.ext.java.syntax.Expression](0)(args)),
     hydra.lib.lists.at[hydra.ext.java.syntax.Expression](1)(args)), Some("applyLazy")))(Tuple2(args,
     None))))))

def wrapInSupplierLambda(expr: hydra.ext.java.syntax.Expression): hydra.ext.java.syntax.Expression =
  hydra.ext.java.syntax.Expression.lambda(hydra.ext.java.syntax.LambdaExpression(hydra.ext.java.syntax.LambdaParameters.tuple(Seq()),
     hydra.ext.java.syntax.LambdaBody.expression(expr)))

def elementJavaIdentifier(isPrim: Boolean)(isMethod: Boolean)(aliases: hydra.ext.java.helpers.Aliases)(name: hydra.core.Name): hydra.ext.java.syntax.Identifier =
  {
  val qn: hydra.module.QualifiedName = hydra.names.qualifyName(name)
  val `ns_`: Option[hydra.module.Namespace] = (qn.namespace)
  val local: scala.Predef.String = (qn.local)
  val sep: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](isMethod)("::")(".")
  hydra.lib.logic.ifElse[hydra.ext.java.syntax.Identifier](isPrim)(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.ext.java.coder.elementJavaIdentifier_qualify(aliases)(`ns_`)(hydra.formatting.capitalize(local)))("."))(hydra.ext.java.names.applyMethodName))(hydra.lib.maybes.cases[hydra.module.Namespace,
     hydra.ext.java.syntax.Identifier](`ns_`)(hydra.ext.java.utils.sanitizeJavaName(local))((n: hydra.module.Namespace) =>
    hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.ext.java.coder.elementJavaIdentifier_qualify(aliases)(Some(n))(hydra.ext.java.coder.elementsClassName(n)))(sep))(hydra.ext.java.utils.sanitizeJavaName(local))))
}

def elementJavaIdentifier_qualify(aliases: hydra.ext.java.helpers.Aliases)(mns: Option[hydra.module.Namespace])(s: scala.Predef.String): scala.Predef.String =
  hydra.ext.java.utils.nameToJavaName(aliases)(hydra.names.unqualifyName(hydra.module.QualifiedName(mns, s)))

def isLambdaBoundIn(name: hydra.core.Name)(lambdaVars: scala.collection.immutable.Set[hydra.core.Name]): Boolean =
  hydra.lib.logic.or(hydra.lib.sets.member[hydra.core.Name](name)(lambdaVars))(hydra.lib.logic.or(hydra.lib.logic.and(hydra.ext.java.coder.isLambdaBoundIn_isQualified(name))(hydra.lib.maybes.isJust[hydra.core.Name](hydra.lib.lists.find[hydra.core.Name]((lv: hydra.core.Name) =>
  hydra.lib.logic.and(hydra.ext.java.coder.isLambdaBoundIn_isQualified(lv))(hydra.lib.equality.equal[scala.Predef.String](hydra.names.localNameOf(lv))(hydra.names.localNameOf(name))))(hydra.lib.sets.toList[hydra.core.Name](lambdaVars)))))(hydra.lib.logic.and(hydra.lib.logic.not(hydra.ext.java.coder.isLambdaBoundIn_isQualified(name)))(hydra.lib.sets.member[hydra.core.Name](hydra.names.localNameOf(name))(lambdaVars))))

def isLambdaBoundIn_isQualified(n: hydra.core.Name): Boolean =
  hydra.lib.maybes.isJust[hydra.module.Namespace](hydra.names.qualifyName(n).namespace)

def findMatchingLambdaVar(name: hydra.core.Name)(lambdaVars: scala.collection.immutable.Set[hydra.core.Name]): hydra.core.Name =
  hydra.lib.logic.ifElse[hydra.core.Name](hydra.lib.sets.member[hydra.core.Name](name)(lambdaVars))(name)(hydra.lib.logic.ifElse[hydra.core.Name](hydra.ext.java.coder.isLambdaBoundIn_isQualified(name))(hydra.lib.maybes.fromMaybe[hydra.core.Name](name)(hydra.lib.lists.find[hydra.core.Name]((lv: hydra.core.Name) =>
  hydra.lib.logic.and(hydra.ext.java.coder.isLambdaBoundIn_isQualified(lv))(hydra.lib.equality.equal[scala.Predef.String](hydra.names.localNameOf(lv))(hydra.names.localNameOf(name))))(hydra.lib.sets.toList[hydra.core.Name](lambdaVars))))(hydra.lib.logic.ifElse[hydra.core.Name](hydra.lib.sets.member[hydra.core.Name](hydra.names.localNameOf(name))(lambdaVars))(hydra.names.localNameOf(name))(name)))

def constructElementsInterface(mod: hydra.module.Module)(members: Seq[hydra.ext.java.syntax.InterfaceMemberDeclaration]): Tuple2[hydra.core.Name,
   hydra.ext.java.syntax.CompilationUnit] =
  {
  val pkg: hydra.ext.java.syntax.PackageDeclaration = hydra.ext.java.utils.javaPackageDeclaration(mod.namespace)
  val mods: Seq[hydra.ext.java.syntax.InterfaceModifier] = Seq(hydra.ext.java.syntax.InterfaceModifier.public)
  val className: scala.Predef.String = hydra.ext.java.coder.elementsClassName(mod.namespace)
  val elName: hydra.core.Name = hydra.names.unqualifyName(hydra.module.QualifiedName(Some(mod.namespace), className))
  val body: hydra.ext.java.syntax.InterfaceBody = members
  val itf: hydra.ext.java.syntax.TypeDeclaration = hydra.ext.java.syntax.TypeDeclaration.interface(hydra.ext.java.syntax.InterfaceDeclaration.normalInterface(hydra.ext.java.syntax.NormalInterfaceDeclaration(mods,
     hydra.ext.java.utils.javaTypeIdentifier(className), Seq(), Seq(), body)))
  val decl: hydra.ext.java.syntax.TypeDeclarationWithComments = hydra.ext.java.syntax.TypeDeclarationWithComments(itf, (mod.description))
  Tuple2(elName, hydra.ext.java.syntax.CompilationUnit.ordinary(hydra.ext.java.syntax.OrdinaryCompilationUnit(Some(pkg), Seq(), Seq(decl))))
}

def splitConstantInitializer(member: hydra.ext.java.syntax.InterfaceMemberDeclaration): Seq[hydra.ext.java.syntax.InterfaceMemberDeclaration] =
  member match
  case hydra.ext.java.syntax.InterfaceMemberDeclaration.constant(v_InterfaceMemberDeclaration_constant_cd) => hydra.lib.lists.bind[hydra.ext.java.syntax.VariableDeclarator,
     hydra.ext.java.syntax.InterfaceMemberDeclaration](v_InterfaceMemberDeclaration_constant_cd.variables)((v1: hydra.ext.java.syntax.VariableDeclarator) =>
    hydra.ext.java.coder.splitConstantInitializer_splitVar(v_InterfaceMemberDeclaration_constant_cd.modifiers)(v_InterfaceMemberDeclaration_constant_cd.`type`)(v1))
  case _ => Seq(member)

def splitConstantInitializer_splitVar(mods: Seq[hydra.ext.java.syntax.ConstantModifier])(utype: hydra.ext.java.syntax.UnannType)(vd: hydra.ext.java.syntax.VariableDeclarator): Seq[hydra.ext.java.syntax.InterfaceMemberDeclaration] =
  {
  val vid: hydra.ext.java.syntax.VariableDeclaratorId = (vd.id)
  val mInit: Option[hydra.ext.java.syntax.VariableInitializer] = (vd.initializer)
  hydra.lib.maybes.cases[hydra.ext.java.syntax.VariableInitializer, Seq[hydra.ext.java.syntax.InterfaceMemberDeclaration]](mInit)(Seq(hydra.ext.java.syntax.InterfaceMemberDeclaration.constant(hydra.ext.java.syntax.ConstantDeclaration(mods,
     utype, Seq(vd)))))((`init_`: hydra.ext.java.syntax.VariableInitializer) =>
    `init_` match
    case hydra.ext.java.syntax.VariableInitializer.expression(v_VariableInitializer_expression_expr) => {
      val varName: scala.Predef.String = hydra.ext.java.coder.javaIdentifierToString(vid.identifier)
      val helperName: scala.Predef.String = hydra.lib.strings.cat2("_init_")(varName)
      val callExpr: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocation(None)(helperName)(Seq()))
      val field: hydra.ext.java.syntax.InterfaceMemberDeclaration = hydra.ext.java.syntax.InterfaceMemberDeclaration.constant(hydra.ext.java.syntax.ConstantDeclaration(mods,
         utype, Seq(hydra.ext.java.syntax.VariableDeclarator(vid, Some(hydra.ext.java.syntax.VariableInitializer.expression(callExpr))))))
      val returnSt: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(v_VariableInitializer_expression_expr)))
      val resultType: hydra.ext.java.syntax.Result = hydra.ext.java.syntax.Result.`type`(utype)
      val helper: hydra.ext.java.syntax.InterfaceMemberDeclaration = hydra.ext.java.utils.interfaceMethodDeclaration(Seq(hydra.ext.java.syntax.InterfaceMethodModifier.static,
         hydra.ext.java.syntax.InterfaceMethodModifier.`private`))(Seq())(helperName)(Seq())(resultType)(Some(Seq(returnSt)))
      Seq(field, helper)
    }
    case _ => Seq(hydra.ext.java.syntax.InterfaceMemberDeclaration.constant(hydra.ext.java.syntax.ConstantDeclaration(mods, utype, Seq(vd)))))
}

def isUnresolvedInferenceVar(name: hydra.core.Name): Boolean =
  {
  val chars: Seq[Int] = hydra.lib.strings.toList(name)
  hydra.lib.logic.ifElse[Boolean](hydra.lib.lists.`null`[Int](chars))(false)(hydra.lib.logic.ifElse[Boolean](hydra.lib.logic.not(hydra.lib.equality.equal[Int](hydra.lib.lists.head[Int](chars))(116)))(false)({
    val rest: Seq[Int] = hydra.lib.lists.tail[Int](chars)
    hydra.lib.logic.and(hydra.lib.logic.not(hydra.lib.lists.`null`[Int](rest)))(hydra.lib.lists.`null`[Int](hydra.lib.lists.filter[Int]((c: Int) =>
      hydra.lib.logic.not(hydra.ext.java.coder.isUnresolvedInferenceVar_isDigit(c)))(rest)))
  }))
}

def isUnresolvedInferenceVar_isDigit(c: Int): Boolean =
  hydra.lib.logic.and(hydra.lib.equality.gte[Int](c)(48))(hydra.lib.equality.lte[Int](c)(57))

def classifyDataTerm(ts: hydra.core.TypeScheme)(term: hydra.core.Term): hydra.ext.java.helpers.JavaSymbolClass =
  hydra.lib.logic.ifElse[hydra.ext.java.helpers.JavaSymbolClass](hydra.rewriting.isLambda(term))({
  val n: Int = hydra.ext.java.coder.classifyDataTerm_countLambdaParams(term)
  hydra.lib.logic.ifElse[hydra.ext.java.helpers.JavaSymbolClass](hydra.lib.equality.gt[Int](n)(1))(hydra.ext.java.helpers.JavaSymbolClass.hoistedLambda(n))(hydra.ext.java.helpers.JavaSymbolClass.unaryFunction)
})({
  val hasTypeParams: Boolean = hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Name](ts.variables))
  hydra.lib.logic.ifElse[hydra.ext.java.helpers.JavaSymbolClass](hasTypeParams)({
    val n2: Int = hydra.ext.java.coder.classifyDataTerm_countLambdaParams(hydra.ext.java.coder.classifyDataTerm_stripTypeLambdas(term))
    hydra.lib.logic.ifElse[hydra.ext.java.helpers.JavaSymbolClass](hydra.lib.equality.gt[Int](n2)(0))(hydra.ext.java.helpers.JavaSymbolClass.hoistedLambda(n2))(hydra.ext.java.helpers.JavaSymbolClass.nullaryFunction)
  })(hydra.ext.java.helpers.JavaSymbolClass.nullaryFunction)
})

def classifyDataTerm_countLambdaParams(t: hydra.core.Term): Int =
  hydra.rewriting.deannotateTerm(t) match
  case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
    case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.lib.math.add(1)(hydra.ext.java.coder.classifyDataTerm_countLambdaParams(v_Function_lambda_lam.body))
    case _ => 0
  case hydra.core.Term.let(v_Term_let_lt) => hydra.ext.java.coder.classifyDataTerm_countLambdaParams(v_Term_let_lt.body)
  case _ => 0

def classifyDataTerm_stripTypeLambdas(t: hydra.core.Term): hydra.core.Term =
  hydra.rewriting.deannotateTerm(t) match
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.ext.java.coder.classifyDataTerm_stripTypeLambdas(v_Term_typeLambda_tl.body)
  case _ => t

def classifyDataReference(name: hydra.core.Name)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.helpers.JavaSymbolClass] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.core.Binding], hydra.ext.java.helpers.JavaSymbolClass](Right(hydra.lexical.dereferenceElement(g)(name)))((mel: Option[hydra.core.Binding]) =>
  hydra.lib.maybes.cases[hydra.core.Binding, Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.helpers.JavaSymbolClass]](mel)(Right(hydra.ext.java.helpers.JavaSymbolClass.localVariable))((el: hydra.core.Binding) =>
  hydra.lib.maybes.cases[hydra.core.TypeScheme, Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.helpers.JavaSymbolClass]](el.`type`)(Left(hydra.context.InContext(hydra.error.Error.other(hydra.lib.strings.cat2("no type scheme for element ")(el.name)),
     cx)))((ts: hydra.core.TypeScheme) => Right(hydra.ext.java.coder.classifyDataTerm(ts)(el.term)))))

def encodeType(aliases: hydra.ext.java.helpers.Aliases)(boundVars: scala.collection.immutable.Set[hydra.core.Name])(t: hydra.core.Type)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.Type] =
  {
  val inScopeTypeParams: scala.collection.immutable.Set[hydra.core.Name] = (aliases.inScopeTypeParams)
  val typeVarSubst: Map[hydra.core.Name, hydra.core.Name] = (aliases.typeVarSubst)
  hydra.rewriting.deannotateType(t) match
    case hydra.core.Type.application(v_Type_application_at) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.Type, hydra.ext.java.syntax.Type](hydra.ext.java.coder.encodeType(aliases)(boundVars)(v_Type_application_at.function)(cx)(g))((jlhs: hydra.ext.java.syntax.Type) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
         hydra.ext.java.syntax.Type](hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
         hydra.ext.java.syntax.Type, hydra.ext.java.syntax.ReferenceType](hydra.ext.java.coder.encodeType(aliases)(boundVars)(v_Type_application_at.argument)(cx)(g))((`jt_`: hydra.ext.java.syntax.Type) => hydra.ext.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jrhs: hydra.ext.java.syntax.ReferenceType) => hydra.ext.java.utils.addJavaTypeParameter(jrhs)(jlhs)(cx)))
    case hydra.core.Type.function(v_Type_function_ft) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.Type](hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.Type, hydra.ext.java.syntax.ReferenceType](hydra.ext.java.coder.encodeType(aliases)(boundVars)(v_Type_function_ft.domain)(cx)(g))((`jt_`: hydra.ext.java.syntax.Type) => hydra.ext.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jdom: hydra.ext.java.syntax.ReferenceType) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
         hydra.ext.java.syntax.Type](hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
         hydra.ext.java.syntax.Type, hydra.ext.java.syntax.ReferenceType](hydra.ext.java.coder.encodeType(aliases)(boundVars)(v_Type_function_ft.codomain)(cx)(g))((`jt_`: hydra.ext.java.syntax.Type) => hydra.ext.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jcod: hydra.ext.java.syntax.ReferenceType) =>
      Right(hydra.ext.java.utils.javaRefType(Seq(jdom, jcod))(hydra.ext.java.names.javaUtilFunctionPackageName)("Function"))))
    case hydra.core.Type.forall(v_Type_forall_fa) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.Type, hydra.ext.java.syntax.Type](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.insert[hydra.core.Name](v_Type_forall_fa.parameter)(boundVars))(v_Type_forall_fa.body)(cx)(g))((jbody: hydra.ext.java.syntax.Type) =>
      hydra.ext.java.utils.addJavaTypeParameter(hydra.ext.java.utils.javaTypeVariable(v_Type_forall_fa.parameter))(jbody)(cx))
    case hydra.core.Type.list(v_Type_list_et) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.Type, hydra.ext.java.syntax.Type](hydra.ext.java.coder.encodeType(aliases)(boundVars)(v_Type_list_et)(cx)(g))((jet: hydra.ext.java.syntax.Type) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
         hydra.ext.java.syntax.Type](hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
         hydra.ext.java.syntax.Type, hydra.ext.java.syntax.ReferenceType](Right(jet))((`jt_`: hydra.ext.java.syntax.Type) => hydra.ext.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((rt: hydra.ext.java.syntax.ReferenceType) =>
      Right(hydra.ext.java.utils.javaRefType(Seq(rt))(hydra.ext.java.names.hydraUtilPackageName)("ConsList"))))
    case hydra.core.Type.literal(v_Type_literal_lt) => hydra.ext.java.coder.encodeLiteralType(v_Type_literal_lt)(cx)(g)
    case hydra.core.Type.either(v_Type_either_et) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.Type](hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.Type, hydra.ext.java.syntax.ReferenceType](hydra.ext.java.coder.encodeType(aliases)(boundVars)(v_Type_either_et.left)(cx)(g))((`jt_`: hydra.ext.java.syntax.Type) => hydra.ext.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jlt: hydra.ext.java.syntax.ReferenceType) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
         hydra.ext.java.syntax.Type](hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
         hydra.ext.java.syntax.Type, hydra.ext.java.syntax.ReferenceType](hydra.ext.java.coder.encodeType(aliases)(boundVars)(v_Type_either_et.right)(cx)(g))((`jt_`: hydra.ext.java.syntax.Type) => hydra.ext.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jrt: hydra.ext.java.syntax.ReferenceType) =>
      Right(hydra.ext.java.utils.javaRefType(Seq(jlt, jrt))(hydra.ext.java.names.hydraUtilPackageName)("Either"))))
    case hydra.core.Type.map(v_Type_map_mt) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.Type](hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.Type, hydra.ext.java.syntax.ReferenceType](hydra.ext.java.coder.encodeType(aliases)(boundVars)(v_Type_map_mt.keys)(cx)(g))((`jt_`: hydra.ext.java.syntax.Type) => hydra.ext.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jkt: hydra.ext.java.syntax.ReferenceType) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
         hydra.ext.java.syntax.Type](hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
         hydra.ext.java.syntax.Type, hydra.ext.java.syntax.ReferenceType](hydra.ext.java.coder.encodeType(aliases)(boundVars)(v_Type_map_mt.values)(cx)(g))((`jt_`: hydra.ext.java.syntax.Type) => hydra.ext.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jvt: hydra.ext.java.syntax.ReferenceType) =>
      Right(hydra.ext.java.utils.javaRefType(Seq(jkt, jvt))(hydra.ext.java.names.hydraUtilPackageName)("PersistentMap"))))
    case hydra.core.Type.pair(v_Type_pair_pt) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.Type](hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.Type, hydra.ext.java.syntax.ReferenceType](hydra.ext.java.coder.encodeType(aliases)(boundVars)(v_Type_pair_pt.first)(cx)(g))((`jt_`: hydra.ext.java.syntax.Type) => hydra.ext.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jfirst: hydra.ext.java.syntax.ReferenceType) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
         hydra.ext.java.syntax.Type](hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
         hydra.ext.java.syntax.Type, hydra.ext.java.syntax.ReferenceType](hydra.ext.java.coder.encodeType(aliases)(boundVars)(v_Type_pair_pt.second)(cx)(g))((`jt_`: hydra.ext.java.syntax.Type) => hydra.ext.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jsecond: hydra.ext.java.syntax.ReferenceType) =>
      Right(hydra.ext.java.utils.javaRefType(Seq(jfirst, jsecond))(hydra.ext.java.names.hydraUtilPackageName)("Pair"))))
    case hydra.core.Type.unit => Right(hydra.ext.java.utils.javaRefType(Seq())(hydra.ext.java.names.javaLangPackageName)("Void"))
    case hydra.core.Type.record(v_Type_record_rt) => hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.Type]](hydra.lib.lists.`null`[hydra.core.FieldType](v_Type_record_rt))(Right(hydra.ext.java.utils.javaRefType(Seq())(hydra.ext.java.names.javaLangPackageName)("Void")))(Left(hydra.context.InContext(hydra.error.Error.other("unexpected anonymous record type"),
       cx)))
    case hydra.core.Type.maybe(v_Type_maybe_ot) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.Type](hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.Type, hydra.ext.java.syntax.ReferenceType](hydra.ext.java.coder.encodeType(aliases)(boundVars)(v_Type_maybe_ot)(cx)(g))((`jt_`: hydra.ext.java.syntax.Type) => hydra.ext.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jot: hydra.ext.java.syntax.ReferenceType) =>
      Right(hydra.ext.java.utils.javaRefType(Seq(jot))(hydra.ext.java.names.hydraUtilPackageName)("Maybe")))
    case hydra.core.Type.set(v_Type_set_st) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.Type](hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.Type, hydra.ext.java.syntax.ReferenceType](hydra.ext.java.coder.encodeType(aliases)(boundVars)(v_Type_set_st)(cx)(g))((`jt_`: hydra.ext.java.syntax.Type) => hydra.ext.java.utils.javaTypeToJavaReferenceType(`jt_`)(cx)))((jst: hydra.ext.java.syntax.ReferenceType) =>
      Right(hydra.ext.java.utils.javaRefType(Seq(jst))(hydra.ext.java.names.hydraUtilPackageName)("PersistentSet")))
    case hydra.core.Type.union(v_Type_union__) => Left(hydra.context.InContext(hydra.error.Error.other("unexpected anonymous union type"), cx))
    case hydra.core.Type.variable(v_Type_variable_name0) => {
      val name: hydra.core.Name = hydra.lib.maybes.fromMaybe[hydra.core.Name](v_Type_variable_name0)(hydra.lib.maps.lookup[hydra.core.Name,
         hydra.core.Name](v_Type_variable_name0)(typeVarSubst))
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type], hydra.ext.java.syntax.Type](hydra.ext.java.coder.encodeType_resolveIfTypedef(aliases)(boundVars)(inScopeTypeParams)(name)(cx)(g))((resolved: Option[hydra.core.Type]) =>
        hydra.lib.maybes.cases[hydra.core.Type, Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type]](resolved)(Right(hydra.lib.logic.ifElse[hydra.ext.java.syntax.Type](hydra.lib.logic.or(hydra.lib.sets.member[hydra.core.Name](name)(boundVars))(hydra.lib.sets.member[hydra.core.Name](name)(inScopeTypeParams)))(hydra.ext.java.syntax.Type.reference(hydra.ext.java.utils.javaTypeVariable(name)))(hydra.lib.logic.ifElse[hydra.ext.java.syntax.Type](hydra.ext.java.coder.isLambdaBoundVariable(name))(hydra.ext.java.syntax.Type.reference(hydra.ext.java.utils.javaTypeVariable(name)))(hydra.lib.logic.ifElse[hydra.ext.java.syntax.Type](hydra.ext.java.coder.isUnresolvedInferenceVar(name))(hydra.ext.java.syntax.Type.reference(hydra.ext.java.syntax.ReferenceType.classOrInterface(hydra.ext.java.syntax.ClassOrInterfaceType.`class`(hydra.ext.java.utils.javaClassType(Seq())(hydra.ext.java.names.javaLangPackageName)("Object")))))(hydra.ext.java.syntax.Type.reference(hydra.ext.java.utils.nameToJavaReferenceType(aliases)(true)(Seq())(name)(None)))))))((resolvedType: hydra.core.Type) =>
        hydra.ext.java.coder.encodeType(aliases)(boundVars)(resolvedType)(cx)(g)))
    }
    case hydra.core.Type.wrap(v_Type_wrap__) => Left(hydra.context.InContext(hydra.error.Error.other("unexpected anonymous wrap type"), cx))
    case _ => Left(hydra.context.InContext(hydra.error.Error.other(hydra.lib.strings.cat2("can't encode unsupported type in Java: ")(hydra.show.core.`type`(t))),
       cx))
}

def encodeType_resolveIfTypedef[T0, T1, T2](aliases: T0)(boundVars: scala.collection.immutable.Set[hydra.core.Name])(inScopeTypeParams: scala.collection.immutable.Set[hydra.core.Name])(name: hydra.core.Name)(cx: T1)(g: hydra.graph.Graph): Either[T2,
   Option[hydra.core.Type]] =
  hydra.lib.logic.ifElse[Either[T2, Option[hydra.core.Type]]](hydra.lib.logic.or(hydra.lib.sets.member[hydra.core.Name](name)(boundVars))(hydra.lib.sets.member[hydra.core.Name](name)(inScopeTypeParams)))(Right(None))(hydra.lib.logic.ifElse[Either[T2,
     Option[hydra.core.Type]]](hydra.ext.java.coder.isLambdaBoundVariable(name))(Right(None))({
  val schemaTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = (g.schemaTypes)
  hydra.lib.maybes.cases[hydra.core.TypeScheme, Either[T2, Option[hydra.core.Type]]](hydra.lib.maps.lookup[hydra.core.Name,
     hydra.core.TypeScheme](name)(schemaTypes))(Right(None))((ts: hydra.core.TypeScheme) =>
    hydra.lib.logic.ifElse[Either[T2, Option[hydra.core.Type]]](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Name](ts.variables)))(Right(None))(hydra.rewriting.deannotateType(ts.`type`) match
    case hydra.core.Type.record(v_Type_record__) => Right(None)
    case hydra.core.Type.union(v_Type_union__) => Right(None)
    case hydra.core.Type.wrap(v_Type_wrap__) => Right(None)
    case _ => Right(Some(ts.`type`))))
}))

def javaTypeArgumentsForNamedType(tname: hydra.core.Name)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   Seq[hydra.ext.java.syntax.TypeArgument]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Seq[hydra.ext.java.syntax.TypeArgument]](hydra.schemas.requireType(cx)(g)(tname))((typ: hydra.core.Type) =>
  Right(hydra.lib.lists.map[hydra.ext.java.syntax.TypeParameter, hydra.ext.java.syntax.TypeArgument]((`tp_`: hydra.ext.java.syntax.TypeParameter) => hydra.ext.java.utils.typeParameterToTypeArgument(`tp_`))(hydra.ext.java.coder.javaTypeParametersForType(typ))))

def encodeLiteral(lit: hydra.core.Literal): hydra.ext.java.syntax.Expression =
  lit match
  case hydra.core.Literal.binary(v_Literal_binary_bs) => {
    val byteValues: Seq[Int] = hydra.lib.literals.binaryToBytes(v_Literal_binary_bs)
    hydra.ext.java.utils.javaArrayCreation(hydra.ext.java.utils.javaBytePrimitiveType)(Some(hydra.ext.java.utils.javaArrayInitializer(hydra.lib.lists.map[Int,
       hydra.ext.java.syntax.Expression]((w: Int) =>
      hydra.ext.java.utils.javaLiteralToJavaExpression(hydra.ext.java.syntax.Literal.integer(hydra.lib.literals.int32ToBigint(w))))(byteValues))))
  }
  case hydra.core.Literal.boolean(v_Literal_boolean_b) => hydra.ext.java.coder.encodeLiteral_litExp(hydra.ext.java.utils.javaBoolean(v_Literal_boolean_b))
  case hydra.core.Literal.float(v_Literal_float_f) => hydra.ext.java.coder.encodeLiteral_encodeFloat(v_Literal_float_f)
  case hydra.core.Literal.integer(v_Literal_integer_i) => hydra.ext.java.coder.encodeLiteral_encodeInteger(v_Literal_integer_i)
  case hydra.core.Literal.string(v_Literal_string_s) => hydra.ext.java.coder.encodeLiteral_litExp(hydra.ext.java.utils.javaString(v_Literal_string_s))

def encodeLiteral_litExp(l: hydra.ext.java.syntax.Literal): hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaLiteralToJavaExpression(l)

def encodeLiteral_primCast(pt: hydra.ext.java.syntax.PrimitiveType)(expr: hydra.ext.java.syntax.Expression): hydra.ext.java.syntax.Expression =
  hydra.ext.java.utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.javaCastPrimitive(pt)(hydra.ext.java.utils.javaExpressionToJavaUnaryExpression(expr)))

def encodeLiteral_encodeFloat(f: hydra.core.FloatValue): hydra.ext.java.syntax.Expression =
  f match
  case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_v) => hydra.ext.java.utils.javaConstructorCall(hydra.ext.java.utils.javaConstructorName("java.math.BigDecimal")(None))(Seq(hydra.ext.java.coder.encodeLiteral(hydra.core.Literal.string(hydra.lib.literals.showBigfloat(v_FloatValue_bigfloat_v)))))(None)
  case hydra.core.FloatValue.float32(v_FloatValue_float32_v) => hydra.ext.java.coder.encodeLiteral_primCast(hydra.ext.java.syntax.PrimitiveType.numeric(hydra.ext.java.syntax.NumericType.floatingPoint(hydra.ext.java.syntax.FloatingPointType.float)))(hydra.ext.java.coder.encodeLiteral_litExp(hydra.ext.java.syntax.Literal.floatingPoint(hydra.lib.literals.float32ToBigfloat(v_FloatValue_float32_v))))
  case hydra.core.FloatValue.float64(v_FloatValue_float64_v) => hydra.ext.java.coder.encodeLiteral_litExp(hydra.ext.java.syntax.Literal.floatingPoint(hydra.lib.literals.float64ToBigfloat(v_FloatValue_float64_v)))

def encodeLiteral_encodeInteger(i: hydra.core.IntegerValue): hydra.ext.java.syntax.Expression =
  i match
  case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_v) => hydra.ext.java.utils.javaConstructorCall(hydra.ext.java.utils.javaConstructorName("java.math.BigInteger")(None))(Seq(hydra.ext.java.coder.encodeLiteral(hydra.core.Literal.string(hydra.lib.literals.showBigint(v_IntegerValue_bigint_v)))))(None)
  case hydra.core.IntegerValue.int8(v_IntegerValue_int8_v) => hydra.ext.java.coder.encodeLiteral_primCast(hydra.ext.java.syntax.PrimitiveType.numeric(hydra.ext.java.syntax.NumericType.integral(hydra.ext.java.syntax.IntegralType.byte)))(hydra.ext.java.coder.encodeLiteral_litExp(hydra.ext.java.syntax.Literal.integer(hydra.lib.literals.int8ToBigint(v_IntegerValue_int8_v))))
  case hydra.core.IntegerValue.int16(v_IntegerValue_int16_v) => hydra.ext.java.coder.encodeLiteral_primCast(hydra.ext.java.syntax.PrimitiveType.numeric(hydra.ext.java.syntax.NumericType.integral(hydra.ext.java.syntax.IntegralType.short)))(hydra.ext.java.coder.encodeLiteral_litExp(hydra.ext.java.syntax.Literal.integer(hydra.lib.literals.int16ToBigint(v_IntegerValue_int16_v))))
  case hydra.core.IntegerValue.int32(v_IntegerValue_int32_v) => hydra.ext.java.coder.encodeLiteral_litExp(hydra.ext.java.syntax.Literal.integer(hydra.lib.literals.int32ToBigint(v_IntegerValue_int32_v)))
  case hydra.core.IntegerValue.int64(v_IntegerValue_int64_v) => hydra.ext.java.coder.encodeLiteral_primCast(hydra.ext.java.syntax.PrimitiveType.numeric(hydra.ext.java.syntax.NumericType.integral(hydra.ext.java.syntax.IntegralType.long)))(hydra.ext.java.coder.encodeLiteral_litExp(hydra.ext.java.syntax.Literal.integer(hydra.lib.literals.int64ToBigint(v_IntegerValue_int64_v))))
  case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_v) => hydra.ext.java.coder.encodeLiteral_primCast(hydra.ext.java.syntax.PrimitiveType.numeric(hydra.ext.java.syntax.NumericType.integral(hydra.ext.java.syntax.IntegralType.short)))(hydra.ext.java.coder.encodeLiteral_litExp(hydra.ext.java.syntax.Literal.integer(hydra.lib.literals.uint8ToBigint(v_IntegerValue_uint8_v))))
  case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_v) => hydra.ext.java.coder.encodeLiteral_litExp(hydra.ext.java.syntax.Literal.character(v_IntegerValue_uint16_v))
  case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_v) => hydra.ext.java.coder.encodeLiteral_primCast(hydra.ext.java.syntax.PrimitiveType.numeric(hydra.ext.java.syntax.NumericType.integral(hydra.ext.java.syntax.IntegralType.long)))(hydra.ext.java.coder.encodeLiteral_litExp(hydra.ext.java.syntax.Literal.integer(hydra.lib.literals.uint32ToBigint(v_IntegerValue_uint32_v))))
  case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_v) => hydra.ext.java.utils.javaConstructorCall(hydra.ext.java.utils.javaConstructorName("java.math.BigInteger")(None))(Seq(hydra.ext.java.coder.encodeLiteral(hydra.core.Literal.string(hydra.lib.literals.showBigint(hydra.lib.literals.uint64ToBigint(v_IntegerValue_uint64_v))))))(None)

def fieldTypeToFormalParam(aliases: hydra.ext.java.helpers.Aliases)(ft: hydra.core.FieldType)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.FormalParameter] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type, hydra.ext.java.syntax.FormalParameter](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(ft.`type`)(cx)(g))((jt: hydra.ext.java.syntax.Type) =>
  Right(hydra.ext.java.utils.javaTypeToJavaFormalParameter(jt)(ft.name)))

def applyCastIfSafe(aliases: hydra.ext.java.helpers.Aliases)(castType: hydra.core.Type)(expr: hydra.ext.java.syntax.Expression)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.Expression] =
  {
  val trusted: scala.collection.immutable.Set[hydra.core.Name] = (aliases.trustedTypeVars)
  val inScope: scala.collection.immutable.Set[hydra.core.Name] = (aliases.inScopeTypeParams)
  val castVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.ext.java.coder.collectTypeVars(castType)
  val javaTypeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) =>
    hydra.lib.logic.or(hydra.lib.sets.member[hydra.core.Name](v)(inScope))(hydra.ext.java.coder.isLambdaBoundVariable(v)))(hydra.lib.sets.toList[hydra.core.Name](castVars)))
  val isSafe: Boolean = hydra.lib.logic.or(hydra.lib.sets.`null`[hydra.core.Name](trusted))(hydra.lib.logic.or(hydra.lib.sets.`null`[hydra.core.Name](javaTypeVars))(hydra.lib.sets.`null`[hydra.core.Name](hydra.lib.sets.difference[hydra.core.Name](javaTypeVars)(trusted))))
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](isSafe)(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
     hydra.ext.java.syntax.Type, hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(castType)(cx)(g))((jtype: hydra.ext.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
       hydra.ext.java.syntax.Expression](hydra.ext.java.utils.javaTypeToJavaReferenceType(jtype)(cx))((rt: hydra.ext.java.syntax.ReferenceType) =>
    Right(hydra.ext.java.utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.javaCastExpression(rt)(hydra.ext.java.utils.javaExpressionToJavaUnaryExpression(expr)))))))(Right(expr))
}

def encodeVariable(env: hydra.ext.java.helpers.JavaEnvironment)(name: hydra.core.Name)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.Expression] =
  {
  val aliases: hydra.ext.java.helpers.Aliases = (env.aliases)
  val resolvedName: hydra.core.Name = hydra.ext.java.utils.lookupJavaVarName(aliases)(name)
  val jid: hydra.ext.java.syntax.Identifier = hydra.ext.java.utils.javaIdentifier(resolvedName)
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](hydra.lib.sets.member[hydra.core.Name](name)(aliases.branchVars))(Right(hydra.ext.java.utils.javaFieldAccessToJavaExpression(hydra.ext.java.syntax.FieldAccess(hydra.ext.java.syntax.FieldAccess_Qualifier.primary(hydra.ext.java.utils.javaExpressionToJavaPrimary(hydra.ext.java.utils.javaIdentifierToJavaExpression(jid))),
     hydra.ext.java.utils.javaIdentifier(hydra.ext.java.names.valueFieldName)))))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
     hydra.ext.java.syntax.Expression]](hydra.lib.logic.and(hydra.lib.equality.equal[hydra.core.Name](name)(hydra.lib.strings.cat(Seq(hydra.ext.java.names.instanceName,
     "_", hydra.ext.java.names.valueFieldName))))(hydra.ext.java.coder.isRecursiveVariable(aliases)(name)))({
    val instanceExpr: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaIdentifierToJavaExpression(hydra.ext.java.utils.javaIdentifier(hydra.ext.java.names.instanceName))
    Right(hydra.ext.java.utils.javaFieldAccessToJavaExpression(hydra.ext.java.syntax.FieldAccess(hydra.ext.java.syntax.FieldAccess_Qualifier.primary(hydra.ext.java.utils.javaExpressionToJavaPrimary(instanceExpr)),
       hydra.ext.java.utils.javaIdentifier(hydra.ext.java.names.valueFieldName))))
  })(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](hydra.lib.logic.and(hydra.ext.java.coder.isRecursiveVariable(aliases)(name))(hydra.lib.logic.not(hydra.ext.java.coder.isLambdaBoundIn(name)(aliases.lambdaVars))))(Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocation(Some(Left(hydra.ext.java.syntax.ExpressionName(None,
     jid))))(hydra.ext.java.names.getMethodName)(Seq()))))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
     hydra.ext.java.syntax.Expression]](hydra.lib.logic.and(hydra.lib.sets.member[hydra.core.Name](name)(aliases.thunkedVars))(hydra.lib.logic.not(hydra.ext.java.coder.isLambdaBoundIn(name)(aliases.lambdaVars))))(Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocation(Some(Left(hydra.ext.java.syntax.ExpressionName(None,
     jid))))(hydra.ext.java.names.getMethodName)(Seq()))))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
     hydra.ext.java.syntax.Expression]](hydra.ext.java.coder.isLambdaBoundIn(name)(aliases.lambdaVars))({
    val actualName: hydra.core.Name = hydra.ext.java.coder.findMatchingLambdaVar(name)(aliases.lambdaVars)
    {
      val resolvedActual: hydra.core.Name = hydra.ext.java.utils.lookupJavaVarName(aliases)(actualName)
      Right(hydra.ext.java.utils.javaIdentifierToJavaExpression(hydra.ext.java.utils.variableToJavaIdentifier(resolvedActual)))
    }
  })(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](hydra.lib.sets.member[hydra.core.Name](name)(aliases.inScopeJavaVars))(Right(hydra.ext.java.utils.javaIdentifierToJavaExpression(hydra.ext.java.coder.elementJavaIdentifier(false)(false)(aliases)(resolvedName))))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
     hydra.ext.java.helpers.JavaSymbolClass, hydra.ext.java.syntax.Expression](hydra.ext.java.coder.classifyDataReference(name)(cx)(g))((cls: hydra.ext.java.helpers.JavaSymbolClass) =>
    cls match
    case hydra.ext.java.helpers.JavaSymbolClass.hoistedLambda(v_JavaSymbolClass_hoistedLambda_arity) => hydra.ext.java.coder.encodeVariable_hoistedLambdaCase(aliases)(name)(v_JavaSymbolClass_hoistedLambda_arity)(cx)(g)
    case hydra.ext.java.helpers.JavaSymbolClass.localVariable => Right(hydra.ext.java.utils.javaIdentifierToJavaExpression(hydra.ext.java.coder.elementJavaIdentifier(false)(false)(aliases)(resolvedName)))
    case hydra.ext.java.helpers.JavaSymbolClass.constant => Right(hydra.ext.java.utils.javaIdentifierToJavaExpression(hydra.ext.java.coder.elementJavaIdentifier(false)(false)(aliases)(name)))
    case hydra.ext.java.helpers.JavaSymbolClass.nullaryFunction => Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocation(None)(hydra.ext.java.coder.elementJavaIdentifier(false)(false)(aliases)(name))(Seq())))
    case hydra.ext.java.helpers.JavaSymbolClass.unaryFunction => Right(hydra.ext.java.utils.javaIdentifierToJavaExpression(hydra.ext.java.coder.elementJavaIdentifier(false)(true)(aliases)(name))))))))))
}

def encodeVariable_buildCurried(params: Seq[hydra.core.Name])(inner: hydra.ext.java.syntax.Expression): hydra.ext.java.syntax.Expression =
  hydra.lib.logic.ifElse[hydra.ext.java.syntax.Expression](hydra.lib.lists.`null`[hydra.core.Name](params))(inner)(hydra.ext.java.utils.javaLambda(hydra.lib.lists.head[hydra.core.Name](params))(hydra.ext.java.coder.encodeVariable_buildCurried(hydra.lib.lists.tail[hydra.core.Name](params))(inner)))

def encodeVariable_hoistedLambdaCase(aliases: hydra.ext.java.helpers.Aliases)(name: hydra.core.Name)(arity: Int)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.Expression] =
  {
  val paramNames: Seq[hydra.core.Name] = hydra.lib.lists.map[Int, hydra.core.Name]((i: Int) => hydra.lib.strings.cat2("p")(hydra.lib.literals.showInt32(i)))(hydra.lib.math.range(0)(hydra.lib.math.sub(arity)(1)))
  val paramExprs: Seq[hydra.ext.java.syntax.Expression] = hydra.lib.lists.map[hydra.core.Name, hydra.ext.java.syntax.Expression]((pn: hydra.core.Name) =>
    hydra.ext.java.utils.javaIdentifierToJavaExpression(hydra.ext.java.utils.variableToJavaIdentifier(pn)))(paramNames)
  val call: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocation(None)(hydra.ext.java.coder.elementJavaIdentifier(false)(false)(aliases)(name))(paramExprs))
  val lam: hydra.ext.java.syntax.Expression = hydra.ext.java.coder.encodeVariable_buildCurried(paramNames)(call)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.core.Binding], hydra.ext.java.syntax.Expression](Right(hydra.lexical.dereferenceElement(g)(name)))((mel: Option[hydra.core.Binding]) =>
    hydra.lib.maybes.cases[hydra.core.Binding, Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](mel)(Right(lam))((el: hydra.core.Binding) =>
    hydra.lib.maybes.cases[hydra.core.TypeScheme, Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](el.`type`)(Right(lam))((ts: hydra.core.TypeScheme) =>
    {
    val typ: hydra.core.Type = (ts.`type`)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type, hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(typ)(cx)(g))((jtype: hydra.ext.java.syntax.Type) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
         hydra.ext.java.syntax.Expression](hydra.ext.java.utils.javaTypeToJavaReferenceType(jtype)(cx))((rt: hydra.ext.java.syntax.ReferenceType) =>
      Right(hydra.ext.java.utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.javaCastExpression(rt)(hydra.ext.java.utils.javaExpressionToJavaUnaryExpression(lam))))))
  })))
}

def encodeNullaryConstant(env: hydra.ext.java.helpers.JavaEnvironment)(typ: hydra.core.Type)(fun: hydra.core.Function)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.Expression] =
  {
  val aliases: hydra.ext.java.helpers.Aliases = (env.aliases)
  fun match
    case hydra.core.Function.primitive(v_Function_primitive_name) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       Seq[hydra.ext.java.syntax.TypeArgument], hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeNullaryConstant_typeArgsFromReturnType(aliases)(typ)(cx)(g))((targs: Seq[hydra.ext.java.syntax.TypeArgument]) =>
      hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](hydra.lib.lists.`null`[hydra.ext.java.syntax.TypeArgument](targs))({
      val header: hydra.ext.java.syntax.MethodInvocation_Header = hydra.ext.java.syntax.MethodInvocation_Header.simple(hydra.ext.java.coder.elementJavaIdentifier(true)(false)(aliases)(v_Function_primitive_name))
      Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.syntax.MethodInvocation(header, Seq())))
    })({
      val fullName: scala.Predef.String = hydra.ext.java.coder.elementJavaIdentifier(true)(false)(aliases)(v_Function_primitive_name)
      {
        val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(fullName)
        {
          val className: hydra.ext.java.syntax.Identifier = hydra.lib.strings.intercalate(".")(hydra.lib.lists.init[scala.Predef.String](parts))
          {
            val methodName: hydra.ext.java.syntax.Identifier = hydra.lib.lists.last[scala.Predef.String](parts)
            Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStaticWithTypeArgs(className)(methodName)(targs)(Seq())))
          }
        }
      }
    }))
    case _ => Left(hydra.context.InContext(hydra.error.Error.other(hydra.lib.strings.cat2("unexpected ")(hydra.lib.strings.cat2("nullary function")(hydra.lib.strings.cat2(" in ")(hydra.show.core.function(fun))))),
       cx))
}

def encodeNullaryConstant_typeArgsFromReturnType(aliases: hydra.ext.java.helpers.Aliases)(t: hydra.core.Type)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   Seq[hydra.ext.java.syntax.TypeArgument]] =
  hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.set(v_Type_set_st) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
     hydra.ext.java.syntax.Type, Seq[hydra.ext.java.syntax.TypeArgument]](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(v_Type_set_st)(cx)(g))((jst: hydra.ext.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
       Seq[hydra.ext.java.syntax.TypeArgument]](hydra.ext.java.utils.javaTypeToJavaReferenceType(jst)(cx))((rt: hydra.ext.java.syntax.ReferenceType) => Right(Seq(hydra.ext.java.syntax.TypeArgument.reference(rt)))))
  case hydra.core.Type.list(v_Type_list_lt_) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
     hydra.ext.java.syntax.Type, Seq[hydra.ext.java.syntax.TypeArgument]](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(`v_Type_list_lt_`)(cx)(g))((jlt: hydra.ext.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
       Seq[hydra.ext.java.syntax.TypeArgument]](hydra.ext.java.utils.javaTypeToJavaReferenceType(jlt)(cx))((rt: hydra.ext.java.syntax.ReferenceType) => Right(Seq(hydra.ext.java.syntax.TypeArgument.reference(rt)))))
  case hydra.core.Type.maybe(v_Type_maybe_mt) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
     hydra.ext.java.syntax.Type, Seq[hydra.ext.java.syntax.TypeArgument]](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(v_Type_maybe_mt)(cx)(g))((jmt: hydra.ext.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
       Seq[hydra.ext.java.syntax.TypeArgument]](hydra.ext.java.utils.javaTypeToJavaReferenceType(jmt)(cx))((rt: hydra.ext.java.syntax.ReferenceType) => Right(Seq(hydra.ext.java.syntax.TypeArgument.reference(rt)))))
  case hydra.core.Type.map(v_Type_map_mp) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
     hydra.ext.java.syntax.Type, Seq[hydra.ext.java.syntax.TypeArgument]](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(v_Type_map_mp.keys)(cx)(g))((jkt: hydra.ext.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
       Seq[hydra.ext.java.syntax.TypeArgument]](hydra.ext.java.utils.javaTypeToJavaReferenceType(jkt)(cx))((rk: hydra.ext.java.syntax.ReferenceType) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type, Seq[hydra.ext.java.syntax.TypeArgument]](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(v_Type_map_mp.values)(cx)(g))((jvt: hydra.ext.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
       Seq[hydra.ext.java.syntax.TypeArgument]](hydra.ext.java.utils.javaTypeToJavaReferenceType(jvt)(cx))((rv: hydra.ext.java.syntax.ReferenceType) =>
    Right(Seq(hydra.ext.java.syntax.TypeArgument.reference(rk), hydra.ext.java.syntax.TypeArgument.reference(rv)))))))
  case _ => Right(Seq())

def buildTypeVarSubst(schemeVarSet: scala.collection.immutable.Set[hydra.core.Name])(freshTyp: hydra.core.Type)(canonTyp: hydra.core.Type): Map[hydra.core.Name,
   hydra.core.Name] =
  hydra.ext.java.coder.buildTypeVarSubst_go(schemeVarSet)(hydra.rewriting.deannotateType(freshTyp))(hydra.rewriting.deannotateType(canonTyp))

def buildTypeVarSubst_go(svs: scala.collection.immutable.Set[hydra.core.Name])(ft: hydra.core.Type)(ct: hydra.core.Type): Map[hydra.core.Name,
   hydra.core.Name] =
  {
  def goSub(a: hydra.core.Type)(b: hydra.core.Type): Map[hydra.core.Name, hydra.core.Name] =
    hydra.ext.java.coder.buildTypeVarSubst_go(svs)(hydra.rewriting.deannotateType(a))(hydra.rewriting.deannotateType(b))
  ft match
    case hydra.core.Type.variable(v_Type_variable_fn) => ct match
      case hydra.core.Type.variable(v_Type_variable_cn) => hydra.lib.logic.ifElse[Map[hydra.core.Name,
         hydra.core.Name]](hydra.lib.logic.and(hydra.lib.logic.not(hydra.lib.equality.equal[hydra.core.Name](v_Type_variable_fn)(v_Type_variable_cn)))(hydra.lib.sets.member[hydra.core.Name](v_Type_variable_cn)(svs)))(hydra.lib.maps.singleton[hydra.core.Name,
         hydra.core.Name](v_Type_variable_fn)(v_Type_variable_cn))(hydra.lib.maps.empty[hydra.core.Name,
         hydra.core.Name])
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.function(v_Type_function_fft) => ct match
      case hydra.core.Type.function(v_Type_function_cft) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](goSub(v_Type_function_fft.domain)(v_Type_function_cft.domain))(goSub(v_Type_function_fft.codomain)(v_Type_function_cft.codomain))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.application(v_Type_application_fat) => ct match
      case hydra.core.Type.application(v_Type_application_cat) => hydra.lib.maps.union[hydra.core.Name,
         hydra.core.Name](goSub(v_Type_application_fat.function)(v_Type_application_cat.function))(goSub(v_Type_application_fat.argument)(v_Type_application_cat.argument))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.list(v_Type_list_fl) => ct match
      case hydra.core.Type.list(v_Type_list_cl) => goSub(v_Type_list_fl)(v_Type_list_cl)
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.set(v_Type_set_fs) => ct match
      case hydra.core.Type.set(v_Type_set_cs) => goSub(v_Type_set_fs)(v_Type_set_cs)
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.maybe(v_Type_maybe_fm) => ct match
      case hydra.core.Type.maybe(v_Type_maybe_cm) => goSub(v_Type_maybe_fm)(v_Type_maybe_cm)
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.map(v_Type_map_fmt) => ct match
      case hydra.core.Type.map(v_Type_map_cmt) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](goSub(v_Type_map_fmt.keys)(v_Type_map_cmt.keys))(goSub(v_Type_map_fmt.values)(v_Type_map_cmt.values))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.pair(v_Type_pair_fpt) => ct match
      case hydra.core.Type.pair(v_Type_pair_cpt) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](goSub(v_Type_pair_fpt.first)(v_Type_pair_cpt.first))(goSub(v_Type_pair_fpt.second)(v_Type_pair_cpt.second))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.either(v_Type_either_fet) => ct match
      case hydra.core.Type.either(v_Type_either_cet) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](goSub(v_Type_either_fet.left)(v_Type_either_cet.left))(goSub(v_Type_either_fet.right)(v_Type_either_cet.right))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case hydra.core.Type.forall(v_Type_forall_ffa) => ct match
      case hydra.core.Type.forall(v_Type_forall_cfa) => goSub(v_Type_forall_ffa.body)(v_Type_forall_cfa.body)
      case _ => hydra.ext.java.coder.buildTypeVarSubst_go(svs)(hydra.rewriting.deannotateType(v_Type_forall_ffa.body))(ct)
    case _ => ct match
      case hydra.core.Type.forall(v_Type_forall_cfa) => hydra.ext.java.coder.buildTypeVarSubst_go(svs)(ft)(hydra.rewriting.deannotateType(v_Type_forall_cfa.body))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
}

def buildTypeSubst(schemeVarSet: scala.collection.immutable.Set[hydra.core.Name])(schemeType: hydra.core.Type)(actualType: hydra.core.Type): Map[hydra.core.Name,
   hydra.core.Type] =
  hydra.ext.java.coder.buildTypeSubst_go(schemeVarSet)(hydra.rewriting.deannotateType(schemeType))(hydra.rewriting.deannotateType(actualType))

def buildTypeSubst_go(svs: scala.collection.immutable.Set[hydra.core.Name])(st: hydra.core.Type)(at: hydra.core.Type): Map[hydra.core.Name, hydra.core.Type] =
  {
  def goSub(a: hydra.core.Type)(b: hydra.core.Type): Map[hydra.core.Name, hydra.core.Type] =
    hydra.ext.java.coder.buildTypeSubst_go(svs)(hydra.rewriting.deannotateType(a))(hydra.rewriting.deannotateType(b))
  st match
    case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.logic.ifElse[Map[hydra.core.Name, hydra.core.Type]](hydra.lib.sets.member[hydra.core.Name](v_Type_variable_v)(svs))(hydra.lib.maps.singleton[hydra.core.Name,
       hydra.core.Type](v_Type_variable_v)(at))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type])
    case hydra.core.Type.function(v_Type_function_sft) => at match
      case hydra.core.Type.function(v_Type_function_aft) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Type](goSub(v_Type_function_sft.domain)(v_Type_function_aft.domain))(goSub(v_Type_function_sft.codomain)(v_Type_function_aft.codomain))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
    case hydra.core.Type.application(v_Type_application_sat) => at match
      case hydra.core.Type.application(v_Type_application_aat) => hydra.lib.maps.union[hydra.core.Name,
         hydra.core.Type](goSub(v_Type_application_sat.function)(v_Type_application_aat.function))(goSub(v_Type_application_sat.argument)(v_Type_application_aat.argument))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
    case hydra.core.Type.list(v_Type_list_sl) => at match
      case hydra.core.Type.list(v_Type_list_al) => goSub(v_Type_list_sl)(v_Type_list_al)
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
    case hydra.core.Type.set(v_Type_set_ss) => at match
      case hydra.core.Type.set(v_Type_set_as_) => goSub(v_Type_set_ss)(`v_Type_set_as_`)
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
    case hydra.core.Type.maybe(v_Type_maybe_sm) => at match
      case hydra.core.Type.maybe(v_Type_maybe_am) => goSub(v_Type_maybe_sm)(v_Type_maybe_am)
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
    case hydra.core.Type.map(v_Type_map_smt) => at match
      case hydra.core.Type.map(v_Type_map_amt) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Type](goSub(v_Type_map_smt.keys)(v_Type_map_amt.keys))(goSub(v_Type_map_smt.values)(v_Type_map_amt.values))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
    case hydra.core.Type.pair(v_Type_pair_spt) => at match
      case hydra.core.Type.pair(v_Type_pair_apt) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Type](goSub(v_Type_pair_spt.first)(v_Type_pair_apt.first))(goSub(v_Type_pair_spt.second)(v_Type_pair_apt.second))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
    case hydra.core.Type.either(v_Type_either_set_) => at match
      case hydra.core.Type.either(v_Type_either_aet) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Type](goSub(`v_Type_either_set_`.left)(v_Type_either_aet.left))(goSub(`v_Type_either_set_`.right)(v_Type_either_aet.right))
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
    case hydra.core.Type.forall(v_Type_forall_sfa) => at match
      case hydra.core.Type.forall(v_Type_forall_afa) => goSub(v_Type_forall_sfa.body)(v_Type_forall_afa.body)
      case _ => goSub(v_Type_forall_sfa.body)(at)
    case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]
}

def javaEnvGetGraph(env: hydra.ext.java.helpers.JavaEnvironment): hydra.graph.Graph = (env.graph)

def javaEnvSetGraph(g: hydra.graph.Graph)(env: hydra.ext.java.helpers.JavaEnvironment): hydra.ext.java.helpers.JavaEnvironment = hydra.ext.java.helpers.JavaEnvironment(env.aliases,
   g)

def analyzeJavaFunction[T0, T1](env: hydra.ext.java.helpers.JavaEnvironment)(term: hydra.core.Term)(cx: hydra.context.Context)(g: T0): Either[T1,
   hydra.typing.FunctionStructure[hydra.ext.java.helpers.JavaEnvironment]] =
  hydra.coderUtils.analyzeFunctionTerm(cx)(hydra.ext.java.coder.javaEnvGetGraph)(hydra.ext.java.coder.javaEnvSetGraph)(env)(term)

def withLambda[T0](env: hydra.ext.java.helpers.JavaEnvironment)(lam: hydra.core.Lambda)(k: (hydra.ext.java.helpers.JavaEnvironment => T0)): T0 =
  hydra.schemas.withLambdaContext(hydra.ext.java.coder.javaEnvGetGraph)(hydra.ext.java.coder.javaEnvSetGraph)(env)(lam)((env1: hydra.ext.java.helpers.JavaEnvironment) =>
  {
  val aliases: hydra.ext.java.helpers.Aliases = (env1.aliases)
  {
    val aliases2: hydra.ext.java.helpers.Aliases = hydra.ext.java.helpers.Aliases(aliases.currentNamespace,
       (aliases.packages), (aliases.branchVars), (aliases.recursiveVars), (aliases.inScopeTypeParams),
       (aliases.polymorphicLocals), (aliases.inScopeJavaVars), (aliases.varRenames), hydra.lib.sets.insert[hydra.core.Name](lam.parameter)(aliases.lambdaVars),
       (aliases.typeVarSubst), (aliases.trustedTypeVars), (aliases.methodCodomain), (aliases.thunkedVars))
    {
      val env2: hydra.ext.java.helpers.JavaEnvironment = hydra.ext.java.helpers.JavaEnvironment(aliases2, (env1.graph))
      k(env2)
    }
  }
})

def withTypeLambda[T0](v1: hydra.ext.java.helpers.JavaEnvironment)(v2: hydra.core.TypeLambda)(v3: (hydra.ext.java.helpers.JavaEnvironment => T0)): T0 =
  hydra.schemas.withTypeLambdaContext(hydra.ext.java.coder.javaEnvGetGraph)(hydra.ext.java.coder.javaEnvSetGraph)(v1)(v2)(v3)

def propagateType(typ: hydra.core.Type)(term: hydra.core.Term): hydra.core.Term =
  {
  def setTypeAnn(t: hydra.core.Term): hydra.core.Term =
    hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(typ)))(t)
  hydra.rewriting.deannotateTerm(term) match
    case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
      case hydra.core.Function.lambda(v_Function_lambda_lam) => {
        val annotated: hydra.core.Term = setTypeAnn(term)
        hydra.rewriting.deannotateType(typ) match
          case hydra.core.Type.function(v_Type_function_ft) => hydra.ext.java.coder.propagateType_propagateIntoLambda(v_Type_function_ft.codomain)(annotated)
          case _ => annotated
      }
      case _ => setTypeAnn(term)
    case hydra.core.Term.let(v_Term_let_lt) => setTypeAnn(hydra.ext.java.coder.propagateType_rebuildLet(term)(v_Term_let_lt.bindings)(hydra.ext.java.coder.propagateType(typ)(v_Term_let_lt.body)))
    case _ => setTypeAnn(term)
}

def propagateType_propagateIntoLambda(cod: hydra.core.Type)(t: hydra.core.Term): hydra.core.Term =
  t match
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.ext.java.coder.propagateType_propagateIntoLambda(cod)(v_Term_annotated_at.body),
     (v_Term_annotated_at.annotation)))
  case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
    case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(v_Function_lambda_lam.parameter,
       (v_Function_lambda_lam.domain), hydra.ext.java.coder.propagateType(cod)(v_Function_lambda_lam.body))))
    case _ => t
  case _ => t

def propagateType_rebuildLet(t: hydra.core.Term)(bindings: Seq[hydra.core.Binding])(newBody: hydra.core.Term): hydra.core.Term =
  t match
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.ext.java.coder.propagateType_rebuildLet(v_Term_annotated_at.body)(bindings)(newBody),
     (v_Term_annotated_at.annotation)))
  case hydra.core.Term.let(v_Term_let__lt) => hydra.core.Term.let(hydra.core.Let(bindings, newBody))
  case _ => t

def flattenBindings(bindings: Seq[hydra.core.Binding]): Seq[hydra.core.Binding] =
  hydra.lib.lists.bind[hydra.core.Binding, hydra.core.Binding](bindings)((b: hydra.core.Binding) =>
  hydra.rewriting.deannotateTerm(b.term) match
  case hydra.core.Term.let(v_Term_let_lt) => hydra.lib.lists.concat2[hydra.core.Binding](hydra.ext.java.coder.flattenBindings(v_Term_let_lt.bindings))(Seq(hydra.core.Binding(b.name,
     (v_Term_let_lt.body), (b.`type`))))
  case _ => Seq(b))

def dedupBindings(inScope: scala.collection.immutable.Set[hydra.core.Name])(bs: Seq[hydra.core.Binding]): Seq[hydra.core.Binding] =
  hydra.lib.logic.ifElse[Seq[hydra.core.Binding]](hydra.lib.lists.`null`[hydra.core.Binding](bs))(Seq())({
  val b: hydra.core.Binding = hydra.lib.lists.head[hydra.core.Binding](bs)
  {
    val rest: Seq[hydra.core.Binding] = hydra.lib.lists.tail[hydra.core.Binding](bs)
    {
      val name: hydra.core.Name = (b.name)
      hydra.lib.logic.ifElse[Seq[hydra.core.Binding]](hydra.lib.sets.member[hydra.core.Name](name)(inScope))({
        val newName: hydra.core.Name = hydra.ext.java.coder.freshJavaName(name)(inScope)
        {
          val subst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maps.singleton[hydra.core.Name, hydra.core.Name](name)(newName)
          {
            val rest2: Seq[hydra.core.Binding] = hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding]((b2: hydra.core.Binding) =>
              hydra.core.Binding(b2.name, hydra.rewriting.substituteVariables(subst)(b2.term), (b2.`type`)))(rest)
            hydra.lib.lists.cons[hydra.core.Binding](hydra.core.Binding(newName, (b.term), (b.`type`)))(hydra.ext.java.coder.dedupBindings(hydra.lib.sets.insert[hydra.core.Name](newName)(inScope))(rest2))
          }
        }
      })(hydra.lib.lists.cons[hydra.core.Binding](b)(hydra.ext.java.coder.dedupBindings(hydra.lib.sets.insert[hydra.core.Name](name)(inScope))(rest)))
    }
  }
})

def freshJavaName(base: hydra.core.Name)(avoid: scala.collection.immutable.Set[hydra.core.Name]): hydra.core.Name = hydra.ext.java.coder.freshJavaName_go(base)(avoid)(2)

def freshJavaName_go(base: hydra.core.Name)(avoid: scala.collection.immutable.Set[hydra.core.Name])(i: Int): hydra.core.Name =
  {
  val candidate: hydra.core.Name = hydra.lib.strings.cat2(base)(hydra.lib.literals.showInt32(i))
  hydra.lib.logic.ifElse[hydra.core.Name](hydra.lib.sets.member[hydra.core.Name](candidate)(avoid))(hydra.ext.java.coder.freshJavaName_go(base)(avoid)(hydra.lib.math.add(i)(1)))(candidate)
}

def needsThunking(t: hydra.core.Term): Boolean =
  hydra.rewriting.deannotateTerm(t) match
  case hydra.core.Term.let(v_Term_let__lt) => true
  case hydra.core.Term.typeApplication(v_Term_typeApplication__ta) => true
  case hydra.core.Term.typeLambda(v_Term_typeLambda__tl) => true
  case _ => hydra.lib.lists.foldl[Boolean, hydra.core.Term]((b: Boolean) =>
    (st: hydra.core.Term) =>
    hydra.lib.logic.or(b)(hydra.ext.java.coder.needsThunking(st)))(false)(hydra.rewriting.subterms(t))

def bindingIsFunctionType(b: hydra.core.Binding): Boolean =
  hydra.lib.maybes.maybe[Boolean, hydra.core.TypeScheme](hydra.rewriting.deannotateTerm(b.term) match
  case hydra.core.Term.function(v_Term_function__f) => true
  case _ => false)((ts: hydra.core.TypeScheme) =>
  hydra.rewriting.deannotateType(ts.`type`) match
  case hydra.core.Type.function(v_Type_function__ft) => true
  case hydra.core.Type.forall(v_Type_forall_fa) => hydra.rewriting.deannotateType(v_Type_forall_fa.body) match
    case hydra.core.Type.function(v_Type_function__ft2) => true
    case _ => false
  case _ => false)(b.`type`)

def decodeTypeFromTerm(term: hydra.core.Term): Option[hydra.core.Type] =
  hydra.rewriting.deannotateTerm(term) match
  case hydra.core.Term.union(v_Term_union_inj) => hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.lib.equality.equal[hydra.core.Name](v_Term_union_inj.typeName)("hydra.core.Type"))({
    val fname: scala.Predef.String = (v_Term_union_inj.field.name)
    {
      val fterm: hydra.core.Term = (v_Term_union_inj.field.term)
      hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.lib.equality.equal[scala.Predef.String](fname)("variable"))(fterm match
        case hydra.core.Term.wrap(v_Term_wrap_wt) => v_Term_wrap_wt.body match
          case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
            case hydra.core.Literal.string(v_Literal_string_s) => Some(hydra.core.Type.variable(v_Literal_string_s))
            case _ => None
          case _ => None
        case _ => None)(hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.lib.equality.equal[scala.Predef.String](fname)("annotated"))(fterm match
        case hydra.core.Term.record(v_Term_record_rec) => hydra.lib.maybes.bind[hydra.core.Field, hydra.core.Type](hydra.lib.lists.safeHead[hydra.core.Field](hydra.lib.lists.filter[hydra.core.Field]((f: hydra.core.Field) => hydra.lib.equality.equal[hydra.core.Name](f.name)("body"))(v_Term_record_rec.fields)))((bodyField: hydra.core.Field) => hydra.ext.java.coder.decodeTypeFromTerm(bodyField.term))
        case _ => None)(hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.lib.equality.equal[scala.Predef.String](fname)("application"))(fterm match
        case hydra.core.Term.record(v_Term_record_rec) => hydra.lib.maybes.bind[hydra.core.Field, hydra.core.Type](hydra.lib.lists.safeHead[hydra.core.Field](hydra.lib.lists.filter[hydra.core.Field]((f: hydra.core.Field) =>
          hydra.lib.equality.equal[hydra.core.Name](f.name)("function"))(v_Term_record_rec.fields)))((funcField: hydra.core.Field) =>
          hydra.lib.maybes.bind[hydra.core.Type, hydra.core.Type](hydra.ext.java.coder.decodeTypeFromTerm(funcField.term))((func: hydra.core.Type) =>
          hydra.lib.maybes.bind[hydra.core.Field, hydra.core.Type](hydra.lib.lists.safeHead[hydra.core.Field](hydra.lib.lists.filter[hydra.core.Field]((f: hydra.core.Field) =>
          hydra.lib.equality.equal[hydra.core.Name](f.name)("argument"))(v_Term_record_rec.fields)))((argField: hydra.core.Field) =>
          hydra.lib.maybes.map[hydra.core.Type, hydra.core.Type]((arg: hydra.core.Type) =>
          hydra.core.Type.application(hydra.core.ApplicationType(func, arg)))(hydra.ext.java.coder.decodeTypeFromTerm(argField.term)))))
        case _ => None)(hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.lib.equality.equal[scala.Predef.String](fname)("function"))(fterm match
        case hydra.core.Term.record(v_Term_record_rec) => hydra.lib.maybes.bind[hydra.core.Field, hydra.core.Type](hydra.lib.lists.safeHead[hydra.core.Field](hydra.lib.lists.filter[hydra.core.Field]((f: hydra.core.Field) => hydra.lib.equality.equal[hydra.core.Name](f.name)("domain"))(v_Term_record_rec.fields)))((domField: hydra.core.Field) =>
          hydra.lib.maybes.bind[hydra.core.Type, hydra.core.Type](hydra.ext.java.coder.decodeTypeFromTerm(domField.term))((dom: hydra.core.Type) =>
          hydra.lib.maybes.bind[hydra.core.Field, hydra.core.Type](hydra.lib.lists.safeHead[hydra.core.Field](hydra.lib.lists.filter[hydra.core.Field]((f: hydra.core.Field) =>
          hydra.lib.equality.equal[hydra.core.Name](f.name)("codomain"))(v_Term_record_rec.fields)))((codField: hydra.core.Field) =>
          hydra.lib.maybes.map[hydra.core.Type, hydra.core.Type]((cod: hydra.core.Type) => hydra.core.Type.function(hydra.core.FunctionType(dom,
             cod)))(hydra.ext.java.coder.decodeTypeFromTerm(codField.term)))))
        case _ => None)(hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.lib.equality.equal[scala.Predef.String](fname)("literal"))(fterm match
        case hydra.core.Term.union(v_Term_union_litInj) => hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.lib.equality.equal[scala.Predef.String](v_Term_union_litInj.field.name)("string"))(Some(hydra.core.Type.literal(hydra.core.LiteralType.string)))(None)
        case _ => None)(None)))))
    }
  })(None)
  case _ => None

def tryInferFunctionType(fun: hydra.core.Function): Option[hydra.core.Type] =
  fun match
  case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.lib.maybes.bind[hydra.core.Type, hydra.core.Type](v_Function_lambda_lam.domain)((dom: hydra.core.Type) =>
    {
    val mCod: Option[hydra.core.Type] = v_Function_lambda_lam.body match
      case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.lib.maybes.bind[hydra.core.Term, hydra.core.Type](hydra.lib.maps.lookup[hydra.core.Name,
         hydra.core.Term](hydra.constants.key_type)(v_Term_annotated_at.annotation))((typeTerm: hydra.core.Term) => hydra.ext.java.coder.decodeTypeFromTerm(typeTerm))
      case hydra.core.Term.function(v_Term_function_innerFun) => hydra.ext.java.coder.tryInferFunctionType(v_Term_function_innerFun)
      case _ => None
    hydra.lib.maybes.map[hydra.core.Type, hydra.core.Type]((cod: hydra.core.Type) => hydra.core.Type.function(hydra.core.FunctionType(dom, cod)))(mCod)
  })
  case _ => None

def collectTypeApps(t: hydra.core.Term)(acc: Seq[hydra.core.Type]): Tuple2[hydra.core.Term, Seq[hydra.core.Type]] =
  hydra.rewriting.deannotateTerm(t) match
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.ext.java.coder.collectTypeApps(v_Term_typeApplication_ta.body)(hydra.lib.lists.cons[hydra.core.Type](v_Term_typeApplication_ta.`type`)(acc))
  case _ => Tuple2(hydra.rewriting.deannotateTerm(t), acc)

def collectTypeApps0(t: hydra.core.Term)(acc: Seq[hydra.core.Type]): Tuple2[hydra.core.Term, Seq[hydra.core.Type]] =
  hydra.rewriting.deannotateTerm(t) match
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.ext.java.coder.collectTypeApps0(v_Term_typeApplication_ta.body)(hydra.lib.lists.cons[hydra.core.Type](v_Term_typeApplication_ta.`type`)(acc))
  case _ => Tuple2(t, acc)

def countFunctionParams(t: hydra.core.Type): Int =
  hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => hydra.lib.math.add(1)(hydra.ext.java.coder.countFunctionParams(v_Type_function_ft.codomain))
  case _ => 0

def peelDomainTypes(n: Int)(t: hydra.core.Type): Tuple2[Seq[hydra.core.Type], hydra.core.Type] =
  hydra.lib.logic.ifElse[Tuple2[Seq[hydra.core.Type], hydra.core.Type]](hydra.lib.equality.lte[Int](n)(0))(Tuple2(Seq(),
     t))(hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => {
    val rest: Tuple2[Seq[hydra.core.Type], hydra.core.Type] = hydra.ext.java.coder.peelDomainTypes(hydra.lib.math.sub(n)(1))(v_Type_function_ft.codomain)
    Tuple2(hydra.lib.lists.cons[hydra.core.Type](v_Type_function_ft.domain)(hydra.lib.pairs.first[Seq[hydra.core.Type],
       hydra.core.Type](rest)), hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.core.Type](rest))
  }
  case _ => Tuple2(Seq(), t))

def unwrapReturnType(t: hydra.core.Type): hydra.core.Type =
  hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => hydra.ext.java.coder.unwrapReturnType(v_Type_function_ft.codomain)
  case hydra.core.Type.application(v_Type_application_at) => hydra.ext.java.coder.unwrapReturnType(v_Type_application_at.argument)
  case _ => t

def findPairFirst(t: hydra.core.Type): Option[hydra.core.Name] =
  hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.pair(v_Type_pair_pt) => hydra.rewriting.deannotateType(v_Type_pair_pt.first) match
    case hydra.core.Type.variable(v_Type_variable_v) => Some(v_Type_variable_v)
    case _ => None
  case _ => None

def extractInOutPair(t: hydra.core.Type): Seq[Tuple2[hydra.core.Name, hydra.core.Name]] =
  hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => hydra.rewriting.deannotateType(v_Type_function_ft.domain) match
    case hydra.core.Type.variable(v_Type_variable_inVar) => {
      val retType: hydra.core.Type = hydra.ext.java.coder.unwrapReturnType(v_Type_function_ft.codomain)
      hydra.rewriting.deannotateType(retType) match
        case hydra.core.Type.pair(v_Type_pair_pt) => hydra.rewriting.deannotateType(v_Type_pair_pt.first) match
          case hydra.core.Type.variable(v_Type_variable_outVar) => Seq(Tuple2(v_Type_variable_inVar, v_Type_variable_outVar))
          case _ => Seq()
        case _ => Seq()
    }
    case _ => Seq()
  case _ => Seq()

def extractDirectReturn(tparamSet: scala.collection.immutable.Set[hydra.core.Name])(t: hydra.core.Type): Seq[Tuple2[hydra.core.Name,
   hydra.core.Name]] = hydra.ext.java.coder.extractDirectReturn_go(tparamSet)(t)

def extractDirectReturn_go(tparamSet: scala.collection.immutable.Set[hydra.core.Name])(t: hydra.core.Type): Seq[Tuple2[hydra.core.Name, hydra.core.Name]] =
  hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => {
    val dom: hydra.core.Type = hydra.rewriting.deannotateType(v_Type_function_ft.domain)
    {
      val cod: hydra.core.Type = (v_Type_function_ft.codomain)
      dom match
        case hydra.core.Type.variable(v_Type_variable_inVar) => hydra.lib.logic.ifElse[Seq[Tuple2[hydra.core.Name,
           hydra.core.Name]]](hydra.lib.sets.member[hydra.core.Name](v_Type_variable_inVar)(tparamSet))(hydra.rewriting.deannotateType(cod) match
          case hydra.core.Type.function(v_Type_function_ft2) => {
            val midArg: hydra.core.Type = hydra.rewriting.deannotateType(v_Type_function_ft2.domain)
            {
              val retPart: hydra.core.Type = hydra.rewriting.deannotateType(v_Type_function_ft2.codomain)
              midArg match
                case hydra.core.Type.variable(v_Type_variable_midVar) => hydra.lib.logic.ifElse[Seq[Tuple2[hydra.core.Name,
                   hydra.core.Name]]](hydra.lib.sets.member[hydra.core.Name](v_Type_variable_midVar)(tparamSet))(Seq())(retPart match
                  case hydra.core.Type.variable(v_Type_variable_outVar) => hydra.lib.logic.ifElse[Seq[Tuple2[hydra.core.Name,
                     hydra.core.Name]]](hydra.lib.sets.member[hydra.core.Name](v_Type_variable_outVar)(tparamSet))(Seq(Tuple2(v_Type_variable_inVar,
                     v_Type_variable_outVar)))(Seq())
                  case _ => Seq())
                case _ => retPart match
                  case hydra.core.Type.variable(v_Type_variable_outVar) => hydra.lib.logic.ifElse[Seq[Tuple2[hydra.core.Name,
                     hydra.core.Name]]](hydra.lib.sets.member[hydra.core.Name](v_Type_variable_outVar)(tparamSet))(Seq(Tuple2(v_Type_variable_inVar,
                     v_Type_variable_outVar)))(Seq())
                  case _ => Seq()
            }
          }
          case _ => Seq())(hydra.ext.java.coder.extractDirectReturn_go(tparamSet)(cod))
        case _ => hydra.ext.java.coder.extractDirectReturn_go(tparamSet)(cod)
    }
  }
  case _ => Seq()

def nameMapToTypeMap[T0](m: Map[T0, hydra.core.Name]): Map[T0, hydra.core.Type] =
  hydra.lib.maps.map[hydra.core.Name, hydra.core.Type, T0]((v: hydra.core.Name) => hydra.core.Type.variable(v))(m)

def groupPairsByFirst[T0, T1](pairs: Seq[Tuple2[T0, T1]]): Map[T0, Seq[T1]] =
  hydra.lib.lists.foldl[Map[T0, Seq[T1]], Tuple2[T0, T1]]((m: Map[T0, Seq[T1]]) =>
  (p: Tuple2[T0, T1]) =>
  {
  val k: T0 = hydra.lib.pairs.first[T0, T1](p)
  {
    val v: T1 = hydra.lib.pairs.second[T0, T1](p)
    hydra.lib.maps.alter[Seq[T1], T0]((mv: Option[Seq[T1]]) =>
      hydra.lib.maybes.maybe[Option[Seq[T1]], Seq[T1]](Some(Seq(v)))((vs: Seq[T1]) => Some(hydra.lib.lists.concat2[T1](vs)(Seq(v))))(mv))(k)(m)
  }
})(hydra.lib.maps.empty[T0, Seq[T1]])(pairs)

def selfRefSubstitution[T0](grouped: Map[T0, Seq[T0]]): Map[T0, T0] =
  hydra.lib.lists.foldl[Map[T0, T0], Tuple2[T0, Seq[T0]]]((subst: Map[T0, T0]) =>
  (entry: Tuple2[T0, Seq[T0]]) =>
  hydra.ext.java.coder.selfRefSubstitution_processGroup(subst)(hydra.lib.pairs.first[T0, Seq[T0]](entry))(hydra.lib.pairs.second[T0,
     Seq[T0]](entry)))(hydra.lib.maps.empty[T0, T0])(hydra.lib.maps.toList[T0, Seq[T0]](grouped))

def selfRefSubstitution_processGroup[T0](subst: Map[T0, T0])(inVar: T0)(outVars: Seq[T0]): Map[T0, T0] =
  hydra.lib.logic.ifElse[Map[T0, T0]](hydra.lib.lists.elem[T0](inVar)(outVars))(hydra.lib.lists.foldl[Map[T0, T0], T0]((s: Map[T0, T0]) =>
  (v: T0) =>
  hydra.lib.logic.ifElse[Map[T0, T0]](hydra.lib.equality.equal[T0](v)(inVar))(s)(hydra.lib.maps.insert[T0, T0](v)(inVar)(s)))(subst)(outVars))(subst)

def directRefSubstitution[T0](directInputVars: scala.collection.immutable.Set[T0])(codVar: Option[T0])(grouped: Map[T0, Seq[T0]]): Map[T0, T0] =
  hydra.lib.lists.foldl[Map[T0, T0], Tuple2[T0, Seq[T0]]]((subst: Map[T0, T0]) =>
  (entry: Tuple2[T0, Seq[T0]]) =>
  hydra.ext.java.coder.directRefSubstitution_processGroup(directInputVars)(codVar)(subst)(hydra.lib.pairs.first[T0,
     Seq[T0]](entry))(hydra.lib.pairs.second[T0, Seq[T0]](entry)))(hydra.lib.maps.empty[T0, T0])(hydra.lib.maps.toList[T0,
     Seq[T0]](grouped))

def directRefSubstitution_processGroup[T0](directInputVars: scala.collection.immutable.Set[T0])(codVar: Option[T0])(subst: Map[T0,
   T0])(inVar: T0)(outVars: Seq[T0]): Map[T0, T0] =
  {
  val selfRefCount: Int = hydra.lib.lists.length[T0](hydra.lib.lists.filter[T0]((v: T0) => hydra.lib.equality.equal[T0](v)(inVar))(outVars))
  val nonSelfVars: Seq[T0] = hydra.lib.lists.filter[T0]((v: T0) => hydra.lib.logic.not(hydra.lib.equality.equal[T0](v)(inVar)))(outVars)
  val safeNonSelfVars: Seq[T0] = hydra.lib.lists.filter[T0]((v: T0) =>
    hydra.lib.logic.and(hydra.lib.logic.not(hydra.lib.sets.member[T0](v)(directInputVars)))(hydra.lib.logic.not(hydra.lib.equality.equal[Option[T0]](Some(v))(codVar))))(nonSelfVars)
  hydra.lib.logic.ifElse[Map[T0, T0]](hydra.lib.logic.and(hydra.lib.equality.gte[Int](selfRefCount)(2))(hydra.lib.logic.not(hydra.lib.lists.`null`[T0](safeNonSelfVars))))(hydra.lib.lists.foldl[Map[T0,
     T0], T0]((s: Map[T0, T0]) => (v: T0) => hydra.lib.maps.insert[T0, T0](v)(inVar)(s))(subst)(safeNonSelfVars))(subst)
}

def findSelfRefVar[T0](grouped: Map[T0, Seq[T0]]): Option[T0] =
  {
  val selfRefs: Seq[Tuple2[T0, Seq[T0]]] = hydra.lib.lists.filter[Tuple2[T0, Seq[T0]]]((entry: Tuple2[T0, Seq[T0]]) =>
    hydra.lib.lists.elem[T0](hydra.lib.pairs.first[T0, Seq[T0]](entry))(hydra.lib.pairs.second[T0, Seq[T0]](entry)))(hydra.lib.maps.toList[T0,
       Seq[T0]](grouped))
  hydra.lib.logic.ifElse[Option[T0]](hydra.lib.lists.`null`[Tuple2[T0, Seq[T0]]](selfRefs))(None)(Some(hydra.lib.pairs.first[T0,
     Seq[T0]](hydra.lib.lists.head[Tuple2[T0, Seq[T0]]](selfRefs))))
}

def detectAccumulatorUnification(doms: Seq[hydra.core.Type])(cod: hydra.core.Type)(tparams: Seq[hydra.core.Name]): Map[hydra.core.Name, hydra.core.Type] =
  {
  val tparamSet: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](tparams)
  val allPairs: Seq[Tuple2[hydra.core.Name, hydra.core.Name]] = hydra.lib.lists.bind[hydra.core.Type,
     Tuple2[hydra.core.Name, hydra.core.Name]](doms)((d: hydra.core.Type) => hydra.ext.java.coder.extractInOutPair(d))
  val groupedByInput: Map[hydra.core.Name, Seq[hydra.core.Name]] = hydra.ext.java.coder.groupPairsByFirst(allPairs)
  val selfRefSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.ext.java.coder.selfRefSubstitution(groupedByInput)
  val directPairs: Seq[Tuple2[hydra.core.Name, hydra.core.Name]] = hydra.lib.lists.bind[hydra.core.Type,
     Tuple2[hydra.core.Name, hydra.core.Name]](doms)((d: hydra.core.Type) => hydra.ext.java.coder.extractDirectReturn(tparamSet)(d))
  val groupedDirect: Map[hydra.core.Name, Seq[hydra.core.Name]] = hydra.ext.java.coder.groupPairsByFirst(directPairs)
  val directInputVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.map[Tuple2[hydra.core.Name,
     hydra.core.Name], hydra.core.Name]((p: Tuple2[hydra.core.Name, hydra.core.Name]) => hydra.lib.pairs.first[hydra.core.Name,
     hydra.core.Name](p))(directPairs))
  val codVar: Option[hydra.core.Name] = hydra.rewriting.deannotateType(cod) match
    case hydra.core.Type.variable(v_Type_variable_v) => Some(v_Type_variable_v)
    case _ => None
  val directRefSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.ext.java.coder.directRefSubstitution(directInputVars)(codVar)(groupedDirect)
  val codSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maybes.maybe[Map[hydra.core.Name, hydra.core.Name],
     hydra.core.Name](hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name])((cv: hydra.core.Name) =>
    hydra.lib.logic.ifElse[Map[hydra.core.Name, hydra.core.Name]](hydra.lib.maps.member[hydra.core.Name,
       hydra.core.Name](cv)(selfRefSubst))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name])(hydra.lib.maybes.maybe[Map[hydra.core.Name,
       hydra.core.Name], hydra.core.Name](hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name])((refVar: hydra.core.Name) =>
    hydra.lib.logic.ifElse[Map[hydra.core.Name, hydra.core.Name]](hydra.lib.equality.equal[hydra.core.Name](cv)(refVar))(hydra.lib.maps.empty[hydra.core.Name,
       hydra.core.Name])(hydra.lib.maps.singleton[hydra.core.Name, hydra.core.Name](cv)(refVar)))(hydra.ext.java.coder.findSelfRefVar(groupedByInput))))(hydra.ext.java.coder.findPairFirst(cod))
  val domVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.bind[hydra.core.Type,
     hydra.core.Name](doms)((d: hydra.core.Type) =>
    hydra.lib.sets.toList[hydra.core.Name](hydra.ext.java.coder.collectTypeVars(d))))
  val danglingSubst: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maybes.maybe[Map[hydra.core.Name,
     hydra.core.Type], hydra.core.Name](hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type])((cv: hydra.core.Name) =>
    hydra.lib.logic.ifElse[Map[hydra.core.Name, hydra.core.Type]](hydra.lib.sets.member[hydra.core.Name](cv)(domVars))(hydra.lib.maps.empty[hydra.core.Name,
       hydra.core.Type])(hydra.lib.maybes.maybe[Map[hydra.core.Name, hydra.core.Type], hydra.core.Name](hydra.lib.maps.empty[hydra.core.Name,
       hydra.core.Type])((refVar: hydra.core.Name) =>
    hydra.lib.maps.singleton[hydra.core.Name, hydra.core.Type](cv)(hydra.core.Type.variable(refVar)))(hydra.ext.java.coder.findSelfRefVar(groupedByInput))))(hydra.ext.java.coder.findPairFirst(cod))
  hydra.lib.maps.union[hydra.core.Name, hydra.core.Type](hydra.lib.maps.union[hydra.core.Name, hydra.core.Type](hydra.lib.maps.union[hydra.core.Name,
     hydra.core.Type](hydra.ext.java.coder.nameMapToTypeMap(selfRefSubst))(hydra.ext.java.coder.nameMapToTypeMap(codSubst)))(danglingSubst))(hydra.ext.java.coder.nameMapToTypeMap(directRefSubst))
}

def typesMatch(a: hydra.core.Type)(b: hydra.core.Type): Boolean =
  a match
  case hydra.core.Type.variable(v_Type_variable_va) => b match
    case hydra.core.Type.variable(v_Type_variable_vb) => hydra.lib.equality.equal[hydra.core.Name](v_Type_variable_va)(v_Type_variable_vb)
    case _ => true
  case hydra.core.Type.wrap(v_Type_wrap_wa) => b match
    case hydra.core.Type.wrap(v_Type_wrap_wb) => hydra.lib.equality.equal[hydra.core.Type](v_Type_wrap_wa)(v_Type_wrap_wb)
    case _ => true
  case _ => true

def isSimpleName(name: hydra.core.Name): Boolean =
  hydra.lib.equality.equal[Int](hydra.lib.lists.length[scala.Predef.String](hydra.lib.strings.splitOn(".")(name)))(1)

def filterPhantomTypeArgs[T0, T1](calleeName: hydra.core.Name)(allTypeArgs: Seq[hydra.core.Type])(cx: T0)(g: hydra.graph.Graph): Either[T1,
   Seq[hydra.core.Type]] =
  hydra.lib.eithers.bind[T1, Option[hydra.core.Binding], Seq[hydra.core.Type]](Right(hydra.lexical.dereferenceElement(g)(calleeName)))((mel: Option[hydra.core.Binding]) =>
  hydra.lib.maybes.cases[hydra.core.Binding, Either[T1, Seq[hydra.core.Type]]](mel)(Right(allTypeArgs))((el: hydra.core.Binding) =>
  hydra.lib.maybes.cases[hydra.core.TypeScheme, Either[T1, Seq[hydra.core.Type]]](el.`type`)(Right(allTypeArgs))((ts: hydra.core.TypeScheme) =>
  {
  val schemeVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) => hydra.ext.java.coder.isSimpleName(v))(ts.variables)
  {
    val schemeTypeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.ext.java.coder.collectTypeVars(ts.`type`)
    {
      val schemeType: hydra.core.Type = (ts.`type`)
      {
        val nParams: Int = hydra.ext.java.coder.countFunctionParams(schemeType)
        {
          val peeled: Tuple2[Seq[hydra.core.Type], hydra.core.Type] = hydra.ext.java.coder.peelDomainTypes(nParams)(schemeType)
          {
            val calleeDoms: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.core.Type](peeled)
            {
              val calleeCod: hydra.core.Type = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.core.Type](peeled)
              {
                val overgenSubst: Map[hydra.core.Name, hydra.core.Type] = hydra.ext.java.coder.detectAccumulatorUnification(calleeDoms)(calleeCod)(schemeVars)
                {
                  val keepFlags: Seq[Boolean] = hydra.lib.lists.map[hydra.core.Name, Boolean]((v: hydra.core.Name) =>
                    hydra.lib.logic.and(hydra.lib.sets.member[hydra.core.Name](v)(schemeTypeVars))(hydra.lib.logic.not(hydra.lib.maps.member[hydra.core.Name,
                       hydra.core.Type](v)(overgenSubst))))(schemeVars)
                  hydra.lib.logic.ifElse[Either[T1, Seq[hydra.core.Type]]](hydra.lib.logic.not(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Name](schemeVars))(hydra.lib.lists.length[hydra.core.Type](allTypeArgs))))(Right(allTypeArgs))(Right(hydra.ext.java.coder.filterPhantomTypeArgs_filterAndApply(allTypeArgs)(keepFlags)(overgenSubst)))
                }
              }
            }
          }
        }
      }
    }
  }
})))

def filterPhantomTypeArgs_filterAndApply(allTypeArgs: Seq[hydra.core.Type])(keepFlags: Seq[Boolean])(overgenSubst: Map[hydra.core.Name,
   hydra.core.Type]): Seq[hydra.core.Type] =
  {
  val filtered: Seq[hydra.core.Type] = hydra.lib.lists.map[Tuple2[hydra.core.Type, Boolean], hydra.core.Type]((p: Tuple2[hydra.core.Type,
     Boolean]) => hydra.lib.pairs.first[hydra.core.Type, Boolean](p))(hydra.lib.lists.filter[Tuple2[hydra.core.Type,
     Boolean]]((p: Tuple2[hydra.core.Type, Boolean]) => hydra.lib.pairs.second[hydra.core.Type, Boolean](p))(hydra.lib.lists.zip[hydra.core.Type,
     Boolean](allTypeArgs)(keepFlags)))
  hydra.lib.logic.ifElse[Seq[hydra.core.Type]](hydra.lib.logic.not(hydra.lib.maps.`null`[hydra.core.Name,
     hydra.core.Type](overgenSubst)))(hydra.lib.lists.map[hydra.core.Type, hydra.core.Type]((t: hydra.core.Type) =>
    hydra.ext.java.coder.substituteTypeVarsWithTypes(overgenSubst)(t))(filtered))(filtered)
}

def filterByFlags[T0](xs: Seq[T0])(flags: Seq[Boolean]): Seq[T0] =
  hydra.lib.lists.map[Tuple2[T0, Boolean], T0]((p: Tuple2[T0, Boolean]) => hydra.lib.pairs.first[T0, Boolean](p))(hydra.lib.lists.filter[Tuple2[T0,
     Boolean]]((p: Tuple2[T0, Boolean]) => hydra.lib.pairs.second[T0, Boolean](p))(hydra.lib.lists.zip[T0,
     Boolean](xs)(flags)))

def applySubstSimple(subst: Map[hydra.core.Name, hydra.core.Type])(t: hydra.core.Type): hydra.core.Type =
  hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.maps.findWithDefault[hydra.core.Type, hydra.core.Name](t)(v_Type_variable_v)(subst)
  case _ => t

def buildArgSubst[T0](schemeVarSet: scala.collection.immutable.Set[hydra.core.Name])(schemeDoms: Seq[hydra.core.Type])(argTypes: Seq[T0]): Map[hydra.core.Name,
   T0] =
  hydra.lib.maps.fromList[hydra.core.Name, T0](hydra.lib.lists.bind[Tuple2[hydra.core.Type, T0], Tuple2[hydra.core.Name,
     T0]](hydra.lib.lists.zip[hydra.core.Type, T0](schemeDoms)(argTypes))((p: Tuple2[hydra.core.Type,
     T0]) =>
  {
  val sdom: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, T0](p)
  {
    val argType: T0 = hydra.lib.pairs.second[hydra.core.Type, T0](p)
    hydra.rewriting.deannotateType(sdom) match
      case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.logic.ifElse[Seq[Tuple2[hydra.core.Name,
         T0]]](hydra.lib.sets.member[hydra.core.Name](v_Type_variable_v)(schemeVarSet))(Seq(Tuple2(v_Type_variable_v,
         argType)))(Seq())
      case _ => Seq()
  }
}))

def resolveTypeApps(schemeVars: Seq[hydra.core.Name])(fallbackTypeApps: Seq[hydra.core.Type])(argSubst: Map[hydra.core.Name,
   hydra.core.Type]): Seq[hydra.core.Type] =
  {
  val resolvedVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name,
     hydra.core.Type](argSubst))
  val unresolvedVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) =>
    hydra.lib.logic.not(hydra.lib.sets.member[hydra.core.Name](v)(resolvedVars)))(schemeVars)
  val usedTypes: scala.collection.immutable.Set[hydra.core.Type] = hydra.lib.sets.fromList[hydra.core.Type](hydra.lib.maps.elems[hydra.core.Name,
     hydra.core.Type](argSubst))
  val unusedIrTypes: Seq[hydra.core.Type] = hydra.lib.lists.filter[hydra.core.Type]((t: hydra.core.Type) =>
    hydra.lib.logic.not(hydra.lib.sets.member[hydra.core.Type](t)(usedTypes)))(fallbackTypeApps)
  val remainingSubst: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maps.fromList[hydra.core.Name,
     hydra.core.Type](hydra.lib.lists.zip[hydra.core.Name, hydra.core.Type](unresolvedVars)(unusedIrTypes))
  val fullSubst: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maps.union[hydra.core.Name, hydra.core.Type](argSubst)(remainingSubst)
  hydra.lib.lists.map[hydra.core.Name, hydra.core.Type]((v: hydra.core.Name) =>
    hydra.lib.maps.findWithDefault[hydra.core.Type, hydra.core.Name](hydra.core.Type.variable(v))(v)(fullSubst))(schemeVars)
}

def correctTypeAppsWithArgs(schemeVars: Seq[hydra.core.Name])(fallbackTypeApps: Seq[hydra.core.Type])(schemeType: hydra.core.Type)(args: Seq[hydra.core.Term])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   Seq[hydra.core.Type]] =
  {
  val schemeVarSet: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](schemeVars)
  val irSubst: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Type](hydra.lib.lists.zip[hydra.core.Name,
     hydra.core.Type](schemeVars)(fallbackTypeApps))
  val peeled: Tuple2[Seq[hydra.core.Type], hydra.core.Type] = hydra.ext.java.coder.peelDomainTypes(hydra.lib.lists.length[hydra.core.Term](args))(schemeType)
  val schemeDoms: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.core.Type](peeled)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[Option[hydra.core.Type]], Seq[hydra.core.Type]](hydra.lib.eithers.mapList[hydra.core.Term,
     Option[hydra.core.Type], hydra.context.InContext[hydra.error.Error]]((arg: hydra.core.Term) =>
    hydra.lib.eithers.bimap[hydra.error.DecodingError, Option[hydra.core.Type], hydra.context.InContext[hydra.error.Error],
       Option[hydra.core.Type]]((__de: hydra.error.DecodingError) => hydra.context.InContext(hydra.error.Error.other(__de),
       cx))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(hydra.annotations.termAnnotationInternal(arg))))(args))((mArgTypes: Seq[Option[hydra.core.Type]]) =>
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.Type]]](hydra.lib.logic.not(hydra.lib.lists.`null`[Option[hydra.core.Type]](hydra.lib.lists.filter[Option[hydra.core.Type]]((m: Option[hydra.core.Type]) => hydra.lib.maybes.isNothing[hydra.core.Type](m))(mArgTypes))))(Right(fallbackTypeApps))({
    val argTypes: Seq[hydra.core.Type] = hydra.lib.lists.bind[Option[hydra.core.Type], hydra.core.Type](mArgTypes)((m: Option[hydra.core.Type]) =>
      hydra.lib.maybes.cases[hydra.core.Type, Seq[hydra.core.Type]](m)(Seq())((x: hydra.core.Type) => hydra.lib.lists.pure[hydra.core.Type](x)))
    {
      val irDoms: Seq[hydra.core.Type] = hydra.lib.lists.map[hydra.core.Type, hydra.core.Type]((d: hydra.core.Type) => hydra.ext.java.coder.applySubstSimple(irSubst)(d))(schemeDoms)
      {
        val domsMatch: Boolean = hydra.lib.lists.`null`[Tuple2[hydra.core.Type, hydra.core.Type]](hydra.lib.lists.filter[Tuple2[hydra.core.Type,
           hydra.core.Type]]((p: Tuple2[hydra.core.Type, hydra.core.Type]) =>
          hydra.lib.logic.not(hydra.ext.java.coder.typesMatch(hydra.rewriting.deannotateType(hydra.lib.pairs.first[hydra.core.Type,
             hydra.core.Type](p)))(hydra.rewriting.deannotateType(hydra.lib.pairs.second[hydra.core.Type,
             hydra.core.Type](p)))))(hydra.lib.lists.zip[hydra.core.Type, hydra.core.Type](irDoms)(argTypes)))
        hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.Type]]](domsMatch)(Right(fallbackTypeApps))(Right(hydra.ext.java.coder.resolveTypeApps(schemeVars)(fallbackTypeApps)(hydra.ext.java.coder.buildArgSubst(schemeVarSet)(schemeDoms)(argTypes))))
      }
    }
  }))
}

def correctTypeApps[T0](gr: T0)(name: hydra.core.Name)(args: Seq[hydra.core.Term])(fallbackTypeApps: Seq[hydra.core.Type])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   Seq[hydra.core.Type]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.core.Binding], Seq[hydra.core.Type]](Right(hydra.lexical.dereferenceElement(g)(name)))((mel: Option[hydra.core.Binding]) =>
  hydra.lib.maybes.cases[hydra.core.Binding, Either[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.Type]]](mel)(Right(fallbackTypeApps))((el: hydra.core.Binding) =>
  hydra.lib.maybes.cases[hydra.core.TypeScheme, Either[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.Type]]](el.`type`)(Right(fallbackTypeApps))((ts: hydra.core.TypeScheme) =>
  {
  val schemeType: hydra.core.Type = (ts.`type`)
  {
    val allSchemeVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) => hydra.ext.java.coder.isSimpleName(v))(ts.variables)
    {
      val schemeTypeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.ext.java.coder.collectTypeVars(schemeType)
      {
        val usedFlags: Seq[Boolean] = hydra.lib.lists.map[hydra.core.Name, Boolean]((v: hydra.core.Name) => hydra.lib.sets.member[hydra.core.Name](v)(schemeTypeVars))(allSchemeVars)
        {
          val usedSchemeVars: Seq[hydra.core.Name] = hydra.ext.java.coder.filterByFlags(allSchemeVars)(usedFlags)
          {
            val nParams: Int = hydra.ext.java.coder.countFunctionParams(schemeType)
            {
              val peeled: Tuple2[Seq[hydra.core.Type], hydra.core.Type] = hydra.ext.java.coder.peelDomainTypes(nParams)(schemeType)
              {
                val calleeDoms: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.core.Type](peeled)
                {
                  val calleeCod: hydra.core.Type = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.core.Type](peeled)
                  {
                    val overgenSubst: Map[hydra.core.Name, hydra.core.Type] = hydra.ext.java.coder.detectAccumulatorUnification(calleeDoms)(calleeCod)(usedSchemeVars)
                    {
                      val keepFlags: Seq[Boolean] = hydra.lib.lists.map[hydra.core.Name, Boolean]((v: hydra.core.Name) =>
                        hydra.lib.logic.and(hydra.lib.sets.member[hydra.core.Name](v)(schemeTypeVars))(hydra.lib.logic.not(hydra.lib.maps.member[hydra.core.Name,
                           hydra.core.Type](v)(overgenSubst))))(allSchemeVars)
                      {
                        val schemeVars: Seq[hydra.core.Name] = hydra.ext.java.coder.filterByFlags(allSchemeVars)(keepFlags)
                        {
                          val filteredFallback0: Seq[hydra.core.Type] = hydra.lib.logic.ifElse[Seq[hydra.core.Type]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Name](allSchemeVars))(hydra.lib.lists.length[hydra.core.Type](fallbackTypeApps)))(hydra.ext.java.coder.filterByFlags(fallbackTypeApps)(keepFlags))(fallbackTypeApps)
                          {
                            val filteredFallback: Seq[hydra.core.Type] = hydra.lib.logic.ifElse[Seq[hydra.core.Type]](hydra.lib.maps.`null`[hydra.core.Name,
                               hydra.core.Type](overgenSubst))(filteredFallback0)(hydra.lib.lists.map[hydra.core.Type,
                               hydra.core.Type]((t: hydra.core.Type) =>
                              hydra.ext.java.coder.substituteTypeVarsWithTypes(overgenSubst)(t))(filteredFallback0))
                            hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
                               Seq[hydra.core.Type]]](hydra.lib.logic.or(hydra.lib.lists.`null`[hydra.core.Name](schemeVars))(hydra.lib.logic.not(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Name](schemeVars))(hydra.lib.lists.length[hydra.core.Type](filteredFallback)))))(Right(filteredFallback))(hydra.ext.java.coder.correctTypeAppsWithArgs(schemeVars)(filteredFallback)(schemeType)(args)(cx)(g))
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
      }
    }
  }
})))

def buildSubstFromAnnotations_go(schemeVarSet: scala.collection.immutable.Set[hydra.core.Name])(g: hydra.graph.Graph)(term: hydra.core.Term): Map[hydra.core.Name,
   hydra.core.Name] =
  term match
  case hydra.core.Term.annotated(v_Term_annotated_at) => {
    val body: hydra.core.Term = (v_Term_annotated_at.body)
    {
      val anns: Map[hydra.core.Name, hydra.core.Term] = (v_Term_annotated_at.annotation)
      {
        val bodySubst: Map[hydra.core.Name, hydra.core.Name] = hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(body)
        {
          val annSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maybes.cases[hydra.core.Term,
             Map[hydra.core.Name, hydra.core.Name]](hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Term](hydra.constants.key_type)(anns))(hydra.lib.maps.empty[hydra.core.Name,
             hydra.core.Name])((typeTerm: hydra.core.Term) =>
            hydra.lib.eithers.either[hydra.error.DecodingError, hydra.core.Type, Map[hydra.core.Name,
               hydra.core.Name]]((_x: hydra.error.DecodingError) => hydra.lib.maps.empty[hydra.core.Name,
               hydra.core.Name])((annType: hydra.core.Type) =>
            hydra.rewriting.deannotateTerm(body) match
            case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
              case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.lib.maybes.cases[hydra.core.Type,
                 Map[hydra.core.Name, hydra.core.Name]](v_Function_lambda_lam.domain)(hydra.lib.maps.empty[hydra.core.Name,
                 hydra.core.Name])((dom: hydra.core.Type) =>
                hydra.rewriting.deannotateType(annType) match
                case hydra.core.Type.function(v_Type_function_ft) => hydra.ext.java.coder.buildTypeVarSubst(schemeVarSet)(v_Type_function_ft.domain)(dom)
                case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name])
              case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
            case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name])(hydra.decode.core.`type`(g)(typeTerm)))
          hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](annSubst)(bodySubst)
        }
      }
    }
  }
  case hydra.core.Term.application(v_Term_application_app) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(v_Term_application_app.function))(hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(v_Term_application_app.argument))
  case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
    case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(v_Function_lambda_lam.body)
    case hydra.core.Function.elimination(v_Function_elimination_elim) => v_Function_elimination_elim match
      case hydra.core.Elimination.union(v_Elimination_union_cs) => {
        val defSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maybes.cases[hydra.core.Term,
           Map[hydra.core.Name, hydra.core.Name]](v_Elimination_union_cs.default)(hydra.lib.maps.empty[hydra.core.Name,
           hydra.core.Name])((d: hydra.core.Term) =>
          hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(d))
        {
          val caseSubsts: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.lists.foldl[Map[hydra.core.Name,
             hydra.core.Name], hydra.core.Field]((acc: Map[hydra.core.Name, hydra.core.Name]) =>
            (fld: hydra.core.Field) =>
            hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](acc)(hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(fld.term)))(hydra.lib.maps.empty[hydra.core.Name,
               hydra.core.Name])(v_Elimination_union_cs.cases)
          hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](defSubst)(caseSubsts)
        }
      }
      case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
    case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]
  case hydra.core.Term.let(v_Term_let_lt) => {
    val bindingSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.lists.foldl[Map[hydra.core.Name,
       hydra.core.Name], hydra.core.Binding]((acc: Map[hydra.core.Name, hydra.core.Name]) =>
      (b: hydra.core.Binding) =>
      hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](acc)(hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(b.term)))(hydra.lib.maps.empty[hydra.core.Name,
         hydra.core.Name])(v_Term_let_lt.bindings)
    hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](bindingSubst)(hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(v_Term_let_lt.body))
  }
  case hydra.core.Term.list(v_Term_list_terms) => hydra.lib.lists.foldl[Map[hydra.core.Name, hydra.core.Name],
     hydra.core.Term]((acc: Map[hydra.core.Name, hydra.core.Name]) =>
    (t: hydra.core.Term) =>
    hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](acc)(hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(t)))(hydra.lib.maps.empty[hydra.core.Name,
       hydra.core.Name])(v_Term_list_terms)
  case hydra.core.Term.maybe(v_Term_maybe_mt) => hydra.lib.maybes.cases[hydra.core.Term, Map[hydra.core.Name,
     hydra.core.Name]](v_Term_maybe_mt)(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name])((t: hydra.core.Term) =>
    hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(t))
  case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(hydra.lib.pairs.first[hydra.core.Term,
     hydra.core.Term](v_Term_pair_p)))(hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(hydra.lib.pairs.second[hydra.core.Term,
     hydra.core.Term](v_Term_pair_p)))
  case hydra.core.Term.record(v_Term_record_r) => hydra.lib.lists.foldl[Map[hydra.core.Name, hydra.core.Name],
     hydra.core.Field]((acc: Map[hydra.core.Name, hydra.core.Name]) =>
    (fld: hydra.core.Field) =>
    hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](acc)(hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(fld.term)))(hydra.lib.maps.empty[hydra.core.Name,
       hydra.core.Name])(v_Term_record_r.fields)
  case hydra.core.Term.set(v_Term_set_terms) => hydra.lib.lists.foldl[Map[hydra.core.Name, hydra.core.Name],
     hydra.core.Term]((acc: Map[hydra.core.Name, hydra.core.Name]) =>
    (t: hydra.core.Term) =>
    hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](acc)(hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(t)))(hydra.lib.maps.empty[hydra.core.Name,
       hydra.core.Name])(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_terms))
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(v_Term_typeApplication_ta.body)
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(v_Term_typeLambda_tl.body)
  case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term,
     Map[hydra.core.Name, hydra.core.Name]]((t: hydra.core.Term) =>
    hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(t))((t: hydra.core.Term) =>
    hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(t))(v_Term_either_e)
  case _ => hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name]

def buildSubstFromAnnotations[T0, T1](schemeVarSet: scala.collection.immutable.Set[hydra.core.Name])(term: hydra.core.Term)(cx: T0)(g: hydra.graph.Graph): Either[T1,
   Map[hydra.core.Name, hydra.core.Name]] = Right(hydra.ext.java.coder.buildSubstFromAnnotations_go(schemeVarSet)(g)(term))

def applyOvergenSubstToTermAnnotations_go(subst: Map[hydra.core.Name, hydra.core.Type])(cx: hydra.graph.Graph)(term: hydra.core.Term): hydra.core.Term =
  term match
  case hydra.core.Term.annotated(v_Term_annotated_at) => {
    val inner: hydra.core.Term = (v_Term_annotated_at.body)
    {
      val ann: Map[hydra.core.Name, hydra.core.Term] = (v_Term_annotated_at.annotation)
      {
        val `ann_`: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.maybes.cases[hydra.core.Term, Map[hydra.core.Name,
           hydra.core.Term]](hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Term](hydra.constants.key_type)(ann))(ann)((typeTerm: hydra.core.Term) =>
          hydra.lib.eithers.either[hydra.error.DecodingError, hydra.core.Type, Map[hydra.core.Name, hydra.core.Term]]((_x: hydra.error.DecodingError) => ann)((t: hydra.core.Type) =>
          {
          val `t_`: hydra.core.Type = hydra.ext.java.coder.substituteTypeVarsWithTypes(subst)(t)
          hydra.lib.maps.insert[hydra.core.Name, hydra.core.Term](hydra.constants.key_type)(hydra.encode.core.`type`(`t_`))(ann)
        })(hydra.decode.core.`type`(cx)(typeTerm)))
        hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.ext.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(inner), `ann_`))
      }
    }
  }
  case hydra.core.Term.application(v_Term_application_app) => hydra.core.Term.application(hydra.core.Application(hydra.ext.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(v_Term_application_app.function),
     hydra.ext.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(v_Term_application_app.argument)))
  case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
    case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(v_Function_lambda_lam.parameter,
       hydra.lib.maybes.map[hydra.core.Type, hydra.core.Type]((d: hydra.core.Type) => hydra.ext.java.coder.substituteTypeVarsWithTypes(subst)(d))(v_Function_lambda_lam.domain),
       hydra.ext.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(v_Function_lambda_lam.body))))
    case hydra.core.Function.elimination(v_Function_elimination_elim) => v_Function_elimination_elim match
      case hydra.core.Elimination.union(v_Elimination_union_cs) => hydra.core.Term.function(hydra.core.Function.elimination(hydra.core.Elimination.union(hydra.core.CaseStatement(v_Elimination_union_cs.typeName,
         hydra.lib.maybes.map[hydra.core.Term, hydra.core.Term]((d: hydra.core.Term) =>
        hydra.ext.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(d))(v_Elimination_union_cs.default),
           hydra.lib.lists.map[hydra.core.Field, hydra.core.Field]((fld: hydra.core.Field) =>
        hydra.core.Field(fld.name, hydra.ext.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(fld.term)))(v_Elimination_union_cs.cases)))))
      case _ => term
    case _ => term
  case hydra.core.Term.let(v_Term_let_lt) => hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding,
     hydra.core.Binding]((b: hydra.core.Binding) =>
    hydra.core.Binding(b.name, hydra.ext.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(b.term),
       (b.`type`)))(v_Term_let_lt.bindings), hydra.ext.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(v_Term_let_lt.body)))
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(hydra.ext.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(v_Term_typeApplication_ta.body),
     hydra.ext.java.coder.substituteTypeVarsWithTypes(subst)(v_Term_typeApplication_ta.`type`)))
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_tl.parameter,
     hydra.ext.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(cx)(v_Term_typeLambda_tl.body)))
  case _ => term

def applyOvergenSubstToTermAnnotations[T0, T1](subst: Map[hydra.core.Name, hydra.core.Type])(term0: hydra.core.Term)(cx: T0)(g: hydra.graph.Graph): Either[T1,
   hydra.core.Term] =
  Right(hydra.ext.java.coder.applyOvergenSubstToTermAnnotations_go(subst)(g)(term0))

val javaComparableRefType: hydra.ext.java.syntax.ReferenceType = hydra.ext.java.syntax.ReferenceType.classOrInterface(hydra.ext.java.syntax.ClassOrInterfaceType.`class`(hydra.ext.java.syntax.ClassType(Seq(),
   hydra.ext.java.syntax.ClassTypeQualifier.none, hydra.ext.java.utils.javaTypeIdentifier("Comparable"),
   Seq())))

def comparableCompareExpr(otherVar: scala.Predef.String)(fname: scala.Predef.String): hydra.ext.java.syntax.Expression =
  {
  val arg: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaExpressionNameToJavaExpression(hydra.ext.java.utils.fieldExpression(hydra.ext.java.utils.javaIdentifier(otherVar))(hydra.ext.java.utils.javaIdentifier(fname)))
  val castVar: hydra.ext.java.syntax.MethodInvocation_Variant = hydra.ext.java.syntax.MethodInvocation_Variant.primary(hydra.ext.java.utils.javaExpressionToJavaPrimary(hydra.ext.java.utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.javaCastExpression(hydra.ext.java.coder.javaComparableRefType)(hydra.ext.java.utils.javaIdentifierToJavaUnaryExpression(hydra.ext.java.utils.sanitizeJavaName(fname))))))
  val header: hydra.ext.java.syntax.MethodInvocation_Header = hydra.ext.java.syntax.MethodInvocation_Header.complex(hydra.ext.java.syntax.MethodInvocation_Complex(castVar,
     Seq(), hydra.ext.java.names.compareToMethodName))
  hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.syntax.MethodInvocation(header, Seq(arg)))
}

def arraysCompareExpr(otherVar: scala.Predef.String)(fname: scala.Predef.String): hydra.ext.java.syntax.Expression =
  {
  val header: hydra.ext.java.syntax.MethodInvocation_Header = hydra.ext.java.syntax.MethodInvocation_Header.complex(hydra.ext.java.syntax.MethodInvocation_Complex(hydra.ext.java.syntax.MethodInvocation_Variant.`type`(hydra.ext.java.utils.javaTypeName("java.util.Arrays")),
     Seq(), "compare"))
  val arg1: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaExpressionNameToJavaExpression(hydra.ext.java.syntax.ExpressionName(None,
     hydra.ext.java.utils.sanitizeJavaName(fname)))
  val arg2: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaExpressionNameToJavaExpression(hydra.ext.java.utils.fieldExpression(hydra.ext.java.utils.javaIdentifier(otherVar))(hydra.ext.java.utils.javaIdentifier(fname)))
  hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.syntax.MethodInvocation(header, Seq(arg1, arg2)))
}

def hashCodeCompareExpr(otherVar: scala.Predef.String)(fname: scala.Predef.String): hydra.ext.java.syntax.Expression =
  {
  val header: hydra.ext.java.syntax.MethodInvocation_Header = hydra.ext.java.syntax.MethodInvocation_Header.complex(hydra.ext.java.syntax.MethodInvocation_Complex(hydra.ext.java.syntax.MethodInvocation_Variant.`type`(hydra.ext.java.utils.javaTypeName("Integer")),
     Seq(), "compare"))
  val thisHashCode: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.syntax.MethodInvocation(hydra.ext.java.syntax.MethodInvocation_Header.complex(hydra.ext.java.syntax.MethodInvocation_Complex(hydra.ext.java.syntax.MethodInvocation_Variant.expression(hydra.ext.java.syntax.ExpressionName(None,
     hydra.ext.java.utils.sanitizeJavaName(fname))), Seq(), hydra.ext.java.names.hashCodeMethodName)),
     Seq()))
  val otherHashCode: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.syntax.MethodInvocation(hydra.ext.java.syntax.MethodInvocation_Header.complex(hydra.ext.java.syntax.MethodInvocation_Complex(hydra.ext.java.syntax.MethodInvocation_Variant.expression(hydra.ext.java.utils.fieldExpression(hydra.ext.java.utils.javaIdentifier(otherVar))(hydra.ext.java.utils.javaIdentifier(fname))),
     Seq(), hydra.ext.java.names.hashCodeMethodName)), Seq()))
  hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.syntax.MethodInvocation(header, Seq(thisHashCode, otherHashCode)))
}

def compareFieldExpr(otherVar: scala.Predef.String)(ft: hydra.core.FieldType): hydra.ext.java.syntax.Expression =
  {
  val fname: scala.Predef.String = (ft.name)
  val ftype: hydra.core.Type = (ft.`type`)
  hydra.lib.logic.ifElse[hydra.ext.java.syntax.Expression](hydra.ext.java.coder.isBinaryType(ftype))(hydra.ext.java.coder.arraysCompareExpr(otherVar)(fname))(hydra.lib.logic.ifElse[hydra.ext.java.syntax.Expression](hydra.ext.java.coder.isNonComparableType(ftype))(hydra.ext.java.coder.hashCodeCompareExpr(otherVar)(fname))(hydra.ext.java.coder.comparableCompareExpr(otherVar)(fname)))
}

val cmpNotZeroExpr: hydra.ext.java.syntax.Expression = {
  val lhs: hydra.ext.java.syntax.EqualityExpression = hydra.ext.java.utils.javaRelationalExpressionToJavaEqualityExpression(hydra.ext.java.utils.javaPostfixExpressionToJavaRelationalExpression(hydra.ext.java.syntax.PostfixExpression.name(hydra.ext.java.syntax.ExpressionName(None,
     hydra.ext.java.utils.javaIdentifier("cmp")))))
  val rhs: hydra.ext.java.syntax.RelationalExpression = hydra.ext.java.utils.javaPostfixExpressionToJavaRelationalExpression(hydra.ext.java.syntax.PostfixExpression.primary(hydra.ext.java.utils.javaLiteralToJavaPrimary(hydra.ext.java.utils.javaInt(BigInt(0L)))))
  hydra.ext.java.utils.javaEqualityExpressionToJavaExpression(hydra.ext.java.syntax.EqualityExpression.notEqual(hydra.ext.java.syntax.EqualityExpression_Binary(lhs,
     rhs)))
}

def cmpDeclStatement[T0](aliases: T0): hydra.ext.java.syntax.BlockStatement =
  hydra.ext.java.utils.variableDeclarationStatement(aliases)(hydra.ext.java.utils.javaIntType)(hydra.ext.java.utils.javaIdentifier("cmp"))(hydra.ext.java.utils.javaIntExpression(BigInt(0L)))

def compareAndReturnStmts(otherVar: scala.Predef.String)(f: hydra.core.FieldType): Seq[hydra.ext.java.syntax.BlockStatement] =
  Seq(hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaAssignmentStatement(hydra.ext.java.syntax.LeftHandSide.expressionName(hydra.ext.java.syntax.ExpressionName(None,
     hydra.ext.java.utils.javaIdentifier("cmp"))))(hydra.ext.java.coder.compareFieldExpr(otherVar)(f))),
     hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.syntax.Statement.ifThen(hydra.ext.java.syntax.IfThenStatement(hydra.ext.java.coder.cmpNotZeroExpr,
     hydra.ext.java.utils.javaReturnStatement(Some(hydra.ext.java.utils.javaExpressionNameToJavaExpression(hydra.ext.java.syntax.ExpressionName(None,
     hydra.ext.java.utils.javaIdentifier("cmp")))))))))

def compareToBody[T0](aliases: T0)(otherVar: scala.Predef.String)(fields: Seq[hydra.core.FieldType]): Seq[hydra.ext.java.syntax.BlockStatement] =
  hydra.lib.logic.ifElse[Seq[hydra.ext.java.syntax.BlockStatement]](hydra.lib.lists.`null`[hydra.core.FieldType](fields))(Seq(hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(hydra.ext.java.utils.javaIntExpression(BigInt(0L)))))))(hydra.lib.logic.ifElse[Seq[hydra.ext.java.syntax.BlockStatement]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.FieldType](fields))(1))(Seq(hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(hydra.ext.java.coder.compareFieldExpr(otherVar)(hydra.lib.lists.head[hydra.core.FieldType](fields)))))))(hydra.lib.lists.concat2[hydra.ext.java.syntax.BlockStatement](Seq(hydra.ext.java.coder.cmpDeclStatement(aliases)))(hydra.lib.lists.concat2[hydra.ext.java.syntax.BlockStatement](hydra.lib.lists.concat[hydra.ext.java.syntax.BlockStatement](hydra.lib.lists.map[hydra.core.FieldType,
     Seq[hydra.ext.java.syntax.BlockStatement]]((f: hydra.core.FieldType) => hydra.ext.java.coder.compareAndReturnStmts(otherVar)(f))(hydra.lib.lists.init[hydra.core.FieldType](fields))))(Seq(hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(hydra.ext.java.coder.compareFieldExpr(otherVar)(hydra.lib.lists.last[hydra.core.FieldType](fields))))))))))

val tagCompareExpr: hydra.ext.java.syntax.Expression = {
  val thisGetClass: hydra.ext.java.syntax.MethodInvocation = hydra.ext.java.syntax.MethodInvocation(hydra.ext.java.syntax.MethodInvocation_Header.complex(hydra.ext.java.syntax.MethodInvocation_Complex(hydra.ext.java.syntax.MethodInvocation_Variant.primary(hydra.ext.java.utils.javaExpressionToJavaPrimary(hydra.ext.java.utils.javaThis)),
     Seq(), "getClass")), Seq())
  val thisGetName: hydra.ext.java.syntax.MethodInvocation = hydra.ext.java.syntax.MethodInvocation(hydra.ext.java.syntax.MethodInvocation_Header.complex(hydra.ext.java.syntax.MethodInvocation_Complex(hydra.ext.java.syntax.MethodInvocation_Variant.primary(hydra.ext.java.utils.javaMethodInvocationToJavaPrimary(thisGetClass)),
     Seq(), "getName")), Seq())
  val otherGetClass: hydra.ext.java.syntax.MethodInvocation = hydra.ext.java.syntax.MethodInvocation(hydra.ext.java.syntax.MethodInvocation_Header.complex(hydra.ext.java.syntax.MethodInvocation_Complex(hydra.ext.java.syntax.MethodInvocation_Variant.expression(hydra.ext.java.syntax.ExpressionName(None,
     hydra.ext.java.names.otherInstanceName)), Seq(), "getClass")), Seq())
  val otherGetName: hydra.ext.java.syntax.MethodInvocation = hydra.ext.java.syntax.MethodInvocation(hydra.ext.java.syntax.MethodInvocation_Header.complex(hydra.ext.java.syntax.MethodInvocation_Complex(hydra.ext.java.syntax.MethodInvocation_Variant.primary(hydra.ext.java.utils.javaMethodInvocationToJavaPrimary(otherGetClass)),
     Seq(), "getName")), Seq())
  hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.syntax.MethodInvocation(hydra.ext.java.syntax.MethodInvocation_Header.complex(hydra.ext.java.syntax.MethodInvocation_Complex(hydra.ext.java.syntax.MethodInvocation_Variant.primary(hydra.ext.java.utils.javaMethodInvocationToJavaPrimary(thisGetName)),
     Seq(), hydra.ext.java.names.compareToMethodName)), Seq(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(otherGetName))))
}

val tagCmpNotZeroExpr: hydra.ext.java.syntax.Expression = {
  val lhs: hydra.ext.java.syntax.EqualityExpression = hydra.ext.java.utils.javaRelationalExpressionToJavaEqualityExpression(hydra.ext.java.utils.javaPostfixExpressionToJavaRelationalExpression(hydra.ext.java.syntax.PostfixExpression.name(hydra.ext.java.syntax.ExpressionName(None,
     hydra.ext.java.utils.javaIdentifier("tagCmp")))))
  val rhs: hydra.ext.java.syntax.RelationalExpression = hydra.ext.java.utils.javaPostfixExpressionToJavaRelationalExpression(hydra.ext.java.syntax.PostfixExpression.primary(hydra.ext.java.utils.javaLiteralToJavaPrimary(hydra.ext.java.utils.javaInt(BigInt(0L)))))
  hydra.ext.java.utils.javaEqualityExpressionToJavaExpression(hydra.ext.java.syntax.EqualityExpression.notEqual(hydra.ext.java.syntax.EqualityExpression_Binary(lhs,
     rhs)))
}

def recordCompareToMethod[T0](aliases: hydra.ext.java.helpers.Aliases)(tparams: T0)(elName: hydra.core.Name)(fields: Seq[hydra.core.FieldType]): hydra.ext.java.syntax.ClassBodyDeclaration =
  {
  val anns: Seq[hydra.ext.java.syntax.Annotation] = Seq(hydra.ext.java.utils.overrideAnnotation, hydra.ext.java.utils.suppressWarningsUncheckedAnnotation)
  val mods: Seq[hydra.ext.java.syntax.MethodModifier] = Seq(hydra.ext.java.syntax.MethodModifier.public)
  val param: hydra.ext.java.syntax.FormalParameter = hydra.ext.java.utils.javaTypeToJavaFormalParameter(hydra.ext.java.utils.javaTypeFromTypeName(aliases)(elName))(hydra.ext.java.names.otherInstanceName)
  val result: hydra.ext.java.syntax.Result = hydra.ext.java.utils.javaTypeToJavaResult(hydra.ext.java.utils.javaIntType)
  hydra.ext.java.utils.methodDeclaration(mods)(Seq())(anns)(hydra.ext.java.names.compareToMethodName)(Seq(param))(result)(Some(hydra.ext.java.coder.compareToBody(aliases)(hydra.ext.java.names.otherInstanceName)(fields)))
}

def variantCompareToMethod[T0](aliases: hydra.ext.java.helpers.Aliases)(tparams: T0)(parentName: hydra.core.Name)(variantName: hydra.core.Name)(fields: Seq[hydra.core.FieldType]): hydra.ext.java.syntax.ClassBodyDeclaration =
  {
  val anns: Seq[hydra.ext.java.syntax.Annotation] = Seq(hydra.ext.java.utils.overrideAnnotation, hydra.ext.java.utils.suppressWarningsUncheckedAnnotation)
  val mods: Seq[hydra.ext.java.syntax.MethodModifier] = Seq(hydra.ext.java.syntax.MethodModifier.public)
  val param: hydra.ext.java.syntax.FormalParameter = hydra.ext.java.utils.javaTypeToJavaFormalParameter(hydra.ext.java.utils.javaTypeFromTypeName(aliases)(parentName))(hydra.ext.java.names.otherInstanceName)
  val result: hydra.ext.java.syntax.Result = hydra.ext.java.utils.javaTypeToJavaResult(hydra.ext.java.utils.javaIntType)
  val varTmpName: scala.Predef.String = "o"
  val tagDeclStmt: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.utils.variableDeclarationStatement(aliases)(hydra.ext.java.utils.javaIntType)(hydra.ext.java.utils.javaIdentifier("tagCmp"))(hydra.ext.java.coder.tagCompareExpr)
  val tagReturnStmt: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.syntax.Statement.ifThen(hydra.ext.java.syntax.IfThenStatement(hydra.ext.java.coder.tagCmpNotZeroExpr,
     hydra.ext.java.utils.javaReturnStatement(Some(hydra.ext.java.utils.javaExpressionNameToJavaExpression(hydra.ext.java.syntax.ExpressionName(None,
     hydra.ext.java.utils.javaIdentifier("tagCmp"))))))))
  val variantJavaType: hydra.ext.java.syntax.Type = hydra.ext.java.utils.javaTypeFromTypeName(aliases)(variantName)
  val castOtherExpr: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.javaCastExpression(hydra.ext.java.utils.nameToJavaReferenceType(aliases)(false)(Seq())(variantName)(None))(hydra.ext.java.utils.javaIdentifierToJavaUnaryExpression(hydra.ext.java.names.otherInstanceName)))
  val castDeclStmt: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.utils.variableDeclarationStatement(aliases)(variantJavaType)(hydra.ext.java.utils.javaIdentifier(varTmpName))(castOtherExpr)
  val emptyReturn: Seq[hydra.ext.java.syntax.BlockStatement] = Seq(hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(hydra.ext.java.utils.javaIntExpression(BigInt(0L))))))
  val valueCompareStmt: Seq[hydra.ext.java.syntax.BlockStatement] = hydra.lib.logic.ifElse[Seq[hydra.ext.java.syntax.BlockStatement]](hydra.lib.lists.`null`[hydra.core.FieldType](fields))(emptyReturn)(hydra.lib.lists.concat2[hydra.ext.java.syntax.BlockStatement](Seq(castDeclStmt))(hydra.ext.java.coder.compareToBody(aliases)(varTmpName)(fields)))
  val body: Seq[hydra.ext.java.syntax.BlockStatement] = hydra.lib.lists.concat2[hydra.ext.java.syntax.BlockStatement](Seq(tagDeclStmt,
     tagReturnStmt))(valueCompareStmt)
  hydra.ext.java.utils.methodDeclaration(mods)(Seq())(anns)(hydra.ext.java.names.compareToMethodName)(Seq(param))(result)(Some(body))
}

def recordMemberVar(aliases: hydra.ext.java.helpers.Aliases)(ft: hydra.core.FieldType)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.ClassBodyDeclaration] =
  {
  val mods: Seq[hydra.ext.java.syntax.FieldModifier] = Seq(hydra.ext.java.syntax.FieldModifier.public, hydra.ext.java.syntax.FieldModifier.`final`)
  val fname: hydra.core.Name = (ft.name)
  val ftype: hydra.core.Type = (ft.`type`)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type, hydra.ext.java.syntax.ClassBodyDeclaration](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(ftype)(cx)(g))((jt: hydra.ext.java.syntax.Type) =>
    Right(hydra.ext.java.utils.javaMemberField(mods)(jt)(hydra.ext.java.utils.fieldNameToJavaVariableDeclarator(fname))))
}

def recordWithMethod(aliases: hydra.ext.java.helpers.Aliases)(elName: hydra.core.Name)(fields: Seq[hydra.core.FieldType])(field: hydra.core.FieldType)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.ClassBodyDeclaration] =
  {
  val mods: Seq[hydra.ext.java.syntax.MethodModifier] = Seq(hydra.ext.java.syntax.MethodModifier.public)
  def anns[T0]: Seq[T0] = Seq()
  val methodName: scala.Predef.String = hydra.lib.strings.cat2("with")(hydra.formatting.nonAlnumToUnderscores(hydra.formatting.capitalize(field.name)))
  val result: hydra.ext.java.syntax.Result = hydra.ext.java.utils.referenceTypeToResult(hydra.ext.java.utils.nameToJavaReferenceType(aliases)(false)(Seq())(elName)(None))
  val consId: hydra.ext.java.syntax.Identifier = hydra.ext.java.utils.sanitizeJavaName(hydra.names.localNameOf(elName))
  val fieldArgs: Seq[hydra.ext.java.syntax.Expression] = hydra.lib.lists.map[hydra.core.FieldType, hydra.ext.java.syntax.Expression]((f: hydra.core.FieldType) => hydra.ext.java.utils.fieldNameToJavaExpression(f.name))(fields)
  val returnStmt: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(hydra.ext.java.utils.javaConstructorCall(hydra.ext.java.utils.javaConstructorName(consId)(None))(fieldArgs)(None))))
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.FormalParameter,
     hydra.ext.java.syntax.ClassBodyDeclaration](hydra.ext.java.coder.fieldTypeToFormalParam(aliases)(field)(cx)(g))((param: hydra.ext.java.syntax.FormalParameter) =>
    Right(hydra.ext.java.utils.methodDeclaration(mods)(Seq())(anns)(methodName)(Seq(param))(result)(Some(Seq(returnStmt)))))
}

def recordConstructor(aliases: hydra.ext.java.helpers.Aliases)(elName: hydra.core.Name)(fields: Seq[hydra.core.FieldType])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.ClassBodyDeclaration] =
  {
  val assignStmts: Seq[hydra.ext.java.syntax.BlockStatement] = hydra.lib.lists.map[hydra.core.FieldType,
     hydra.ext.java.syntax.BlockStatement]((f: hydra.core.FieldType) =>
    hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.toAssignStmt(f.name)))(fields)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.FormalParameter],
     hydra.ext.java.syntax.ClassBodyDeclaration](hydra.lib.eithers.mapList[hydra.core.FieldType, hydra.ext.java.syntax.FormalParameter,
     hydra.context.InContext[hydra.error.Error]]((f: hydra.core.FieldType) =>
    hydra.ext.java.coder.fieldTypeToFormalParam(aliases)(f)(cx)(g))(fields))((params: Seq[hydra.ext.java.syntax.FormalParameter]) =>
    Right(hydra.ext.java.utils.makeConstructor(aliases)(elName)(false)(params)(assignStmts)))
}

def eqClause(tmpName: scala.Predef.String)(ft: hydra.core.FieldType): hydra.ext.java.syntax.InclusiveOrExpression =
  {
  val fname: scala.Predef.String = (ft.name)
  val ftype: hydra.core.Type = (ft.`type`)
  hydra.lib.logic.ifElse[hydra.ext.java.syntax.InclusiveOrExpression](hydra.ext.java.coder.isBinaryType(ftype))(hydra.ext.java.coder.arraysEqualsClause(tmpName)(fname))(hydra.lib.logic.ifElse[hydra.ext.java.syntax.InclusiveOrExpression](hydra.ext.java.coder.isBigNumericType(ftype))(hydra.ext.java.coder.compareToZeroClause(tmpName)(fname))(hydra.ext.java.coder.equalsClause(tmpName)(fname)))
}

def equalsClause(tmpName: scala.Predef.String)(fname: scala.Predef.String): hydra.ext.java.syntax.InclusiveOrExpression =
  {
  val thisArg: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaExpressionNameToJavaExpression(hydra.ext.java.utils.fieldExpression("this")(hydra.ext.java.utils.javaIdentifier(fname)))
  val otherArg: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaExpressionNameToJavaExpression(hydra.ext.java.utils.fieldExpression(hydra.ext.java.utils.javaIdentifier(tmpName))(hydra.ext.java.utils.javaIdentifier(fname)))
  val header: hydra.ext.java.syntax.MethodInvocation_Header = hydra.ext.java.syntax.MethodInvocation_Header.complex(hydra.ext.java.syntax.MethodInvocation_Complex(hydra.ext.java.syntax.MethodInvocation_Variant.`type`(hydra.ext.java.utils.javaTypeName("java.util.Objects")),
     Seq(), hydra.ext.java.names.equalsMethodName))
  hydra.ext.java.utils.javaPostfixExpressionToJavaInclusiveOrExpression(hydra.ext.java.utils.javaMethodInvocationToJavaPostfixExpression(hydra.ext.java.syntax.MethodInvocation(header,
     Seq(thisArg, otherArg))))
}

def arraysEqualsClause(tmpName: scala.Predef.String)(fname: scala.Predef.String): hydra.ext.java.syntax.InclusiveOrExpression =
  {
  val thisArg: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaExpressionNameToJavaExpression(hydra.ext.java.utils.fieldExpression("this")(hydra.ext.java.utils.javaIdentifier(fname)))
  val otherArg: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaExpressionNameToJavaExpression(hydra.ext.java.utils.fieldExpression(hydra.ext.java.utils.javaIdentifier(tmpName))(hydra.ext.java.utils.javaIdentifier(fname)))
  val header: hydra.ext.java.syntax.MethodInvocation_Header = hydra.ext.java.syntax.MethodInvocation_Header.complex(hydra.ext.java.syntax.MethodInvocation_Complex(hydra.ext.java.syntax.MethodInvocation_Variant.`type`(hydra.ext.java.utils.javaTypeName("java.util.Arrays")),
     Seq(), hydra.ext.java.names.equalsMethodName))
  hydra.ext.java.utils.javaPostfixExpressionToJavaInclusiveOrExpression(hydra.ext.java.utils.javaMethodInvocationToJavaPostfixExpression(hydra.ext.java.syntax.MethodInvocation(header,
     Seq(thisArg, otherArg))))
}

def compareToZeroClause(tmpName: scala.Predef.String)(fname: scala.Predef.String): hydra.ext.java.syntax.InclusiveOrExpression =
  {
  val compareToArg: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaExpressionNameToJavaExpression(hydra.ext.java.utils.fieldExpression(hydra.ext.java.utils.javaIdentifier(tmpName))(hydra.ext.java.utils.javaIdentifier(fname)))
  val compareToVar: hydra.ext.java.syntax.MethodInvocation_Variant = hydra.ext.java.syntax.MethodInvocation_Variant.expression(hydra.ext.java.utils.fieldExpression("this")(hydra.ext.java.utils.javaIdentifier(fname)))
  val compareToHeader: hydra.ext.java.syntax.MethodInvocation_Header = hydra.ext.java.syntax.MethodInvocation_Header.complex(hydra.ext.java.syntax.MethodInvocation_Complex(compareToVar,
     Seq(), hydra.ext.java.names.compareToMethodName))
  val lhs: hydra.ext.java.syntax.EqualityExpression = hydra.ext.java.utils.javaRelationalExpressionToJavaEqualityExpression(hydra.ext.java.utils.javaPostfixExpressionToJavaRelationalExpression(hydra.ext.java.utils.javaMethodInvocationToJavaPostfixExpression(hydra.ext.java.syntax.MethodInvocation(compareToHeader,
     Seq(compareToArg)))))
  val rhs: hydra.ext.java.syntax.RelationalExpression = hydra.ext.java.utils.javaPostfixExpressionToJavaRelationalExpression(hydra.ext.java.syntax.PostfixExpression.primary(hydra.ext.java.utils.javaLiteralToJavaPrimary(hydra.ext.java.utils.javaInt(BigInt(0L)))))
  hydra.ext.java.utils.javaEqualityExpressionToJavaInclusiveOrExpression(hydra.ext.java.syntax.EqualityExpression.equal(hydra.ext.java.syntax.EqualityExpression_Binary(lhs,
     rhs)))
}

def recordEqualsMethod(aliases: hydra.ext.java.helpers.Aliases)(elName: hydra.core.Name)(fields: Seq[hydra.core.FieldType]): hydra.ext.java.syntax.ClassBodyDeclaration =
  {
  val anns: Seq[hydra.ext.java.syntax.Annotation] = Seq(hydra.ext.java.utils.overrideAnnotation)
  val mods: Seq[hydra.ext.java.syntax.MethodModifier] = Seq(hydra.ext.java.syntax.MethodModifier.public)
  val param: hydra.ext.java.syntax.FormalParameter = hydra.ext.java.utils.javaTypeToJavaFormalParameter(hydra.ext.java.utils.javaRefType(Seq())(None)("Object"))(hydra.ext.java.names.otherInstanceName)
  val result: hydra.ext.java.syntax.Result = hydra.ext.java.utils.javaTypeToJavaResult(hydra.ext.java.utils.javaBooleanType)
  val tmpName: scala.Predef.String = "o"
  val instanceOfStmt: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.syntax.Statement.ifThen(hydra.ext.java.syntax.IfThenStatement(hydra.ext.java.utils.javaUnaryExpressionToJavaExpression(hydra.ext.java.syntax.UnaryExpression.other(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.not(hydra.ext.java.utils.javaRelationalExpressionToJavaUnaryExpression(hydra.ext.java.utils.javaInstanceOf(hydra.ext.java.utils.javaIdentifierToJavaRelationalExpression(hydra.ext.java.utils.javaIdentifier(hydra.ext.java.names.otherInstanceName)))(hydra.ext.java.utils.nameToJavaReferenceType(aliases)(false)(Seq())(elName)(None)))))),
     hydra.ext.java.utils.javaReturnStatement(Some(hydra.ext.java.utils.javaBooleanExpression(false))))))
  val castStmt: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.utils.variableDeclarationStatement(aliases)(hydra.ext.java.utils.javaTypeFromTypeName(aliases)(elName))(hydra.ext.java.utils.javaIdentifier(tmpName))(hydra.ext.java.utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.javaCastExpression(hydra.ext.java.utils.nameToJavaReferenceType(aliases)(false)(Seq())(elName)(None))(hydra.ext.java.utils.javaIdentifierToJavaUnaryExpression(hydra.ext.java.utils.sanitizeJavaName(hydra.ext.java.names.otherInstanceName)))))
  val returnAllFieldsEqual: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(hydra.lib.logic.ifElse[hydra.ext.java.syntax.Expression](hydra.lib.lists.`null`[hydra.core.FieldType](fields))(hydra.ext.java.utils.javaBooleanExpression(true))(hydra.ext.java.utils.javaConditionalAndExpressionToJavaExpression(hydra.lib.lists.map[hydra.core.FieldType,
     hydra.ext.java.syntax.InclusiveOrExpression]((f: hydra.core.FieldType) => hydra.ext.java.coder.eqClause(tmpName)(f))(fields))))))
  hydra.ext.java.utils.methodDeclaration(mods)(Seq())(anns)(hydra.ext.java.names.equalsMethodName)(Seq(param))(result)(Some(Seq(instanceOfStmt,
     castStmt, returnAllFieldsEqual)))
}

def hashCodeMultPair(i: BigInt)(fname: hydra.core.Name): hydra.ext.java.syntax.MultiplicativeExpression =
  {
  val fnameStr: scala.Predef.String = fname
  val lhs: hydra.ext.java.syntax.MultiplicativeExpression = hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.utils.javaPrimaryToJavaUnaryExpression(hydra.ext.java.utils.javaLiteralToJavaPrimary(hydra.ext.java.utils.javaInt(i))))
  val rhs: hydra.ext.java.syntax.UnaryExpression = hydra.ext.java.utils.javaPostfixExpressionToJavaUnaryExpression(hydra.ext.java.utils.javaMethodInvocationToJavaPostfixExpression(hydra.ext.java.syntax.MethodInvocation(hydra.ext.java.syntax.MethodInvocation_Header.complex(hydra.ext.java.syntax.MethodInvocation_Complex(hydra.ext.java.syntax.MethodInvocation_Variant.`type`(hydra.ext.java.utils.javaTypeName("java.util.Objects")),
     Seq(), hydra.ext.java.names.hashCodeMethodName)), Seq(hydra.ext.java.utils.javaExpressionNameToJavaExpression(hydra.ext.java.syntax.ExpressionName(None,
     hydra.ext.java.utils.sanitizeJavaName(fnameStr)))))))
  hydra.ext.java.syntax.MultiplicativeExpression.times(hydra.ext.java.syntax.MultiplicativeExpression_Binary(lhs, rhs))
}

val first20Primes: Seq[BigInt] = Seq(BigInt(2L), BigInt(3L), BigInt(5L), BigInt(7L), BigInt(11L), BigInt(13L),
   BigInt(17L), BigInt(19L), BigInt(23L), BigInt(29L), BigInt(31L), BigInt(37L), BigInt(41L), BigInt(43L),
   BigInt(47L), BigInt(53L), BigInt(59L), BigInt(61L), BigInt(67L), BigInt(71L))

def recordHashCodeMethod(fields: Seq[hydra.core.FieldType]): hydra.ext.java.syntax.ClassBodyDeclaration =
  {
  val anns: Seq[hydra.ext.java.syntax.Annotation] = Seq(hydra.ext.java.utils.overrideAnnotation)
  val mods: Seq[hydra.ext.java.syntax.MethodModifier] = Seq(hydra.ext.java.syntax.MethodModifier.public)
  val result: hydra.ext.java.syntax.Result = hydra.ext.java.utils.javaTypeToJavaResult(hydra.ext.java.utils.javaIntType)
  val returnSum: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.syntax.BlockStatement.statement(hydra.lib.logic.ifElse[hydra.ext.java.syntax.Statement](hydra.lib.lists.`null`[hydra.core.FieldType](fields))(hydra.ext.java.utils.javaReturnStatement(Some(hydra.ext.java.utils.javaIntExpression(BigInt(0L)))))(hydra.ext.java.utils.javaReturnStatement(Some(hydra.ext.java.utils.javaAdditiveExpressionToJavaExpression(hydra.ext.java.utils.addExpressions(hydra.lib.lists.zipWith[BigInt,
     hydra.core.Name, hydra.ext.java.syntax.MultiplicativeExpression](hydra.ext.java.coder.hashCodeMultPair)(hydra.ext.java.coder.first20Primes)(hydra.lib.lists.map[hydra.core.FieldType,
     hydra.core.Name]((f: hydra.core.FieldType) => (f.name))(fields))))))))
  hydra.ext.java.utils.methodDeclaration(mods)(Seq())(anns)(hydra.ext.java.names.hashCodeMethodName)(Seq())(result)(Some(Seq(returnSum)))
}

def constantDecl(javaName: scala.Predef.String)(aliases: hydra.ext.java.helpers.Aliases)(name: hydra.core.Name)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.ClassBodyDeclarationWithComments] =
  {
  val mods: Seq[hydra.ext.java.syntax.FieldModifier] = Seq(hydra.ext.java.syntax.FieldModifier.public,
     hydra.ext.java.syntax.FieldModifier.static, hydra.ext.java.syntax.FieldModifier.`final`)
  val nameName: hydra.ext.java.syntax.Identifier = hydra.ext.java.utils.nameToJavaName(aliases)("hydra.core.Name")
  val env: hydra.ext.java.helpers.JavaEnvironment = hydra.ext.java.helpers.JavaEnvironment(aliases, g)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type, hydra.ext.java.syntax.ClassBodyDeclarationWithComments](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(hydra.core.Type.variable("hydra.core.Name"))(cx)(g))((jt: hydra.ext.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
       hydra.ext.java.syntax.ClassBodyDeclarationWithComments](hydra.ext.java.coder.encodeTerm(env)(hydra.core.Term.literal(hydra.core.Literal.string(name)))(cx)(g))((arg: hydra.ext.java.syntax.Expression) =>
    {
    val init: hydra.ext.java.syntax.VariableInitializer = hydra.ext.java.syntax.VariableInitializer.expression(hydra.ext.java.utils.javaConstructorCall(hydra.ext.java.utils.javaConstructorName(nameName)(None))(Seq(arg))(None))
    {
      val `var`: hydra.ext.java.syntax.VariableDeclarator = hydra.ext.java.utils.javaVariableDeclarator(javaName)(Some(init))
      Right(hydra.ext.java.coder.noComment(hydra.ext.java.utils.javaMemberField(mods)(jt)(`var`)))
    }
  }))
}

def constantDeclForFieldType(aliases: hydra.ext.java.helpers.Aliases)(ftyp: hydra.core.FieldType)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.ClassBodyDeclarationWithComments] =
  {
  val name: hydra.core.Name = (ftyp.name)
  val javaName: scala.Predef.String = hydra.formatting.nonAlnumToUnderscores(hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.upperSnake)(name))
  hydra.ext.java.coder.constantDecl(javaName)(aliases)(name)(cx)(g)
}

def constantDeclForTypeName(aliases: hydra.ext.java.helpers.Aliases)(name: hydra.core.Name)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.ClassBodyDeclarationWithComments] = hydra.ext.java.coder.constantDecl("TYPE_")(aliases)(name)(cx)(g)

def declarationForRecordType(isInner: Boolean)(isSer: Boolean)(aliases: hydra.ext.java.helpers.Aliases)(tparams: Seq[hydra.ext.java.syntax.TypeParameter])(elName: hydra.core.Name)(fields: Seq[hydra.core.FieldType])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.ClassDeclaration] =
  hydra.ext.java.coder.`declarationForRecordType_`(isInner)(isSer)(aliases)(tparams)(elName)(None)(fields)(cx)(g)

def `declarationForRecordType_`(isInner: Boolean)(isSer: Boolean)(aliases: hydra.ext.java.helpers.Aliases)(tparams: Seq[hydra.ext.java.syntax.TypeParameter])(elName: hydra.core.Name)(parentName: Option[hydra.core.Name])(fields: Seq[hydra.core.FieldType])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.ClassDeclaration] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.ClassBodyDeclaration],
     hydra.ext.java.syntax.ClassDeclaration](hydra.lib.eithers.mapList[hydra.core.FieldType, hydra.ext.java.syntax.ClassBodyDeclaration,
     hydra.context.InContext[hydra.error.Error]]((f: hydra.core.FieldType) => hydra.ext.java.coder.recordMemberVar(aliases)(f)(cx)(g))(fields))((memberVars: Seq[hydra.ext.java.syntax.ClassBodyDeclaration]) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments],
     hydra.ext.java.syntax.ClassDeclaration](hydra.lib.eithers.mapList[Tuple2[hydra.ext.java.syntax.ClassBodyDeclaration,
     hydra.core.FieldType], hydra.ext.java.syntax.ClassBodyDeclarationWithComments, hydra.context.InContext[hydra.error.Error]]((p: Tuple2[hydra.ext.java.syntax.ClassBodyDeclaration,
     hydra.core.FieldType]) =>
  hydra.ext.java.coder.addComment(hydra.lib.pairs.first[hydra.ext.java.syntax.ClassBodyDeclaration, hydra.core.FieldType](p))(hydra.lib.pairs.second[hydra.ext.java.syntax.ClassBodyDeclaration,
     hydra.core.FieldType](p))(cx)(g))(hydra.lib.lists.zip[hydra.ext.java.syntax.ClassBodyDeclaration,
     hydra.core.FieldType](memberVars)(fields)))((`memberVars_`: Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments]) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.ClassBodyDeclaration],
     hydra.ext.java.syntax.ClassDeclaration](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
     Seq[hydra.ext.java.syntax.ClassBodyDeclaration]]](hydra.lib.equality.gt[Int](hydra.lib.lists.length[hydra.core.FieldType](fields))(1))(hydra.lib.eithers.mapList[hydra.core.FieldType,
     hydra.ext.java.syntax.ClassBodyDeclaration, hydra.context.InContext[hydra.error.Error]]((f: hydra.core.FieldType) =>
  hydra.ext.java.coder.recordWithMethod(aliases)(elName)(fields)(f)(cx)(g))(fields))(Right(Seq())))((withMethods: Seq[hydra.ext.java.syntax.ClassBodyDeclaration]) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ClassBodyDeclaration,
     hydra.ext.java.syntax.ClassDeclaration](hydra.ext.java.coder.recordConstructor(aliases)(elName)(fields)(cx)(g))((cons: hydra.ext.java.syntax.ClassBodyDeclaration) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments],
     hydra.ext.java.syntax.ClassDeclaration](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
     Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments]]](isInner)(Right(Seq()))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
     hydra.ext.java.syntax.ClassBodyDeclarationWithComments, Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments]](hydra.ext.java.coder.constantDeclForTypeName(aliases)(elName)(cx)(g))((d: hydra.ext.java.syntax.ClassBodyDeclarationWithComments) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments],
     Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments]](hydra.lib.eithers.mapList[hydra.core.FieldType,
     hydra.ext.java.syntax.ClassBodyDeclarationWithComments, hydra.context.InContext[hydra.error.Error]]((f: hydra.core.FieldType) =>
  hydra.ext.java.coder.constantDeclForFieldType(aliases)(f)(cx)(g))(fields))((dfields: Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments]) =>
  Right(hydra.lib.lists.cons[hydra.ext.java.syntax.ClassBodyDeclarationWithComments](d)(dfields))))))((tn: Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments]) =>
  {
  val comparableMethods: Seq[hydra.ext.java.syntax.ClassBodyDeclaration] = hydra.lib.maybes.cases[hydra.core.Name,
     Seq[hydra.ext.java.syntax.ClassBodyDeclaration]](parentName)(hydra.lib.logic.ifElse[Seq[hydra.ext.java.syntax.ClassBodyDeclaration]](hydra.lib.logic.and(hydra.lib.logic.not(isInner))(isSer))(Seq(hydra.ext.java.coder.recordCompareToMethod(aliases)(tparams)(elName)(fields)))(Seq()))((pn: hydra.core.Name) =>
    hydra.lib.logic.ifElse[Seq[hydra.ext.java.syntax.ClassBodyDeclaration]](isSer)(Seq(hydra.ext.java.coder.variantCompareToMethod(aliases)(tparams)(pn)(elName)(fields)))(Seq()))
  {
    val bodyDecls: Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments] = hydra.lib.lists.concat2[hydra.ext.java.syntax.ClassBodyDeclarationWithComments](tn)(hydra.lib.lists.concat2[hydra.ext.java.syntax.ClassBodyDeclarationWithComments](`memberVars_`)(hydra.lib.lists.map[hydra.ext.java.syntax.ClassBodyDeclaration,
       hydra.ext.java.syntax.ClassBodyDeclarationWithComments]((x: hydra.ext.java.syntax.ClassBodyDeclaration) => hydra.ext.java.coder.noComment(x))(hydra.lib.lists.concat2[hydra.ext.java.syntax.ClassBodyDeclaration](Seq(cons,
       hydra.ext.java.coder.recordEqualsMethod(aliases)(elName)(fields), hydra.ext.java.coder.recordHashCodeMethod(fields)))(hydra.lib.lists.concat2[hydra.ext.java.syntax.ClassBodyDeclaration](comparableMethods)(withMethods)))))
    {
      val ifaces: Seq[hydra.ext.java.syntax.InterfaceType] = hydra.lib.logic.ifElse[Seq[hydra.ext.java.syntax.InterfaceType]](isInner)(hydra.ext.java.coder.serializableTypes(isSer))(hydra.ext.java.coder.interfaceTypes(isSer)(aliases)(tparams)(elName))
      Right(hydra.ext.java.utils.javaClassDeclaration(aliases)(tparams)(elName)(hydra.ext.java.coder.classModsPublic)(None)(ifaces)(bodyDecls))
    }
  }
})))))

def takeTypeArgs[T0](label: scala.Predef.String)(n: Int)(tyapps: Seq[hydra.ext.java.syntax.Type])(cx: hydra.context.Context)(g: T0): Either[hydra.context.InContext[hydra.error.Error],
   Seq[hydra.ext.java.syntax.TypeArgument]] =
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.TypeArgument]]](hydra.lib.equality.lt[Int](hydra.lib.lists.length[hydra.ext.java.syntax.Type](tyapps))(n))(Left(hydra.context.InContext(hydra.error.Error.other(hydra.lib.strings.cat(Seq("needed type arguments for ",
     label, ", found too few"))), cx)))(hydra.lib.eithers.mapList[hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeArgument,
     hydra.context.InContext[hydra.error.Error]]((jt: hydra.ext.java.syntax.Type) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
     hydra.ext.java.syntax.TypeArgument](hydra.ext.java.utils.javaTypeToJavaReferenceType(jt)(cx))((rt: hydra.ext.java.syntax.ReferenceType) => Right(hydra.ext.java.syntax.TypeArgument.reference(rt))))(hydra.lib.lists.take[hydra.ext.java.syntax.Type](n)(tyapps)))

def isFieldUnitType[T0, T1](typeName: hydra.core.Name)(fieldName: hydra.core.Name)(cx: T0)(g: hydra.graph.Graph): Either[T1, Boolean] =
  {
  val schemaTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = (g.schemaTypes)
  hydra.lib.maybes.cases[hydra.core.TypeScheme, Either[T1, Boolean]](hydra.lib.maps.lookup[hydra.core.Name,
     hydra.core.TypeScheme](typeName)(schemaTypes))(Right(false))((ts: hydra.core.TypeScheme) =>
    hydra.rewriting.deannotateType(ts.`type`) match
    case hydra.core.Type.union(v_Type_union_rt) => Right(hydra.lib.maybes.cases[hydra.core.FieldType,
       Boolean](hydra.lib.lists.find[hydra.core.FieldType]((ft: hydra.core.FieldType) =>
      hydra.lib.equality.equal[hydra.core.Name](ft.name)(fieldName))(v_Type_union_rt))(false)((ft: hydra.core.FieldType) =>
      hydra.schemas.isUnitType(hydra.rewriting.deannotateType(ft.`type`))))
    case _ => Right(false))
}

def encodeTerm(env: hydra.ext.java.helpers.JavaEnvironment)(term: hydra.core.Term)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.Expression] = hydra.ext.java.coder.encodeTermInternal(env)(Seq())(Seq())(term)(cx)(g)

def encodeTermInternal(env: hydra.ext.java.helpers.JavaEnvironment)(anns: Seq[Map[hydra.core.Name, hydra.core.Term]])(tyapps: Seq[hydra.ext.java.syntax.Type])(term: hydra.core.Term)(cx: hydra.context.Context)(g0: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.Expression] =
  {
  val aliases: hydra.ext.java.helpers.Aliases = (env.aliases)
  val g: hydra.graph.Graph = (env.graph)
  def encode(t: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression] = hydra.ext.java.coder.encodeTerm(env)(t)(cx)(g)
  term match
    case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.ext.java.coder.encodeTermInternal(env)(hydra.lib.lists.cons[Map[hydra.core.Name,
       hydra.core.Term]](v_Term_annotated_at.annotation)(anns))(tyapps)(v_Term_annotated_at.body)(cx)(g)
    case hydra.core.Term.application(v_Term_application_app) => hydra.ext.java.coder.encodeApplication(env)(v_Term_application_app)(cx)(g)
    case hydra.core.Term.either(v_Term_either_et) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       Seq[hydra.ext.java.syntax.TypeArgument], hydra.ext.java.syntax.Expression](hydra.ext.java.coder.takeTypeArgs("either")(2)(tyapps)(cx)(g))((targs: Seq[hydra.ext.java.syntax.TypeArgument]) =>
      {
      val combinedAnns: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.lists.foldl[Map[hydra.core.Name,
         hydra.core.Term], Map[hydra.core.Name, hydra.core.Term]]((acc: Map[hydra.core.Name, hydra.core.Term]) =>
        (m: Map[hydra.core.Name, hydra.core.Term]) =>
        hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](acc)(m))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term])(anns)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type], hydra.ext.java.syntax.Expression](hydra.lib.eithers.bimap[hydra.error.DecodingError,
         Option[hydra.core.Type], hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type]]((__de: hydra.error.DecodingError) => hydra.context.InContext(hydra.error.Error.other(__de),
         cx))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(combinedAnns)))((mEitherType: Option[hydra.core.Type]) =>
        {
        val branchTypes: Option[Tuple2[hydra.core.Type, hydra.core.Type]] = hydra.lib.maybes.bind[hydra.core.Type,
           Tuple2[hydra.core.Type, hydra.core.Type]](mEitherType)((etyp: hydra.core.Type) =>
          hydra.rewriting.deannotateType(etyp) match
          case hydra.core.Type.either(v_Type_either_et2) => Some(Tuple2(v_Type_either_et2.left, (v_Type_either_et2.right)))
          case _ => None)
        {
          def encodeWithType(branchType: hydra.core.Type)(t1: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error],
             hydra.ext.java.syntax.Expression] =
            {
            val annotated: hydra.core.Term = hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(branchType)))(t1)
            hydra.ext.java.coder.encodeTermInternal(env)(anns)(Seq())(annotated)(cx)(g)
          }
          hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, Either[hydra.context.InContext[hydra.error.Error],
             hydra.ext.java.syntax.Expression]]((term1: hydra.core.Term) =>
            hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
               hydra.ext.java.syntax.Expression](hydra.lib.maybes.cases[Tuple2[hydra.core.Type, hydra.core.Type],
               Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](branchTypes)(encode(term1))((bt: Tuple2[hydra.core.Type,
               hydra.core.Type]) =>
            encodeWithType(hydra.lib.pairs.first[hydra.core.Type, hydra.core.Type](bt))(term1)))((expr: hydra.ext.java.syntax.Expression) =>
            Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStaticWithTypeArgs("hydra.util.Either")("left")(targs)(Seq(expr))))))((term1: hydra.core.Term) =>
            hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
               hydra.ext.java.syntax.Expression](hydra.lib.maybes.cases[Tuple2[hydra.core.Type, hydra.core.Type],
               Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](branchTypes)(encode(term1))((bt: Tuple2[hydra.core.Type,
               hydra.core.Type]) =>
            encodeWithType(hydra.lib.pairs.second[hydra.core.Type, hydra.core.Type](bt))(term1)))((expr: hydra.ext.java.syntax.Expression) =>
            Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStaticWithTypeArgs("hydra.util.Either")("right")(targs)(Seq(expr))))))(v_Term_either_et)
        }
      })
    })
    case hydra.core.Term.function(v_Term_function_f) => {
      val combinedAnns: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.lists.foldl[Map[hydra.core.Name,
         hydra.core.Term], Map[hydra.core.Name, hydra.core.Term]]((acc: Map[hydra.core.Name, hydra.core.Term]) =>
        (m: Map[hydra.core.Name, hydra.core.Term]) =>
        hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](acc)(m))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term])(anns)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type], hydra.ext.java.syntax.Expression](hydra.lib.eithers.bimap[hydra.error.DecodingError,
         Option[hydra.core.Type], hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type]]((__de: hydra.error.DecodingError) => hydra.context.InContext(hydra.error.Error.other(__de),
         cx))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(combinedAnns)))((mt: Option[hydra.core.Type]) =>
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, hydra.ext.java.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Type,
           Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type]](mt)(hydra.lib.maybes.cases[hydra.core.Type,
           Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type]](hydra.ext.java.coder.tryInferFunctionType(v_Term_function_f))(hydra.coderUtils.typeOfTerm(cx)(g)(term))((inferredType: hydra.core.Type) => Right(inferredType)))((t: hydra.core.Type) => Right(t)))((typ: hydra.core.Type) =>
        hydra.rewriting.deannotateType(typ) match
        case hydra.core.Type.function(v_Type_function_ft) => hydra.ext.java.coder.encodeFunction(env)(v_Type_function_ft.domain)(v_Type_function_ft.codomain)(v_Term_function_f)(cx)(g)
        case _ => hydra.ext.java.coder.encodeNullaryConstant(env)(typ)(v_Term_function_f)(cx)(g)))
    }
    case hydra.core.Term.let(v_Term_let_lt) => {
      val bindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
      {
        val body: hydra.core.Term = (v_Term_let_lt.body)
        hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](hydra.lib.lists.`null`[hydra.core.Binding](bindings))(hydra.ext.java.coder.encodeTermInternal(env)(anns)(Seq())(body)(cx)(g))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
           Tuple2[Seq[hydra.ext.java.syntax.BlockStatement], hydra.ext.java.helpers.JavaEnvironment],
           hydra.ext.java.syntax.Expression](hydra.ext.java.coder.bindingsToStatements(env)(bindings)(cx)(g))((bindResult: Tuple2[Seq[hydra.ext.java.syntax.BlockStatement],
           hydra.ext.java.helpers.JavaEnvironment]) =>
          {
          val bindingStmts: Seq[hydra.ext.java.syntax.BlockStatement] = hydra.lib.pairs.first[Seq[hydra.ext.java.syntax.BlockStatement],
             hydra.ext.java.helpers.JavaEnvironment](bindResult)
          {
            val env2: hydra.ext.java.helpers.JavaEnvironment = hydra.lib.pairs.second[Seq[hydra.ext.java.syntax.BlockStatement],
               hydra.ext.java.helpers.JavaEnvironment](bindResult)
            hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
               hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeTermInternal(env2)(anns)(Seq())(body)(cx)(g))((jbody: hydra.ext.java.syntax.Expression) =>
              {
              val returnSt: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(jbody)))
              {
                val block: hydra.ext.java.syntax.Block = hydra.lib.lists.concat2[hydra.ext.java.syntax.BlockStatement](bindingStmts)(Seq(returnSt))
                {
                  val nullaryLambda: hydra.ext.java.syntax.Expression = hydra.ext.java.syntax.Expression.lambda(hydra.ext.java.syntax.LambdaExpression(hydra.ext.java.syntax.LambdaParameters.tuple(Seq()),
                     hydra.ext.java.syntax.LambdaBody.block(block)))
                  {
                    val combinedAnns: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.lists.foldl[Map[hydra.core.Name,
                       hydra.core.Term], Map[hydra.core.Name, hydra.core.Term]]((acc: Map[hydra.core.Name,
                       hydra.core.Term]) =>
                      (m: Map[hydra.core.Name, hydra.core.Term]) =>
                      hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](acc)(m))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term])(anns)
                    {
                      val g2: hydra.graph.Graph = (env2.graph)
                      {
                        val aliases2: hydra.ext.java.helpers.Aliases = (env2.aliases)
                        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type],
                           hydra.ext.java.syntax.Expression](hydra.lib.eithers.bimap[hydra.error.DecodingError,
                           Option[hydra.core.Type], hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type]]((__de: hydra.error.DecodingError) => hydra.context.InContext(hydra.error.Error.other(__de),
                           cx))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(combinedAnns)))((mt: Option[hydra.core.Type]) =>
                          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type,
                             hydra.ext.java.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Type,
                             Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type]](mt)(hydra.coderUtils.typeOfTerm(cx)(g2)(body))((t: hydra.core.Type) => Right(t)))((letType: hydra.core.Type) =>
                          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type,
                             hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeType(aliases2)(hydra.lib.sets.empty[hydra.core.Name])(letType)(cx)(g))((jLetType: hydra.ext.java.syntax.Type) =>
                          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
                             hydra.ext.java.syntax.Expression](hydra.ext.java.utils.javaTypeToJavaReferenceType(jLetType)(cx))((rt: hydra.ext.java.syntax.ReferenceType) =>
                          {
                          val supplierRt: hydra.ext.java.syntax.ReferenceType = hydra.ext.java.syntax.ReferenceType.classOrInterface(hydra.ext.java.syntax.ClassOrInterfaceType.`class`(hydra.ext.java.utils.javaClassType(Seq(rt))(hydra.ext.java.names.javaUtilFunctionPackageName)("Supplier")))
                          {
                            val castExpr: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.javaCastExpression(supplierRt)(hydra.ext.java.utils.javaExpressionToJavaUnaryExpression(nullaryLambda)))
                            Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocation(Some(Right(hydra.ext.java.utils.javaExpressionToJavaPrimary(castExpr))))("get")(Seq())))
                          }
                        }))))
                      }
                    }
                  }
                }
              }
            })
          }
        }))
      }
    }
    case hydra.core.Term.list(v_Term_list_els) => hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.Expression]](hydra.lib.lists.`null`[hydra.core.Term](v_Term_list_els))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       Seq[hydra.ext.java.syntax.TypeArgument], hydra.ext.java.syntax.Expression](hydra.ext.java.coder.takeTypeArgs("list")(1)(tyapps)(cx)(g))((targs: Seq[hydra.ext.java.syntax.TypeArgument]) =>
      Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStaticWithTypeArgs("hydra.util.ConsList")("empty")(targs)(Seq())))))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
         Seq[hydra.ext.java.syntax.Expression], hydra.ext.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term,
         hydra.ext.java.syntax.Expression, hydra.context.InContext[hydra.error.Error]](encode)(v_Term_list_els))((jels: Seq[hydra.ext.java.syntax.Expression]) =>
      Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStatic("hydra.util.ConsList")("of")(jels)))))
    case hydra.core.Term.literal(v_Term_literal_l) => Right(hydra.ext.java.coder.encodeLiteral(v_Term_literal_l))
    case hydra.core.Term.map(v_Term_map_m) => hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.Expression]](hydra.lib.maps.`null`[hydra.core.Term, hydra.core.Term](v_Term_map_m))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       Seq[hydra.ext.java.syntax.TypeArgument], hydra.ext.java.syntax.Expression](hydra.ext.java.coder.takeTypeArgs("map")(2)(tyapps)(cx)(g))((targs: Seq[hydra.ext.java.syntax.TypeArgument]) =>
      Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStaticWithTypeArgs("hydra.util.PersistentMap")("empty")(targs)(Seq())))))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
         Seq[hydra.ext.java.syntax.Expression], hydra.ext.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term,
         hydra.ext.java.syntax.Expression, hydra.context.InContext[hydra.error.Error]](encode)(hydra.lib.maps.keys[hydra.core.Term,
         hydra.core.Term](v_Term_map_m)))((jkeys: Seq[hydra.ext.java.syntax.Expression]) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.Expression],
         hydra.ext.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term, hydra.ext.java.syntax.Expression,
         hydra.context.InContext[hydra.error.Error]](encode)(hydra.lib.maps.elems[hydra.core.Term, hydra.core.Term](v_Term_map_m)))((jvals: Seq[hydra.ext.java.syntax.Expression]) =>
      {
      val pairExprs: Seq[hydra.ext.java.syntax.Expression] = hydra.lib.lists.map[Tuple2[hydra.ext.java.syntax.Expression,
         hydra.ext.java.syntax.Expression], hydra.ext.java.syntax.Expression]((kv: Tuple2[hydra.ext.java.syntax.Expression,
         hydra.ext.java.syntax.Expression]) =>
        hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStatic("hydra.util.PersistentMap")("entry")(Seq(hydra.lib.pairs.first[hydra.ext.java.syntax.Expression,
           hydra.ext.java.syntax.Expression](kv), hydra.lib.pairs.second[hydra.ext.java.syntax.Expression,
           hydra.ext.java.syntax.Expression](kv)))))(hydra.lib.lists.zip[hydra.ext.java.syntax.Expression,
           hydra.ext.java.syntax.Expression](jkeys)(jvals))
      Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStatic("hydra.util.PersistentMap")("ofEntries")(pairExprs)))
    })))
    case hydra.core.Term.maybe(v_Term_maybe_mt) => hydra.lib.maybes.cases[hydra.core.Term, Either[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.Expression]](v_Term_maybe_mt)(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       Seq[hydra.ext.java.syntax.TypeArgument], hydra.ext.java.syntax.Expression](hydra.ext.java.coder.takeTypeArgs("maybe")(1)(tyapps)(cx)(g))((targs: Seq[hydra.ext.java.syntax.TypeArgument]) =>
      Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStaticWithTypeArgs("hydra.util.Maybe")("nothing")(targs)(Seq())))))((term1: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
         hydra.ext.java.syntax.Expression](encode(term1))((expr: hydra.ext.java.syntax.Expression) =>
      Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStatic("hydra.util.Maybe")("just")(Seq(expr))))))
    case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression](encode(hydra.lib.pairs.first[hydra.core.Term,
       hydra.core.Term](v_Term_pair_p)))((jterm1: hydra.ext.java.syntax.Expression) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
         hydra.ext.java.syntax.Expression](encode(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p)))((jterm2: hydra.ext.java.syntax.Expression) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.ext.java.syntax.TypeArgumentsOrDiamond],
         hydra.ext.java.syntax.Expression](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
         Option[hydra.ext.java.syntax.TypeArgumentsOrDiamond]]](hydra.lib.lists.`null`[hydra.ext.java.syntax.Type](tyapps))(Right(None))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
         Seq[hydra.ext.java.syntax.ReferenceType], Option[hydra.ext.java.syntax.TypeArgumentsOrDiamond]](hydra.lib.eithers.mapList[hydra.ext.java.syntax.Type,
         hydra.ext.java.syntax.ReferenceType, hydra.context.InContext[hydra.error.Error]]((jt: hydra.ext.java.syntax.Type) => hydra.ext.java.utils.javaTypeToJavaReferenceType(jt)(cx))(tyapps))((rts: Seq[hydra.ext.java.syntax.ReferenceType]) =>
      Right(Some(hydra.ext.java.syntax.TypeArgumentsOrDiamond.arguments(hydra.lib.lists.map[hydra.ext.java.syntax.ReferenceType,
         hydra.ext.java.syntax.TypeArgument]((rt: hydra.ext.java.syntax.ReferenceType) => hydra.ext.java.syntax.TypeArgument.reference(rt))(rts)))))))((mtargs: Option[hydra.ext.java.syntax.TypeArgumentsOrDiamond]) =>
      Right(hydra.ext.java.utils.javaConstructorCall(hydra.ext.java.utils.javaConstructorName("hydra.util.Pair")(mtargs))(Seq(jterm1, jterm2))(None)))))
    case hydra.core.Term.record(v_Term_record_rec) => {
      val recName: hydra.core.Name = (v_Term_record_rec.typeName)
      {
        val mRecordType: Option[hydra.core.Type] = hydra.lib.eithers.either[hydra.context.InContext[hydra.error.Error],
           hydra.core.Type, Option[hydra.core.Type]]((_x: hydra.context.InContext[hydra.error.Error]) => None)((t: hydra.core.Type) => Some(t))(hydra.schemas.requireType(cx)(g)(recName))
        {
          val strippedRecTyp: Option[hydra.core.Type] = hydra.lib.maybes.map[hydra.core.Type, hydra.core.Type]((recTyp: hydra.core.Type) =>
            hydra.ext.java.coder.stripForalls(hydra.rewriting.deannotateType(recTyp)))(mRecordType)
          {
            val mFieldTypeMap: Option[Map[hydra.core.Name, hydra.core.Type]] = hydra.lib.maybes.bind[hydra.core.Type,
               Map[hydra.core.Name, hydra.core.Type]](strippedRecTyp)((bodyTyp: hydra.core.Type) =>
              bodyTyp match
              case hydra.core.Type.record(v_Type_record_rt) => Some(hydra.lib.maps.fromList[hydra.core.Name,
                 hydra.core.Type](hydra.lib.lists.map[hydra.core.FieldType, Tuple2[hydra.core.Name, hydra.core.Type]]((ft: hydra.core.FieldType) => Tuple2(ft.name,
                 (ft.`type`)))(v_Type_record_rt)))
              case _ => None)
            {
              val combinedAnnsRec: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.lists.foldl[Map[hydra.core.Name,
                 hydra.core.Term], Map[hydra.core.Name, hydra.core.Term]]((acc: Map[hydra.core.Name, hydra.core.Term]) =>
                (m: Map[hydra.core.Name, hydra.core.Term]) =>
                hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](acc)(m))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term])(anns)
              hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type],
                 hydra.ext.java.syntax.Expression](hydra.lib.eithers.bimap[hydra.error.DecodingError,
                 Option[hydra.core.Type], hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type]]((__de: hydra.error.DecodingError) => hydra.context.InContext(hydra.error.Error.other(__de),
                 cx))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(combinedAnnsRec)))((mAnnotType: Option[hydra.core.Type]) =>
                {
                val mTypeSubst: Option[Map[hydra.core.Name, hydra.core.Type]] = hydra.lib.maybes.bind[hydra.core.Type,
                   Map[hydra.core.Name, hydra.core.Type]](mAnnotType)((annTyp: hydra.core.Type) =>
                  hydra.lib.maybes.bind[hydra.core.Type, Map[hydra.core.Name, hydra.core.Type]](mRecordType)((recTyp: hydra.core.Type) =>
                  {
                  val args: Seq[hydra.core.Type] = hydra.ext.java.coder.extractTypeApplicationArgs(hydra.rewriting.deannotateType(annTyp))
                  {
                    val params: Seq[hydra.core.Name] = hydra.ext.java.coder.collectForallParams(hydra.rewriting.deannotateType(recTyp))
                    hydra.lib.logic.ifElse[Option[Map[hydra.core.Name, hydra.core.Type]]](hydra.lib.logic.or(hydra.lib.lists.`null`[hydra.core.Type](args))(hydra.lib.logic.not(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Type](args))(hydra.lib.lists.length[hydra.core.Name](params)))))(None)(Some(hydra.lib.maps.fromList[hydra.core.Name,
                       hydra.core.Type](hydra.lib.lists.zip[hydra.core.Name, hydra.core.Type](params)(args))))
                  }
                }))
                {
                  def encodeField(fld: hydra.core.Field): Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression] =
                    hydra.lib.maybes.cases[Map[hydra.core.Name, hydra.core.Type], Either[hydra.context.InContext[hydra.error.Error],
                       hydra.ext.java.syntax.Expression]](mFieldTypeMap)(encode(fld.term))((ftmap: Map[hydra.core.Name,
                       hydra.core.Type]) =>
                    {
                    val mftyp: Option[hydra.core.Type] = hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Type](fld.name)(ftmap)
                    hydra.lib.maybes.cases[hydra.core.Type, Either[hydra.context.InContext[hydra.error.Error],
                       hydra.ext.java.syntax.Expression]](mftyp)(encode(fld.term))((ftyp: hydra.core.Type) =>
                      {
                      val resolvedType: hydra.core.Type = hydra.lib.maybes.cases[Map[hydra.core.Name,
                         hydra.core.Type], hydra.core.Type](mTypeSubst)(ftyp)((subst: Map[hydra.core.Name,
                         hydra.core.Type]) => hydra.ext.java.coder.applySubstFull(subst)(ftyp))
                      {
                        val annotatedFieldTerm: hydra.core.Term = hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(resolvedType)))(fld.term)
                        hydra.ext.java.coder.encodeTermInternal(env)(anns)(Seq())(annotatedFieldTerm)(cx)(g)
                      }
                    })
                  })
                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.Expression],
                     hydra.ext.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Field, hydra.ext.java.syntax.Expression,
                     hydra.context.InContext[hydra.error.Error]](encodeField)(v_Term_record_rec.fields))((fieldExprs: Seq[hydra.ext.java.syntax.Expression]) =>
                    {
                    val consId: hydra.ext.java.syntax.Identifier = hydra.ext.java.utils.nameToJavaName(aliases)(recName)
                    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.ext.java.syntax.TypeArgumentsOrDiamond],
                       hydra.ext.java.syntax.Expression](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
                       Option[hydra.ext.java.syntax.TypeArgumentsOrDiamond]]](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.ext.java.syntax.Type](tyapps)))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                       Seq[hydra.ext.java.syntax.ReferenceType], Option[hydra.ext.java.syntax.TypeArgumentsOrDiamond]](hydra.lib.eithers.mapList[hydra.ext.java.syntax.Type,
                       hydra.ext.java.syntax.ReferenceType, hydra.context.InContext[hydra.error.Error]]((jt: hydra.ext.java.syntax.Type) => hydra.ext.java.utils.javaTypeToJavaReferenceType(jt)(cx))(tyapps))((rts: Seq[hydra.ext.java.syntax.ReferenceType]) =>
                      Right(Some(hydra.ext.java.syntax.TypeArgumentsOrDiamond.arguments(hydra.lib.lists.map[hydra.ext.java.syntax.ReferenceType,
                         hydra.ext.java.syntax.TypeArgument]((rt: hydra.ext.java.syntax.ReferenceType) => hydra.ext.java.syntax.TypeArgument.reference(rt))(rts))))))({
                      val combinedAnns: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.lists.foldl[Map[hydra.core.Name,
                         hydra.core.Term], Map[hydra.core.Name, hydra.core.Term]]((acc: Map[hydra.core.Name,
                         hydra.core.Term]) =>
                        (m: Map[hydra.core.Name, hydra.core.Term]) =>
                        hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](acc)(m))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term])(anns)
                      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type],
                         Option[hydra.ext.java.syntax.TypeArgumentsOrDiamond]](hydra.lib.eithers.bimap[hydra.error.DecodingError,
                         Option[hydra.core.Type], hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type]]((__de: hydra.error.DecodingError) => hydra.context.InContext(hydra.error.Error.other(__de),
                         cx))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(combinedAnns)))((mtyp: Option[hydra.core.Type]) =>
                        hydra.lib.maybes.cases[hydra.core.Type, Either[hydra.context.InContext[hydra.error.Error],
                           Option[hydra.ext.java.syntax.TypeArgumentsOrDiamond]]](mtyp)(Right(None))((annTyp: hydra.core.Type) =>
                        {
                        val typeArgs: Seq[hydra.core.Type] = hydra.ext.java.coder.extractTypeApplicationArgs(hydra.rewriting.deannotateType(annTyp))
                        hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Option[hydra.ext.java.syntax.TypeArgumentsOrDiamond]]](hydra.lib.lists.`null`[hydra.core.Type](typeArgs))(Right(None))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                           Seq[hydra.ext.java.syntax.ReferenceType], Option[hydra.ext.java.syntax.TypeArgumentsOrDiamond]](hydra.lib.eithers.mapList[hydra.core.Type,
                           hydra.ext.java.syntax.ReferenceType, hydra.context.InContext[hydra.error.Error]]((t: hydra.core.Type) =>
                          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type,
                             hydra.ext.java.syntax.ReferenceType](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(t)(cx)(g))((jt: hydra.ext.java.syntax.Type) => hydra.ext.java.utils.javaTypeToJavaReferenceType(jt)(cx)))(typeArgs))((jTypeArgs: Seq[hydra.ext.java.syntax.ReferenceType]) =>
                          Right(Some(hydra.ext.java.syntax.TypeArgumentsOrDiamond.arguments(hydra.lib.lists.map[hydra.ext.java.syntax.ReferenceType,
                             hydra.ext.java.syntax.TypeArgument]((rt: hydra.ext.java.syntax.ReferenceType) => hydra.ext.java.syntax.TypeArgument.reference(rt))(jTypeArgs))))))
                      }))
                    }))((mtargs: Option[hydra.ext.java.syntax.TypeArgumentsOrDiamond]) =>
                      Right(hydra.ext.java.utils.javaConstructorCall(hydra.ext.java.utils.javaConstructorName(consId)(mtargs))(fieldExprs)(None)))
                  })
                }
              })
            }
          }
        }
      }
    }
    case hydra.core.Term.set(v_Term_set_s) => hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.Expression]](hydra.lib.sets.`null`[hydra.core.Term](v_Term_set_s))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       Seq[hydra.ext.java.syntax.TypeArgument], hydra.ext.java.syntax.Expression](hydra.ext.java.coder.takeTypeArgs("set")(1)(tyapps)(cx)(g))((targs: Seq[hydra.ext.java.syntax.TypeArgument]) =>
      Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStaticWithTypeArgs("hydra.util.PersistentSet")("empty")(targs)(Seq())))))({
      val slist: Seq[hydra.core.Term] = hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.Expression],
         hydra.ext.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term, hydra.ext.java.syntax.Expression,
         hydra.context.InContext[hydra.error.Error]](encode)(slist))((jels: Seq[hydra.ext.java.syntax.Expression]) =>
        Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStatic("hydra.util.PersistentSet")("of")(jels))))
    })
    case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.ext.java.coder.withTypeLambda(env)(v_Term_typeLambda_tl)((env2: hydra.ext.java.helpers.JavaEnvironment) =>
      {
      val combinedAnns: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.lists.foldl[Map[hydra.core.Name,
         hydra.core.Term], Map[hydra.core.Name, hydra.core.Term]]((acc: Map[hydra.core.Name, hydra.core.Term]) =>
        (m: Map[hydra.core.Name, hydra.core.Term]) =>
        hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](acc)(m))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term])(anns)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type], hydra.ext.java.syntax.Expression](hydra.lib.eithers.bimap[hydra.error.DecodingError,
         Option[hydra.core.Type], hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type]]((__de: hydra.error.DecodingError) => hydra.context.InContext(hydra.error.Error.other(__de),
         cx))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(combinedAnns)))((mtyp: Option[hydra.core.Type]) =>
        {
        val annotatedBody: hydra.core.Term = hydra.lib.maybes.cases[hydra.core.Type, hydra.core.Term](mtyp)(v_Term_typeLambda_tl.body)((t: hydra.core.Type) =>
          t match
          case hydra.core.Type.forall(v_Type_forall_fa) => hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(v_Type_forall_fa.body)))(v_Term_typeLambda_tl.body)
          case _ => (v_Term_typeLambda_tl.body))
        hydra.ext.java.coder.encodeTerm(env2)(annotatedBody)(cx)(g)
      })
    })
    case hydra.core.Term.union(v_Term_union_inj) => {
      val injTypeName: hydra.core.Name = (v_Term_union_inj.typeName)
      {
        val injField: hydra.core.Field = (v_Term_union_inj.field)
        {
          val injFieldName: hydra.core.Name = (injField.name)
          {
            val injFieldTerm: hydra.core.Term = (injField.term)
            {
              val typeId: scala.Predef.String = hydra.ext.java.utils.nameToJavaName(aliases)(injTypeName)
              {
                val consId: hydra.ext.java.syntax.Identifier = hydra.lib.strings.cat(Seq(typeId, ".",
                   hydra.ext.java.utils.sanitizeJavaName(hydra.formatting.capitalize(injFieldName))))
                hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Boolean, hydra.ext.java.syntax.Expression](hydra.ext.java.coder.isFieldUnitType(injTypeName)(injFieldName)(cx)(g))((fieldIsUnit: Boolean) =>
                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.Expression],
                     hydra.ext.java.syntax.Expression](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
                     Seq[hydra.ext.java.syntax.Expression]]](hydra.lib.logic.or(hydra.schemas.isUnitTerm(hydra.rewriting.deannotateTerm(injFieldTerm)))(fieldIsUnit))(Right(Seq()))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                     hydra.ext.java.syntax.Expression, Seq[hydra.ext.java.syntax.Expression]](encode(injFieldTerm))((ex: hydra.ext.java.syntax.Expression) => Right(Seq(ex)))))((args: Seq[hydra.ext.java.syntax.Expression]) =>
                  Right(hydra.ext.java.utils.javaConstructorCall(hydra.ext.java.utils.javaConstructorName(consId)(None))(args)(None))))
              }
            }
          }
        }
      }
    }
    case hydra.core.Term.variable(v_Term_variable_name) => hydra.ext.java.coder.encodeVariable(env)(v_Term_variable_name)(cx)(g)
    case hydra.core.Term.unit => Right(hydra.ext.java.utils.javaLiteralToJavaExpression(hydra.ext.java.syntax.Literal.`null`))
    case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression](encode(v_Term_wrap_wt.body))((jarg: hydra.ext.java.syntax.Expression) =>
      Right(hydra.ext.java.utils.javaConstructorCall(hydra.ext.java.utils.javaConstructorName(hydra.ext.java.utils.nameToJavaName(aliases)(v_Term_wrap_wt.typeName))(None))(Seq(jarg))(None)))
    case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => {
      val atyp: hydra.core.Type = (v_Term_typeApplication_ta.`type`)
      {
        val body: hydra.core.Term = (v_Term_typeApplication_ta.body)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type,
           hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(atyp)(cx)(g))((jatyp: hydra.ext.java.syntax.Type) =>
          {
          val combinedAnns: Map[hydra.core.Name, hydra.core.Term] = hydra.lib.lists.foldl[Map[hydra.core.Name,
             hydra.core.Term], Map[hydra.core.Name, hydra.core.Term]]((acc: Map[hydra.core.Name, hydra.core.Term]) =>
            (m: Map[hydra.core.Name, hydra.core.Term]) =>
            hydra.lib.maps.union[hydra.core.Name, hydra.core.Term](acc)(m))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term])(anns)
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type],
             hydra.ext.java.syntax.Expression](hydra.lib.eithers.bimap[hydra.error.DecodingError, Option[hydra.core.Type],
             hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type]]((__de: hydra.error.DecodingError) => hydra.context.InContext(hydra.error.Error.other(__de),
             cx))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(combinedAnns)))((mtyp: Option[hydra.core.Type]) =>
            hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, hydra.ext.java.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Type,
               Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type]](mtyp)(hydra.coderUtils.typeOfTerm(cx)(g)(term))((t: hydra.core.Type) => Right(t)))((typ: hydra.core.Type) =>
            {
            val collected0: Tuple2[hydra.core.Term, Seq[hydra.core.Type]] = hydra.ext.java.coder.collectTypeApps0(body)(Seq(atyp))
            {
              val innermostBody0: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, Seq[hydra.core.Type]](collected0)
              {
                val allTypeArgs0: Seq[hydra.core.Type] = hydra.lib.pairs.second[hydra.core.Term, Seq[hydra.core.Type]](collected0)
                hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, hydra.ext.java.syntax.Expression](hydra.ext.java.coder.correctCastType(innermostBody0)(allTypeArgs0)(typ)(cx)(g))((correctedTyp: hydra.core.Type) =>
                  {
                  val collected: Tuple2[hydra.core.Term, Seq[hydra.core.Type]] = hydra.ext.java.coder.collectTypeApps(body)(Seq(atyp))
                  {
                    val innermostBody: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, Seq[hydra.core.Type]](collected)
                    {
                      val allTypeArgs: Seq[hydra.core.Type] = hydra.lib.pairs.second[hydra.core.Term, Seq[hydra.core.Type]](collected)
                      innermostBody match
                        case hydra.core.Term.variable(v_Term_variable_varName) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                           hydra.ext.java.helpers.JavaSymbolClass, hydra.ext.java.syntax.Expression](hydra.ext.java.coder.classifyDataReference(v_Term_variable_varName)(cx)(g))((cls: hydra.ext.java.helpers.JavaSymbolClass) =>
                          hydra.ext.java.coder.typeAppNullaryOrHoisted(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(v_Term_variable_varName)(cls)(allTypeArgs)(cx)(g))
                        case hydra.core.Term.either(v_Term_either_eitherTerm) => hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
                           hydra.ext.java.syntax.Expression]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Type](allTypeArgs))(2))({
                          val eitherBranchTypes: Tuple2[hydra.core.Type, hydra.core.Type] = Tuple2(hydra.lib.lists.head[hydra.core.Type](allTypeArgs),
                             hydra.lib.lists.head[hydra.core.Type](hydra.lib.lists.tail[hydra.core.Type](allTypeArgs)))
                          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.ReferenceType],
                             hydra.ext.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Type,
                             hydra.ext.java.syntax.ReferenceType, hydra.context.InContext[hydra.error.Error]]((t: hydra.core.Type) =>
                            hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type,
                               hydra.ext.java.syntax.ReferenceType](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(t)(cx)(g))((jt: hydra.ext.java.syntax.Type) => hydra.ext.java.utils.javaTypeToJavaReferenceType(jt)(cx)))(allTypeArgs))((jTypeArgs: Seq[hydra.ext.java.syntax.ReferenceType]) =>
                            {
                            val eitherTargs: Seq[hydra.ext.java.syntax.TypeArgument] = hydra.lib.lists.map[hydra.ext.java.syntax.ReferenceType,
                               hydra.ext.java.syntax.TypeArgument]((rt: hydra.ext.java.syntax.ReferenceType) => hydra.ext.java.syntax.TypeArgument.reference(rt))(jTypeArgs)
                            {
                              def encodeEitherBranch(branchType: hydra.core.Type)(t1: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error],
                                 hydra.ext.java.syntax.Expression] =
                                {
                                val annotated: hydra.core.Term = hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(branchType)))(t1)
                                hydra.ext.java.coder.encodeTermInternal(env)(anns)(Seq())(annotated)(cx)(g)
                              }
                              hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, Either[hydra.context.InContext[hydra.error.Error],
                                 hydra.ext.java.syntax.Expression]]((term1: hydra.core.Term) =>
                                hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
                                   hydra.ext.java.syntax.Expression](encodeEitherBranch(hydra.lib.pairs.first[hydra.core.Type,
                                   hydra.core.Type](eitherBranchTypes))(term1))((expr: hydra.ext.java.syntax.Expression) =>
                                Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStaticWithTypeArgs("hydra.util.Either")("left")(eitherTargs)(Seq(expr))))))((term1: hydra.core.Term) =>
                                hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
                                   hydra.ext.java.syntax.Expression](encodeEitherBranch(hydra.lib.pairs.second[hydra.core.Type,
                                   hydra.core.Type](eitherBranchTypes))(term1))((expr: hydra.ext.java.syntax.Expression) =>
                                Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStaticWithTypeArgs("hydra.util.Either")("right")(eitherTargs)(Seq(expr))))))(v_Term_either_eitherTerm)
                            }
                          })
                        })(hydra.ext.java.coder.typeAppFallbackCast(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(cx)(g))
                        case _ => hydra.ext.java.coder.typeAppFallbackCast(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(cx)(g)
                    }
                  }
                })
              }
            }
          }))
        })
      }
    }
    case _ => Right(hydra.ext.java.coder.encodeLiteral(hydra.core.Literal.string("Unimplemented term variant")))
}

def annotateLambdaArgs[T0, T1](cname: hydra.core.Name)(tApps: Seq[hydra.core.Type])(argTerms: Seq[hydra.core.Term])(cx: T0)(g: hydra.graph.Graph): Either[T1,
   Seq[hydra.core.Term]] =
  hydra.lib.logic.ifElse[Either[T1, Seq[hydra.core.Term]]](hydra.lib.lists.`null`[hydra.core.Type](tApps))(Right(argTerms))(hydra.lib.eithers.bind[T1,
     Option[hydra.core.TypeScheme], Seq[hydra.core.Term]](hydra.lib.eithers.bind[T1, Option[hydra.core.Binding],
     Option[hydra.core.TypeScheme]](Right(hydra.lexical.dereferenceElement(g)(cname)))((mel: Option[hydra.core.Binding]) =>
  hydra.lib.maybes.cases[hydra.core.Binding, Either[T1, Option[hydra.core.TypeScheme]]](mel)(Right(hydra.lib.maybes.map[hydra.graph.Primitive,
     hydra.core.TypeScheme]((prim: hydra.graph.Primitive) => (prim.`type`))(hydra.lib.maps.lookup[hydra.core.Name,
     hydra.graph.Primitive](cname)(g.primitives))))((el: hydra.core.Binding) => Right(el.`type`))))((mts: Option[hydra.core.TypeScheme]) =>
  hydra.lib.maybes.cases[hydra.core.TypeScheme, Either[T1, Seq[hydra.core.Term]]](mts)(Right(argTerms))((ts: hydra.core.TypeScheme) =>
  {
  val schemeType: hydra.core.Type = (ts.`type`)
  {
    val schemeTypeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.ext.java.coder.collectTypeVars(schemeType)
    {
      val schemeVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) => hydra.lib.sets.member[hydra.core.Name](v)(schemeTypeVars))(ts.variables)
      hydra.lib.logic.ifElse[Either[T1, Seq[hydra.core.Term]]](hydra.lib.logic.or(hydra.lib.lists.`null`[hydra.core.Name](schemeVars))(hydra.lib.logic.not(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Name](schemeVars))(hydra.lib.lists.length[hydra.core.Type](tApps)))))(Right(argTerms))({
        val subst: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Type](hydra.lib.lists.zip[hydra.core.Name,
           hydra.core.Type](schemeVars)(tApps))
        {
          val expectedTypes: Seq[hydra.core.Type] = hydra.ext.java.coder.peelExpectedTypes(subst)(hydra.lib.lists.length[hydra.core.Term](argTerms))(schemeType)
          Right(hydra.lib.lists.zipWith[hydra.core.Term, hydra.core.Type, hydra.core.Term]((arg: hydra.core.Term) =>
            (mExpected: hydra.core.Type) => hydra.ext.java.coder.propagateType(mExpected)(arg))(argTerms)(hydra.lib.lists.concat2[hydra.core.Type](expectedTypes)(hydra.lib.lists.replicate[hydra.core.Type](hydra.lib.lists.length[hydra.core.Term](argTerms))(hydra.core.Type.variable("unused")))))
        }
      })
    }
  }
})))

def applyJavaArg(expr: hydra.ext.java.syntax.Expression)(jarg: hydra.ext.java.syntax.Expression): hydra.ext.java.syntax.Expression =
  hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocation(Some(Right(hydra.ext.java.utils.javaExpressionToJavaPrimary(expr))))(hydra.ext.java.names.applyMethodName)(Seq(jarg)))

def encodeApplication(env: hydra.ext.java.helpers.JavaEnvironment)(app: hydra.core.Application)(cx: hydra.context.Context)(g0: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.Expression] =
  {
  val aliases: hydra.ext.java.helpers.Aliases = (env.aliases)
  val g: hydra.graph.Graph = (env.graph)
  val gathered: Tuple2[hydra.core.Term, Tuple2[Seq[hydra.core.Term], Seq[hydra.core.Type]]] = hydra.coderUtils.gatherArgsWithTypeApps(hydra.core.Term.application(app))(Seq())(Seq())
  val fun: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, Tuple2[Seq[hydra.core.Term], Seq[hydra.core.Type]]](gathered)
  val args: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], Seq[hydra.core.Type]](hydra.lib.pairs.second[hydra.core.Term,
     Tuple2[Seq[hydra.core.Term], Seq[hydra.core.Type]]](gathered))
  val typeApps: Seq[hydra.core.Type] = hydra.lib.pairs.second[Seq[hydra.core.Term], Seq[hydra.core.Type]](hydra.lib.pairs.second[hydra.core.Term,
     Tuple2[Seq[hydra.core.Term], Seq[hydra.core.Type]]](gathered))
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type], hydra.ext.java.syntax.Expression](hydra.lib.eithers.bimap[hydra.error.DecodingError,
     Option[hydra.core.Type], hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type]]((__de: hydra.error.DecodingError) => hydra.context.InContext(hydra.error.Error.other(__de),
     cx))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(hydra.annotations.termAnnotationInternal(fun))))((mfunTyp: Option[hydra.core.Type]) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, hydra.ext.java.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Type,
       Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type]](mfunTyp)(hydra.coderUtils.typeOfTerm(cx)(g)(fun))((t: hydra.core.Type) => Right(t)))((funTyp: hydra.core.Type) =>
    {
    val arity: Int = hydra.arity.typeArity(funTyp)
    {
      val deannotatedFun: hydra.core.Term = hydra.rewriting.deannotateTerm(fun)
      {
        val calleeName: Option[hydra.core.Name] = deannotatedFun match
          case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
            case hydra.core.Function.primitive(v_Function_primitive_n) => Some(v_Function_primitive_n)
            case _ => None
          case hydra.core.Term.variable(v_Term_variable_n) => Some(v_Term_variable_n)
          case _ => None
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.Term], hydra.ext.java.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Name,
           Either[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.Term]]](calleeName)(Right(args))((cname: hydra.core.Name) =>
          hydra.ext.java.coder.annotateLambdaArgs(cname)(typeApps)(args)(cx)(g)))((annotatedArgs: Seq[hydra.core.Term]) =>
          deannotatedFun match
          case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
            case hydra.core.Function.primitive(v_Function_primitive_name) => {
              val hargs: Seq[hydra.core.Term] = hydra.lib.lists.take[hydra.core.Term](arity)(annotatedArgs)
              {
                val rargs: Seq[hydra.core.Term] = hydra.lib.lists.drop[hydra.core.Term](arity)(annotatedArgs)
                hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
                   hydra.ext.java.syntax.Expression](hydra.ext.java.coder.functionCall(env)(true)(v_Function_primitive_name)(hargs)(Seq())(cx)(g))((initialCall: hydra.ext.java.syntax.Expression) =>
                  hydra.lib.eithers.foldl[hydra.ext.java.syntax.Expression, hydra.core.Term, hydra.context.InContext[hydra.error.Error]]((acc: hydra.ext.java.syntax.Expression) =>
                  (h: hydra.core.Term) =>
                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
                     hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeTerm(env)(h)(cx)(g))((jarg: hydra.ext.java.syntax.Expression) => Right(hydra.ext.java.coder.applyJavaArg(acc)(jarg))))(initialCall)(rargs))
              }
            }
            case _ => hydra.ext.java.coder.encodeApplication_fallback(env)(aliases)(g)(typeApps)(app.function)(app.argument)(cx)(g)
          case hydra.core.Term.variable(v_Term_variable_name) => hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
             hydra.ext.java.syntax.Expression]](hydra.lib.logic.and(hydra.ext.java.coder.isRecursiveVariable(aliases)(v_Term_variable_name))(hydra.lib.logic.not(hydra.ext.java.coder.isLambdaBoundIn(v_Term_variable_name)(aliases.lambdaVars))))(hydra.ext.java.coder.encodeApplication_fallback(env)(aliases)(g)(typeApps)(app.function)(app.argument)(cx)(g))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
             hydra.ext.java.helpers.JavaSymbolClass, hydra.ext.java.syntax.Expression](hydra.ext.java.coder.classifyDataReference(v_Term_variable_name)(cx)(g))((symClass: hydra.ext.java.helpers.JavaSymbolClass) =>
            {
            val methodArity: Int = symClass match
              case hydra.ext.java.helpers.JavaSymbolClass.hoistedLambda(v_JavaSymbolClass_hoistedLambda_n) => v_JavaSymbolClass_hoistedLambda_n
              case _ => arity
            {
              val hargs: Seq[hydra.core.Term] = hydra.lib.lists.take[hydra.core.Term](methodArity)(annotatedArgs)
              {
                val rargs: Seq[hydra.core.Term] = hydra.lib.lists.drop[hydra.core.Term](methodArity)(annotatedArgs)
                {
                  val trusted: scala.collection.immutable.Set[hydra.core.Name] = (aliases.trustedTypeVars)
                  {
                    val inScope: scala.collection.immutable.Set[hydra.core.Name] = (aliases.inScopeTypeParams)
                    {
                      val filteredTypeApps: Seq[hydra.core.Type] = hydra.lib.logic.ifElse[Seq[hydra.core.Type]](hydra.lib.logic.or(hydra.lib.sets.`null`[hydra.core.Name](trusted))(hydra.lib.sets.`null`[hydra.core.Name](inScope)))(Seq())({
                        val allVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.unions[hydra.core.Name](hydra.lib.lists.map[hydra.core.Type,
                           scala.collection.immutable.Set[hydra.core.Name]]((t: hydra.core.Type) => hydra.ext.java.coder.collectTypeVars(t))(typeApps))
                        hydra.lib.logic.ifElse[Seq[hydra.core.Type]](hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](hydra.lib.sets.difference[hydra.core.Name](allVars)(inScope))))(Seq())(hydra.lib.logic.ifElse[Seq[hydra.core.Type]](hydra.lib.sets.`null`[hydra.core.Name](hydra.lib.sets.difference[hydra.core.Name](allVars)(trusted)))(typeApps)(Seq()))
                      })
                      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.Type],
                         hydra.ext.java.syntax.Expression](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
                         Seq[hydra.core.Type]]](hydra.lib.lists.`null`[hydra.core.Type](filteredTypeApps))(Right(Seq()))(hydra.ext.java.coder.correctTypeApps(g)(v_Term_variable_name)(hargs)(filteredTypeApps)(cx)(g)))((safeTypeApps: Seq[hydra.core.Type]) =>
                        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.Type],
                           hydra.ext.java.syntax.Expression](hydra.ext.java.coder.filterPhantomTypeArgs(v_Term_variable_name)(safeTypeApps)(cx)(g))((finalTypeApps: Seq[hydra.core.Type]) =>
                        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
                           hydra.ext.java.syntax.Expression](hydra.ext.java.coder.functionCall(env)(false)(v_Term_variable_name)(hargs)(finalTypeApps)(cx)(g))((initialCall: hydra.ext.java.syntax.Expression) =>
                        hydra.lib.eithers.foldl[hydra.ext.java.syntax.Expression, hydra.core.Term, hydra.context.InContext[hydra.error.Error]]((acc: hydra.ext.java.syntax.Expression) =>
                        (h: hydra.core.Term) =>
                        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
                           hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeTerm(env)(h)(cx)(g))((jarg: hydra.ext.java.syntax.Expression) => Right(hydra.ext.java.coder.applyJavaArg(acc)(jarg))))(initialCall)(rargs))))
                    }
                  }
                }
              }
            }
          }))
          case _ => hydra.ext.java.coder.encodeApplication_fallback(env)(aliases)(g)(typeApps)(app.function)(app.argument)(cx)(g))
      }
    }
  }))
}

def encodeApplication_fallback(env: hydra.ext.java.helpers.JavaEnvironment)(aliases: hydra.ext.java.helpers.Aliases)(gr: hydra.graph.Graph)(typeApps: Seq[hydra.core.Type])(lhs: hydra.core.Term)(rhs: hydra.core.Term)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.Expression] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type], hydra.ext.java.syntax.Expression](hydra.lib.eithers.bimap[hydra.error.DecodingError,
     Option[hydra.core.Type], hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type]]((__de: hydra.error.DecodingError) => hydra.context.InContext(hydra.error.Error.other(__de),
     cx))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(hydra.annotations.termAnnotationInternal(lhs))))((mt: Option[hydra.core.Type]) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, hydra.ext.java.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Type,
     Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type]](mt)(hydra.coderUtils.typeOfTerm(cx)(g)(lhs))((typ: hydra.core.Type) => Right(typ)))((t: hydra.core.Type) =>
  hydra.rewriting.deannotateTypeParameters(hydra.rewriting.deannotateType(t)) match
  case hydra.core.Type.function(v_Type_function_ft) => {
    val dom: hydra.core.Type = (v_Type_function_ft.domain)
    {
      val cod: hydra.core.Type = (v_Type_function_ft.codomain)
      hydra.rewriting.deannotateTerm(lhs) match
        case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
          case hydra.core.Function.elimination(v_Function_elimination_e) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
             hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeTerm(env)(rhs)(cx)(g))((jarg: hydra.ext.java.syntax.Expression) =>
            hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, hydra.ext.java.syntax.Expression](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
               hydra.core.Type]](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.ext.java.syntax.TypeArgument](hydra.ext.java.coder.javaTypeArgumentsForType(dom))))(Right(dom))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
               Option[hydra.core.Type], hydra.core.Type](hydra.lib.eithers.bimap[hydra.error.DecodingError,
               Option[hydra.core.Type], hydra.context.InContext[hydra.error.Error], Option[hydra.core.Type]]((__de: hydra.error.DecodingError) => hydra.context.InContext(hydra.error.Error.other(__de),
               cx))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(hydra.annotations.termAnnotationInternal(rhs))))((mrt: Option[hydra.core.Type]) =>
            hydra.lib.maybes.cases[hydra.core.Type, Either[hydra.context.InContext[hydra.error.Error],
               hydra.core.Type]](mrt)(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
               hydra.core.Type, hydra.core.Type](hydra.coderUtils.typeOfTerm(cx)(g)(rhs))((rt: hydra.core.Type) =>
            Right(hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.ext.java.syntax.TypeArgument](hydra.ext.java.coder.javaTypeArgumentsForType(rt))))(rt)(dom))))((rt: hydra.core.Type) =>
            Right(hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.ext.java.syntax.TypeArgument](hydra.ext.java.coder.javaTypeArgumentsForType(rt))))(rt)(dom))))))((enrichedDom: hydra.core.Type) =>
            hydra.ext.java.coder.encodeElimination(env)(Some(jarg))(enrichedDom)(cod)(v_Function_elimination_e)(cx)(g)))
          case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
             hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeTerm(env)(lhs)(cx)(g))((jfun: hydra.ext.java.syntax.Expression) =>
            hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
               hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeTerm(env)(rhs)(cx)(g))((jarg: hydra.ext.java.syntax.Expression) => Right(hydra.ext.java.coder.applyJavaArg(jfun)(jarg))))
        case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
           hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeTerm(env)(lhs)(cx)(g))((jfun: hydra.ext.java.syntax.Expression) =>
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
             hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeTerm(env)(rhs)(cx)(g))((jarg: hydra.ext.java.syntax.Expression) => Right(hydra.ext.java.coder.applyJavaArg(jfun)(jarg))))
    }
  }
  case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
     hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeTerm(env)(lhs)(cx)(g))((jfun: hydra.ext.java.syntax.Expression) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
       hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeTerm(env)(rhs)(cx)(g))((jarg: hydra.ext.java.syntax.Expression) => Right(hydra.ext.java.coder.applyJavaArg(jfun)(jarg))))))

def functionCall(env: hydra.ext.java.helpers.JavaEnvironment)(isPrim: Boolean)(name: hydra.core.Name)(args: Seq[hydra.core.Term])(typeApps: Seq[hydra.core.Type])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.Expression] =
  {
  val aliases: hydra.ext.java.helpers.Aliases = (env.aliases)
  val isLambdaBound: Boolean = hydra.ext.java.coder.isLambdaBoundIn(name)(aliases.lambdaVars)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.Expression],
     hydra.ext.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term, hydra.ext.java.syntax.Expression,
     hydra.context.InContext[hydra.error.Error]]((arg: hydra.core.Term) => hydra.ext.java.coder.encodeTerm(env)(arg)(cx)(g))(args))((jargs0: Seq[hydra.ext.java.syntax.Expression]) =>
    {
    val wrapResult: Tuple2[Seq[hydra.ext.java.syntax.Expression], Option[scala.Predef.String]] = hydra.ext.java.coder.wrapLazyArguments(name)(jargs0)
    {
      val jargs: Seq[hydra.ext.java.syntax.Expression] = hydra.lib.pairs.first[Seq[hydra.ext.java.syntax.Expression], Option[scala.Predef.String]](wrapResult)
      {
        val mMethodOverride: Option[scala.Predef.String] = hydra.lib.pairs.second[Seq[hydra.ext.java.syntax.Expression],
           Option[scala.Predef.String]](wrapResult)
        hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](hydra.lib.logic.or(hydra.ext.java.coder.isLocalVariable(name))(isLambdaBound))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
           hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeVariable(env)(name)(cx)(g))((baseExpr: hydra.ext.java.syntax.Expression) =>
          Right(hydra.lib.lists.foldl[hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression]((acc: hydra.ext.java.syntax.Expression) =>
          (jarg: hydra.ext.java.syntax.Expression) => hydra.ext.java.coder.applyJavaArg(acc)(jarg))(baseExpr)(jargs))))({
          def overrideMethodName(jid: hydra.ext.java.syntax.Identifier): hydra.ext.java.syntax.Identifier =
            hydra.lib.maybes.cases[scala.Predef.String, hydra.ext.java.syntax.Identifier](mMethodOverride)(jid)((m: scala.Predef.String) =>
            {
            val s: scala.Predef.String = jid
            hydra.lib.strings.cat2(hydra.lib.strings.fromList(hydra.lib.lists.take[Int](hydra.lib.math.sub(hydra.lib.strings.length(s))(hydra.lib.strings.length(hydra.ext.java.names.applyMethodName)))(hydra.lib.strings.toList(s))))(m)
          })
          hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](hydra.lib.lists.`null`[hydra.core.Type](typeApps))({
            val header: hydra.ext.java.syntax.MethodInvocation_Header = hydra.ext.java.syntax.MethodInvocation_Header.simple(overrideMethodName(hydra.ext.java.coder.elementJavaIdentifier(isPrim)(false)(aliases)(name)))
            Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.syntax.MethodInvocation(header, jargs)))
          })({
            val qn: hydra.module.QualifiedName = hydra.names.qualifyName(name)
            {
              val mns: Option[hydra.module.Namespace] = (qn.namespace)
              {
                val localName: scala.Predef.String = (qn.local)
                hydra.lib.maybes.cases[hydra.module.Namespace, Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](mns)({
                  val header: hydra.ext.java.syntax.MethodInvocation_Header = hydra.ext.java.syntax.MethodInvocation_Header.simple(overrideMethodName(hydra.ext.java.coder.elementJavaIdentifier(isPrim)(false)(aliases)(name)))
                  Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.syntax.MethodInvocation(header, jargs)))
                })((`ns_`: hydra.module.Namespace) =>
                  {
                  val classId: hydra.ext.java.syntax.Identifier = hydra.ext.java.utils.nameToJavaName(aliases)(hydra.names.unqualifyName(hydra.module.QualifiedName(Some(`ns_`),
                     hydra.ext.java.coder.elementsClassName(`ns_`))))
                  {
                    val methodId: hydra.ext.java.syntax.Identifier = hydra.lib.logic.ifElse[hydra.ext.java.syntax.Identifier](isPrim)(overrideMethodName(hydra.lib.strings.cat2(hydra.ext.java.utils.nameToJavaName(aliases)(hydra.names.unqualifyName(hydra.module.QualifiedName(Some(`ns_`),
                       hydra.formatting.capitalize(localName)))))(hydra.lib.strings.cat2(".")(hydra.ext.java.names.applyMethodName))))(hydra.ext.java.utils.sanitizeJavaName(localName))
                    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.TypeArgument],
                       hydra.ext.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Type, hydra.ext.java.syntax.TypeArgument,
                       hydra.context.InContext[hydra.error.Error]]((t: hydra.core.Type) =>
                      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type,
                         hydra.ext.java.syntax.TypeArgument](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(t)(cx)(g))((jt: hydra.ext.java.syntax.Type) =>
                      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
                         hydra.ext.java.syntax.TypeArgument](hydra.ext.java.utils.javaTypeToJavaReferenceType(jt)(cx))((rt: hydra.ext.java.syntax.ReferenceType) => Right(hydra.ext.java.syntax.TypeArgument.reference(rt)))))(typeApps))((jTypeArgs: Seq[hydra.ext.java.syntax.TypeArgument]) =>
                      Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStaticWithTypeArgs(classId)(methodId)(jTypeArgs)(jargs))))
                  }
                })
              }
            }
          })
        })
      }
    }
  })
}

def buildCurriedLambda(params: Seq[hydra.core.Name])(inner: hydra.ext.java.syntax.Expression): hydra.ext.java.syntax.Expression =
  hydra.lib.lists.foldl[hydra.ext.java.syntax.Expression, hydra.core.Name]((acc: hydra.ext.java.syntax.Expression) =>
  (p: hydra.core.Name) => hydra.ext.java.utils.javaLambda(p)(acc))(inner)(hydra.lib.lists.reverse[hydra.core.Name](params))

def encodeFunction(env: hydra.ext.java.helpers.JavaEnvironment)(dom: hydra.core.Type)(cod: hydra.core.Type)(fun: hydra.core.Function)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.Expression] =
  {
  val aliases: hydra.ext.java.helpers.Aliases = (env.aliases)
  fun match
    case hydra.core.Function.elimination(v_Function_elimination_elm) => hydra.ext.java.coder.encodeElimination(env)(None)(dom)(cod)(v_Function_elimination_elm)(cx)(g)
    case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.ext.java.coder.withLambda(env)(v_Function_lambda_lam)((env2: hydra.ext.java.helpers.JavaEnvironment) =>
      {
      val lambdaVar: hydra.core.Name = (v_Function_lambda_lam.parameter)
      {
        val body: hydra.core.Term = (v_Function_lambda_lam.body)
        hydra.rewriting.deannotateTerm(body) match
          case hydra.core.Term.function(v_Term_function_f2) => v_Term_function_f2 match
            case hydra.core.Function.lambda(v_Function_lambda_innerLam) => hydra.rewriting.deannotateType(cod) match
              case hydra.core.Type.function(v_Type_function_ft) => {
                val dom2: hydra.core.Type = (v_Type_function_ft.domain)
                {
                  val cod2: hydra.core.Type = (v_Type_function_ft.codomain)
                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
                     hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeFunction(env2)(dom2)(cod2)(hydra.core.Function.lambda(v_Function_lambda_innerLam))(cx)(g))((innerJavaLambda: hydra.ext.java.syntax.Expression) =>
                    {
                    val lam1: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaLambda(lambdaVar)(innerJavaLambda)
                    hydra.ext.java.coder.applyCastIfSafe(aliases)(hydra.core.Type.function(hydra.core.FunctionType(dom, cod)))(lam1)(cx)(g)
                  })
                }
              }
              case _ => Left(hydra.context.InContext(hydra.error.Error.other(hydra.lib.strings.cat2("expected function type for lambda body, but got: ")(hydra.show.core.`type`(cod))),
                 cx))
            case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.FunctionStructure[hydra.ext.java.helpers.JavaEnvironment],
               hydra.ext.java.syntax.Expression](hydra.ext.java.coder.analyzeJavaFunction(env2)(body)(cx)(g))((fs: hydra.typing.FunctionStructure[hydra.ext.java.helpers.JavaEnvironment]) =>
              {
              val bindings: Seq[hydra.core.Binding] = (fs.bindings)
              {
                val innerBody: hydra.core.Term = (fs.body)
                {
                  val env3: hydra.ext.java.helpers.JavaEnvironment = (fs.environment)
                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.ext.java.syntax.BlockStatement],
                     hydra.ext.java.helpers.JavaEnvironment], hydra.ext.java.syntax.Expression](hydra.ext.java.coder.bindingsToStatements(env3)(bindings)(cx)(g))((bindResult: Tuple2[Seq[hydra.ext.java.syntax.BlockStatement],
                     hydra.ext.java.helpers.JavaEnvironment]) =>
                    {
                    val bindingStmts: Seq[hydra.ext.java.syntax.BlockStatement] = hydra.lib.pairs.first[Seq[hydra.ext.java.syntax.BlockStatement],
                       hydra.ext.java.helpers.JavaEnvironment](bindResult)
                    {
                      val env4: hydra.ext.java.helpers.JavaEnvironment = hydra.lib.pairs.second[Seq[hydra.ext.java.syntax.BlockStatement],
                         hydra.ext.java.helpers.JavaEnvironment](bindResult)
                      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
                         hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeTerm(env4)(innerBody)(cx)(g))((jbody: hydra.ext.java.syntax.Expression) =>
                        {
                        val lam1: hydra.ext.java.syntax.Expression = hydra.lib.logic.ifElse[hydra.ext.java.syntax.Expression](hydra.lib.lists.`null`[hydra.core.Binding](bindings))(hydra.ext.java.utils.javaLambda(lambdaVar)(jbody))({
                          val returnSt: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(jbody)))
                          hydra.ext.java.utils.javaLambdaFromBlock(lambdaVar)(hydra.lib.lists.concat2[hydra.ext.java.syntax.BlockStatement](bindingStmts)(Seq(returnSt)))
                        })
                        hydra.ext.java.coder.applyCastIfSafe(aliases)(hydra.core.Type.function(hydra.core.FunctionType(dom, cod)))(lam1)(cx)(g)
                      })
                    }
                  })
                }
              }
            })
          case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.FunctionStructure[hydra.ext.java.helpers.JavaEnvironment],
             hydra.ext.java.syntax.Expression](hydra.ext.java.coder.analyzeJavaFunction(env2)(body)(cx)(g))((fs: hydra.typing.FunctionStructure[hydra.ext.java.helpers.JavaEnvironment]) =>
            {
            val bindings: Seq[hydra.core.Binding] = (fs.bindings)
            {
              val innerBody: hydra.core.Term = (fs.body)
              {
                val env3: hydra.ext.java.helpers.JavaEnvironment = (fs.environment)
                hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.ext.java.syntax.BlockStatement],
                   hydra.ext.java.helpers.JavaEnvironment], hydra.ext.java.syntax.Expression](hydra.ext.java.coder.bindingsToStatements(env3)(bindings)(cx)(g))((bindResult: Tuple2[Seq[hydra.ext.java.syntax.BlockStatement],
                   hydra.ext.java.helpers.JavaEnvironment]) =>
                  {
                  val bindingStmts: Seq[hydra.ext.java.syntax.BlockStatement] = hydra.lib.pairs.first[Seq[hydra.ext.java.syntax.BlockStatement],
                     hydra.ext.java.helpers.JavaEnvironment](bindResult)
                  {
                    val env4: hydra.ext.java.helpers.JavaEnvironment = hydra.lib.pairs.second[Seq[hydra.ext.java.syntax.BlockStatement],
                       hydra.ext.java.helpers.JavaEnvironment](bindResult)
                    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
                       hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeTerm(env4)(innerBody)(cx)(g))((jbody: hydra.ext.java.syntax.Expression) =>
                      {
                      val lam1: hydra.ext.java.syntax.Expression = hydra.lib.logic.ifElse[hydra.ext.java.syntax.Expression](hydra.lib.lists.`null`[hydra.core.Binding](bindings))(hydra.ext.java.utils.javaLambda(lambdaVar)(jbody))({
                        val returnSt: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(jbody)))
                        hydra.ext.java.utils.javaLambdaFromBlock(lambdaVar)(hydra.lib.lists.concat2[hydra.ext.java.syntax.BlockStatement](bindingStmts)(Seq(returnSt)))
                      })
                      hydra.ext.java.coder.applyCastIfSafe(aliases)(hydra.core.Type.function(hydra.core.FunctionType(dom, cod)))(lam1)(cx)(g)
                    })
                  }
                })
              }
            }
          })
      }
    })
    case hydra.core.Function.primitive(v_Function_primitive_name) => {
      val classWithApply: scala.Predef.String = hydra.ext.java.coder.elementJavaIdentifier(true)(false)(aliases)(v_Function_primitive_name)
      {
        val suffix: scala.Predef.String = hydra.lib.strings.cat2(".")(hydra.ext.java.names.applyMethodName)
        {
          val className: scala.Predef.String = hydra.lib.strings.fromList(hydra.lib.lists.take[Int](hydra.lib.math.sub(hydra.lib.strings.length(classWithApply))(hydra.lib.strings.length(suffix)))(hydra.lib.strings.toList(classWithApply)))
          {
            val arity: Int = hydra.arity.typeArity(hydra.core.Type.function(hydra.core.FunctionType(dom, cod)))
            hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](hydra.lib.equality.lte[Int](arity)(1))(Right(hydra.ext.java.utils.javaIdentifierToJavaExpression(hydra.lib.strings.cat(Seq(className,
               "::", hydra.ext.java.names.applyMethodName)))))({
              val paramNames: Seq[hydra.core.Name] = hydra.lib.lists.map[Int, hydra.core.Name]((i: Int) => hydra.lib.strings.cat2("p")(hydra.lib.literals.showInt32(i)))(hydra.lib.math.range(0)(hydra.lib.math.sub(arity)(1)))
              {
                val paramExprs: Seq[hydra.ext.java.syntax.Expression] = hydra.lib.lists.map[hydra.core.Name,
                   hydra.ext.java.syntax.Expression]((p: hydra.core.Name) =>
                  hydra.ext.java.utils.javaIdentifierToJavaExpression(hydra.ext.java.utils.variableToJavaIdentifier(p)))(paramNames)
                {
                  val classId: hydra.ext.java.syntax.Identifier = className
                  {
                    val call: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStatic(classId)(hydra.ext.java.names.applyMethodName)(paramExprs))
                    {
                      val curried: hydra.ext.java.syntax.Expression = hydra.ext.java.coder.buildCurriedLambda(paramNames)(call)
                      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type,
                         hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(hydra.core.Type.function(hydra.core.FunctionType(dom,
                         cod)))(cx)(g))((jtype: hydra.ext.java.syntax.Type) =>
                        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
                           hydra.ext.java.syntax.Expression](hydra.ext.java.utils.javaTypeToJavaReferenceType(jtype)(cx))((rt: hydra.ext.java.syntax.ReferenceType) =>
                        Right(hydra.ext.java.utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.javaCastExpression(rt)(hydra.ext.java.utils.javaExpressionToJavaUnaryExpression(curried))))))
                    }
                  }
                }
              }
            })
          }
        }
      }
    }
    case _ => Right(hydra.ext.java.coder.encodeLiteral(hydra.core.Literal.string(hydra.lib.strings.cat2("Unimplemented function variant: ")(hydra.show.core.function(fun)))))
}

def extractArgType[T0](_lhs: T0)(typ: hydra.core.Type): hydra.core.Type =
  typ match
  case hydra.core.Type.application(v_Type_application_at1) => v_Type_application_at1.function match
    case hydra.core.Type.application(v_Type_application__at2) => (v_Type_application_at1.argument)
    case _ => typ
  case _ => typ

def annotateBodyWithCod(typ: hydra.core.Type)(term: hydra.core.Term): hydra.core.Term =
  {
  def setAnn(t: hydra.core.Term): hydra.core.Term =
    hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(typ)))(t)
  hydra.rewriting.deannotateTerm(term) match
    case hydra.core.Term.typeApplication(v_Term_typeApplication__ta) => setAnn(term)
    case hydra.core.Term.application(v_Term_application_app) => {
      val lhs: hydra.core.Term = (v_Term_application_app.function)
      {
        val rhs: hydra.core.Term = (v_Term_application_app.argument)
        {
          val annotatedRhs: hydra.core.Term = hydra.rewriting.deannotateTerm(rhs) match
            case hydra.core.Term.typeApplication(v_Term_typeApplication__ta2) => hydra.ext.java.coder.annotateBodyWithCod(hydra.ext.java.coder.extractArgType(lhs)(typ))(rhs)
            case _ => rhs
          setAnn(hydra.core.Term.application(hydra.core.Application(lhs, annotatedRhs)))
        }
      }
    }
    case _ => setAnn(term)
}

def domTypeArgs(aliases: hydra.ext.java.helpers.Aliases)(d: hydra.core.Type)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   Seq[hydra.ext.java.syntax.TypeArgument]] =
  {
  val args: Seq[hydra.core.Type] = hydra.ext.java.coder.extractTypeApplicationArgs(hydra.rewriting.deannotateType(d))
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.TypeArgument]]](hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Type](args)))(hydra.lib.eithers.mapList[hydra.core.Type,
     hydra.ext.java.syntax.TypeArgument, hydra.context.InContext[hydra.error.Error]]((t: hydra.core.Type) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type, hydra.ext.java.syntax.TypeArgument](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(t)(cx)(g))((jt: hydra.ext.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
       hydra.ext.java.syntax.TypeArgument](hydra.ext.java.utils.javaTypeToJavaReferenceType(jt)(cx))((rt: hydra.ext.java.syntax.ReferenceType) => Right(hydra.ext.java.syntax.TypeArgument.reference(rt)))))(args))(Right(hydra.ext.java.coder.javaTypeArgumentsForType(d)))
}

def otherwiseBranch(env: hydra.ext.java.helpers.JavaEnvironment)(aliases: hydra.ext.java.helpers.Aliases)(dom: hydra.core.Type)(cod: hydra.core.Type)(tname: hydra.core.Name)(jcod: hydra.ext.java.syntax.Type)(targs: Seq[hydra.ext.java.syntax.TypeArgument])(d: hydra.core.Term)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.ClassBodyDeclarationWithComments] =
  {
  val jdom: hydra.ext.java.syntax.Type = hydra.ext.java.syntax.Type.reference(hydra.ext.java.utils.nameToJavaReferenceType(aliases)(true)(targs)(tname)(None))
  val mods: Seq[hydra.ext.java.syntax.MethodModifier] = Seq(hydra.ext.java.syntax.MethodModifier.public)
  val anns: Seq[hydra.ext.java.syntax.Annotation] = Seq(hydra.ext.java.utils.overrideAnnotation)
  val param: hydra.ext.java.syntax.FormalParameter = hydra.ext.java.utils.javaTypeToJavaFormalParameter(jdom)("instance")
  val result: hydra.ext.java.syntax.Result = hydra.ext.java.syntax.Result.`type`(jcod)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.FunctionStructure[hydra.ext.java.helpers.JavaEnvironment],
     hydra.ext.java.syntax.ClassBodyDeclarationWithComments](hydra.ext.java.coder.analyzeJavaFunction(env)(d)(cx)(g))((fs: hydra.typing.FunctionStructure[hydra.ext.java.helpers.JavaEnvironment]) =>
    {
    val bindings: Seq[hydra.core.Binding] = (fs.bindings)
    {
      val rawBody: hydra.core.Term = (fs.body)
      {
        val innerBody: hydra.core.Term = hydra.ext.java.coder.annotateBodyWithCod(cod)(rawBody)
        {
          val env2: hydra.ext.java.helpers.JavaEnvironment = (fs.environment)
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.ext.java.syntax.BlockStatement],
             hydra.ext.java.helpers.JavaEnvironment], hydra.ext.java.syntax.ClassBodyDeclarationWithComments](hydra.ext.java.coder.bindingsToStatements(env2)(bindings)(cx)(g))((bindResult: Tuple2[Seq[hydra.ext.java.syntax.BlockStatement],
             hydra.ext.java.helpers.JavaEnvironment]) =>
            {
            val bindingStmts: Seq[hydra.ext.java.syntax.BlockStatement] = hydra.lib.pairs.first[Seq[hydra.ext.java.syntax.BlockStatement],
               hydra.ext.java.helpers.JavaEnvironment](bindResult)
            {
              val env3: hydra.ext.java.helpers.JavaEnvironment = hydra.lib.pairs.second[Seq[hydra.ext.java.syntax.BlockStatement],
                 hydra.ext.java.helpers.JavaEnvironment](bindResult)
              hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
                 hydra.ext.java.syntax.ClassBodyDeclarationWithComments](hydra.ext.java.coder.encodeTerm(env3)(innerBody)(cx)(g))((jret: hydra.ext.java.syntax.Expression) =>
                {
                val returnStmt: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(jret)))
                {
                  val allStmts: Seq[hydra.ext.java.syntax.BlockStatement] = hydra.lib.lists.concat2[hydra.ext.java.syntax.BlockStatement](bindingStmts)(Seq(returnStmt))
                  Right(hydra.ext.java.coder.noComment(hydra.ext.java.utils.methodDeclaration(mods)(Seq())(anns)(hydra.ext.java.names.otherwiseMethodName)(Seq(param))(result)(Some(allStmts))))
                }
              })
            }
          })
        }
      }
    }
  })
}

def visitBranch(env: hydra.ext.java.helpers.JavaEnvironment)(aliases: hydra.ext.java.helpers.Aliases)(dom: hydra.core.Type)(tname: hydra.core.Name)(jcod: hydra.ext.java.syntax.Type)(targs: Seq[hydra.ext.java.syntax.TypeArgument])(field: hydra.core.Field)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.ClassBodyDeclarationWithComments] =
  {
  val jdom: hydra.ext.java.syntax.Type = hydra.ext.java.syntax.Type.reference(hydra.ext.java.utils.nameToJavaReferenceType(aliases)(true)(targs)(tname)(Some(hydra.formatting.capitalize(field.name))))
  val mods: Seq[hydra.ext.java.syntax.MethodModifier] = Seq(hydra.ext.java.syntax.MethodModifier.public)
  val anns: Seq[hydra.ext.java.syntax.Annotation] = Seq(hydra.ext.java.utils.overrideAnnotation)
  val result: hydra.ext.java.syntax.Result = hydra.ext.java.syntax.Result.`type`(jcod)
  hydra.rewriting.deannotateTerm(field.term) match
    case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
      case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.ext.java.coder.withLambda(env)(v_Function_lambda_lam)((env2: hydra.ext.java.helpers.JavaEnvironment) =>
        {
        val lambdaParam: hydra.core.Name = (v_Function_lambda_lam.parameter)
        {
          val body: hydra.core.Term = (v_Function_lambda_lam.body)
          {
            val env3: hydra.ext.java.helpers.JavaEnvironment = hydra.ext.java.coder.insertBranchVar(lambdaParam)(env2)
            hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.FunctionStructure[hydra.ext.java.helpers.JavaEnvironment],
               hydra.ext.java.syntax.ClassBodyDeclarationWithComments](hydra.ext.java.coder.analyzeJavaFunction(env3)(body)(cx)(g))((fs: hydra.typing.FunctionStructure[hydra.ext.java.helpers.JavaEnvironment]) =>
              {
              val bindings: Seq[hydra.core.Binding] = (fs.bindings)
              {
                val innerBody: hydra.core.Term = (fs.body)
                {
                  val env4: hydra.ext.java.helpers.JavaEnvironment = (fs.environment)
                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.ext.java.syntax.BlockStatement],
                     hydra.ext.java.helpers.JavaEnvironment], hydra.ext.java.syntax.ClassBodyDeclarationWithComments](hydra.ext.java.coder.bindingsToStatements(env4)(bindings)(cx)(g))((bindResult: Tuple2[Seq[hydra.ext.java.syntax.BlockStatement],
                     hydra.ext.java.helpers.JavaEnvironment]) =>
                    {
                    val bindingStmts: Seq[hydra.ext.java.syntax.BlockStatement] = hydra.lib.pairs.first[Seq[hydra.ext.java.syntax.BlockStatement],
                       hydra.ext.java.helpers.JavaEnvironment](bindResult)
                    {
                      val env5: hydra.ext.java.helpers.JavaEnvironment = hydra.lib.pairs.second[Seq[hydra.ext.java.syntax.BlockStatement],
                         hydra.ext.java.helpers.JavaEnvironment](bindResult)
                      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
                         hydra.ext.java.syntax.ClassBodyDeclarationWithComments](hydra.ext.java.coder.encodeTerm(env5)(innerBody)(cx)(g))((jret: hydra.ext.java.syntax.Expression) =>
                        {
                        val param: hydra.ext.java.syntax.FormalParameter = hydra.ext.java.utils.javaTypeToJavaFormalParameter(jdom)(lambdaParam)
                        {
                          val returnStmt: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(jret)))
                          {
                            val allStmts: Seq[hydra.ext.java.syntax.BlockStatement] = hydra.lib.lists.concat2[hydra.ext.java.syntax.BlockStatement](bindingStmts)(Seq(returnStmt))
                            Right(hydra.ext.java.coder.noComment(hydra.ext.java.utils.methodDeclaration(mods)(Seq())(anns)(hydra.ext.java.names.visitMethodName)(Seq(param))(result)(Some(allStmts))))
                          }
                        }
                      })
                    }
                  })
                }
              }
            })
          }
        }
      })
      case _ => Left(hydra.context.InContext(hydra.error.Error.other(hydra.lib.strings.cat2("visitBranch: field term is not a lambda: ")(hydra.show.core.term(field.term))),
         cx))
    case _ => Left(hydra.context.InContext(hydra.error.Error.other(hydra.lib.strings.cat2("visitBranch: field term is not a lambda: ")(hydra.show.core.term(field.term))),
       cx))
}

def encodeElimination(env: hydra.ext.java.helpers.JavaEnvironment)(marg: Option[hydra.ext.java.syntax.Expression])(dom: hydra.core.Type)(cod: hydra.core.Type)(elm: hydra.core.Elimination)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.Expression] =
  {
  val aliases: hydra.ext.java.helpers.Aliases = (env.aliases)
  elm match
    case hydra.core.Elimination.record(v_Elimination_record_proj) => {
      val fname: hydra.core.Name = (v_Elimination_record_proj.field)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type, hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(dom)(cx)(g))((jdom0: hydra.ext.java.syntax.Type) =>
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
           hydra.ext.java.syntax.Expression](hydra.ext.java.utils.javaTypeToJavaReferenceType(jdom0)(cx))((jdomr: hydra.ext.java.syntax.ReferenceType) =>
        hydra.lib.maybes.cases[hydra.ext.java.syntax.Expression, Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](marg)({
        val projVar: hydra.core.Name = "projected"
        {
          val jbody: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaExpressionNameToJavaExpression(hydra.ext.java.utils.fieldExpression(hydra.ext.java.utils.variableToJavaIdentifier(projVar))(hydra.ext.java.utils.javaIdentifier(fname)))
          Right(hydra.ext.java.utils.javaLambda(projVar)(jbody))
        }
      })((jarg: hydra.ext.java.syntax.Expression) =>
        {
        val qual: hydra.ext.java.syntax.FieldAccess_Qualifier = hydra.ext.java.syntax.FieldAccess_Qualifier.primary(hydra.ext.java.utils.javaExpressionToJavaPrimary(jarg))
        Right(hydra.ext.java.utils.javaFieldAccessToJavaExpression(hydra.ext.java.syntax.FieldAccess(qual, hydra.ext.java.utils.javaIdentifier(fname))))
      })))
    }
    case hydra.core.Elimination.union(v_Elimination_union_cs) => {
      val tname: hydra.core.Name = (v_Elimination_union_cs.typeName)
      {
        val `def_`: Option[hydra.core.Term] = (v_Elimination_union_cs.default)
        {
          val fields: Seq[hydra.core.Field] = (v_Elimination_union_cs.cases)
          hydra.lib.maybes.cases[hydra.ext.java.syntax.Expression, Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](marg)({
            val uVar: hydra.core.Name = "u"
            {
              val typedLambda: hydra.core.Term = hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(uVar,
                 Some(dom), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.function(hydra.core.Function.elimination(elm)),
                 hydra.core.Term.variable(uVar))))))
              hydra.ext.java.coder.encodeTerm(env)(typedLambda)(cx)(g)
            }
          })((jarg: hydra.ext.java.syntax.Expression) =>
            {
            val prim: hydra.ext.java.syntax.Primary = hydra.ext.java.utils.javaExpressionToJavaPrimary(jarg)
            {
              val consId: hydra.ext.java.syntax.Identifier = hydra.ext.java.coder.innerClassRef(aliases)(tname)(hydra.ext.java.names.partialVisitorName)
              hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type,
                 hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(cod)(cx)(g))((jcod: hydra.ext.java.syntax.Type) =>
                hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
                   hydra.ext.java.syntax.Expression](hydra.ext.java.utils.javaTypeToJavaReferenceType(jcod)(cx))((rt: hydra.ext.java.syntax.ReferenceType) =>
                hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.TypeArgument],
                   hydra.ext.java.syntax.Expression](hydra.ext.java.coder.domTypeArgs(aliases)(dom)(cx)(g))((domArgs: Seq[hydra.ext.java.syntax.TypeArgument]) =>
                {
                val targs: hydra.ext.java.syntax.TypeArgumentsOrDiamond = hydra.ext.java.coder.typeArgsOrDiamond(hydra.lib.lists.concat2[hydra.ext.java.syntax.TypeArgument](domArgs)(Seq(hydra.ext.java.syntax.TypeArgument.reference(rt))))
                hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments],
                   hydra.ext.java.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Term, Either[hydra.context.InContext[hydra.error.Error],
                   Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments]]](`def_`)(Right(Seq()))((d: hydra.core.Term) =>
                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ClassBodyDeclarationWithComments,
                     Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments]](hydra.ext.java.coder.otherwiseBranch(env)(aliases)(dom)(cod)(tname)(jcod)(domArgs)(d)(cx)(g))((b: hydra.ext.java.syntax.ClassBodyDeclarationWithComments) => Right(Seq(b)))))((otherwiseBranches: Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments]) =>
                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments],
                     hydra.ext.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Field, hydra.ext.java.syntax.ClassBodyDeclarationWithComments,
                     hydra.context.InContext[hydra.error.Error]]((f: hydra.core.Field) =>
                  hydra.ext.java.coder.visitBranch(env)(aliases)(dom)(tname)(jcod)(domArgs)(f)(cx)(g))(fields))((visitBranches: Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments]) =>
                  {
                  val body: hydra.ext.java.syntax.ClassBody = hydra.lib.lists.concat2[hydra.ext.java.syntax.ClassBodyDeclarationWithComments](otherwiseBranches)(visitBranches)
                  {
                    val visitor: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaConstructorCall(hydra.ext.java.utils.javaConstructorName(consId)(Some(targs)))(Seq())(Some(body))
                    Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocation(Some(Right(prim)))(hydra.ext.java.names.acceptMethodName)(Seq(visitor))))
                  }
                }))
              })))
            }
          })
        }
      }
    }
    case hydra.core.Elimination.wrap(v_Elimination_wrap_wrapName) => {
      def withArg(ja: hydra.ext.java.syntax.Expression): hydra.ext.java.syntax.Expression =
        hydra.ext.java.utils.javaFieldAccessToJavaExpression(hydra.ext.java.syntax.FieldAccess(hydra.ext.java.syntax.FieldAccess_Qualifier.primary(hydra.ext.java.utils.javaExpressionToJavaPrimary(ja)),
           hydra.ext.java.utils.javaIdentifier(hydra.ext.java.names.valueFieldName)))
      Right(hydra.lib.maybes.cases[hydra.ext.java.syntax.Expression, hydra.ext.java.syntax.Expression](marg)({
        val wVar: hydra.core.Name = "wrapped"
        {
          val wArg: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaIdentifierToJavaExpression(hydra.ext.java.utils.variableToJavaIdentifier(wVar))
          hydra.ext.java.utils.javaLambda(wVar)(withArg(wArg))
        }
      })((jarg: hydra.ext.java.syntax.Expression) => withArg(jarg)))
    }
    case _ => Left(hydra.context.InContext(hydra.error.Error.other(hydra.lib.strings.cat2("unexpected ")(hydra.lib.strings.cat2("elimination case")(hydra.lib.strings.cat2(" in ")("encodeElimination")))),
       cx))
}

def toDeclInit(aliasesExt: hydra.ext.java.helpers.Aliases)(gExt: hydra.graph.Graph)(recursiveVars: scala.collection.immutable.Set[hydra.core.Name])(flatBindings: Seq[hydra.core.Binding])(name: hydra.core.Name)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   Option[hydra.ext.java.syntax.BlockStatement]] =
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Option[hydra.ext.java.syntax.BlockStatement]]](hydra.lib.sets.member[hydra.core.Name](name)(recursiveVars))({
  val binding: hydra.core.Binding = hydra.lib.lists.head[hydra.core.Binding](hydra.lib.lists.filter[hydra.core.Binding]((b: hydra.core.Binding) => hydra.lib.equality.equal[hydra.core.Name](b.name)(name))(flatBindings))
  {
    val value: hydra.core.Term = (binding.term)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, Option[hydra.ext.java.syntax.BlockStatement]](hydra.lib.maybes.cases[hydra.core.TypeScheme,
       Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type]](binding.`type`)(hydra.coderUtils.typeOfTerm(cx)(gExt)(value))((ts: hydra.core.TypeScheme) => Right(ts.`type`)))((typ: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type, Option[hydra.ext.java.syntax.BlockStatement]](hydra.ext.java.coder.encodeType(aliasesExt)(hydra.lib.sets.empty[hydra.core.Name])(typ)(cx)(g))((jtype: hydra.ext.java.syntax.Type) =>
      {
      val id: hydra.ext.java.syntax.Identifier = hydra.ext.java.utils.variableToJavaIdentifier(name)
      {
        val arid: hydra.ext.java.syntax.Identifier = "java.util.concurrent.atomic.AtomicReference"
        {
          val aid: hydra.ext.java.syntax.AnnotatedIdentifier = hydra.ext.java.syntax.AnnotatedIdentifier(Seq(), arid)
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
             Option[hydra.ext.java.syntax.BlockStatement]](hydra.ext.java.utils.javaTypeToJavaReferenceType(jtype)(cx))((rt: hydra.ext.java.syntax.ReferenceType) =>
            {
            val targs: hydra.ext.java.syntax.TypeArgumentsOrDiamond = hydra.ext.java.coder.typeArgsOrDiamond(Seq(hydra.ext.java.syntax.TypeArgument.reference(rt)))
            {
              val ci: hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate = hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate(Seq(aid), Some(targs))
              {
                val body: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaConstructorCall(ci)(Seq())(None)
                {
                  val pkg: hydra.ext.java.syntax.PackageName = hydra.ext.java.names.javaPackageName(Seq("java", "util", "concurrent", "atomic"))
                  {
                    val artype: hydra.ext.java.syntax.Type = hydra.ext.java.utils.javaRefType(Seq(rt))(Some(pkg))("AtomicReference")
                    Right(Some(hydra.ext.java.utils.variableDeclarationStatement(aliasesExt)(artype)(id)(body)))
                  }
                }
              }
            }
          })
        }
      }
    }))
  }
})(Right(None))

def toDeclStatement(envExt: hydra.ext.java.helpers.JavaEnvironment)(aliasesExt: hydra.ext.java.helpers.Aliases)(gExt: hydra.graph.Graph)(recursiveVars: scala.collection.immutable.Set[hydra.core.Name])(thunkedVars: scala.collection.immutable.Set[hydra.core.Name])(flatBindings: Seq[hydra.core.Binding])(name: hydra.core.Name)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.BlockStatement] =
  {
  val binding: hydra.core.Binding = hydra.lib.lists.head[hydra.core.Binding](hydra.lib.lists.filter[hydra.core.Binding]((b: hydra.core.Binding) => hydra.lib.equality.equal[hydra.core.Name](b.name)(name))(flatBindings))
  val value: hydra.core.Term = (binding.term)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, hydra.ext.java.syntax.BlockStatement](hydra.lib.maybes.cases[hydra.core.TypeScheme,
     Either[hydra.context.InContext[hydra.error.Error], hydra.core.Type]](binding.`type`)(hydra.coderUtils.typeOfTerm(cx)(gExt)(value))((ts: hydra.core.TypeScheme) => Right(ts.`type`)))((typ: hydra.core.Type) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type, hydra.ext.java.syntax.BlockStatement](hydra.ext.java.coder.encodeType(aliasesExt)(hydra.lib.sets.empty[hydra.core.Name])(typ)(cx)(g))((jtype: hydra.ext.java.syntax.Type) =>
    {
    val id: hydra.ext.java.syntax.Identifier = hydra.ext.java.utils.variableToJavaIdentifier(name)
    {
      val annotatedValue: hydra.core.Term = hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(typ)))(value)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
         hydra.ext.java.syntax.BlockStatement](hydra.ext.java.coder.encodeTerm(envExt)(annotatedValue)(cx)(g))((rhs: hydra.ext.java.syntax.Expression) =>
        hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.BlockStatement]](hydra.lib.sets.member[hydra.core.Name](name)(recursiveVars))(Right(hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaMethodInvocationToJavaStatement(hydra.ext.java.utils.methodInvocation(Some(Left(hydra.ext.java.syntax.ExpressionName(None,
           id))))(hydra.ext.java.names.setMethodName)(Seq(rhs))))))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
           hydra.ext.java.syntax.BlockStatement]](hydra.lib.sets.member[hydra.core.Name](name)(thunkedVars))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
           hydra.ext.java.syntax.ReferenceType, hydra.ext.java.syntax.BlockStatement](hydra.ext.java.utils.javaTypeToJavaReferenceType(jtype)(cx))((rt: hydra.ext.java.syntax.ReferenceType) =>
        {
        val lazyType: hydra.ext.java.syntax.Type = hydra.ext.java.utils.javaRefType(Seq(rt))(hydra.ext.java.names.hydraUtilPackageName)("Lazy")
        {
          val lambdaBody: hydra.ext.java.syntax.LambdaBody = hydra.ext.java.syntax.LambdaBody.expression(rhs)
          {
            val supplierLambda: hydra.ext.java.syntax.Expression = hydra.ext.java.syntax.Expression.lambda(hydra.ext.java.syntax.LambdaExpression(hydra.ext.java.syntax.LambdaParameters.tuple(Seq()),
               lambdaBody))
            {
              val targs: hydra.ext.java.syntax.TypeArgumentsOrDiamond = hydra.ext.java.coder.typeArgsOrDiamond(Seq(hydra.ext.java.syntax.TypeArgument.reference(rt)))
              {
                val lazyExpr: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaConstructorCall(hydra.ext.java.utils.javaConstructorName("hydra.util.Lazy")(Some(targs)))(Seq(supplierLambda))(None)
                Right(hydra.ext.java.utils.variableDeclarationStatement(aliasesExt)(lazyType)(id)(lazyExpr))
              }
            }
          }
        }
      }))(Right(hydra.ext.java.utils.variableDeclarationStatement(aliasesExt)(jtype)(id)(rhs)))))
    }
  }))
}

def bindingsToStatements(env: hydra.ext.java.helpers.JavaEnvironment)(bindings: Seq[hydra.core.Binding])(cx: hydra.context.Context)(g0: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[Seq[hydra.ext.java.syntax.BlockStatement], hydra.ext.java.helpers.JavaEnvironment]] =
  {
  val aliases: hydra.ext.java.helpers.Aliases = (env.aliases)
  val g: hydra.graph.Graph = (env.graph)
  val flatBindings: Seq[hydra.core.Binding] = hydra.ext.java.coder.dedupBindings(aliases.inScopeJavaVars)(hydra.ext.java.coder.flattenBindings(bindings))
  val gExtended: hydra.graph.Graph = hydra.schemas.extendGraphForLet(hydra.coderUtils.bindingMetadata)(g)(hydra.core.Let(flatBindings,
     hydra.core.Term.variable("dummy")))
  val bindingVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.map[hydra.core.Binding,
     hydra.core.Name]((b: hydra.core.Binding) => (b.name))(flatBindings))
  val allDeps: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.core.Name]] = hydra.lib.maps.fromList[hydra.core.Name,
     scala.collection.immutable.Set[hydra.core.Name]](hydra.lib.lists.map[hydra.core.Binding, Tuple2[hydra.core.Name,
     scala.collection.immutable.Set[hydra.core.Name]]]((b: hydra.core.Binding) =>
    {
    val key: hydra.core.Name = (b.name)
    {
      val deps: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.intersection[hydra.core.Name](bindingVars)(hydra.rewriting.freeVariablesInTerm(b.term))
      Tuple2(key, deps)
    }
  })(flatBindings))
  val sorted: Seq[Seq[hydra.core.Name]] = hydra.sorting.topologicalSortComponents(hydra.lib.lists.map[Tuple2[hydra.core.Name,
     scala.collection.immutable.Set[hydra.core.Name]], Tuple2[hydra.core.Name, Seq[hydra.core.Name]]]((entry: Tuple2[hydra.core.Name,
     scala.collection.immutable.Set[hydra.core.Name]]) =>
    {
    val key: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, scala.collection.immutable.Set[hydra.core.Name]](entry)
    {
      val deps: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.pairs.second[hydra.core.Name,
         scala.collection.immutable.Set[hydra.core.Name]](entry)
      Tuple2(key, hydra.lib.sets.toList[hydra.core.Name](deps))
    }
  })(hydra.lib.maps.toList[hydra.core.Name, scala.collection.immutable.Set[hydra.core.Name]](allDeps)))
  val recursiveVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.concat[hydra.core.Name](hydra.lib.lists.map[Seq[hydra.core.Name],
     Seq[hydra.core.Name]]((names: Seq[hydra.core.Name]) =>
    hydra.lib.logic.ifElse[Seq[hydra.core.Name]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Name](names))(1))({
    val singleName: hydra.core.Name = hydra.lib.lists.head[hydra.core.Name](names)
    hydra.lib.maybes.cases[scala.collection.immutable.Set[hydra.core.Name], Seq[hydra.core.Name]](hydra.lib.maps.lookup[hydra.core.Name,
       scala.collection.immutable.Set[hydra.core.Name]](singleName)(allDeps))(Seq())((deps: scala.collection.immutable.Set[hydra.core.Name]) =>
      hydra.lib.logic.ifElse[Seq[hydra.core.Name]](hydra.lib.sets.member[hydra.core.Name](singleName)(deps))(Seq(singleName))(Seq()))
  })(names))(sorted)))
  val thunkedVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.concat[hydra.core.Name](hydra.lib.lists.map[hydra.core.Binding,
     Seq[hydra.core.Name]]((b: hydra.core.Binding) =>
    {
    val bname: hydra.core.Name = (b.name)
    hydra.lib.logic.ifElse[Seq[hydra.core.Name]](hydra.lib.logic.and(hydra.lib.logic.not(hydra.lib.sets.member[hydra.core.Name](bname)(recursiveVars)))(hydra.lib.logic.and(hydra.ext.java.coder.needsThunking(b.term))(hydra.lib.logic.not(hydra.ext.java.coder.bindingIsFunctionType(b)))))(Seq(bname))(Seq())
  })(flatBindings)))
  val aliasesExtended: hydra.ext.java.helpers.Aliases = hydra.ext.java.helpers.Aliases(aliases.currentNamespace,
     (aliases.packages), (aliases.branchVars), hydra.lib.sets.union[hydra.core.Name](aliases.recursiveVars)(recursiveVars),
     (aliases.inScopeTypeParams), (aliases.polymorphicLocals), hydra.lib.sets.union[hydra.core.Name](aliases.inScopeJavaVars)(bindingVars),
     (aliases.varRenames), (aliases.lambdaVars), (aliases.typeVarSubst), (aliases.trustedTypeVars), (aliases.methodCodomain),
     hydra.lib.sets.union[hydra.core.Name](aliases.thunkedVars)(thunkedVars))
  val envExtended: hydra.ext.java.helpers.JavaEnvironment = hydra.ext.java.helpers.JavaEnvironment(aliasesExtended, gExtended)
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.ext.java.syntax.BlockStatement],
     hydra.ext.java.helpers.JavaEnvironment]]](hydra.lib.lists.`null`[hydra.core.Binding](bindings))(Right(Tuple2(Seq(),
     envExtended)))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[Seq[hydra.ext.java.syntax.BlockStatement]],
     Tuple2[Seq[hydra.ext.java.syntax.BlockStatement], hydra.ext.java.helpers.JavaEnvironment]](hydra.lib.eithers.mapList[Seq[hydra.core.Name],
     Seq[hydra.ext.java.syntax.BlockStatement], hydra.context.InContext[hydra.error.Error]]((names: Seq[hydra.core.Name]) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[Option[hydra.ext.java.syntax.BlockStatement]],
       Seq[hydra.ext.java.syntax.BlockStatement]](hydra.lib.eithers.mapList[hydra.core.Name, Option[hydra.ext.java.syntax.BlockStatement],
       hydra.context.InContext[hydra.error.Error]]((n: hydra.core.Name) =>
    hydra.ext.java.coder.toDeclInit(aliasesExtended)(gExtended)(recursiveVars)(flatBindings)(n)(cx)(g))(names))((inits: Seq[Option[hydra.ext.java.syntax.BlockStatement]]) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.BlockStatement],
       Seq[hydra.ext.java.syntax.BlockStatement]](hydra.lib.eithers.mapList[hydra.core.Name, hydra.ext.java.syntax.BlockStatement,
       hydra.context.InContext[hydra.error.Error]]((n: hydra.core.Name) =>
    hydra.ext.java.coder.toDeclStatement(envExtended)(aliasesExtended)(gExtended)(recursiveVars)(thunkedVars)(flatBindings)(n)(cx)(g))(names))((decls: Seq[hydra.ext.java.syntax.BlockStatement]) =>
    Right(hydra.lib.lists.concat2[hydra.ext.java.syntax.BlockStatement](hydra.lib.maybes.cat[hydra.ext.java.syntax.BlockStatement](inits))(decls)))))(sorted))((groups: Seq[Seq[hydra.ext.java.syntax.BlockStatement]]) =>
    Right(Tuple2(hydra.lib.lists.concat[hydra.ext.java.syntax.BlockStatement](groups), envExtended))))
}

def toClassDecl(isInner: Boolean)(isSer: Boolean)(aliases: hydra.ext.java.helpers.Aliases)(tparams: Seq[hydra.ext.java.syntax.TypeParameter])(elName: hydra.core.Name)(t: hydra.core.Type)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.ClassDeclaration] =
  {
  def wrap(`t_`: hydra.core.Type): Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ClassDeclaration] =
    hydra.ext.java.coder.declarationForRecordType(isInner)(isSer)(aliases)(tparams)(elName)(Seq(hydra.core.FieldType("value",
       hydra.rewriting.deannotateType(`t_`))))(cx)(g)
  hydra.rewriting.deannotateType(t) match
    case hydra.core.Type.record(v_Type_record_rt) => hydra.ext.java.coder.declarationForRecordType(isInner)(isSer)(aliases)(tparams)(elName)(v_Type_record_rt)(cx)(g)
    case hydra.core.Type.union(v_Type_union_rt) => hydra.ext.java.coder.declarationForUnionType(isSer)(aliases)(tparams)(elName)(v_Type_union_rt)(cx)(g)
    case hydra.core.Type.forall(v_Type_forall_fa) => {
      val v: hydra.core.Name = (v_Type_forall_fa.parameter)
      {
        val body: hydra.core.Type = (v_Type_forall_fa.body)
        {
          val param: hydra.ext.java.syntax.TypeParameter = hydra.ext.java.utils.javaTypeParameter(hydra.formatting.capitalize(v))
          hydra.ext.java.coder.toClassDecl(false)(isSer)(aliases)(hydra.lib.lists.concat2[hydra.ext.java.syntax.TypeParameter](tparams)(Seq(param)))(elName)(body)(cx)(g)
        }
      }
    }
    case hydra.core.Type.wrap(v_Type_wrap_wt) => hydra.ext.java.coder.declarationForRecordType(isInner)(isSer)(aliases)(tparams)(elName)(Seq(hydra.core.FieldType("value",
       v_Type_wrap_wt)))(cx)(g)
    case _ => wrap(t)
}

def declarationForUnionType(isSer: Boolean)(aliases: hydra.ext.java.helpers.Aliases)(tparams: Seq[hydra.ext.java.syntax.TypeParameter])(elName: hydra.core.Name)(fields: Seq[hydra.core.FieldType])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.ClassDeclaration] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.ClassDeclaration],
     hydra.ext.java.syntax.ClassDeclaration](hydra.lib.eithers.mapList[hydra.core.FieldType, hydra.ext.java.syntax.ClassDeclaration,
     hydra.context.InContext[hydra.error.Error]]((ft: hydra.core.FieldType) =>
  {
  val fname: hydra.core.Name = (ft.name)
  {
    val ftype: hydra.core.Type = (ft.`type`)
    {
      val rfields: Seq[hydra.core.FieldType] = hydra.lib.logic.ifElse[Seq[hydra.core.FieldType]](hydra.schemas.isUnitType(hydra.rewriting.deannotateType(ftype)))(Seq())(Seq(hydra.core.FieldType("value",
         hydra.rewriting.deannotateType(ftype))))
      {
        val varName: hydra.core.Name = hydra.ext.java.utils.variantClassName(false)(elName)(fname)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ClassDeclaration,
           hydra.ext.java.syntax.ClassDeclaration](hydra.ext.java.coder.`declarationForRecordType_`(true)(isSer)(aliases)(Seq())(varName)(hydra.lib.logic.ifElse[Option[hydra.core.Name]](isSer)(Some(elName))(None))(rfields)(cx)(g))((innerDecl: hydra.ext.java.syntax.ClassDeclaration) =>
          Right(hydra.ext.java.coder.augmentVariantClass(aliases)(tparams)(elName)(innerDecl)))
      }
    }
  }
})(fields))((variantClasses: Seq[hydra.ext.java.syntax.ClassDeclaration]) =>
  {
  val variantDecls: Seq[hydra.ext.java.syntax.ClassBodyDeclaration] = hydra.lib.lists.map[hydra.ext.java.syntax.ClassDeclaration,
     hydra.ext.java.syntax.ClassBodyDeclaration]((vc: hydra.ext.java.syntax.ClassDeclaration) =>
    hydra.ext.java.syntax.ClassBodyDeclaration.classMember(hydra.ext.java.syntax.ClassMemberDeclaration.`class`(vc)))(variantClasses)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments],
     hydra.ext.java.syntax.ClassDeclaration](hydra.lib.eithers.mapList[Tuple2[hydra.ext.java.syntax.ClassBodyDeclaration,
     hydra.core.FieldType], hydra.ext.java.syntax.ClassBodyDeclarationWithComments, hydra.context.InContext[hydra.error.Error]]((pair: Tuple2[hydra.ext.java.syntax.ClassBodyDeclaration,
     hydra.core.FieldType]) =>
    hydra.ext.java.coder.addComment(hydra.lib.pairs.first[hydra.ext.java.syntax.ClassBodyDeclaration,
       hydra.core.FieldType](pair))(hydra.lib.pairs.second[hydra.ext.java.syntax.ClassBodyDeclaration,
       hydra.core.FieldType](pair))(cx)(g))(hydra.lib.lists.zip[hydra.ext.java.syntax.ClassBodyDeclaration,
       hydra.core.FieldType](variantDecls)(fields)))((`variantDecls_`: Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments]) =>
    {
    val privateConst: hydra.ext.java.syntax.ClassBodyDeclaration = hydra.ext.java.utils.makeConstructor(aliases)(elName)(true)(Seq())(Seq())
    {
      val acceptDecl: hydra.ext.java.syntax.ClassBodyDeclaration = hydra.ext.java.utils.toAcceptMethod(true)(tparams)
      {
        val vtparams: Seq[hydra.ext.java.syntax.TypeParameter] = hydra.lib.lists.concat2[hydra.ext.java.syntax.TypeParameter](tparams)(Seq(hydra.ext.java.utils.javaTypeParameter(hydra.ext.java.names.visitorReturnParameter)))
        {
          val visitorMethods: Seq[hydra.ext.java.syntax.InterfaceMemberDeclaration] = hydra.lib.lists.map[hydra.core.FieldType,
             hydra.ext.java.syntax.InterfaceMemberDeclaration]((ft: hydra.core.FieldType) =>
            {
            val fname: hydra.core.Name = (ft.name)
            {
              val typeArgs: Seq[hydra.ext.java.syntax.TypeArgument] = hydra.lib.lists.map[hydra.ext.java.syntax.TypeParameter,
                 hydra.ext.java.syntax.TypeArgument]((tp: hydra.ext.java.syntax.TypeParameter) => hydra.ext.java.utils.typeParameterToTypeArgument(tp))(tparams)
              {
                val varRef: hydra.ext.java.syntax.Type = hydra.ext.java.utils.javaClassTypeToJavaType(hydra.ext.java.utils.nameToJavaClassType(aliases)(false)(typeArgs)(hydra.ext.java.utils.variantClassName(false)(elName)(fname))(None))
                {
                  val param: hydra.ext.java.syntax.FormalParameter = hydra.ext.java.utils.javaTypeToJavaFormalParameter(varRef)("instance")
                  {
                    val resultR: hydra.ext.java.syntax.Result = hydra.ext.java.utils.javaTypeToJavaResult(hydra.ext.java.syntax.Type.reference(hydra.ext.java.utils.visitorTypeVariable))
                    hydra.ext.java.utils.interfaceMethodDeclaration(Seq())(Seq())(hydra.ext.java.names.visitMethodName)(Seq(param))(resultR)(None)
                  }
                }
              }
            }
          })(fields)
          {
            val visitorBody: hydra.ext.java.syntax.InterfaceBody = visitorMethods
            {
              val visitor: hydra.ext.java.syntax.ClassBodyDeclaration = hydra.ext.java.utils.javaInterfaceDeclarationToJavaClassBodyDeclaration(hydra.ext.java.syntax.NormalInterfaceDeclaration(Seq(hydra.ext.java.syntax.InterfaceModifier.public),
                 hydra.ext.java.names.visitorName, vtparams, Seq(), visitorBody))
              {
                val typeArgs: Seq[hydra.ext.java.syntax.TypeArgument] = hydra.lib.lists.map[hydra.ext.java.syntax.TypeParameter,
                   hydra.ext.java.syntax.TypeArgument]((tp: hydra.ext.java.syntax.TypeParameter) => hydra.ext.java.utils.typeParameterToTypeArgument(tp))(tparams)
                {
                  val visitorClassType: hydra.ext.java.syntax.ClassType = hydra.ext.java.utils.javaClassType(hydra.lib.lists.concat2[hydra.ext.java.syntax.ReferenceType](hydra.lib.lists.map[hydra.ext.java.syntax.TypeParameter,
                     hydra.ext.java.syntax.ReferenceType]((tp: hydra.ext.java.syntax.TypeParameter) => hydra.ext.java.utils.typeParameterToReferenceType(tp))(tparams))(Seq(hydra.ext.java.utils.visitorTypeVariable)))(None)(hydra.ext.java.names.visitorName)
                  {
                    val mainInstanceParam: hydra.ext.java.syntax.FormalParameter = hydra.ext.java.utils.javaTypeToJavaFormalParameter(hydra.ext.java.utils.javaClassTypeToJavaType(hydra.ext.java.utils.nameToJavaClassType(aliases)(false)(typeArgs)(elName)(None)))("instance")
                    {
                      val resultR: hydra.ext.java.syntax.Result = hydra.ext.java.utils.javaTypeToJavaResult(hydra.ext.java.syntax.Type.reference(hydra.ext.java.utils.visitorTypeVariable))
                      {
                        val throwStmt: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaThrowIllegalStateException(Seq(hydra.ext.java.utils.javaAdditiveExpressionToJavaExpression(hydra.ext.java.utils.addExpressions(Seq(hydra.ext.java.utils.javaStringMultiplicativeExpression("Non-exhaustive patterns when matching: "),
                           hydra.ext.java.syntax.MultiplicativeExpression.unary(hydra.ext.java.utils.javaIdentifierToJavaUnaryExpression("instance"))))))))
                        {
                          val defaultMod: Seq[hydra.ext.java.syntax.InterfaceMethodModifier] = Seq(hydra.ext.java.syntax.InterfaceMethodModifier.default)
                          {
                            val otherwiseDecl: hydra.ext.java.syntax.InterfaceMemberDeclaration = hydra.ext.java.utils.interfaceMethodDeclaration(defaultMod)(Seq())(hydra.ext.java.names.otherwiseMethodName)(Seq(mainInstanceParam))(resultR)(Some(Seq(throwStmt)))
                            {
                              val pvVisitMethods: Seq[hydra.ext.java.syntax.InterfaceMemberDeclaration] = hydra.lib.lists.map[hydra.core.FieldType,
                                 hydra.ext.java.syntax.InterfaceMemberDeclaration]((ft: hydra.core.FieldType) =>
                                {
                                val fname: hydra.core.Name = (ft.name)
                                {
                                  val varRef: hydra.ext.java.syntax.Type = hydra.ext.java.utils.javaClassTypeToJavaType(hydra.ext.java.utils.nameToJavaClassType(aliases)(false)(typeArgs)(hydra.ext.java.utils.variantClassName(false)(elName)(fname))(None))
                                  {
                                    val param: hydra.ext.java.syntax.FormalParameter = hydra.ext.java.utils.javaTypeToJavaFormalParameter(varRef)("instance")
                                    {
                                      val mi: hydra.ext.java.syntax.MethodInvocation = hydra.ext.java.utils.methodInvocation(None)(hydra.ext.java.names.otherwiseMethodName)(Seq(hydra.ext.java.utils.javaIdentifierToJavaExpression("instance")))
                                      {
                                        val returnOtherwise: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(hydra.ext.java.utils.javaPrimaryToJavaExpression(hydra.ext.java.utils.javaMethodInvocationToJavaPrimary(mi)))))
                                        hydra.ext.java.utils.interfaceMethodDeclaration(defaultMod)(Seq())(hydra.ext.java.names.visitMethodName)(Seq(param))(resultR)(Some(Seq(returnOtherwise)))
                                      }
                                    }
                                  }
                                }
                              })(fields)
                              {
                                val pvBody: hydra.ext.java.syntax.InterfaceBody = hydra.lib.lists.concat2[hydra.ext.java.syntax.InterfaceMemberDeclaration](Seq(otherwiseDecl))(pvVisitMethods)
                                {
                                  val partialVisitor: hydra.ext.java.syntax.ClassBodyDeclaration = hydra.ext.java.utils.javaInterfaceDeclarationToJavaClassBodyDeclaration(hydra.ext.java.syntax.NormalInterfaceDeclaration(Seq(hydra.ext.java.syntax.InterfaceModifier.public),
                                     hydra.ext.java.names.partialVisitorName, vtparams, Seq(visitorClassType),
                                     pvBody))
                                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ClassBodyDeclarationWithComments,
                                     hydra.ext.java.syntax.ClassDeclaration](hydra.ext.java.coder.constantDeclForTypeName(aliases)(elName)(cx)(g))((tn0: hydra.ext.java.syntax.ClassBodyDeclarationWithComments) =>
                                    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                                       Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments], hydra.ext.java.syntax.ClassDeclaration](hydra.lib.eithers.mapList[hydra.core.FieldType,
                                       hydra.ext.java.syntax.ClassBodyDeclarationWithComments, hydra.context.InContext[hydra.error.Error]]((ft: hydra.core.FieldType) =>
                                    hydra.ext.java.coder.constantDeclForFieldType(aliases)(ft)(cx)(g))(fields))((tn1: Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments]) =>
                                    {
                                    val tn: Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments] = hydra.lib.lists.concat2[hydra.ext.java.syntax.ClassBodyDeclarationWithComments](Seq(tn0))(tn1)
                                    {
                                      val otherDecls: Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments] = hydra.lib.lists.map[hydra.ext.java.syntax.ClassBodyDeclaration,
                                         hydra.ext.java.syntax.ClassBodyDeclarationWithComments]((d: hydra.ext.java.syntax.ClassBodyDeclaration) => hydra.ext.java.coder.noComment(d))(Seq(privateConst,
                                         acceptDecl, visitor, partialVisitor))
                                      {
                                        val bodyDecls: Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments] = hydra.lib.lists.concat[hydra.ext.java.syntax.ClassBodyDeclarationWithComments](Seq(tn,
                                           otherDecls, `variantDecls_`))
                                        {
                                          val mods: Seq[hydra.ext.java.syntax.ClassModifier] = hydra.lib.lists.concat2[hydra.ext.java.syntax.ClassModifier](hydra.ext.java.coder.classModsPublic)(Seq(hydra.ext.java.syntax.ClassModifier.`abstract`))
                                          Right(hydra.ext.java.utils.javaClassDeclaration(aliases)(tparams)(elName)(mods)(None)(hydra.ext.java.coder.interfaceTypes(isSer)(aliases)(tparams)(elName))(bodyDecls))
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
              }
            }
          }
        }
      }
    }
  })
})

def augmentVariantClass(aliases: hydra.ext.java.helpers.Aliases)(tparams: Seq[hydra.ext.java.syntax.TypeParameter])(elName: hydra.core.Name)(cd: hydra.ext.java.syntax.ClassDeclaration): hydra.ext.java.syntax.ClassDeclaration =
  cd match
  case hydra.ext.java.syntax.ClassDeclaration.normal(v_ClassDeclaration_normal_ncd) => {
    val args: Seq[hydra.ext.java.syntax.TypeArgument] = hydra.lib.lists.map[hydra.ext.java.syntax.TypeParameter,
       hydra.ext.java.syntax.TypeArgument]((tp: hydra.ext.java.syntax.TypeParameter) => hydra.ext.java.utils.typeParameterToTypeArgument(tp))(tparams)
    {
      val extendsPart: hydra.ext.java.syntax.ClassType = hydra.ext.java.utils.nameToJavaClassType(aliases)(true)(args)(elName)(None)
      {
        val newMods: Seq[hydra.ext.java.syntax.ClassModifier] = Seq(hydra.ext.java.syntax.ClassModifier.public,
           hydra.ext.java.syntax.ClassModifier.static, hydra.ext.java.syntax.ClassModifier.`final`)
        {
          val oldBody: hydra.ext.java.syntax.ClassBody = (v_ClassDeclaration_normal_ncd.body)
          {
            val oldDecls: Seq[hydra.ext.java.syntax.ClassBodyDeclarationWithComments] = oldBody
            {
              val acceptDecl: hydra.ext.java.syntax.ClassBodyDeclarationWithComments = hydra.ext.java.coder.noComment(hydra.ext.java.utils.toAcceptMethod(false)(tparams))
              {
                val newBody: hydra.ext.java.syntax.ClassBody = hydra.lib.lists.concat2[hydra.ext.java.syntax.ClassBodyDeclarationWithComments](oldDecls)(Seq(acceptDecl))
                hydra.ext.java.syntax.ClassDeclaration.normal(hydra.ext.java.syntax.NormalClassDeclaration(newMods,
                   (v_ClassDeclaration_normal_ncd.identifier), tparams, Some(extendsPart), (v_ClassDeclaration_normal_ncd.implements),
                   newBody))
              }
            }
          }
        }
      }
    }
  }
  case _ => cd

def encodeTypeDefinition(pkg: hydra.ext.java.syntax.PackageDeclaration)(aliases: hydra.ext.java.helpers.Aliases)(tdef: hydra.module.TypeDefinition)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   Tuple2[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit]] =
  {
  val name: hydra.core.Name = (tdef.name)
  val typ: hydra.core.Type = (tdef.`type`)
  val serializable: Boolean = hydra.ext.java.coder.isSerializableJavaType(typ)
  val imports: Seq[hydra.ext.java.syntax.ImportDeclaration] = hydra.lib.logic.ifElse[Seq[hydra.ext.java.syntax.ImportDeclaration]](serializable)(Seq(hydra.ext.java.syntax.ImportDeclaration.singleType(hydra.ext.java.utils.javaTypeName("java.io.Serializable"))))(Seq())
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ClassDeclaration,
     Tuple2[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit]](hydra.ext.java.coder.toClassDecl(false)(serializable)(aliases)(Seq())(name)(typ)(cx)(g))((decl: hydra.ext.java.syntax.ClassDeclaration) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Option[scala.Predef.String], Tuple2[hydra.core.Name,
       hydra.ext.java.syntax.CompilationUnit]](hydra.annotations.getTypeDescription(cx)(g)(typ))((comment: Option[scala.Predef.String]) =>
    {
    val tdecl: hydra.ext.java.syntax.TypeDeclarationWithComments = hydra.ext.java.syntax.TypeDeclarationWithComments(hydra.ext.java.syntax.TypeDeclaration.`class`(decl),
       comment)
    Right(Tuple2(name, hydra.ext.java.syntax.CompilationUnit.ordinary(hydra.ext.java.syntax.OrdinaryCompilationUnit(Some(pkg), imports, Seq(tdecl)))))
  }))
}

def peelDomainsAndCod(n: Int)(t: hydra.core.Type): Tuple2[Seq[hydra.core.Type], hydra.core.Type] =
  hydra.lib.logic.ifElse[Tuple2[Seq[hydra.core.Type], hydra.core.Type]](hydra.lib.equality.lte[Int](n)(0))(Tuple2(Seq(),
     t))(hydra.rewriting.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => {
    val rest: Tuple2[Seq[hydra.core.Type], hydra.core.Type] = hydra.ext.java.coder.peelDomainsAndCod(hydra.lib.math.sub(n)(1))(v_Type_function_ft.codomain)
    Tuple2(hydra.lib.lists.cons[hydra.core.Type](v_Type_function_ft.domain)(hydra.lib.pairs.first[Seq[hydra.core.Type],
       hydra.core.Type](rest)), hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.core.Type](rest))
  }
  case _ => Tuple2(Seq(), t))

def isSerializableJavaType(typ: hydra.core.Type): Boolean =
  hydra.rewriting.deannotateType(typ) match
  case hydra.core.Type.record(v_Type_record_rt) => true
  case hydra.core.Type.union(v_Type_union_rt) => true
  case hydra.core.Type.wrap(v_Type_wrap_wt) => true
  case hydra.core.Type.forall(v_Type_forall_fa) => hydra.ext.java.coder.isSerializableJavaType(v_Type_forall_fa.body)
  case _ => false

def correctCastType[T0, T1, T2](innerBody: hydra.core.Term)(typeArgs: Seq[hydra.core.Type])(fallback: hydra.core.Type)(cx: T0)(g: T1): Either[T2,
   hydra.core.Type] =
  hydra.rewriting.deannotateTerm(innerBody) match
  case hydra.core.Term.pair(v_Term_pair__p) => hydra.lib.logic.ifElse[Either[T2, hydra.core.Type]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Type](typeArgs))(2))(Right(hydra.core.Type.pair(hydra.core.PairType(hydra.lib.lists.head[hydra.core.Type](typeArgs),
     hydra.lib.lists.head[hydra.core.Type](hydra.lib.lists.tail[hydra.core.Type](typeArgs))))))(Right(fallback))
  case _ => Right(fallback)

def typeAppFallbackCast(env: hydra.ext.java.helpers.JavaEnvironment)(aliases: hydra.ext.java.helpers.Aliases)(anns: Seq[Map[hydra.core.Name,
   hydra.core.Term]])(tyapps: Seq[hydra.ext.java.syntax.Type])(jatyp: hydra.ext.java.syntax.Type)(body: hydra.core.Term)(typ: hydra.core.Type)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.Expression] =
  {
  val annotatedBody: hydra.core.Term = hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(typ)))(body)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
     hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeTermInternal(env)(anns)(hydra.lib.lists.cons[hydra.ext.java.syntax.Type](jatyp)(tyapps))(annotatedBody)(cx)(g))((jbody: hydra.ext.java.syntax.Expression) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type, hydra.ext.java.syntax.Expression](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(typ)(cx)(g))((jtype: hydra.ext.java.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
       hydra.ext.java.syntax.Expression](hydra.ext.java.utils.javaTypeToJavaReferenceType(jtype)(cx))((rt: hydra.ext.java.syntax.ReferenceType) =>
    Right(hydra.ext.java.utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.javaCastExpression(rt)(hydra.ext.java.utils.javaExpressionToJavaUnaryExpression(jbody)))))))
}

def typeAppNullaryOrHoisted(env: hydra.ext.java.helpers.JavaEnvironment)(aliases: hydra.ext.java.helpers.Aliases)(anns: Seq[Map[hydra.core.Name,
   hydra.core.Term]])(tyapps: Seq[hydra.ext.java.syntax.Type])(jatyp: hydra.ext.java.syntax.Type)(body: hydra.core.Term)(correctedTyp: hydra.core.Type)(varName: hydra.core.Name)(cls: hydra.ext.java.helpers.JavaSymbolClass)(allTypeArgs: Seq[hydra.core.Type])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.Expression] =
  {
  val qn: hydra.module.QualifiedName = hydra.names.qualifyName(varName)
  val mns: Option[hydra.module.Namespace] = (qn.namespace)
  val localName: scala.Predef.String = (qn.local)
  cls match
    case hydra.ext.java.helpers.JavaSymbolClass.nullaryFunction => hydra.lib.maybes.cases[hydra.module.Namespace,
       Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](mns)(hydra.ext.java.coder.typeAppFallbackCast(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(cx)(g))((`ns_`: hydra.module.Namespace) =>
      {
      val classId: hydra.ext.java.syntax.Identifier = hydra.ext.java.utils.nameToJavaName(aliases)(hydra.names.unqualifyName(hydra.module.QualifiedName(Some(`ns_`),
         hydra.ext.java.coder.elementsClassName(`ns_`))))
      {
        val methodId: hydra.ext.java.syntax.Identifier = hydra.ext.java.utils.sanitizeJavaName(localName)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.Type], hydra.ext.java.syntax.Expression](hydra.ext.java.coder.filterPhantomTypeArgs(varName)(allTypeArgs)(cx)(g))((filteredTypeArgs: Seq[hydra.core.Type]) =>
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.TypeArgument],
             hydra.ext.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Type, hydra.ext.java.syntax.TypeArgument,
             hydra.context.InContext[hydra.error.Error]]((t: hydra.core.Type) =>
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type,
             hydra.ext.java.syntax.TypeArgument](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(t)(cx)(g))((jt: hydra.ext.java.syntax.Type) =>
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
             hydra.ext.java.syntax.TypeArgument](hydra.ext.java.utils.javaTypeToJavaReferenceType(jt)(cx))((rt: hydra.ext.java.syntax.ReferenceType) => Right(hydra.ext.java.syntax.TypeArgument.reference(rt)))))(filteredTypeArgs))((jTypeArgs: Seq[hydra.ext.java.syntax.TypeArgument]) =>
          Right(hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStaticWithTypeArgs(classId)(methodId)(jTypeArgs)(Seq())))))
      }
    })
    case hydra.ext.java.helpers.JavaSymbolClass.hoistedLambda(v_JavaSymbolClass_hoistedLambda_arity) => hydra.lib.maybes.cases[hydra.module.Namespace,
       Either[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression]](mns)(hydra.ext.java.coder.typeAppFallbackCast(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(cx)(g))((`ns_`: hydra.module.Namespace) =>
      {
      val classId: hydra.ext.java.syntax.Identifier = hydra.ext.java.utils.nameToJavaName(aliases)(hydra.names.unqualifyName(hydra.module.QualifiedName(Some(`ns_`),
         hydra.ext.java.coder.elementsClassName(`ns_`))))
      {
        val methodId: hydra.ext.java.syntax.Identifier = hydra.ext.java.utils.sanitizeJavaName(localName)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.Type], hydra.ext.java.syntax.Expression](hydra.ext.java.coder.filterPhantomTypeArgs(varName)(allTypeArgs)(cx)(g))((filteredTypeArgs: Seq[hydra.core.Type]) =>
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.TypeArgument],
             hydra.ext.java.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Type, hydra.ext.java.syntax.TypeArgument,
             hydra.context.InContext[hydra.error.Error]]((t: hydra.core.Type) =>
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Type,
             hydra.ext.java.syntax.TypeArgument](hydra.ext.java.coder.encodeType(aliases)(hydra.lib.sets.empty[hydra.core.Name])(t)(cx)(g))((jt: hydra.ext.java.syntax.Type) =>
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.ReferenceType,
             hydra.ext.java.syntax.TypeArgument](hydra.ext.java.utils.javaTypeToJavaReferenceType(jt)(cx))((rt: hydra.ext.java.syntax.ReferenceType) => Right(hydra.ext.java.syntax.TypeArgument.reference(rt)))))(filteredTypeArgs))((jTypeArgs: Seq[hydra.ext.java.syntax.TypeArgument]) =>
          {
          val paramNames: Seq[hydra.core.Name] = hydra.lib.lists.map[Int, hydra.core.Name]((i: Int) => hydra.lib.strings.cat2("p")(hydra.lib.literals.showInt32(i)))(hydra.lib.math.range(0)(hydra.lib.math.sub(v_JavaSymbolClass_hoistedLambda_arity)(1)))
          {
            val paramExprs: Seq[hydra.ext.java.syntax.Expression] = hydra.lib.lists.map[hydra.core.Name,
               hydra.ext.java.syntax.Expression]((p: hydra.core.Name) =>
              hydra.ext.java.utils.javaIdentifierToJavaExpression(hydra.ext.java.utils.variableToJavaIdentifier(p)))(paramNames)
            {
              val call: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaMethodInvocationToJavaExpression(hydra.ext.java.utils.methodInvocationStaticWithTypeArgs(classId)(methodId)(jTypeArgs)(paramExprs))
              Right(hydra.ext.java.coder.buildCurriedLambda(paramNames)(call))
            }
          }
        }))
      }
    })
    case _ => hydra.ext.java.coder.typeAppFallbackCast(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(cx)(g)
}

def flattenApps(t: hydra.core.Term)(acc: Seq[hydra.core.Term]): Tuple2[Seq[hydra.core.Term], hydra.core.Term] =
  hydra.rewriting.deannotateTerm(t) match
  case hydra.core.Term.application(v_Term_application_app) => hydra.ext.java.coder.flattenApps(v_Term_application_app.function)(hydra.lib.lists.cons[hydra.core.Term](v_Term_application_app.argument)(acc))
  case _ => Tuple2(acc, t)

def collectLambdaDomains(t: hydra.core.Term): Tuple2[Seq[hydra.core.Type], hydra.core.Term] =
  hydra.rewriting.deannotateTerm(t) match
  case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
    case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.lib.maybes.cases[hydra.core.Type,
       Tuple2[Seq[hydra.core.Type], hydra.core.Term]](v_Function_lambda_lam.domain)(Tuple2(Seq(), t))((dom: hydra.core.Type) =>
      {
      val rest: Tuple2[Seq[hydra.core.Type], hydra.core.Term] = hydra.ext.java.coder.collectLambdaDomains(v_Function_lambda_lam.body)
      Tuple2(hydra.lib.lists.cons[hydra.core.Type](dom)(hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.core.Term](rest)),
         hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.core.Term](rest))
    })
    case _ => Tuple2(Seq(), t)
  case _ => Tuple2(Seq(), t)

def rebuildApps(f: hydra.core.Term)(args: Seq[hydra.core.Term])(fType: hydra.core.Type): hydra.core.Term =
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.lists.`null`[hydra.core.Term](args))(f)(hydra.rewriting.deannotateType(fType) match
  case hydra.core.Type.function(v_Type_function_ft) => {
    val arg: hydra.core.Term = hydra.lib.lists.head[hydra.core.Term](args)
    {
      val rest: Seq[hydra.core.Term] = hydra.lib.lists.tail[hydra.core.Term](args)
      {
        val remainingType: hydra.core.Type = (v_Type_function_ft.codomain)
        {
          val app: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(f, arg))
          {
            val annotatedApp: hydra.core.Term = hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(remainingType)))(app)
            hydra.ext.java.coder.rebuildApps(annotatedApp)(rest)(remainingType)
          }
        }
      }
    }
  }
  case _ => hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Term]((acc: hydra.core.Term) =>
    (a: hydra.core.Term) => hydra.core.Term.application(hydra.core.Application(acc, a)))(f)(args))

def propagateTypesInAppChain(fixedCod: hydra.core.Type)(resultType: hydra.core.Type)(t: hydra.core.Term): hydra.core.Term =
  {
  val flattened: Tuple2[Seq[hydra.core.Term], hydra.core.Term] = hydra.ext.java.coder.flattenApps(t)(Seq())
  val args: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], hydra.core.Term](flattened)
  val fun: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Term], hydra.core.Term](flattened)
  val lambdaDomsResult: Tuple2[Seq[hydra.core.Type], hydra.core.Term] = hydra.ext.java.coder.collectLambdaDomains(fun)
  val lambdaDoms: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.core.Term](lambdaDomsResult)
  val nArgs: Int = hydra.lib.lists.length[hydra.core.Term](args)
  val nLambdaDoms: Int = hydra.lib.lists.length[hydra.core.Type](lambdaDoms)
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.logic.and(hydra.lib.equality.gt[Int](nLambdaDoms)(0))(hydra.lib.equality.gt[Int](nArgs)(0)))({
    val bodyRetType: hydra.core.Type = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.core.Type](hydra.ext.java.coder.peelDomainsAndCod(hydra.lib.math.sub(nLambdaDoms)(nArgs))(resultType))
    {
      val funType: hydra.core.Type = hydra.lib.lists.foldl[hydra.core.Type, hydra.core.Type]((c: hydra.core.Type) =>
        (d: hydra.core.Type) => hydra.core.Type.function(hydra.core.FunctionType(d, c)))(bodyRetType)(hydra.lib.lists.reverse[hydra.core.Type](lambdaDoms))
      {
        val annotatedFun: hydra.core.Term = hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(funType)))(fun)
        hydra.ext.java.coder.rebuildApps(annotatedFun)(args)(funType)
      }
    }
  })(hydra.rewriting.deannotateTerm(t) match
    case hydra.core.Term.application(v_Term_application_app) => {
      val lhs: hydra.core.Term = (v_Term_application_app.function)
      {
        val rhs: hydra.core.Term = (v_Term_application_app.argument)
        {
          val annotatedLhs: hydra.core.Term = hydra.rewriting.deannotateTerm(lhs) match
            case hydra.core.Term.function(v_Term_function_fn) => v_Term_function_fn match
              case hydra.core.Function.elimination(v_Function_elimination_elim) => v_Function_elimination_elim match
                case hydra.core.Elimination.union(v_Elimination_union_cs) => {
                  val dom: hydra.core.Type = hydra.schemas.nominalApplication(v_Elimination_union_cs.typeName)(Seq())
                  {
                    val ft: hydra.core.Type = hydra.core.Type.function(hydra.core.FunctionType(dom, fixedCod))
                    hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(ft)))(lhs)
                  }
                }
                case _ => lhs
              case _ => lhs
            case _ => lhs
          hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(resultType)))(hydra.core.Term.application(hydra.core.Application(annotatedLhs,
             rhs)))
        }
      }
    }
    case _ => hydra.annotations.setTermAnnotation(hydra.constants.key_type)(Some(hydra.encode.core.`type`(resultType)))(t))
}

def encodeTermTCO(env0: hydra.ext.java.helpers.JavaEnvironment)(funcName: hydra.core.Name)(paramNames: Seq[hydra.core.Name])(tcoVarRenames: Map[hydra.core.Name,
   hydra.core.Name])(tcoDepth: Int)(term: hydra.core.Term)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   Seq[hydra.ext.java.syntax.BlockStatement]] =
  {
  val aliases0: hydra.ext.java.helpers.Aliases = (env0.aliases)
  val env: hydra.ext.java.helpers.JavaEnvironment = hydra.ext.java.helpers.JavaEnvironment(hydra.ext.java.helpers.Aliases(aliases0.currentNamespace,
     (aliases0.packages), (aliases0.branchVars), (aliases0.recursiveVars), (aliases0.inScopeTypeParams),
     (aliases0.polymorphicLocals), (aliases0.inScopeJavaVars), hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](tcoVarRenames)(aliases0.varRenames),
     (aliases0.lambdaVars), (aliases0.typeVarSubst), (aliases0.trustedTypeVars), (aliases0.methodCodomain),
     (aliases0.thunkedVars)), (env0.graph))
  val stripped: hydra.core.Term = hydra.rewriting.deannotateAndDetypeTerm(term)
  val gathered: Tuple2[Seq[hydra.core.Term], hydra.core.Term] = hydra.coderUtils.gatherApplications(stripped)
  val gatherArgs: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], hydra.core.Term](gathered)
  val gatherFun: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Term], hydra.core.Term](gathered)
  val strippedFun: hydra.core.Term = hydra.rewriting.deannotateAndDetypeTerm(gatherFun)
  val isSelfCall: Boolean = strippedFun match
    case hydra.core.Term.variable(v_Term_variable_n) => hydra.lib.equality.equal[hydra.core.Name](v_Term_variable_n)(funcName)
    case _ => false
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.BlockStatement]]](hydra.lib.logic.and(isSelfCall)(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Term](gatherArgs))(hydra.lib.lists.length[hydra.core.Name](paramNames))))({
    val changePairs: Seq[Tuple2[hydra.core.Name, hydra.core.Term]] = hydra.lib.lists.filter[Tuple2[hydra.core.Name,
       hydra.core.Term]]((pair: Tuple2[hydra.core.Name, hydra.core.Term]) =>
      hydra.lib.logic.not(hydra.rewriting.deannotateAndDetypeTerm(hydra.lib.pairs.second[hydra.core.Name, hydra.core.Term](pair)) match
      case hydra.core.Term.variable(v_Term_variable_n) => hydra.lib.equality.equal[hydra.core.Name](v_Term_variable_n)(hydra.lib.pairs.first[hydra.core.Name,
         hydra.core.Term](pair))
      case _ => false))(hydra.lib.lists.zip[hydra.core.Name, hydra.core.Term](paramNames)(gatherArgs))
    {
      val changedParams: Seq[hydra.core.Name] = hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.Term],
         hydra.core.Name](hydra.lib.pairs.first[hydra.core.Name, hydra.core.Term])(changePairs)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.Expression],
         Seq[hydra.ext.java.syntax.BlockStatement]](hydra.lib.eithers.mapList[Tuple2[hydra.core.Name,
         hydra.core.Term], hydra.ext.java.syntax.Expression, hydra.context.InContext[hydra.error.Error]]((pair: Tuple2[hydra.core.Name,
         hydra.core.Term]) =>
        hydra.ext.java.coder.encodeTerm(env)(hydra.lib.pairs.second[hydra.core.Name, hydra.core.Term](pair))(cx)(g))(changePairs))((jChangedArgs: Seq[hydra.ext.java.syntax.Expression]) =>
        {
        val assignments: Seq[hydra.ext.java.syntax.BlockStatement] = hydra.lib.lists.map[Tuple2[hydra.core.Name,
           hydra.ext.java.syntax.Expression], hydra.ext.java.syntax.BlockStatement]((pair: Tuple2[hydra.core.Name,
           hydra.ext.java.syntax.Expression]) =>
          {
          val paramName: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.ext.java.syntax.Expression](pair)
          {
            val jArg: hydra.ext.java.syntax.Expression = hydra.lib.pairs.second[hydra.core.Name, hydra.ext.java.syntax.Expression](pair)
            hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaAssignmentStatement(hydra.ext.java.syntax.LeftHandSide.expressionName(hydra.ext.java.utils.javaIdentifierToJavaExpressionName(hydra.ext.java.utils.variableToJavaIdentifier(paramName))))(jArg))
          }
        })(hydra.lib.lists.zip[hydra.core.Name, hydra.ext.java.syntax.Expression](changedParams)(jChangedArgs))
        {
          val continueStmt: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.syntax.Statement.withoutTrailing(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.continue(None)))
          Right(hydra.lib.lists.concat2[hydra.ext.java.syntax.BlockStatement](assignments)(Seq(continueStmt)))
        }
      })
    }
  })(stripped match
    case hydra.core.Term.let(v_Term_let_lt) => {
      val letBindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
      {
        val letBody: hydra.core.Term = (v_Term_let_lt.body)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Seq[hydra.ext.java.syntax.BlockStatement],
           hydra.ext.java.helpers.JavaEnvironment], Seq[hydra.ext.java.syntax.BlockStatement]](hydra.ext.java.coder.bindingsToStatements(env)(letBindings)(cx)(g))((bindResult: Tuple2[Seq[hydra.ext.java.syntax.BlockStatement],
           hydra.ext.java.helpers.JavaEnvironment]) =>
          {
          val letStmts: Seq[hydra.ext.java.syntax.BlockStatement] = hydra.lib.pairs.first[Seq[hydra.ext.java.syntax.BlockStatement],
             hydra.ext.java.helpers.JavaEnvironment](bindResult)
          {
            val envAfterLet: hydra.ext.java.helpers.JavaEnvironment = hydra.lib.pairs.second[Seq[hydra.ext.java.syntax.BlockStatement],
               hydra.ext.java.helpers.JavaEnvironment](bindResult)
            hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.BlockStatement],
               Seq[hydra.ext.java.syntax.BlockStatement]](hydra.ext.java.coder.encodeTermTCO(envAfterLet)(funcName)(paramNames)(tcoVarRenames)(tcoDepth)(letBody)(cx)(g))((tcoBodyStmts: Seq[hydra.ext.java.syntax.BlockStatement]) =>
              Right(hydra.lib.lists.concat2[hydra.ext.java.syntax.BlockStatement](letStmts)(tcoBodyStmts)))
          }
        })
      }
    }
    case _ => {
      val gathered2: Tuple2[Seq[hydra.core.Term], hydra.core.Term] = hydra.coderUtils.gatherApplications(term)
      {
        val args2: Seq[hydra.core.Term] = hydra.lib.pairs.first[Seq[hydra.core.Term], hydra.core.Term](gathered2)
        {
          val body2: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Term], hydra.core.Term](gathered2)
          hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.BlockStatement]]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Term](args2))(1))({
            val arg: hydra.core.Term = hydra.lib.lists.head[hydra.core.Term](args2)
            hydra.rewriting.deannotateAndDetypeTerm(body2) match
              case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
                case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
                  case hydra.core.Elimination.union(v_Elimination_union_cs) => {
                    val aliases: hydra.ext.java.helpers.Aliases = (env.aliases)
                    {
                      val tname: hydra.core.Name = (v_Elimination_union_cs.typeName)
                      {
                        val dflt: Option[hydra.core.Term] = (v_Elimination_union_cs.default)
                        {
                          val `cases_`: Seq[hydra.core.Field] = (v_Elimination_union_cs.cases)
                          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.TypeArgument],
                             Seq[hydra.ext.java.syntax.BlockStatement]](hydra.ext.java.coder.domTypeArgs(aliases)(hydra.schemas.nominalApplication(tname)(Seq()))(cx)(g))((domArgs: Seq[hydra.ext.java.syntax.TypeArgument]) =>
                            hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
                               Seq[hydra.ext.java.syntax.BlockStatement]](hydra.ext.java.coder.encodeTerm(env)(arg)(cx)(g))((jArgRaw: hydra.ext.java.syntax.Expression) =>
                            {
                            val depthSuffix: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](tcoDepth)(0))("")(hydra.lib.literals.showInt32(tcoDepth))
                            {
                              val matchVarId: hydra.ext.java.syntax.Identifier = hydra.ext.java.utils.javaIdentifier(hydra.lib.strings.cat(Seq("_tco_match_",
                                 hydra.formatting.decapitalize(hydra.names.localNameOf(tname)), depthSuffix)))
                              {
                                val matchDecl: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.utils.varDeclarationStatement(matchVarId)(jArgRaw)
                                {
                                  val jArg: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaIdentifierToJavaExpression(matchVarId)
                                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.BlockStatement],
                                     Seq[hydra.ext.java.syntax.BlockStatement]](hydra.lib.eithers.mapList[hydra.core.Field,
                                     hydra.ext.java.syntax.BlockStatement, hydra.context.InContext[hydra.error.Error]]((field: hydra.core.Field) =>
                                    {
                                    val fieldName: hydra.core.Name = (field.name)
                                    {
                                      val variantRefType: hydra.ext.java.syntax.ReferenceType = hydra.ext.java.utils.nameToJavaReferenceType(aliases)(true)(domArgs)(tname)(Some(hydra.formatting.capitalize(fieldName)))
                                      hydra.rewriting.deannotateTerm(field.term) match
                                        case hydra.core.Term.function(v_Term_function_f2) => v_Term_function_f2 match
                                          case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.ext.java.coder.withLambda(env)(v_Function_lambda_lam)((env2: hydra.ext.java.helpers.JavaEnvironment) =>
                                            {
                                            val lambdaParam: hydra.core.Name = (v_Function_lambda_lam.parameter)
                                            {
                                              val branchBody: hydra.core.Term = (v_Function_lambda_lam.body)
                                              {
                                                val env3: hydra.ext.java.helpers.JavaEnvironment = hydra.ext.java.coder.insertBranchVar(lambdaParam)(env2)
                                                {
                                                  val varId: hydra.ext.java.syntax.Identifier = hydra.ext.java.utils.variableToJavaIdentifier(lambdaParam)
                                                  {
                                                    val castExpr: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaCastExpressionToJavaExpression(hydra.ext.java.utils.javaCastExpression(variantRefType)(hydra.ext.java.utils.javaExpressionToJavaUnaryExpression(jArg)))
                                                    {
                                                      val localDecl: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.utils.varDeclarationStatement(varId)(castExpr)
                                                      {
                                                        val isBranchTailCall: Boolean = hydra.coderUtils.isTailRecursiveInTailPosition(funcName)(branchBody)
                                                        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                                                           Seq[hydra.ext.java.syntax.BlockStatement],
                                                           hydra.ext.java.syntax.BlockStatement](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
                                                           Seq[hydra.ext.java.syntax.BlockStatement]]](isBranchTailCall)(hydra.ext.java.coder.encodeTermTCO(env3)(funcName)(paramNames)(tcoVarRenames)(hydra.lib.math.add(tcoDepth)(1))(branchBody)(cx)(g))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                                                           hydra.typing.FunctionStructure[hydra.ext.java.helpers.JavaEnvironment],
                                                           Seq[hydra.ext.java.syntax.BlockStatement]](hydra.ext.java.coder.analyzeJavaFunction(env3)(branchBody)(cx)(g))((fs: hydra.typing.FunctionStructure[hydra.ext.java.helpers.JavaEnvironment]) =>
                                                          {
                                                          val bindings: Seq[hydra.core.Binding] = (fs.bindings)
                                                          {
                                                            val innerBody: hydra.core.Term = (fs.body)
                                                            {
                                                              val env4: hydra.ext.java.helpers.JavaEnvironment = (fs.environment)
                                                              hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                                                                 Tuple2[Seq[hydra.ext.java.syntax.BlockStatement],
                                                                 hydra.ext.java.helpers.JavaEnvironment],
                                                                 Seq[hydra.ext.java.syntax.BlockStatement]](hydra.ext.java.coder.bindingsToStatements(env4)(bindings)(cx)(g))((bindResult: Tuple2[Seq[hydra.ext.java.syntax.BlockStatement],
                                                                 hydra.ext.java.helpers.JavaEnvironment]) =>
                                                                {
                                                                val bindingStmts: Seq[hydra.ext.java.syntax.BlockStatement] = hydra.lib.pairs.first[Seq[hydra.ext.java.syntax.BlockStatement],
                                                                   hydra.ext.java.helpers.JavaEnvironment](bindResult)
                                                                {
                                                                  val env5: hydra.ext.java.helpers.JavaEnvironment = hydra.lib.pairs.second[Seq[hydra.ext.java.syntax.BlockStatement],
                                                                     hydra.ext.java.helpers.JavaEnvironment](bindResult)
                                                                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                                                                     hydra.ext.java.syntax.Expression,
                                                                     Seq[hydra.ext.java.syntax.BlockStatement]](hydra.ext.java.coder.encodeTerm(env5)(innerBody)(cx)(g))((jret: hydra.ext.java.syntax.Expression) =>
                                                                    {
                                                                    val returnStmt: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(jret)))
                                                                    Right(hydra.lib.lists.concat2[hydra.ext.java.syntax.BlockStatement](bindingStmts)(Seq(returnStmt)))
                                                                  })
                                                                }
                                                              })
                                                            }
                                                          }
                                                        })))((bodyStmts: Seq[hydra.ext.java.syntax.BlockStatement]) =>
                                                          {
                                                          val relExpr: hydra.ext.java.syntax.RelationalExpression = hydra.ext.java.utils.javaInstanceOf(hydra.ext.java.utils.javaUnaryExpressionToJavaRelationalExpression(hydra.ext.java.utils.javaExpressionToJavaUnaryExpression(jArg)))(variantRefType)
                                                          {
                                                            val condExpr: hydra.ext.java.syntax.Expression = hydra.ext.java.utils.javaRelationalExpressionToJavaExpression(relExpr)
                                                            {
                                                              val blockStmts: Seq[hydra.ext.java.syntax.BlockStatement] = hydra.lib.lists.cons[hydra.ext.java.syntax.BlockStatement](localDecl)(bodyStmts)
                                                              {
                                                                val ifBody: hydra.ext.java.syntax.Statement = hydra.ext.java.syntax.Statement.withoutTrailing(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.block(blockStmts))
                                                                Right(hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.syntax.Statement.ifThen(hydra.ext.java.syntax.IfThenStatement(condExpr,
                                                                   ifBody))))
                                                              }
                                                            }
                                                          }
                                                        })
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          })
                                          case _ => Left(hydra.context.InContext(hydra.error.Error.other("TCO: case branch is not a lambda"), cx))
                                        case _ => Left(hydra.context.InContext(hydra.error.Error.other("TCO: case branch is not a lambda"), cx))
                                    }
                                  })(`cases_`))((ifBlocks: Seq[hydra.ext.java.syntax.BlockStatement]) =>
                                    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                                       Seq[hydra.ext.java.syntax.BlockStatement], Seq[hydra.ext.java.syntax.BlockStatement]](hydra.lib.maybes.cases[hydra.core.Term,
                                       Either[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.java.syntax.BlockStatement]]](dflt)(Right(Seq(hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(jArg))))))((d: hydra.core.Term) =>
                                    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                                       hydra.ext.java.syntax.Expression, Seq[hydra.ext.java.syntax.BlockStatement]](hydra.ext.java.coder.encodeTerm(env)(d)(cx)(g))((dExpr: hydra.ext.java.syntax.Expression) =>
                                    Right(Seq(hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(dExpr))))))))((defaultStmt: Seq[hydra.ext.java.syntax.BlockStatement]) =>
                                    Right(hydra.lib.lists.concat[hydra.ext.java.syntax.BlockStatement](Seq(Seq(matchDecl), ifBlocks, defaultStmt)))))
                                }
                              }
                            }
                          }))
                        }
                      }
                    }
                  }
                  case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
                     Seq[hydra.ext.java.syntax.BlockStatement]](hydra.ext.java.coder.encodeTerm(env)(term)(cx)(g))((expr: hydra.ext.java.syntax.Expression) =>
                    Right(Seq(hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(expr))))))
                case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
                   Seq[hydra.ext.java.syntax.BlockStatement]](hydra.ext.java.coder.encodeTerm(env)(term)(cx)(g))((expr: hydra.ext.java.syntax.Expression) =>
                  Right(Seq(hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(expr))))))
              case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
                 Seq[hydra.ext.java.syntax.BlockStatement]](hydra.ext.java.coder.encodeTerm(env)(term)(cx)(g))((expr: hydra.ext.java.syntax.Expression) =>
                Right(Seq(hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(expr))))))
          })(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.java.syntax.Expression,
             Seq[hydra.ext.java.syntax.BlockStatement]](hydra.ext.java.coder.encodeTerm(env)(term)(cx)(g))((expr: hydra.ext.java.syntax.Expression) =>
            Right(Seq(hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(expr)))))))
        }
      }
    })
}

def encodeTermDefinition(env: hydra.ext.java.helpers.JavaEnvironment)(tdef: hydra.module.TermDefinition)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.java.syntax.InterfaceMemberDeclaration] =
  {
  val name: hydra.core.Name = (tdef.name)
  val term0: hydra.core.Term = (tdef.term)
  val ts: hydra.core.TypeScheme = (tdef.`type`)
  val term: hydra.core.Term = hydra.rewriting.unshadowVariables(term0)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.typing.FunctionStructure[hydra.ext.java.helpers.JavaEnvironment],
     hydra.ext.java.syntax.InterfaceMemberDeclaration](hydra.ext.java.coder.analyzeJavaFunction(env)(term)(cx)(g))((fs: hydra.typing.FunctionStructure[hydra.ext.java.helpers.JavaEnvironment]) =>
    {
    val schemeVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) => hydra.ext.java.coder.isSimpleName(v))(ts.variables)
    {
      val termVars: Seq[hydra.core.Name] = (fs.typeParams)
      {
        val schemeTypeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.ext.java.coder.collectTypeVars(ts.`type`)
        {
          val usedSchemeVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) => hydra.lib.sets.member[hydra.core.Name](v)(schemeTypeVars))(schemeVars)
          {
            val tparams: Seq[hydra.core.Name] = hydra.lib.logic.ifElse[Seq[hydra.core.Name]](hydra.lib.lists.`null`[hydra.core.Name](usedSchemeVars))(termVars)(usedSchemeVars)
            {
              val params: Seq[hydra.core.Name] = (fs.params)
              {
                val bindings: Seq[hydra.core.Binding] = (fs.bindings)
                {
                  val body: hydra.core.Term = (fs.body)
                  {
                    val doms: Seq[hydra.core.Type] = (fs.domains)
                    {
                      val env2: hydra.ext.java.helpers.JavaEnvironment = (fs.environment)
                      {
                        val schemeType: hydra.core.Type = (ts.`type`)
                        {
                          val numParams: Int = hydra.lib.lists.length[hydra.core.Name](params)
                          {
                            val peelResult: Tuple2[Seq[hydra.core.Type], hydra.core.Type] = hydra.ext.java.coder.peelDomainsAndCod(numParams)(schemeType)
                            {
                              val schemeDoms: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.core.Type](peelResult)
                              {
                                val cod: hydra.core.Type = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.core.Type](peelResult)
                                {
                                  val schemeVarSet: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](tparams)
                                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Map[hydra.core.Name,
                                     hydra.core.Name], hydra.ext.java.syntax.InterfaceMemberDeclaration](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
                                     Map[hydra.core.Name, hydra.core.Name]]](hydra.lib.lists.`null`[hydra.core.Name](tparams))(Right(hydra.lib.maps.empty[hydra.core.Name,
                                     hydra.core.Name]))(hydra.ext.java.coder.buildSubstFromAnnotations(schemeVarSet)(term)(cx)(g)))((typeVarSubst: Map[hydra.core.Name,
                                     hydra.core.Name]) =>
                                    {
                                    val overgenSubst: Map[hydra.core.Name, hydra.core.Type] = hydra.ext.java.coder.detectAccumulatorUnification(schemeDoms)(cod)(tparams)
                                    {
                                      val overgenVarSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maps.fromList[hydra.core.Name,
                                         hydra.core.Name](hydra.lib.maybes.cat[Tuple2[hydra.core.Name,
                                         hydra.core.Name]](hydra.lib.lists.map[Tuple2[hydra.core.Name,
                                         hydra.core.Type], Option[Tuple2[hydra.core.Name, hydra.core.Name]]]((entry: Tuple2[hydra.core.Name,
                                         hydra.core.Type]) =>
                                        {
                                        val k: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.core.Type](entry)
                                        {
                                          val v: hydra.core.Type = hydra.lib.pairs.second[hydra.core.Name, hydra.core.Type](entry)
                                          v match
                                            case hydra.core.Type.variable(v_Type_variable_n) => Some(Tuple2(k, v_Type_variable_n))
                                            case _ => None
                                        }
                                      })(hydra.lib.maps.toList[hydra.core.Name, hydra.core.Type](overgenSubst))))
                                      {
                                        val fixedCod: hydra.core.Type = hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.maps.`null`[hydra.core.Name,
                                           hydra.core.Type](overgenSubst))(cod)(hydra.ext.java.coder.substituteTypeVarsWithTypes(overgenSubst)(cod))
                                        {
                                          val fixedDoms: Seq[hydra.core.Type] = hydra.lib.logic.ifElse[Seq[hydra.core.Type]](hydra.lib.maps.`null`[hydra.core.Name,
                                             hydra.core.Type](overgenSubst))(schemeDoms)(hydra.lib.lists.map[hydra.core.Type,
                                             hydra.core.Type]((d: hydra.core.Type) =>
                                            hydra.ext.java.coder.substituteTypeVarsWithTypes(overgenSubst)(d))(schemeDoms))
                                          {
                                            val fixedTparams: Seq[hydra.core.Name] = hydra.lib.logic.ifElse[Seq[hydra.core.Name]](hydra.lib.maps.`null`[hydra.core.Name,
                                               hydra.core.Type](overgenSubst))(tparams)(hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) =>
                                              hydra.lib.logic.not(hydra.lib.maps.member[hydra.core.Name, hydra.core.Type](v)(overgenSubst)))(tparams))
                                            {
                                              val constraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.lib.maybes.fromMaybe[Map[hydra.core.Name,
                                                 hydra.core.TypeVariableMetadata]](hydra.lib.maps.empty[hydra.core.Name,
                                                 hydra.core.TypeVariableMetadata])(ts.constraints)
                                              {
                                                val jparams: Seq[hydra.ext.java.syntax.TypeParameter] = hydra.lib.lists.map[hydra.core.Name,
                                                   hydra.ext.java.syntax.TypeParameter]((v: hydra.core.Name) =>
                                                  hydra.ext.java.utils.javaTypeParameter(hydra.formatting.capitalize(v)))(fixedTparams)
                                                {
                                                  val aliases2base: hydra.ext.java.helpers.Aliases = (env2.aliases)
                                                  {
                                                    val trustedVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.unions[hydra.core.Name](hydra.lib.lists.map[hydra.core.Type,
                                                       scala.collection.immutable.Set[hydra.core.Name]]((d: hydra.core.Type) => hydra.ext.java.coder.collectTypeVars(d))(hydra.lib.lists.concat2[hydra.core.Type](fixedDoms)(Seq(fixedCod))))
                                                    {
                                                      val fixedSchemeVarSet: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](fixedTparams)
                                                      {
                                                        val aliases2: hydra.ext.java.helpers.Aliases = hydra.ext.java.helpers.Aliases(aliases2base.currentNamespace,
                                                           (aliases2base.packages), (aliases2base.branchVars),
                                                           (aliases2base.recursiveVars), fixedSchemeVarSet,
                                                           (aliases2base.polymorphicLocals), (aliases2base.inScopeJavaVars),
                                                           (aliases2base.varRenames), hydra.lib.sets.union[hydra.core.Name](aliases2base.lambdaVars)(hydra.lib.sets.fromList[hydra.core.Name](params)),
                                                           hydra.lib.maps.union[hydra.core.Name, hydra.core.Name](overgenVarSubst)(typeVarSubst),
                                                           hydra.lib.sets.intersection[hydra.core.Name](trustedVars)(fixedSchemeVarSet),
                                                           Some(fixedCod), (aliases2base.thunkedVars))
                                                        {
                                                          val env2WithTypeParams: hydra.ext.java.helpers.JavaEnvironment = hydra.ext.java.helpers.JavaEnvironment(aliases2,
                                                             (env2.graph))
                                                          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                                                             Tuple2[Seq[hydra.ext.java.syntax.BlockStatement],
                                                             hydra.ext.java.helpers.JavaEnvironment],
                                                             hydra.ext.java.syntax.InterfaceMemberDeclaration](hydra.ext.java.coder.bindingsToStatements(env2WithTypeParams)(bindings)(cx)(g))((bindResult: Tuple2[Seq[hydra.ext.java.syntax.BlockStatement],
                                                             hydra.ext.java.helpers.JavaEnvironment]) =>
                                                            {
                                                            val bindingStmts: Seq[hydra.ext.java.syntax.BlockStatement] = hydra.lib.pairs.first[Seq[hydra.ext.java.syntax.BlockStatement],
                                                               hydra.ext.java.helpers.JavaEnvironment](bindResult)
                                                            {
                                                              val env3: hydra.ext.java.helpers.JavaEnvironment = hydra.lib.pairs.second[Seq[hydra.ext.java.syntax.BlockStatement],
                                                                 hydra.ext.java.helpers.JavaEnvironment](bindResult)
                                                              hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                                                                 hydra.core.Term, hydra.ext.java.syntax.InterfaceMemberDeclaration](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
                                                                 hydra.core.Term]](hydra.lib.maps.`null`[hydra.core.Name,
                                                                 hydra.core.Type](overgenSubst))(Right(body))(hydra.ext.java.coder.applyOvergenSubstToTermAnnotations(overgenSubst)(body)(cx)(g)))((`body_`: hydra.core.Term) =>
                                                                {
                                                                val annotatedBody: hydra.core.Term = hydra.ext.java.coder.propagateTypesInAppChain(fixedCod)(fixedCod)(`body_`)
                                                                hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                                                                   Seq[hydra.ext.java.syntax.FormalParameter],
                                                                   hydra.ext.java.syntax.InterfaceMemberDeclaration](hydra.lib.eithers.mapList[Tuple2[hydra.core.Type,
                                                                   hydra.core.Name], hydra.ext.java.syntax.FormalParameter,
                                                                   hydra.context.InContext[hydra.error.Error]]((pair: Tuple2[hydra.core.Type,
                                                                   hydra.core.Name]) =>
                                                                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                                                                     hydra.ext.java.syntax.Type, hydra.ext.java.syntax.FormalParameter](hydra.ext.java.coder.encodeType(aliases2)(hydra.lib.sets.empty[hydra.core.Name])(hydra.lib.pairs.first[hydra.core.Type,
                                                                     hydra.core.Name](pair))(cx)(g))((jdom: hydra.ext.java.syntax.Type) =>
                                                                  Right(hydra.ext.java.utils.javaTypeToJavaFormalParameter(jdom)(hydra.lib.pairs.second[hydra.core.Type,
                                                                     hydra.core.Name](pair)))))(hydra.lib.lists.zip[hydra.core.Type,
                                                                     hydra.core.Name](fixedDoms)(params)))((jformalParams: Seq[hydra.ext.java.syntax.FormalParameter]) =>
                                                                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                                                                     hydra.ext.java.syntax.Type, hydra.ext.java.syntax.InterfaceMemberDeclaration](hydra.ext.java.coder.encodeType(aliases2)(hydra.lib.sets.empty[hydra.core.Name])(fixedCod)(cx)(g))((jcod: hydra.ext.java.syntax.Type) =>
                                                                  {
                                                                  val result: hydra.ext.java.syntax.Result = hydra.ext.java.utils.javaTypeToJavaResult(jcod)
                                                                  {
                                                                    val mods: Seq[hydra.ext.java.syntax.InterfaceMethodModifier] = Seq(hydra.ext.java.syntax.InterfaceMethodModifier.static)
                                                                    {
                                                                      val jname: scala.Predef.String = hydra.ext.java.utils.sanitizeJavaName(hydra.formatting.decapitalize(hydra.names.localNameOf(name)))
                                                                      {
                                                                        val isTCO: Boolean = false
                                                                        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                                                                           Seq[hydra.ext.java.syntax.BlockStatement],
                                                                           hydra.ext.java.syntax.InterfaceMemberDeclaration](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
                                                                           Seq[hydra.ext.java.syntax.BlockStatement]]](isTCO)({
                                                                          val tcoSuffix: scala.Predef.String = "_tco"
                                                                          {
                                                                            val snapshotNames: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.Name,
                                                                               hydra.core.Name]((p: hydra.core.Name) => hydra.lib.strings.cat2(p)(tcoSuffix))(params)
                                                                            {
                                                                              val tcoVarRenames: Map[hydra.core.Name,
                                                                                 hydra.core.Name] = hydra.lib.maps.fromList[hydra.core.Name,
                                                                                 hydra.core.Name](hydra.lib.lists.zip[hydra.core.Name,
                                                                                 hydra.core.Name](params)(snapshotNames))
                                                                              {
                                                                                val snapshotDecls: Seq[hydra.ext.java.syntax.BlockStatement] = hydra.lib.lists.map[Tuple2[hydra.core.Name,
                                                                                   hydra.core.Name], hydra.ext.java.syntax.BlockStatement]((pair: Tuple2[hydra.core.Name,
                                                                                   hydra.core.Name]) =>
                                                                                  hydra.ext.java.utils.finalVarDeclarationStatement(hydra.ext.java.utils.variableToJavaIdentifier(hydra.lib.pairs.second[hydra.core.Name,
                                                                                     hydra.core.Name](pair)))(hydra.ext.java.utils.javaIdentifierToJavaExpression(hydra.ext.java.utils.variableToJavaIdentifier(hydra.lib.pairs.first[hydra.core.Name,
                                                                                     hydra.core.Name](pair)))))(hydra.lib.lists.zip[hydra.core.Name,
                                                                                     hydra.core.Name](params)(snapshotNames))
                                                                                {
                                                                                  val tcoBody: hydra.core.Term = hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.lists.`null`[hydra.core.Binding](bindings))(annotatedBody)(hydra.core.Term.let(hydra.core.Let(bindings,
                                                                                     annotatedBody)))
                                                                                  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                                                                                     Seq[hydra.ext.java.syntax.BlockStatement],
                                                                                     Seq[hydra.ext.java.syntax.BlockStatement]](hydra.ext.java.coder.encodeTermTCO(env2WithTypeParams)(name)(params)(tcoVarRenames)(0)(tcoBody)(cx)(g))((tcoStmts: Seq[hydra.ext.java.syntax.BlockStatement]) =>
                                                                                    {
                                                                                    val whileBodyStmts: Seq[hydra.ext.java.syntax.BlockStatement] = hydra.lib.lists.concat2[hydra.ext.java.syntax.BlockStatement](snapshotDecls)(tcoStmts)
                                                                                    {
                                                                                      val whileBodyBlock: hydra.ext.java.syntax.Statement = hydra.ext.java.syntax.Statement.withoutTrailing(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.block(whileBodyStmts))
                                                                                      {
                                                                                        def noCond[T0]: Option[T0] = None
                                                                                        {
                                                                                          val whileStmt: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.syntax.Statement.`while`(hydra.ext.java.syntax.WhileStatement(noCond,
                                                                                             whileBodyBlock)))
                                                                                          Right(Seq(whileStmt))
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                  })
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        })(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
                                                                           hydra.ext.java.syntax.Expression,
                                                                           Seq[hydra.ext.java.syntax.BlockStatement]](hydra.ext.java.coder.encodeTerm(env3)(annotatedBody)(cx)(g))((jbody: hydra.ext.java.syntax.Expression) =>
                                                                          {
                                                                          val returnSt: hydra.ext.java.syntax.BlockStatement = hydra.ext.java.syntax.BlockStatement.statement(hydra.ext.java.utils.javaReturnStatement(Some(jbody)))
                                                                          Right(hydra.lib.lists.concat2[hydra.ext.java.syntax.BlockStatement](bindingStmts)(Seq(returnSt)))
                                                                        })))((methodBody: Seq[hydra.ext.java.syntax.BlockStatement]) =>
                                                                          Right(hydra.ext.java.utils.interfaceMethodDeclaration(mods)(jparams)(jname)(jformalParams)(result)(Some(methodBody))))
                                                                      }
                                                                    }
                                                                  }
                                                                }))
                                                              })
                                                            }
                                                          })
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
                                    }
                                  })
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
            }
          }
        }
      }
    }
  })
}

def encodeDefinitions(mod: hydra.module.Module)(defs: Seq[hydra.module.Definition])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   Map[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit]] =
  {
  val aliases: hydra.ext.java.helpers.Aliases = hydra.ext.java.utils.importAliasesForModule(mod)
  val env: hydra.ext.java.helpers.JavaEnvironment = hydra.ext.java.helpers.JavaEnvironment(aliases, g)
  val pkg: hydra.ext.java.syntax.PackageDeclaration = hydra.ext.java.utils.javaPackageDeclaration(mod.namespace)
  val partitioned: Tuple2[Seq[hydra.module.TypeDefinition], Seq[hydra.module.TermDefinition]] = hydra.schemas.partitionDefinitions(defs)
  val typeDefs: Seq[hydra.module.TypeDefinition] = hydra.lib.pairs.first[Seq[hydra.module.TypeDefinition], Seq[hydra.module.TermDefinition]](partitioned)
  val termDefs: Seq[hydra.module.TermDefinition] = hydra.lib.pairs.second[Seq[hydra.module.TypeDefinition], Seq[hydra.module.TermDefinition]](partitioned)
  val nonTypedefDefs: Seq[hydra.module.TypeDefinition] = hydra.lib.lists.filter[hydra.module.TypeDefinition]((td: hydra.module.TypeDefinition) =>
    {
    val typ: hydra.core.Type = (td.`type`)
    hydra.ext.java.coder.isSerializableJavaType(typ)
  })(typeDefs)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[Tuple2[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit]],
     Map[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit]](hydra.lib.eithers.mapList[hydra.module.TypeDefinition,
     Tuple2[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit], hydra.context.InContext[hydra.error.Error]]((td: hydra.module.TypeDefinition) =>
    hydra.ext.java.coder.encodeTypeDefinition(pkg)(aliases)(td)(cx)(g))(nonTypedefDefs))((typeUnits: Seq[Tuple2[hydra.core.Name,
       hydra.ext.java.syntax.CompilationUnit]]) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[Tuple2[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit]],
       Map[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit]](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
       Seq[Tuple2[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit]]]](hydra.lib.lists.`null`[hydra.module.TermDefinition](termDefs))(Right(Seq()))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       Seq[hydra.ext.java.syntax.InterfaceMemberDeclaration], Seq[Tuple2[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit]]](hydra.lib.eithers.mapList[hydra.module.TermDefinition,
       hydra.ext.java.syntax.InterfaceMemberDeclaration, hydra.context.InContext[hydra.error.Error]]((td: hydra.module.TermDefinition) => hydra.ext.java.coder.encodeTermDefinition(env)(td)(cx)(g))(termDefs))((dataMembers: Seq[hydra.ext.java.syntax.InterfaceMemberDeclaration]) =>
    Right(Seq(hydra.ext.java.coder.constructElementsInterface(mod)(dataMembers))))))((termUnits: Seq[Tuple2[hydra.core.Name,
       hydra.ext.java.syntax.CompilationUnit]]) =>
    Right(hydra.lib.maps.fromList[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit](hydra.lib.lists.concat2[Tuple2[hydra.core.Name,
       hydra.ext.java.syntax.CompilationUnit]](typeUnits)(termUnits)))))
}

def moduleToJava(mod: hydra.module.Module)(defs: Seq[hydra.module.Definition])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   Map[scala.Predef.String, scala.Predef.String]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Map[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit],
     Map[scala.Predef.String, scala.Predef.String]](hydra.ext.java.coder.encodeDefinitions(mod)(defs)(cx)(g))((units: Map[hydra.core.Name,
     hydra.ext.java.syntax.CompilationUnit]) =>
  Right(hydra.lib.maps.fromList[scala.Predef.String, scala.Predef.String](hydra.lib.lists.map[Tuple2[hydra.core.Name,
     hydra.ext.java.syntax.CompilationUnit], Tuple2[scala.Predef.String, scala.Predef.String]]((entry: Tuple2[hydra.core.Name,
     hydra.ext.java.syntax.CompilationUnit]) =>
  {
  val name: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit](entry)
  {
    val unit: hydra.ext.java.syntax.CompilationUnit = hydra.lib.pairs.second[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit](entry)
    Tuple2(hydra.ext.java.coder.bindingNameToFilePath(name), hydra.serialization.printExpr(hydra.serialization.parenthesize(hydra.ext.java.serde.writeCompilationUnit(unit))))
  }
})(hydra.lib.maps.toList[hydra.core.Name, hydra.ext.java.syntax.CompilationUnit](units)))))
