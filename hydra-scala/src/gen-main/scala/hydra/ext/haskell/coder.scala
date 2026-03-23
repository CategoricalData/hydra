package hydra.ext.haskell.coder

import hydra.classes.*

import hydra.coders.*

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.ext.haskell.ast.*

import hydra.graph.*

import hydra.module.*

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

lazy val includeTypeDefinitions: Boolean = false

lazy val useCoreImport: Boolean = true

lazy val keyHaskellVar: hydra.core.Name = "haskellVar"

def adaptTypeToHaskellAndEncode[T0](namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(typ: hydra.core.Type)(cx: hydra.context.Context)(g: T0): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type] =
  {
  def enc(t: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type] = hydra.ext.haskell.coder.encodeType(namespaces)(t)(cx)(g)
  hydra.rewriting.deannotateType(typ) match
    case hydra.core.Type.variable(v_Type_variable__) => enc(typ)
    case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Type, hydra.ext.haskell.ast.Type](hydra.lib.eithers.bimap[scala.Predef.String, hydra.core.Type, hydra.context.InContext[hydra.errors.Error], hydra.core.Type]((_s: scala.Predef.String) => hydra.context.InContext(hydra.errors.Error.other(_s), cx))((_x: hydra.core.Type) => _x)(hydra.adapt.adaptTypeForLanguage(hydra.ext.haskell.language.haskellLanguage)(typ)))((adaptedType: hydra.core.Type) => enc(adaptedType))
}

def constantForFieldName(tname: hydra.core.Name)(fname: hydra.core.Name): scala.Predef.String = hydra.lib.strings.cat(Seq("_", hydra.names.localNameOf(tname), "_", fname))

def constantForTypeName(tname: hydra.core.Name): scala.Predef.String = hydra.lib.strings.cat2("_")(hydra.names.localNameOf(tname))

def constructModule(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(mod: hydra.module.Module)(defs: Seq[hydra.module.Definition])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Module] =
  {
  def h(namespace: hydra.module.Namespace): scala.Predef.String = namespace
  def createDeclarations(`def`: hydra.module.Definition): Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.haskell.ast.DeclarationWithComments]] =
    `def` match
    case hydra.module.Definition.`type`(v_Definition_type_type) => {
      lazy val name: hydra.core.Name = (v_Definition_type_type.name)
      lazy val typ: hydra.core.Type = (v_Definition_type_type.`type`)
      hydra.ext.haskell.coder.toTypeDeclarationsFrom(namespaces)(name)(typ)(cx)(g)
    }
    case hydra.module.Definition.term(v_Definition_term_term) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.DeclarationWithComments, Seq[hydra.ext.haskell.ast.DeclarationWithComments]](hydra.ext.haskell.coder.toDataDeclaration(namespaces)(v_Definition_term_term)(cx)(g))((d: hydra.ext.haskell.ast.DeclarationWithComments) => Right(Seq(d)))
  def importName(name: scala.Predef.String): hydra.ext.haskell.ast.ModuleName =
    hydra.lib.strings.intercalate(".")(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String](hydra.formatting.capitalize)(hydra.lib.strings.splitOn(".")(name)))
  lazy val imports: Seq[hydra.ext.haskell.ast.Import] = hydra.lib.lists.concat2[hydra.ext.haskell.ast.Import](domainImports)(standardImports)
  lazy val domainImports: Seq[hydra.ext.haskell.ast.Import] = {
    def toImport(pair: Tuple2[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName]): hydra.ext.haskell.ast.Import =
      {
      lazy val namespace: hydra.module.Namespace = hydra.lib.pairs.first[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName](pair)
      lazy val alias: hydra.ext.haskell.ast.ModuleName = hydra.lib.pairs.second[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName](pair)
      lazy val name: scala.Predef.String = h(namespace)
      hydra.ext.haskell.ast.Import(true, importName(name), Some(alias), None)
    }
    hydra.lib.lists.map[Tuple2[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName], hydra.ext.haskell.ast.Import](toImport)(hydra.lib.maps.toList[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName](namespaces.mapping))
  }
  lazy val standardImports: Seq[hydra.ext.haskell.ast.Import] = {
    def toImport(triple: Tuple2[Tuple2[scala.Predef.String, Option[scala.Predef.String]], Seq[scala.Predef.String]]): hydra.ext.haskell.ast.Import =
      {
      lazy val name: scala.Predef.String = hydra.lib.pairs.first[scala.Predef.String, Option[scala.Predef.String]](hydra.lib.pairs.first[Tuple2[scala.Predef.String, Option[scala.Predef.String]], Seq[scala.Predef.String]](triple))
      lazy val malias: Option[scala.Predef.String] = hydra.lib.pairs.second[scala.Predef.String, Option[scala.Predef.String]](hydra.lib.pairs.first[Tuple2[scala.Predef.String, Option[scala.Predef.String]], Seq[scala.Predef.String]](triple))
      lazy val hidden: Seq[scala.Predef.String] = hydra.lib.pairs.second[Tuple2[scala.Predef.String, Option[scala.Predef.String]], Seq[scala.Predef.String]](triple)
      lazy val spec: Option[hydra.ext.haskell.ast.SpecImport] = hydra.lib.logic.ifElse[Option[hydra.ext.haskell.ast.SpecImport]](hydra.lib.lists.`null`[scala.Predef.String](hidden))(None)(Some(hydra.ext.haskell.ast.SpecImport.hiding(hydra.lib.lists.map[scala.Predef.String, hydra.ext.haskell.ast.ImportExportSpec]((n: scala.Predef.String) =>
        hydra.ext.haskell.ast.ImportExportSpec(None, hydra.ext.haskell.utils.simpleName(n), None))(hidden))))
      hydra.ext.haskell.ast.Import(hydra.lib.maybes.isJust[scala.Predef.String](malias), name, hydra.lib.maybes.map[scala.Predef.String, hydra.ext.haskell.ast.ModuleName]((x: scala.Predef.String) => x)(malias), spec)
    }
    hydra.lib.lists.map[Tuple2[Tuple2[scala.Predef.String, Option[scala.Predef.String]], Seq[scala.Predef.String]], hydra.ext.haskell.ast.Import](toImport)(hydra.lib.lists.concat2[Tuple2[Tuple2[scala.Predef.String, Option[scala.Predef.String]], Seq[scala.Predef.String]]](Seq(Tuple2(Tuple2("Prelude", None), Seq("Enum", "Ordering", "decodeFloat", "encodeFloat", "fail", "map", "pure", "sum")), Tuple2(Tuple2("Data.ByteString", Some("B")), Seq()), Tuple2(Tuple2("Data.Int", Some("I")), Seq()), Tuple2(Tuple2("Data.List", Some("L")), Seq()), Tuple2(Tuple2("Data.Map", Some("M")), Seq()), Tuple2(Tuple2("Data.Set", Some("S")), Seq())))(hydra.lib.logic.ifElse[Seq[Tuple2[Tuple2[scala.Predef.String, Option[scala.Predef.String]], Seq[scala.Predef.String]]]](hydra.schemas.moduleContainsBinaryLiterals(mod))(Seq(Tuple2(Tuple2("Hydra.Lib.Literals", Some("Literals")), Seq())))(Seq())))
  }
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[Seq[hydra.ext.haskell.ast.DeclarationWithComments]], hydra.ext.haskell.ast.Module](hydra.lib.eithers.mapList[hydra.module.Definition, Seq[hydra.ext.haskell.ast.DeclarationWithComments], hydra.context.InContext[hydra.errors.Error]](createDeclarations)(defs))((declLists: Seq[Seq[hydra.ext.haskell.ast.DeclarationWithComments]]) =>
    {
    lazy val decls: Seq[hydra.ext.haskell.ast.DeclarationWithComments] = hydra.lib.lists.concat[hydra.ext.haskell.ast.DeclarationWithComments](declLists)
    lazy val mc: Option[scala.Predef.String] = (mod.description)
    Right(hydra.ext.haskell.ast.Module(Some(hydra.ext.haskell.ast.ModuleHead(mc, importName(h(mod.namespace)), Seq())), imports, decls))
  })
}

def encodeCaseExpression(depth: Int)(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(stmt: hydra.core.CaseStatement)(scrutinee: hydra.ext.haskell.ast.Expression)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression] =
  {
  lazy val dn: hydra.core.Name = (stmt.typeName)
  lazy val `def`: Option[hydra.core.Term] = (stmt.default)
  lazy val fields: Seq[hydra.core.Field] = (stmt.cases)
  def toAlt(fieldMap: Map[hydra.core.Name, hydra.core.FieldType])(field: hydra.core.Field): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Alternative] =
    {
    lazy val fn: hydra.core.Name = (field.name)
    lazy val `fun_`: hydra.core.Term = (field.term)
    lazy val v0: scala.Predef.String = hydra.lib.strings.cat2("v")(hydra.lib.literals.showInt32(depth))
    lazy val raw: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(`fun_`, hydra.core.Term.variable(v0)))
    lazy val rhsTerm: hydra.core.Term = hydra.rewriting.simplifyTerm(raw)
    lazy val v1: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](hydra.rewriting.isFreeVariableInTerm(v0)(rhsTerm))(hydra.constants.ignoredVariable)(v0)
    lazy val hname: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.unionFieldReference(hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name, hydra.core.Term](g.boundTerms)))(namespaces)(dn)(fn)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.haskell.ast.Pattern], hydra.ext.haskell.ast.Alternative](hydra.lib.maybes.cases[hydra.core.FieldType, Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.haskell.ast.Pattern]]](hydra.lib.maps.lookup[hydra.core.Name, hydra.core.FieldType](fn)(fieldMap))(Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat(Seq("field ", hydra.lib.literals.showString(fn), " not found in ", hydra.lib.literals.showString(dn)))), cx)))((fieldType: hydra.core.FieldType) =>
      {
      lazy val ft: hydra.core.Type = (fieldType.`type`)
      def noArgs[T0]: Seq[T0] = Seq()
      lazy val singleArg: Seq[hydra.ext.haskell.ast.Pattern] = Seq(hydra.ext.haskell.ast.Pattern.name(hydra.ext.haskell.utils.rawName(v1)))
      hydra.rewriting.deannotateType(ft) match
        case hydra.core.Type.unit => Right(noArgs)
        case _ => Right(singleArg)
    }))((args: Seq[hydra.ext.haskell.ast.Pattern]) =>
      {
      lazy val lhs: hydra.ext.haskell.ast.Pattern = hydra.ext.haskell.utils.applicationPattern(hname)(args)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.CaseRhs, hydra.ext.haskell.ast.Alternative](hydra.lib.eithers.map[hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.CaseRhs, hydra.context.InContext[hydra.errors.Error]]((x: hydra.ext.haskell.ast.Expression) => x)(hydra.ext.haskell.coder.encodeTerm(hydra.lib.math.add(depth)(1))(namespaces)(rhsTerm)(cx)(g)))((rhs: hydra.ext.haskell.ast.CaseRhs) => Right(hydra.ext.haskell.ast.Alternative(lhs, rhs, None)))
    })
  }
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.FieldType], hydra.ext.haskell.ast.Expression](hydra.schemas.requireUnionType(cx)(g)(dn))((rt: Seq[hydra.core.FieldType]) =>
    {
    def toFieldMapEntry(f: hydra.core.FieldType): Tuple2[hydra.core.Name, hydra.core.FieldType] = Tuple2(f.name, f)
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.FieldType] = hydra.lib.maps.fromList[hydra.core.Name, hydra.core.FieldType](hydra.lib.lists.map[hydra.core.FieldType, Tuple2[hydra.core.Name, hydra.core.FieldType]](toFieldMapEntry)(rt))
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.haskell.ast.Alternative], hydra.ext.haskell.ast.Expression](hydra.lib.eithers.mapList[hydra.core.Field, hydra.ext.haskell.ast.Alternative, hydra.context.InContext[hydra.errors.Error]]((v1: hydra.core.Field) => toAlt(fieldMap)(v1))(fields))((ecases: Seq[hydra.ext.haskell.ast.Alternative]) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.haskell.ast.Alternative], hydra.ext.haskell.ast.Expression](hydra.lib.maybes.cases[hydra.core.Term, Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.haskell.ast.Alternative]]](`def`)(Right(Seq()))((d: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.CaseRhs, Seq[hydra.ext.haskell.ast.Alternative]](hydra.lib.eithers.map[hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.CaseRhs, hydra.context.InContext[hydra.errors.Error]]((x: hydra.ext.haskell.ast.Expression) => x)(hydra.ext.haskell.coder.encodeTerm(depth)(namespaces)(d)(cx)(g)))((cs: hydra.ext.haskell.ast.CaseRhs) =>
      {
      lazy val lhs: hydra.ext.haskell.ast.Pattern = hydra.ext.haskell.ast.Pattern.name(hydra.ext.haskell.utils.rawName(hydra.constants.ignoredVariable))
      lazy val alt: hydra.ext.haskell.ast.Alternative = hydra.ext.haskell.ast.Alternative(lhs, cs, None)
      Right(Seq(alt))
    })))((dcases: Seq[hydra.ext.haskell.ast.Alternative]) =>
      Right(hydra.ext.haskell.ast.Expression.`case`(hydra.ext.haskell.ast.CaseExpression(scrutinee, hydra.lib.lists.concat2[hydra.ext.haskell.ast.Alternative](ecases)(dcases))))))
  })
}

def encodeFunction(depth: Int)(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(fun: hydra.core.Function)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression] =
  fun match
  case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
    case hydra.core.Elimination.wrap(v_Elimination_wrap_name) => Right(hydra.ext.haskell.ast.Expression.variable(hydra.ext.haskell.utils.elementReference(namespaces)(hydra.names.qname(hydra.lib.maybes.fromJust[hydra.module.Namespace](hydra.names.namespaceOf(v_Elimination_wrap_name)))(hydra.ext.haskell.utils.newtypeAccessorName(v_Elimination_wrap_name)))))
    case hydra.core.Elimination.record(v_Elimination_record_proj) => {
      lazy val dn: hydra.core.Name = (v_Elimination_record_proj.typeName)
      lazy val fname: hydra.core.Name = (v_Elimination_record_proj.field)
      Right(hydra.ext.haskell.ast.Expression.variable(hydra.ext.haskell.utils.recordFieldReference(namespaces)(dn)(fname)))
    }
    case hydra.core.Elimination.union(v_Elimination_union_stmt) => hydra.lib.eithers.map[hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression, hydra.context.InContext[hydra.errors.Error]]((v1: hydra.ext.haskell.ast.Expression) =>
      hydra.ext.haskell.utils.hslambda(hydra.ext.haskell.utils.rawName("x"))(v1))(hydra.ext.haskell.coder.encodeCaseExpression(depth)(namespaces)(v_Elimination_union_stmt)(hydra.ext.haskell.utils.hsvar("x"))(cx)(g))
  case hydra.core.Function.lambda(v_Function_lambda_lam) => {
    lazy val v: hydra.core.Name = (v_Function_lambda_lam.parameter)
    lazy val body: hydra.core.Term = (v_Function_lambda_lam.body)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](hydra.ext.haskell.coder.encodeTerm(depth)(namespaces)(body)(cx)(g))((hbody: hydra.ext.haskell.ast.Expression) =>
      Right(hydra.ext.haskell.utils.hslambda(hydra.ext.haskell.utils.elementReference(namespaces)(v))(hbody)))
  }
  case hydra.core.Function.primitive(v_Function_primitive_name) => Right(hydra.ext.haskell.ast.Expression.variable(hydra.ext.haskell.utils.elementReference(namespaces)(v_Function_primitive_name)))

def encodeLiteral(l: hydra.core.Literal)(cx: hydra.context.Context): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression] =
  l match
  case hydra.core.Literal.binary(v_Literal_binary_bs) => Right(hydra.ext.haskell.utils.hsapp(hydra.ext.haskell.utils.hsvar("Literals.stringToBinary"))(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.string(hydra.lib.literals.binaryToString(v_Literal_binary_bs)))))
  case hydra.core.Literal.boolean(v_Literal_boolean_b) => Right(hydra.ext.haskell.utils.hsvar(hydra.lib.logic.ifElse[scala.Predef.String](v_Literal_boolean_b)("True")("False")))
  case hydra.core.Literal.float(v_Literal_float_fv) => v_Literal_float_fv match
    case hydra.core.FloatValue.float32(v_FloatValue_float32_f) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.float(v_FloatValue_float32_f)))
    case hydra.core.FloatValue.float64(v_FloatValue_float64_f) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.double(v_FloatValue_float64_f)))
    case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_f) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.double(hydra.lib.literals.bigfloatToFloat64(v_FloatValue_bigfloat_f))))
  case hydra.core.Literal.integer(v_Literal_integer_iv) => v_Literal_integer_iv match
    case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.integer(v_IntegerValue_bigint_i)))
    case hydra.core.IntegerValue.int8(v_IntegerValue_int8_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.integer(hydra.lib.literals.int8ToBigint(v_IntegerValue_int8_i))))
    case hydra.core.IntegerValue.int16(v_IntegerValue_int16_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.integer(hydra.lib.literals.int16ToBigint(v_IntegerValue_int16_i))))
    case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.int(v_IntegerValue_int32_i)))
    case hydra.core.IntegerValue.int64(v_IntegerValue_int64_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.integer(hydra.lib.literals.int64ToBigint(v_IntegerValue_int64_i))))
    case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.integer(hydra.lib.literals.uint8ToBigint(v_IntegerValue_uint8_i))))
    case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.integer(hydra.lib.literals.uint16ToBigint(v_IntegerValue_uint16_i))))
    case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.integer(hydra.lib.literals.uint32ToBigint(v_IntegerValue_uint32_i))))
    case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.integer(hydra.lib.literals.uint64ToBigint(v_IntegerValue_uint64_i))))
  case hydra.core.Literal.string(v_Literal_string_s) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.string(v_Literal_string_s)))
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2("literal value ")(hydra.show.core.literal(l))), cx))

def encodeTerm(depth: Int)(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(term: hydra.core.Term)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression] =
  {
  def encode(t: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression] = hydra.ext.haskell.coder.encodeTerm(depth)(namespaces)(t)(cx)(g)
  def nonemptyMap(m: Map[hydra.core.Term, hydra.core.Term]): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression] =
    {
    lazy val lhs: hydra.ext.haskell.ast.Expression = hydra.ext.haskell.utils.hsvar("M.fromList")
    def encodePair(pair: Tuple2[hydra.core.Term, hydra.core.Term]): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression] =
      {
      lazy val k: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](pair)
      lazy val v: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](pair)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(k))((hk: hydra.ext.haskell.ast.Expression) =>
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(v))((hv: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.ast.Expression.tuple(Seq(hk, hv)))))
    }
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](hydra.lib.eithers.map[Seq[hydra.ext.haskell.ast.Expression], hydra.ext.haskell.ast.Expression, hydra.context.InContext[hydra.errors.Error]]((x: Seq[hydra.ext.haskell.ast.Expression]) => hydra.ext.haskell.ast.Expression.list(x))(hydra.lib.eithers.mapList[Tuple2[hydra.core.Term, hydra.core.Term], hydra.ext.haskell.ast.Expression, hydra.context.InContext[hydra.errors.Error]](encodePair)(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](m))))((rhs: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.utils.hsapp(lhs)(rhs)))
  }
  def nonemptySet(s: scala.collection.immutable.Set[hydra.core.Term]): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression] =
    {
    lazy val lhs: hydra.ext.haskell.ast.Expression = hydra.ext.haskell.utils.hsvar("S.fromList")
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](hydra.ext.haskell.coder.encodeTerm(depth)(namespaces)(hydra.core.Term.list(hydra.lib.sets.toList[hydra.core.Term](s)))(cx)(g))((rhs: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.utils.hsapp(lhs)(rhs)))
  }
  hydra.rewriting.deannotateTerm(term) match
    case hydra.core.Term.application(v_Term_application_app) => {
      lazy val fun: hydra.core.Term = (v_Term_application_app.function)
      lazy val arg: hydra.core.Term = (v_Term_application_app.argument)
      lazy val deannotatedFun: hydra.core.Term = hydra.rewriting.deannotateTerm(fun)
      deannotatedFun match
        case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
          case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
            case hydra.core.Elimination.union(v_Elimination_union_stmt) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(arg))((harg: hydra.ext.haskell.ast.Expression) =>
              hydra.ext.haskell.coder.encodeCaseExpression(depth)(namespaces)(v_Elimination_union_stmt)(harg)(cx)(g))
            case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(fun))((hfun: hydra.ext.haskell.ast.Expression) =>
              hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(arg))((harg: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.utils.hsapp(hfun)(harg))))
          case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(fun))((hfun: hydra.ext.haskell.ast.Expression) =>
            hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(arg))((harg: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.utils.hsapp(hfun)(harg))))
        case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(fun))((hfun: hydra.ext.haskell.ast.Expression) =>
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(arg))((harg: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.utils.hsapp(hfun)(harg))))
    }
    case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression]]((l: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(l))((hl: hydra.ext.haskell.ast.Expression) =>
      Right(hydra.ext.haskell.utils.hsapp(hydra.ext.haskell.utils.hsvar("Left"))(hl))))((r: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(r))((hr: hydra.ext.haskell.ast.Expression) =>
      Right(hydra.ext.haskell.utils.hsapp(hydra.ext.haskell.utils.hsvar("Right"))(hr))))(v_Term_either_e)
    case hydra.core.Term.function(v_Term_function_f) => hydra.ext.haskell.coder.encodeFunction(depth)(namespaces)(v_Term_function_f)(cx)(g)
    case hydra.core.Term.let(v_Term_let_letTerm) => {
      def collectBindings(lt: hydra.core.Let): Tuple2[Seq[hydra.core.Binding], hydra.core.Term] =
        {
        lazy val bs: Seq[hydra.core.Binding] = (lt.bindings)
        lazy val body: hydra.core.Term = (lt.body)
        hydra.rewriting.deannotateTerm(body) match
          case hydra.core.Term.let(v_Term_let_innerLt) => {
            lazy val innerResult: Tuple2[Seq[hydra.core.Binding], hydra.core.Term] = collectBindings(v_Term_let_innerLt)
            Tuple2(hydra.lib.lists.concat2[hydra.core.Binding](bs)(hydra.lib.pairs.first[Seq[hydra.core.Binding], hydra.core.Term](innerResult)), hydra.lib.pairs.second[Seq[hydra.core.Binding], hydra.core.Term](innerResult))
          }
          case _ => Tuple2(bs, body)
      }
      lazy val collected: Tuple2[Seq[hydra.core.Binding], hydra.core.Term] = collectBindings(v_Term_let_letTerm)
      lazy val allBindings: Seq[hydra.core.Binding] = hydra.lib.pairs.first[Seq[hydra.core.Binding], hydra.core.Term](collected)
      lazy val finalBody: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Binding], hydra.core.Term](collected)
      def encodeBinding(binding: hydra.core.Binding): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.LocalBinding] =
        {
        lazy val name: hydra.core.Name = (binding.name)
        lazy val `term_`: hydra.core.Term = (binding.term)
        lazy val hname: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.simpleName(name)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.LocalBinding](encode(`term_`))((hexpr: hydra.ext.haskell.ast.Expression) =>
          Right(hydra.ext.haskell.ast.LocalBinding.value(hydra.ext.haskell.utils.simpleValueBinding(hname)(hexpr)(None))))
      }
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.haskell.ast.LocalBinding], hydra.ext.haskell.ast.Expression](hydra.lib.eithers.mapList[hydra.core.Binding, hydra.ext.haskell.ast.LocalBinding, hydra.context.InContext[hydra.errors.Error]](encodeBinding)(allBindings))((hbindings: Seq[hydra.ext.haskell.ast.LocalBinding]) =>
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(finalBody))((hinner: hydra.ext.haskell.ast.Expression) =>
        Right(hydra.ext.haskell.ast.Expression.let(hydra.ext.haskell.ast.LetExpression(hbindings, hinner)))))
    }
    case hydra.core.Term.list(v_Term_list_els) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.haskell.ast.Expression], hydra.ext.haskell.ast.Expression](hydra.lib.eithers.mapList[hydra.core.Term, hydra.ext.haskell.ast.Expression, hydra.context.InContext[hydra.errors.Error]](encode)(v_Term_list_els))((helems: Seq[hydra.ext.haskell.ast.Expression]) => Right(hydra.ext.haskell.ast.Expression.list(helems)))
    case hydra.core.Term.literal(v_Term_literal_v) => hydra.ext.haskell.coder.encodeLiteral(v_Term_literal_v)(cx)
    case hydra.core.Term.map(v_Term_map_m) => hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression]](hydra.lib.maps.`null`[hydra.core.Term, hydra.core.Term](v_Term_map_m))(Right(hydra.ext.haskell.utils.hsvar("M.empty")))(nonemptyMap(v_Term_map_m))
    case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.lib.maybes.cases[hydra.core.Term, Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression]](v_Term_maybe_m)(Right(hydra.ext.haskell.utils.hsvar("Nothing")))((t: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(t))((ht: hydra.ext.haskell.ast.Expression) =>
      Right(hydra.ext.haskell.utils.hsapp(hydra.ext.haskell.utils.hsvar("Just"))(ht))))
    case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)))((f: hydra.ext.haskell.ast.Expression) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p)))((s: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.ast.Expression.tuple(Seq(f, s)))))
    case hydra.core.Term.record(v_Term_record_record) => {
      lazy val sname: hydra.core.Name = (v_Term_record_record.typeName)
      lazy val fields: Seq[hydra.core.Field] = (v_Term_record_record.fields)
      def toFieldUpdate(field: hydra.core.Field): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.FieldUpdate] =
        {
        lazy val fn: hydra.core.Name = (field.name)
        lazy val ft: hydra.core.Term = (field.term)
        lazy val fieldRef: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.recordFieldReference(namespaces)(sname)(fn)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.FieldUpdate](encode(ft))((hft: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.ast.FieldUpdate(fieldRef, hft)))
      }
      lazy val typeName: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.elementReference(namespaces)(sname)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.haskell.ast.FieldUpdate], hydra.ext.haskell.ast.Expression](hydra.lib.eithers.mapList[hydra.core.Field, hydra.ext.haskell.ast.FieldUpdate, hydra.context.InContext[hydra.errors.Error]](toFieldUpdate)(fields))((updates: Seq[hydra.ext.haskell.ast.FieldUpdate]) =>
        Right(hydra.ext.haskell.ast.Expression.constructRecord(hydra.ext.haskell.ast.ConstructRecordExpression(typeName, updates))))
    }
    case hydra.core.Term.set(v_Term_set_s) => hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression]](hydra.lib.sets.`null`[hydra.core.Term](v_Term_set_s))(Right(hydra.ext.haskell.utils.hsvar("S.empty")))(nonemptySet(v_Term_set_s))
    case hydra.core.Term.typeLambda(v_Term_typeLambda_abs) => {
      lazy val term1: hydra.core.Term = (v_Term_typeLambda_abs.body)
      encode(term1)
    }
    case hydra.core.Term.typeApplication(v_Term_typeApplication_typed) => {
      lazy val term1: hydra.core.Term = (v_Term_typeApplication_typed.body)
      encode(term1)
    }
    case hydra.core.Term.union(v_Term_union_injection) => {
      lazy val sname: hydra.core.Name = (v_Term_union_injection.typeName)
      lazy val field: hydra.core.Field = (v_Term_union_injection.field)
      lazy val fn: hydra.core.Name = (field.name)
      lazy val ft: hydra.core.Term = (field.term)
      lazy val lhs: hydra.ext.haskell.ast.Expression = hydra.ext.haskell.ast.Expression.variable(hydra.ext.haskell.utils.unionFieldReference(hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name, hydra.core.Term](g.boundTerms)))(namespaces)(sname)(fn))
      lazy val dflt: Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression] = hydra.lib.eithers.map[hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression, hydra.context.InContext[hydra.errors.Error]]((v1: hydra.ext.haskell.ast.Expression) => hydra.ext.haskell.utils.hsapp(lhs)(v1))(encode(ft))
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Type, hydra.ext.haskell.ast.Expression](hydra.schemas.requireUnionField(cx)(g)(sname)(fn))((ftyp: hydra.core.Type) =>
        hydra.rewriting.deannotateType(ftyp) match
        case hydra.core.Type.unit => Right(lhs)
        case _ => dflt)
    }
    case hydra.core.Term.unit => Right(hydra.ext.haskell.ast.Expression.tuple(Seq()))
    case hydra.core.Term.variable(v_Term_variable_name) => Right(hydra.ext.haskell.ast.Expression.variable(hydra.ext.haskell.utils.elementReference(namespaces)(v_Term_variable_name)))
    case hydra.core.Term.wrap(v_Term_wrap_wrapped) => {
      lazy val tname: hydra.core.Name = (v_Term_wrap_wrapped.typeName)
      lazy val `term_`: hydra.core.Term = (v_Term_wrap_wrapped.body)
      lazy val lhs: hydra.ext.haskell.ast.Expression = hydra.ext.haskell.ast.Expression.variable(hydra.ext.haskell.utils.elementReference(namespaces)(tname))
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(`term_`))((rhs: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.utils.hsapp(lhs)(rhs)))
    }
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2("unexpected term: ")(hydra.show.core.term(term))), cx))
}

def encodeType[T0](namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(typ: hydra.core.Type)(cx: hydra.context.Context)(g: T0): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type] =
  {
  def encode(t: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type] = hydra.ext.haskell.coder.encodeType(namespaces)(t)(cx)(g)
  def ref[T1](name: hydra.core.Name): Either[T1, hydra.ext.haskell.ast.Type] =
    Right(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.elementReference(namespaces)(name)))
  lazy val unitTuple: hydra.ext.haskell.ast.Type = hydra.ext.haskell.ast.Type.tuple(Seq())
  hydra.rewriting.deannotateType(typ) match
    case hydra.core.Type.application(v_Type_application_app) => {
      lazy val lhs: hydra.core.Type = (v_Type_application_app.function)
      lazy val rhs: hydra.core.Type = (v_Type_application_app.argument)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(lhs))((hlhs: hydra.ext.haskell.ast.Type) =>
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(rhs))((hrhs: hydra.ext.haskell.ast.Type) =>
        Right(hydra.ext.haskell.utils.toTypeApplication(Seq(hlhs, hrhs)))))
    }
    case hydra.core.Type.either(v_Type_either_eitherType) => {
      lazy val `left_`: hydra.core.Type = (v_Type_either_eitherType.left)
      lazy val `right_`: hydra.core.Type = (v_Type_either_eitherType.right)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(`left_`))((hleft: hydra.ext.haskell.ast.Type) =>
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(`right_`))((hright: hydra.ext.haskell.ast.Type) =>
        Right(hydra.ext.haskell.utils.toTypeApplication(Seq(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("Either")), hleft, hright)))))
    }
    case hydra.core.Type.function(v_Type_function_funType) => {
      lazy val dom: hydra.core.Type = (v_Type_function_funType.domain)
      lazy val cod: hydra.core.Type = (v_Type_function_funType.codomain)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(dom))((hdom: hydra.ext.haskell.ast.Type) =>
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(cod))((hcod: hydra.ext.haskell.ast.Type) =>
        Right(hydra.ext.haskell.ast.Type.function(hydra.ext.haskell.ast.FunctionType(hdom, hcod)))))
    }
    case hydra.core.Type.forall(v_Type_forall_forallType) => {
      lazy val v: hydra.core.Name = (v_Type_forall_forallType.parameter)
      lazy val body: hydra.core.Type = (v_Type_forall_forallType.body)
      encode(body)
    }
    case hydra.core.Type.list(v_Type_list_lt) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(v_Type_list_lt))((hlt: hydra.ext.haskell.ast.Type) => Right(hydra.ext.haskell.ast.Type.list(hlt)))
    case hydra.core.Type.literal(v_Type_literal_lt) => v_Type_literal_lt match
      case hydra.core.LiteralType.binary => Right(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("B.ByteString")))
      case hydra.core.LiteralType.boolean => Right(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("Bool")))
      case hydra.core.LiteralType.float(v_LiteralType_float_ft) => v_LiteralType_float_ft match
        case hydra.core.FloatType.float32 => Right(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("Float")))
        case hydra.core.FloatType.float64 => Right(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("Double")))
        case hydra.core.FloatType.bigfloat => Right(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("Double")))
      case hydra.core.LiteralType.integer(v_LiteralType_integer_it) => v_LiteralType_integer_it match
        case hydra.core.IntegerType.bigint => Right(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("Integer")))
        case hydra.core.IntegerType.int8 => Right(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("I.Int8")))
        case hydra.core.IntegerType.int16 => Right(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("I.Int16")))
        case hydra.core.IntegerType.int32 => Right(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("Int")))
        case hydra.core.IntegerType.int64 => Right(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("I.Int64")))
        case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2("unexpected integer type: ")(hydra.show.core.integerType(v_LiteralType_integer_it))), cx))
      case hydra.core.LiteralType.string => Right(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("String")))
      case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2("unexpected literal type: ")(hydra.show.core.literalType(v_Type_literal_lt))), cx))
    case hydra.core.Type.map(v_Type_map_mapType) => {
      lazy val kt: hydra.core.Type = (v_Type_map_mapType.keys)
      lazy val vt: hydra.core.Type = (v_Type_map_mapType.values)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(kt))((hkt: hydra.ext.haskell.ast.Type) =>
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(vt))((hvt: hydra.ext.haskell.ast.Type) =>
        Right(hydra.ext.haskell.utils.toTypeApplication(Seq(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("M.Map")), hkt, hvt)))))
    }
    case hydra.core.Type.maybe(v_Type_maybe_ot) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(v_Type_maybe_ot))((hot: hydra.ext.haskell.ast.Type) =>
      Right(hydra.ext.haskell.utils.toTypeApplication(Seq(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("Maybe")), hot))))
    case hydra.core.Type.pair(v_Type_pair_pt) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(v_Type_pair_pt.first))((f: hydra.ext.haskell.ast.Type) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(v_Type_pair_pt.second))((s: hydra.ext.haskell.ast.Type) => Right(hydra.ext.haskell.ast.Type.tuple(Seq(f, s)))))
    case hydra.core.Type.record(v_Type_record__) => ref("placeholder")
    case hydra.core.Type.set(v_Type_set_st) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(v_Type_set_st))((hst: hydra.ext.haskell.ast.Type) =>
      Right(hydra.ext.haskell.utils.toTypeApplication(Seq(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("S.Set")), hst))))
    case hydra.core.Type.union(v_Type_union__) => ref("placeholder")
    case hydra.core.Type.unit => Right(unitTuple)
    case hydra.core.Type.variable(v_Type_variable_v1) => ref(v_Type_variable_v1)
    case hydra.core.Type.void => Right(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("Void")))
    case hydra.core.Type.wrap(v_Type_wrap__) => ref("placeholder")
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other(hydra.lib.strings.cat2("unexpected type: ")(hydra.show.core.`type`(typ))), cx))
}

def encodeTypeWithClassAssertions[T0](namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(explicitClasses: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]])(typ: hydra.core.Type)(cx: hydra.context.Context)(g: T0): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type] =
  {
  lazy val classes: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]] = hydra.lib.maps.union[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]](explicitClasses)(hydra.ext.haskell.coder.getImplicitTypeClasses(typ))
  lazy val implicitClasses: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]] = hydra.ext.haskell.coder.getImplicitTypeClasses(typ)
  def encodeAssertion(pair: Tuple2[hydra.core.Name, hydra.classes.TypeClass]): hydra.ext.haskell.ast.Assertion =
    {
    lazy val name: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.classes.TypeClass](pair)
    lazy val cls: hydra.classes.TypeClass = hydra.lib.pairs.second[hydra.core.Name, hydra.classes.TypeClass](pair)
    lazy val hname: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.rawName(cls match
      case hydra.classes.TypeClass.equality => "Eq"
      case hydra.classes.TypeClass.ordering => "Ord")
    lazy val htype: hydra.ext.haskell.ast.Type = hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName(name))
    hydra.ext.haskell.ast.Assertion.`class`(hydra.ext.haskell.ast.ClassAssertion(hname, Seq(htype)))
  }
  lazy val assertPairs: Seq[Tuple2[hydra.core.Name, hydra.classes.TypeClass]] = hydra.lib.lists.concat[Tuple2[hydra.core.Name, hydra.classes.TypeClass]](hydra.lib.lists.map[Tuple2[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]], Seq[Tuple2[hydra.core.Name, hydra.classes.TypeClass]]](toPairs)(hydra.lib.maps.toList[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]](classes)))
  def toPairs[T1, T2](mapEntry: Tuple2[T1, scala.collection.immutable.Set[T2]]): Seq[Tuple2[T1, T2]] =
    {
    lazy val name: T1 = hydra.lib.pairs.first[T1, scala.collection.immutable.Set[T2]](mapEntry)
    lazy val clsSet: scala.collection.immutable.Set[T2] = hydra.lib.pairs.second[T1, scala.collection.immutable.Set[T2]](mapEntry)
    def toPair[T3](c: T3): Tuple2[T1, T3] = Tuple2(name, c)
    hydra.lib.lists.map[T2, Tuple2[T1, T2]](toPair)(hydra.lib.sets.toList[T2](clsSet))
  }
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](hydra.ext.haskell.coder.adaptTypeToHaskellAndEncode(namespaces)(typ)(cx)(g))((htyp: hydra.ext.haskell.ast.Type) =>
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type]](hydra.lib.lists.`null`[Tuple2[hydra.core.Name, hydra.classes.TypeClass]](assertPairs))(Right(htyp))({
    lazy val encoded: Seq[hydra.ext.haskell.ast.Assertion] = hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.classes.TypeClass], hydra.ext.haskell.ast.Assertion](encodeAssertion)(assertPairs)
    lazy val hassert: hydra.ext.haskell.ast.Assertion = hydra.lib.logic.ifElse[hydra.ext.haskell.ast.Assertion](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ext.haskell.ast.Assertion](encoded))(1))(hydra.lib.lists.head[hydra.ext.haskell.ast.Assertion](encoded))(hydra.ext.haskell.ast.Assertion.tuple(encoded))
    Right(hydra.ext.haskell.ast.Type.ctx(hydra.ext.haskell.ast.ContextType(hassert, htyp)))
  }))
}

def findOrdVariables(typ: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
  {
  def fold(names: scala.collection.immutable.Set[hydra.core.Name])(`typ_`: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
    `typ_` match
    case hydra.core.Type.map(v_Type_map_mapType) => {
      lazy val kt: hydra.core.Type = (v_Type_map_mapType.keys)
      tryType(names)(kt)
    }
    case hydra.core.Type.set(v_Type_set_et) => tryType(names)(v_Type_set_et)
    case _ => names
  def isTypeVariable(v: hydra.core.Name): Boolean = hydra.lib.maybes.isNothing[hydra.module.Namespace](hydra.names.namespaceOf(v))
  def tryType(names: scala.collection.immutable.Set[hydra.core.Name])(t: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
    hydra.rewriting.deannotateType(t) match
    case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.logic.ifElse[scala.collection.immutable.Set[hydra.core.Name]](isTypeVariable(v_Type_variable_v))(hydra.lib.sets.insert[hydra.core.Name](v_Type_variable_v)(names))(names)
    case _ => names
  hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)(fold)(hydra.lib.sets.empty[hydra.core.Name])(typ)
}

def getImplicitTypeClasses(typ: hydra.core.Type): Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]] =
  {
  def toPair[T0](name: T0): Tuple2[T0, scala.collection.immutable.Set[hydra.classes.TypeClass]] =
    Tuple2(name, hydra.lib.sets.fromList[hydra.classes.TypeClass](Seq(hydra.classes.TypeClass.ordering)))
  hydra.lib.maps.fromList[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]](hydra.lib.lists.map[hydra.core.Name, Tuple2[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]]](toPair)(hydra.lib.sets.toList[hydra.core.Name](hydra.ext.haskell.coder.findOrdVariables(typ))))
}

def moduleToHaskellModule(mod: hydra.module.Module)(defs: Seq[hydra.module.Definition])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Module] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName], hydra.ext.haskell.ast.Module](hydra.ext.haskell.utils.namespacesForModule(mod)(cx)(g))((namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName]) =>
  hydra.ext.haskell.coder.constructModule(namespaces)(mod)(defs)(cx)(g))

def moduleToHaskell(mod: hydra.module.Module)(defs: Seq[hydra.module.Definition])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.errors.Error], Map[scala.Predef.String, scala.Predef.String]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Module, Map[scala.Predef.String, scala.Predef.String]](hydra.ext.haskell.coder.moduleToHaskellModule(mod)(defs)(cx)(g))((hsmod: hydra.ext.haskell.ast.Module) =>
  {
  lazy val s: scala.Predef.String = hydra.serialization.printExpr(hydra.serialization.parenthesize(hydra.ext.haskell.serde.moduleToExpr(hsmod)))
  lazy val filepath: scala.Predef.String = hydra.names.namespaceToFilePath(hydra.util.CaseConvention.pascal)("hs")(mod.namespace)
  Right(hydra.lib.maps.singleton[scala.Predef.String, scala.Predef.String](filepath)(s))
})

def nameDecls(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(name: hydra.core.Name)(typ: hydra.core.Type): Seq[hydra.ext.haskell.ast.DeclarationWithComments] =
  {
  lazy val nm: scala.Predef.String = name
  def toDecl(n: hydra.core.Name)(pair: Tuple2[scala.Predef.String, scala.Predef.String]): hydra.ext.haskell.ast.DeclarationWithComments =
    {
    lazy val k: scala.Predef.String = hydra.lib.pairs.first[scala.Predef.String, scala.Predef.String](pair)
    lazy val v: scala.Predef.String = hydra.lib.pairs.second[scala.Predef.String, scala.Predef.String](pair)
    lazy val decl: hydra.ext.haskell.ast.Declaration = hydra.ext.haskell.ast.Declaration.valueBinding(hydra.ext.haskell.ast.ValueBinding.simple(hydra.ext.haskell.ast.SimpleValueBinding(hydra.ext.haskell.utils.applicationPattern(hydra.ext.haskell.utils.simpleName(k))(Seq()), hydra.ext.haskell.ast.Expression.application(hydra.ext.haskell.ast.ApplicationExpression(hydra.ext.haskell.ast.Expression.variable(hydra.ext.haskell.utils.elementReference(namespaces)(n)), hydra.ext.haskell.ast.Expression.literal(hydra.ext.haskell.ast.Literal.string(v)))), None)))
    hydra.ext.haskell.ast.DeclarationWithComments(decl, None)
  }
  lazy val nameDecl: Tuple2[scala.Predef.String, scala.Predef.String] = Tuple2(hydra.ext.haskell.coder.constantForTypeName(name), nm)
  lazy val fieldDecls: Seq[Tuple2[scala.Predef.String, scala.Predef.String]] = hydra.lib.lists.map[hydra.core.FieldType, Tuple2[scala.Predef.String, scala.Predef.String]](toConstant)(hydra.lexical.fieldsOf(typ))
  def toConstant(fieldType: hydra.core.FieldType): Tuple2[scala.Predef.String, scala.Predef.String] =
    {
    lazy val fname: hydra.core.Name = (fieldType.name)
    Tuple2(hydra.ext.haskell.coder.constantForFieldName(name)(fname), fname)
  }
  hydra.lib.logic.ifElse[Seq[hydra.ext.haskell.ast.DeclarationWithComments]](hydra.ext.haskell.coder.useCoreImport)(hydra.lib.lists.cons[hydra.ext.haskell.ast.DeclarationWithComments](toDecl("hydra.core.Name")(nameDecl))(hydra.lib.lists.map[Tuple2[scala.Predef.String, scala.Predef.String], hydra.ext.haskell.ast.DeclarationWithComments]((v1: Tuple2[scala.Predef.String, scala.Predef.String]) => toDecl("hydra.core.Name")(v1))(fieldDecls)))(Seq())
}

def toDataDeclaration(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(`def`: hydra.module.TermDefinition)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.DeclarationWithComments] =
  {
  lazy val name: hydra.core.Name = (`def`.name)
  lazy val term: hydra.core.Term = (`def`.term)
  lazy val typ: hydra.core.TypeScheme = (`def`.`type`)
  lazy val hname: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.simpleName(hydra.names.localNameOf(name))
  def rewriteValueBinding(vb: hydra.ext.haskell.ast.ValueBinding): hydra.ext.haskell.ast.ValueBinding =
    vb match
    case hydra.ext.haskell.ast.ValueBinding.simple(v_ValueBinding_simple_simple) => {
      lazy val `pattern_`: hydra.ext.haskell.ast.Pattern = (v_ValueBinding_simple_simple.pattern)
      lazy val rhs: hydra.ext.haskell.ast.RightHandSide = (v_ValueBinding_simple_simple.rhs)
      lazy val bindings: Option[hydra.ext.haskell.ast.LocalBindings] = (v_ValueBinding_simple_simple.localBindings)
      `pattern_` match
        case hydra.ext.haskell.ast.Pattern.application(v_Pattern_application_appPat) => {
          lazy val `name_`: hydra.ext.haskell.ast.Name = (v_Pattern_application_appPat.name)
          lazy val args: Seq[hydra.ext.haskell.ast.Pattern] = (v_Pattern_application_appPat.args)
          lazy val rhsExpr: hydra.ext.haskell.ast.Expression = rhs
          rhsExpr match
            case hydra.ext.haskell.ast.Expression.lambda(v_Expression_lambda_lambda_) => {
              lazy val vars: Seq[hydra.ext.haskell.ast.Pattern] = (`v_Expression_lambda_lambda_`.bindings)
              lazy val body: hydra.ext.haskell.ast.Expression = (`v_Expression_lambda_lambda_`.inner)
              lazy val newPattern: hydra.ext.haskell.ast.Pattern = hydra.ext.haskell.utils.applicationPattern(`name_`)(hydra.lib.lists.concat2[hydra.ext.haskell.ast.Pattern](args)(vars))
              lazy val newRhs: hydra.ext.haskell.ast.RightHandSide = body
              rewriteValueBinding(hydra.ext.haskell.ast.ValueBinding.simple(hydra.ext.haskell.ast.SimpleValueBinding(newPattern, newRhs, bindings)))
            }
            case _ => vb
        }
        case _ => vb
    }
  def toDecl(comments: Option[scala.Predef.String])(`hname_`: hydra.ext.haskell.ast.Name)(`term_`: hydra.core.Term)(bindings: Option[hydra.ext.haskell.ast.LocalBindings]): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.DeclarationWithComments] =
    hydra.rewriting.deannotateTerm(`term_`) match
    case hydra.core.Term.let(v_Term_let_letTerm) => {
      lazy val lbindings: Seq[hydra.core.Binding] = (v_Term_let_letTerm.bindings)
      lazy val env: hydra.core.Term = (v_Term_let_letTerm.body)
      def toBinding(`hname__`: hydra.ext.haskell.ast.Name)(`hterm_`: hydra.ext.haskell.ast.Expression): hydra.ext.haskell.ast.LocalBinding =
        hydra.ext.haskell.ast.LocalBinding.value(hydra.ext.haskell.utils.simpleValueBinding(`hname__`)(`hterm_`)(None))
      lazy val hnames: Seq[hydra.ext.haskell.ast.Name] = hydra.lib.lists.map[hydra.core.Binding, hydra.ext.haskell.ast.Name]((binding: hydra.core.Binding) => hydra.ext.haskell.utils.simpleName(binding.name))(lbindings)
      lazy val terms: Seq[hydra.core.Term] = hydra.lib.lists.map[hydra.core.Binding, hydra.core.Term]((x: hydra.core.Binding) => (x.term))(lbindings)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.haskell.ast.Expression], hydra.ext.haskell.ast.DeclarationWithComments](hydra.lib.eithers.mapList[hydra.core.Term, hydra.ext.haskell.ast.Expression, hydra.context.InContext[hydra.errors.Error]]((t: hydra.core.Term) => hydra.ext.haskell.coder.encodeTerm(0)(namespaces)(t)(cx)(g))(terms))((hterms: Seq[hydra.ext.haskell.ast.Expression]) =>
        {
        lazy val hbindings: Seq[hydra.ext.haskell.ast.LocalBinding] = hydra.lib.lists.zipWith[hydra.ext.haskell.ast.Name, hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.LocalBinding](toBinding)(hnames)(hterms)
        lazy val prevBindings: Seq[hydra.ext.haskell.ast.LocalBinding] = hydra.lib.maybes.maybe[Seq[hydra.ext.haskell.ast.LocalBinding], hydra.ext.haskell.ast.LocalBindings](Seq())((lb: hydra.ext.haskell.ast.LocalBindings) => lb)(bindings)
        lazy val allBindings: Seq[hydra.ext.haskell.ast.LocalBinding] = hydra.lib.lists.concat2[hydra.ext.haskell.ast.LocalBinding](prevBindings)(hbindings)
        toDecl(comments)(`hname_`)(env)(Some(allBindings))
      })
    }
    case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.DeclarationWithComments](hydra.ext.haskell.coder.encodeTerm(0)(namespaces)(`term_`)(cx)(g))((hterm: hydra.ext.haskell.ast.Expression) =>
      {
      lazy val vb: hydra.ext.haskell.ast.ValueBinding = hydra.ext.haskell.utils.simpleValueBinding(`hname_`)(hterm)(bindings)
      lazy val schemeConstraints: Option[Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = (typ.constraints)
      lazy val schemeClasses: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]] = hydra.ext.haskell.coder.typeSchemeConstraintsToClassMap(schemeConstraints)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]], hydra.ext.haskell.ast.DeclarationWithComments](hydra.annotations.getTypeClasses(cx)(g)(hydra.rewriting.removeTypesFromTerm(term)))((explicitClasses: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]]) =>
        {
        lazy val combinedClasses: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]] = hydra.lib.maps.union[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]](schemeClasses)(explicitClasses)
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.DeclarationWithComments](hydra.ext.haskell.coder.encodeTypeWithClassAssertions(namespaces)(combinedClasses)(typ.`type`)(cx)(g))((htype: hydra.ext.haskell.ast.Type) =>
          {
          lazy val decl: hydra.ext.haskell.ast.Declaration = hydra.ext.haskell.ast.Declaration.typedBinding(hydra.ext.haskell.ast.TypedBinding(hydra.ext.haskell.ast.TypeSignature(`hname_`, htype), rewriteValueBinding(vb)))
          Right(hydra.ext.haskell.ast.DeclarationWithComments(decl, comments))
        })
      })
    })
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[scala.Predef.String], hydra.ext.haskell.ast.DeclarationWithComments](hydra.annotations.getTermDescription(cx)(g)(term))((comments: Option[scala.Predef.String]) => toDecl(comments)(hname)(term)(None))
}

def toTypeDeclarationsFrom(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(elementName: hydra.core.Name)(typ: hydra.core.Type)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.haskell.ast.DeclarationWithComments]] =
  {
  lazy val lname: scala.Predef.String = hydra.names.localNameOf(elementName)
  lazy val hname: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.simpleName(lname)
  def declHead(name: hydra.ext.haskell.ast.Name)(`vars_`: Seq[hydra.core.Name]): hydra.ext.haskell.ast.DeclarationHead =
    hydra.lib.logic.ifElse[hydra.ext.haskell.ast.DeclarationHead](hydra.lib.lists.`null`[hydra.core.Name](`vars_`))(hydra.ext.haskell.ast.DeclarationHead.simple(name))({
    lazy val h: hydra.core.Name = hydra.lib.lists.head[hydra.core.Name](`vars_`)
    lazy val rest: Seq[hydra.core.Name] = hydra.lib.lists.tail[hydra.core.Name](`vars_`)
    lazy val hvar: hydra.ext.haskell.ast.Variable = hydra.ext.haskell.utils.simpleName(h)
    hydra.ext.haskell.ast.DeclarationHead.application(hydra.ext.haskell.ast.ApplicationDeclarationHead(declHead(name)(rest), hvar))
  })
  def newtypeCons(tname: hydra.core.Name)(`typ_`: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.ConstructorWithComments] =
    {
    lazy val hname0: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.simpleName(hydra.ext.haskell.utils.newtypeAccessorName(tname))
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.ConstructorWithComments](hydra.ext.haskell.coder.adaptTypeToHaskellAndEncode(namespaces)(`typ_`)(cx)(g))((htype: hydra.ext.haskell.ast.Type) =>
      {
      lazy val hfield: hydra.ext.haskell.ast.FieldWithComments = hydra.ext.haskell.ast.FieldWithComments(hydra.ext.haskell.ast.Field(hname0, htype), None)
      lazy val constructorName: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.simpleName(hydra.names.localNameOf(tname))
      Right(hydra.ext.haskell.ast.ConstructorWithComments(hydra.ext.haskell.ast.Constructor.record(hydra.ext.haskell.ast.RecordConstructor(constructorName, Seq(hfield))), None))
    })
  }
  def recordCons(`lname_`: scala.Predef.String)(fields: Seq[hydra.core.FieldType]): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.ConstructorWithComments] =
    {
    def toField(fieldType: hydra.core.FieldType): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.FieldWithComments] =
      {
      lazy val fname: hydra.core.Name = (fieldType.name)
      lazy val ftype: hydra.core.Type = (fieldType.`type`)
      lazy val `hname_`: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.simpleName(hydra.lib.strings.cat2(hydra.formatting.decapitalize(`lname_`))(hydra.formatting.capitalize(fname)))
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.FieldWithComments](hydra.ext.haskell.coder.adaptTypeToHaskellAndEncode(namespaces)(ftype)(cx)(g))((htype: hydra.ext.haskell.ast.Type) =>
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[scala.Predef.String], hydra.ext.haskell.ast.FieldWithComments](hydra.annotations.getTypeDescription(cx)(g)(ftype))((comments: Option[scala.Predef.String]) =>
        Right(hydra.ext.haskell.ast.FieldWithComments(hydra.ext.haskell.ast.Field(`hname_`, htype), comments))))
    }
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.haskell.ast.FieldWithComments], hydra.ext.haskell.ast.ConstructorWithComments](hydra.lib.eithers.mapList[hydra.core.FieldType, hydra.ext.haskell.ast.FieldWithComments, hydra.context.InContext[hydra.errors.Error]](toField)(fields))((hFields: Seq[hydra.ext.haskell.ast.FieldWithComments]) =>
      Right(hydra.ext.haskell.ast.ConstructorWithComments(hydra.ext.haskell.ast.Constructor.record(hydra.ext.haskell.ast.RecordConstructor(hydra.ext.haskell.utils.simpleName(`lname_`), hFields)), None)))
  }
  def unionCons(`boundNames_`: scala.collection.immutable.Set[hydra.core.Name])(`lname_`: scala.Predef.String)(fieldType: hydra.core.FieldType): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.ConstructorWithComments] =
    {
    lazy val fname: hydra.core.Name = (fieldType.name)
    lazy val ftype: hydra.core.Type = (fieldType.`type`)
    def deconflict(name: scala.Predef.String): scala.Predef.String =
      {
      lazy val tname: hydra.core.Name = hydra.names.unqualifyName(hydra.module.QualifiedName(Some(hydra.lib.pairs.first[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName](namespaces.focus)), name))
      hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.sets.member[hydra.core.Name](tname)(`boundNames_`))(deconflict(hydra.lib.strings.cat2(name)("_")))(name)
    }
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[scala.Predef.String], hydra.ext.haskell.ast.ConstructorWithComments](hydra.annotations.getTypeDescription(cx)(g)(ftype))((comments: Option[scala.Predef.String]) =>
      {
      lazy val nm: scala.Predef.String = deconflict(hydra.lib.strings.cat2(hydra.formatting.capitalize(`lname_`))(hydra.formatting.capitalize(fname)))
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.haskell.ast.Type], hydra.ext.haskell.ast.ConstructorWithComments](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.haskell.ast.Type]]](hydra.lib.equality.equal[hydra.core.Type](hydra.rewriting.deannotateType(ftype))(hydra.core.Type.unit))(Right(Seq()))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, Seq[hydra.ext.haskell.ast.Type]](hydra.ext.haskell.coder.adaptTypeToHaskellAndEncode(namespaces)(ftype)(cx)(g))((htype: hydra.ext.haskell.ast.Type) => Right(Seq(htype)))))((typeList: Seq[hydra.ext.haskell.ast.Type]) =>
        Right(hydra.ext.haskell.ast.ConstructorWithComments(hydra.ext.haskell.ast.Constructor.ordinary(hydra.ext.haskell.ast.OrdinaryConstructor(hydra.ext.haskell.utils.simpleName(nm), typeList)), comments)))
    })
  }
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Boolean, Seq[hydra.ext.haskell.ast.DeclarationWithComments]](hydra.schemas.isSerializableByName(cx)(g)(elementName))((isSer: Boolean) =>
    {
    lazy val deriv: hydra.ext.haskell.ast.Deriving = hydra.lib.logic.ifElse[Seq[hydra.ext.haskell.ast.Name]](isSer)(hydra.lib.lists.map[scala.Predef.String, hydra.ext.haskell.ast.Name](hydra.ext.haskell.utils.rawName)(Seq("Eq", "Ord", "Read", "Show")))(Seq())
    lazy val unpackResult: Tuple2[Seq[hydra.core.Name], hydra.core.Type] = hydra.ext.haskell.utils.unpackForallType(typ)
    lazy val vars: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name], hydra.core.Type](unpackResult)
    lazy val `t_`: hydra.core.Type = hydra.lib.pairs.second[Seq[hydra.core.Name], hydra.core.Type](unpackResult)
    lazy val hd: hydra.ext.haskell.ast.DeclarationHead = declHead(hname)(hydra.lib.lists.reverse[hydra.core.Name](vars))
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Declaration, Seq[hydra.ext.haskell.ast.DeclarationWithComments]](hydra.rewriting.deannotateType(`t_`) match
      case hydra.core.Type.record(v_Type_record_rt) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.ConstructorWithComments, hydra.ext.haskell.ast.Declaration](recordCons(lname)(v_Type_record_rt))((cons: hydra.ext.haskell.ast.ConstructorWithComments) =>
        Right(hydra.ext.haskell.ast.Declaration.data(hydra.ext.haskell.ast.DataDeclaration(hydra.ext.haskell.ast.DataOrNewtype.data, Seq(), hd, Seq(cons), Seq(deriv)))))
      case hydra.core.Type.union(v_Type_union_rt) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.haskell.ast.ConstructorWithComments], hydra.ext.haskell.ast.Declaration](hydra.lib.eithers.mapList[hydra.core.FieldType, hydra.ext.haskell.ast.ConstructorWithComments, hydra.context.InContext[hydra.errors.Error]]((v1: hydra.core.FieldType) =>
        unionCons(hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name, hydra.core.Term](g.boundTerms)))(lname)(v1))(v_Type_union_rt))((cons: Seq[hydra.ext.haskell.ast.ConstructorWithComments]) =>
        Right(hydra.ext.haskell.ast.Declaration.data(hydra.ext.haskell.ast.DataDeclaration(hydra.ext.haskell.ast.DataOrNewtype.data, Seq(), hd, cons, Seq(deriv)))))
      case hydra.core.Type.wrap(v_Type_wrap_wrapped) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.ConstructorWithComments, hydra.ext.haskell.ast.Declaration](newtypeCons(elementName)(v_Type_wrap_wrapped))((cons: hydra.ext.haskell.ast.ConstructorWithComments) =>
        Right(hydra.ext.haskell.ast.Declaration.data(hydra.ext.haskell.ast.DataDeclaration(hydra.ext.haskell.ast.DataOrNewtype.newtype, Seq(), hd, Seq(cons), Seq(deriv)))))
      case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Declaration](hydra.ext.haskell.coder.adaptTypeToHaskellAndEncode(namespaces)(typ)(cx)(g))((htype: hydra.ext.haskell.ast.Type) =>
        Right(hydra.ext.haskell.ast.Declaration.`type`(hydra.ext.haskell.ast.TypeDeclaration(hd, htype)))))((decl: hydra.ext.haskell.ast.Declaration) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[scala.Predef.String], Seq[hydra.ext.haskell.ast.DeclarationWithComments]](hydra.annotations.getTypeDescription(cx)(g)(typ))((comments: Option[scala.Predef.String]) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.haskell.ast.DeclarationWithComments], Seq[hydra.ext.haskell.ast.DeclarationWithComments]](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.haskell.ast.DeclarationWithComments]]](hydra.ext.haskell.coder.includeTypeDefinitions)(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.DeclarationWithComments, Seq[hydra.ext.haskell.ast.DeclarationWithComments]](hydra.ext.haskell.coder.typeDecl(namespaces)(elementName)(typ)(cx)(g))((`decl_`: hydra.ext.haskell.ast.DeclarationWithComments) => Right(Seq(`decl_`))))(Right(Seq())))((tdecls: Seq[hydra.ext.haskell.ast.DeclarationWithComments]) =>
      {
      lazy val mainDecl: hydra.ext.haskell.ast.DeclarationWithComments = hydra.ext.haskell.ast.DeclarationWithComments(decl, comments)
      lazy val `nameDecls_`: Seq[hydra.ext.haskell.ast.DeclarationWithComments] = hydra.ext.haskell.coder.nameDecls(namespaces)(elementName)(typ)
      Right(hydra.lib.lists.concat[hydra.ext.haskell.ast.DeclarationWithComments](Seq(Seq(mainDecl), `nameDecls_`, tdecls)))
    })))
  })
}

def typeDecl(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(name: hydra.core.Name)(typ: hydra.core.Type)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.DeclarationWithComments] =
  {
  def typeName(ns: hydra.module.Namespace)(`name_`: hydra.core.Name): hydra.core.Name = hydra.names.qname(ns)(typeNameLocal(`name_`))
  def typeNameLocal(`name_`: hydra.core.Name): scala.Predef.String = hydra.lib.strings.cat(Seq("_", hydra.names.localNameOf(`name_`), "_type_"))
  lazy val rawTerm: hydra.core.Term = hydra.encode.core.`type`(typ)
  def rewrite(recurse: (hydra.core.Term => hydra.core.Term))(term: hydra.core.Term): hydra.core.Term =
    {
    lazy val variantResult: Option[hydra.core.Field] = hydra.rewriting.deannotateTerm(term) match
      case hydra.core.Term.union(v_Term_union_inj) => hydra.lib.logic.ifElse[Option[hydra.core.Field]](hydra.lib.equality.equal[hydra.core.Name](v_Term_union_inj.typeName)("hydra.core.Type"))(Some(v_Term_union_inj.field))(None)
      case _ => None
    def decodeString(term2: hydra.core.Term): Option[scala.Predef.String] =
      hydra.rewriting.deannotateTerm(term2) match
      case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
        case hydra.core.Literal.string(v_Literal_string_s) => Some(v_Literal_string_s)
        case _ => None
      case _ => None
    def decodeName(term2: hydra.core.Term): Option[hydra.core.Name] =
      hydra.rewriting.deannotateTerm(term2) match
      case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.lib.logic.ifElse[Option[hydra.core.Name]](hydra.lib.equality.equal[hydra.core.Name](v_Term_wrap_wt.typeName)("hydra.core.Name"))(hydra.lib.maybes.map[scala.Predef.String, hydra.core.Name]((x: scala.Predef.String) => x)(decodeString(v_Term_wrap_wt.body)))(None)
      case _ => None
    def forType(field: hydra.core.Field): Option[hydra.core.Term] =
      {
      lazy val fname: hydra.core.Name = (field.name)
      lazy val fterm: hydra.core.Term = (field.term)
      hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](fname)("record"))(None)(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](fname)("variable"))(hydra.lib.maybes.bind[hydra.core.Name, hydra.core.Term](decodeName(fterm))(forVariableType))(None))
    }
    def forVariableType(vname: hydra.core.Name): Option[hydra.core.Term] =
      {
      lazy val qname: hydra.module.QualifiedName = hydra.names.qualifyName(vname)
      lazy val mns: Option[hydra.module.Namespace] = (qname.namespace)
      lazy val local: scala.Predef.String = (qname.local)
      hydra.lib.maybes.map[hydra.module.Namespace, hydra.core.Term]((ns: hydra.module.Namespace) =>
        hydra.core.Term.variable(hydra.names.qname(ns)(hydra.lib.strings.cat(Seq("_", local, "_type_")))))(mns)
    }
    hydra.lib.maybes.fromMaybe[hydra.core.Term](recurse(term))(hydra.lib.maybes.bind[hydra.core.Field, hydra.core.Term](variantResult)(forType))
  }
  lazy val finalTerm: hydra.core.Term = hydra.rewriting.rewriteTerm(rewrite)(rawTerm)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.DeclarationWithComments](hydra.ext.haskell.coder.encodeTerm(0)(namespaces)(finalTerm)(cx)(g))((expr: hydra.ext.haskell.ast.Expression) =>
    {
    lazy val rhs: hydra.ext.haskell.ast.RightHandSide = expr
    lazy val hname: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.simpleName(typeNameLocal(name))
    lazy val pat: hydra.ext.haskell.ast.Pattern = hydra.ext.haskell.utils.applicationPattern(hname)(Seq())
    lazy val decl: hydra.ext.haskell.ast.Declaration = hydra.ext.haskell.ast.Declaration.valueBinding(hydra.ext.haskell.ast.ValueBinding.simple(hydra.ext.haskell.ast.SimpleValueBinding(pat, rhs, None)))
    Right(hydra.ext.haskell.ast.DeclarationWithComments(decl, None))
  })
}

def typeSchemeConstraintsToClassMap[T0](maybeConstraints: Option[Map[T0, hydra.core.TypeVariableMetadata]]): Map[T0, scala.collection.immutable.Set[hydra.classes.TypeClass]] =
  {
  def nameToTypeClass(className: hydra.core.Name): Option[hydra.classes.TypeClass] =
    {
    lazy val classNameStr: scala.Predef.String = className
    lazy val isEq: Boolean = hydra.lib.equality.equal[scala.Predef.String](classNameStr)("equality")
    lazy val isOrd: Boolean = hydra.lib.equality.equal[scala.Predef.String](classNameStr)("ordering")
    hydra.lib.logic.ifElse[Option[hydra.classes.TypeClass]](isEq)(Some(hydra.classes.TypeClass.equality))(hydra.lib.logic.ifElse[Option[hydra.classes.TypeClass]](isOrd)(Some(hydra.classes.TypeClass.ordering))(None))
  }
  hydra.lib.maybes.maybe[Map[T0, scala.collection.immutable.Set[hydra.classes.TypeClass]], Map[T0, hydra.core.TypeVariableMetadata]](hydra.lib.maps.empty[T0, scala.collection.immutable.Set[hydra.classes.TypeClass]])((constraints: Map[T0, hydra.core.TypeVariableMetadata]) =>
    hydra.lib.maps.map[hydra.core.TypeVariableMetadata, scala.collection.immutable.Set[hydra.classes.TypeClass], T0]((meta: hydra.core.TypeVariableMetadata) =>
    hydra.lib.sets.fromList[hydra.classes.TypeClass](hydra.lib.maybes.cat[hydra.classes.TypeClass](hydra.lib.lists.map[hydra.core.Name, Option[hydra.classes.TypeClass]](nameToTypeClass)(hydra.lib.sets.toList[hydra.core.Name](meta.classes)))))(constraints))(maybeConstraints)
}
