package hydra.ext.haskell.coder

import hydra.classes.*

import hydra.coders.*

import hydra.context.*

import hydra.core.*

import hydra.error.*

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

val includeTypeDefinitions: Boolean = false

val useCoreImport: Boolean = true

val keyHaskellVar: hydra.core.Name = "haskellVar"

def adaptTypeToHaskellAndEncode[T0](namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(typ: hydra.core.Type)(cx: hydra.context.Context)(g: T0): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.haskell.ast.Type] =
  {
  def enc(t: hydra.core.Type): Either[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Type] = hydra.ext.haskell.coder.encodeType(namespaces)(t)(cx)(g)
  hydra.rewriting.deannotateType(typ) match
    case hydra.core.Type.variable(v_Type_variable__) => enc(typ)
    case _ => eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, hydra.ext.haskell.ast.Type](eithers.bimap[scala.Predef.String,
       hydra.core.Type, hydra.context.InContext[hydra.error.Error], hydra.core.Type]((_s: scala.Predef.String) => hydra.context.InContext(hydra.error.Error.other(_s),
       cx))((_x: hydra.core.Type) => _x)(hydra.adapt.adaptTypeForLanguage(hydra.ext.haskell.language.haskellLanguage)(typ)))((adaptedType: hydra.core.Type) => enc(adaptedType))
}

def constantForFieldName(tname: hydra.core.Name)(fname: hydra.core.Name): scala.Predef.String = strings.cat(Seq("_",
   hydra.names.localNameOf(tname), "_", fname))

def constantForTypeName(tname: hydra.core.Name): scala.Predef.String = strings.cat2("_")(hydra.names.localNameOf(tname))

def constructModule(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(mod: hydra.module.Module)(defs: Seq[hydra.module.Definition])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.haskell.ast.Module] =
  {
  def h(namespace: hydra.module.Namespace): scala.Predef.String = namespace
  def createDeclarations(`def`: hydra.module.Definition): Either[hydra.context.InContext[hydra.error.Error],
     Seq[hydra.ext.haskell.ast.DeclarationWithComments]] =
    `def` match
    case hydra.module.Definition.`type`(v_Definition_type_type) => {
      val name: hydra.core.Name = (v_Definition_type_type.name)
      val typ: hydra.core.Type = (v_Definition_type_type.`type`)
      hydra.ext.haskell.coder.toTypeDeclarationsFrom(namespaces)(name)(typ)(cx)(g)
    }
    case hydra.module.Definition.term(v_Definition_term_term) => eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.haskell.ast.DeclarationWithComments, Seq[hydra.ext.haskell.ast.DeclarationWithComments]](hydra.ext.haskell.coder.toDataDeclaration(namespaces)(v_Definition_term_term)(cx)(g))((d: hydra.ext.haskell.ast.DeclarationWithComments) => Right(Seq(d)))
  def importName(name: scala.Predef.String): hydra.ext.haskell.ast.ModuleName =
    strings.intercalate(".")(lists.map[scala.Predef.String, scala.Predef.String](hydra.formatting.capitalize)(strings.splitOn(".")(name)))
  val imports: Seq[hydra.ext.haskell.ast.Import] = lists.concat2[hydra.ext.haskell.ast.Import](domainImports)(standardImports)
  val domainImports: Seq[hydra.ext.haskell.ast.Import] = {
    def toImport(pair: Tuple2[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName]): hydra.ext.haskell.ast.Import =
      {
      val namespace: hydra.module.Namespace = pairs.first[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName](pair)
      val alias: hydra.ext.haskell.ast.ModuleName = pairs.second[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName](pair)
      val name: scala.Predef.String = h(namespace)
      hydra.ext.haskell.ast.Import(true, importName(name), Some(alias), None)
    }
    lists.map[Tuple2[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName], hydra.ext.haskell.ast.Import](toImport)(maps.toList[hydra.module.Namespace,
       hydra.ext.haskell.ast.ModuleName](namespaces.mapping))
  }
  val standardImports: Seq[hydra.ext.haskell.ast.Import] = {
    def toImport(triple: Tuple2[Tuple2[scala.Predef.String, Option[scala.Predef.String]], Seq[scala.Predef.String]]): hydra.ext.haskell.ast.Import =
      {
      val name: scala.Predef.String = pairs.first[scala.Predef.String, Option[scala.Predef.String]](pairs.first[Tuple2[scala.Predef.String,
         Option[scala.Predef.String]], Seq[scala.Predef.String]](triple))
      val malias: Option[scala.Predef.String] = pairs.second[scala.Predef.String, Option[scala.Predef.String]](pairs.first[Tuple2[scala.Predef.String,
         Option[scala.Predef.String]], Seq[scala.Predef.String]](triple))
      val hidden: Seq[scala.Predef.String] = pairs.second[Tuple2[scala.Predef.String, Option[scala.Predef.String]], Seq[scala.Predef.String]](triple)
      val spec: Option[hydra.ext.haskell.ast.SpecImport] = logic.ifElse[Option[hydra.ext.haskell.ast.SpecImport]](lists.`null`[scala.Predef.String](hidden))(None)(Some(hydra.ext.haskell.ast.SpecImport.hiding(lists.map[scala.Predef.String,
         hydra.ext.haskell.ast.ImportExportSpec]((n: scala.Predef.String) =>
        hydra.ext.haskell.ast.ImportExportSpec(None, hydra.ext.haskell.utils.simpleName(n), None))(hidden))))
      hydra.ext.haskell.ast.Import(maybes.isJust[scala.Predef.String](malias), name, maybes.map[scala.Predef.String,
         hydra.ext.haskell.ast.ModuleName]((x: scala.Predef.String) => x)(malias), spec)
    }
    lists.map[Tuple2[Tuple2[scala.Predef.String, Option[scala.Predef.String]], Seq[scala.Predef.String]],
       hydra.ext.haskell.ast.Import](toImport)(lists.concat2[Tuple2[Tuple2[scala.Predef.String, Option[scala.Predef.String]],
       Seq[scala.Predef.String]]](Seq(Tuple2(Tuple2("Prelude", None), Seq("Enum", "Ordering", "decodeFloat",
       "encodeFloat", "fail", "map", "pure", "sum")), Tuple2(Tuple2("Data.ByteString", Some("B")), Seq()),
       Tuple2(Tuple2("Data.Int", Some("I")), Seq()), Tuple2(Tuple2("Data.List", Some("L")), Seq()), Tuple2(Tuple2("Data.Map",
       Some("M")), Seq()), Tuple2(Tuple2("Data.Set", Some("S")), Seq())))(logic.ifElse[Seq[Tuple2[Tuple2[scala.Predef.String,
       Option[scala.Predef.String]], Seq[scala.Predef.String]]]](hydra.schemas.moduleContainsBinaryLiterals(mod))(Seq(Tuple2(Tuple2("Hydra.Lib.Literals",
       Some("Literals")), Seq())))(Seq())))
  }
  eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[Seq[hydra.ext.haskell.ast.DeclarationWithComments]],
     hydra.ext.haskell.ast.Module](eithers.mapList[hydra.module.Definition, Seq[hydra.ext.haskell.ast.DeclarationWithComments],
     hydra.context.InContext[hydra.error.Error]](createDeclarations)(defs))((declLists: Seq[Seq[hydra.ext.haskell.ast.DeclarationWithComments]]) =>
    {
    val decls: Seq[hydra.ext.haskell.ast.DeclarationWithComments] = lists.concat[hydra.ext.haskell.ast.DeclarationWithComments](declLists)
    val mc: Option[scala.Predef.String] = (mod.description)
    Right(hydra.ext.haskell.ast.Module(Some(hydra.ext.haskell.ast.ModuleHead(mc, importName(h(mod.namespace)), Seq())), imports, decls))
  })
}

def encodeCaseExpression(depth: Int)(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(stmt: hydra.core.CaseStatement)(scrutinee: hydra.ext.haskell.ast.Expression)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.haskell.ast.Expression] =
  {
  val dn: hydra.core.Name = (stmt.typeName)
  val `def`: Option[hydra.core.Term] = (stmt.default)
  val fields: Seq[hydra.core.Field] = (stmt.cases)
  def toAlt(fieldMap: Map[hydra.core.Name, hydra.core.FieldType])(field: hydra.core.Field): Either[hydra.context.InContext[hydra.error.Error],
     hydra.ext.haskell.ast.Alternative] =
    {
    val fn: hydra.core.Name = (field.name)
    val `fun_`: hydra.core.Term = (field.term)
    val v0: scala.Predef.String = strings.cat2("v")(literals.showInt32(depth))
    val raw: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(`fun_`, hydra.core.Term.variable(v0)))
    val rhsTerm: hydra.core.Term = hydra.rewriting.simplifyTerm(raw)
    val v1: scala.Predef.String = logic.ifElse[scala.Predef.String](hydra.rewriting.isFreeVariableInTerm(v0)(rhsTerm))(hydra.constants.ignoredVariable)(v0)
    val hname: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.unionFieldReference(sets.fromList[hydra.core.Name](maps.keys[hydra.core.Name,
       hydra.core.Term](g.boundTerms)))(namespaces)(dn)(fn)
    eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.haskell.ast.Pattern], hydra.ext.haskell.ast.Alternative](maybes.cases[hydra.core.FieldType,
       Either[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.haskell.ast.Pattern]]](maps.lookup[hydra.core.Name,
       hydra.core.FieldType](fn)(fieldMap))(Left(hydra.context.InContext(hydra.error.Error.other(strings.cat(Seq("field ",
       literals.showString(fn), " not found in ", literals.showString(dn)))), cx)))((fieldType: hydra.core.FieldType) =>
      {
      val ft: hydra.core.Type = (fieldType.`type`)
      def noArgs[T0]: Seq[T0] = Seq()
      val singleArg: Seq[hydra.ext.haskell.ast.Pattern] = Seq(hydra.ext.haskell.ast.Pattern.name(hydra.ext.haskell.utils.rawName(v1)))
      hydra.rewriting.deannotateType(ft) match
        case hydra.core.Type.unit => Right(noArgs)
        case _ => Right(singleArg)
    }))((args: Seq[hydra.ext.haskell.ast.Pattern]) =>
      {
      val lhs: hydra.ext.haskell.ast.Pattern = hydra.ext.haskell.utils.applicationPattern(hname)(args)
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.CaseRhs, hydra.ext.haskell.ast.Alternative](eithers.map[hydra.ext.haskell.ast.Expression,
         hydra.ext.haskell.ast.CaseRhs, hydra.context.InContext[hydra.error.Error]]((x: hydra.ext.haskell.ast.Expression) => x)(hydra.ext.haskell.coder.encodeTerm(math.add(depth)(1))(namespaces)(rhsTerm)(cx)(g)))((rhs: hydra.ext.haskell.ast.CaseRhs) => Right(hydra.ext.haskell.ast.Alternative(lhs,
         rhs, None)))
    })
  }
  eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.FieldType], hydra.ext.haskell.ast.Expression](hydra.schemas.requireUnionType(cx)(g)(dn))((rt: Seq[hydra.core.FieldType]) =>
    {
    def toFieldMapEntry(f: hydra.core.FieldType): Tuple2[hydra.core.Name, hydra.core.FieldType] = Tuple2(f.name, f)
    val fieldMap: Map[hydra.core.Name, hydra.core.FieldType] = maps.fromList[hydra.core.Name, hydra.core.FieldType](lists.map[hydra.core.FieldType,
       Tuple2[hydra.core.Name, hydra.core.FieldType]](toFieldMapEntry)(rt))
    eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.haskell.ast.Alternative], hydra.ext.haskell.ast.Expression](eithers.mapList[hydra.core.Field,
       hydra.ext.haskell.ast.Alternative, hydra.context.InContext[hydra.error.Error]]((v1: hydra.core.Field) => toAlt(fieldMap)(v1))(fields))((ecases: Seq[hydra.ext.haskell.ast.Alternative]) =>
      eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.haskell.ast.Alternative],
         hydra.ext.haskell.ast.Expression](maybes.cases[hydra.core.Term, Either[hydra.context.InContext[hydra.error.Error],
         Seq[hydra.ext.haskell.ast.Alternative]]](`def`)(Right(Seq()))((d: hydra.core.Term) =>
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.CaseRhs, Seq[hydra.ext.haskell.ast.Alternative]](eithers.map[hydra.ext.haskell.ast.Expression,
         hydra.ext.haskell.ast.CaseRhs, hydra.context.InContext[hydra.error.Error]]((x: hydra.ext.haskell.ast.Expression) => x)(hydra.ext.haskell.coder.encodeTerm(depth)(namespaces)(d)(cx)(g)))((cs: hydra.ext.haskell.ast.CaseRhs) =>
      {
      val lhs: hydra.ext.haskell.ast.Pattern = hydra.ext.haskell.ast.Pattern.name(hydra.ext.haskell.utils.rawName(hydra.constants.ignoredVariable))
      val alt: hydra.ext.haskell.ast.Alternative = hydra.ext.haskell.ast.Alternative(lhs, cs, None)
      Right(Seq(alt))
    })))((dcases: Seq[hydra.ext.haskell.ast.Alternative]) =>
      Right(hydra.ext.haskell.ast.Expression.`case`(hydra.ext.haskell.ast.CaseExpression(scrutinee, lists.concat2[hydra.ext.haskell.ast.Alternative](ecases)(dcases))))))
  })
}

def encodeFunction(depth: Int)(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(fun: hydra.core.Function)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.haskell.ast.Expression] =
  fun match
  case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
    case hydra.core.Elimination.wrap(v_Elimination_wrap_name) => Right(hydra.ext.haskell.ast.Expression.variable(hydra.ext.haskell.utils.elementReference(namespaces)(hydra.names.qname(maybes.fromJust[hydra.module.Namespace](hydra.names.namespaceOf(v_Elimination_wrap_name)))(hydra.ext.haskell.utils.newtypeAccessorName(v_Elimination_wrap_name)))))
    case hydra.core.Elimination.record(v_Elimination_record_proj) => {
      val dn: hydra.core.Name = (v_Elimination_record_proj.typeName)
      val fname: hydra.core.Name = (v_Elimination_record_proj.field)
      Right(hydra.ext.haskell.ast.Expression.variable(hydra.ext.haskell.utils.recordFieldReference(namespaces)(dn)(fname)))
    }
    case hydra.core.Elimination.union(v_Elimination_union_stmt) => eithers.map[hydra.ext.haskell.ast.Expression,
       hydra.ext.haskell.ast.Expression, hydra.context.InContext[hydra.error.Error]]((v1: hydra.ext.haskell.ast.Expression) =>
      hydra.ext.haskell.utils.hslambda(hydra.ext.haskell.utils.rawName("x"))(v1))(hydra.ext.haskell.coder.encodeCaseExpression(depth)(namespaces)(v_Elimination_union_stmt)(hydra.ext.haskell.utils.hsvar("x"))(cx)(g))
  case hydra.core.Function.lambda(v_Function_lambda_lam) => {
    val v: hydra.core.Name = (v_Function_lambda_lam.parameter)
    val body: hydra.core.Term = (v_Function_lambda_lam.body)
    eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](hydra.ext.haskell.coder.encodeTerm(depth)(namespaces)(body)(cx)(g))((hbody: hydra.ext.haskell.ast.Expression) =>
      Right(hydra.ext.haskell.utils.hslambda(hydra.ext.haskell.utils.elementReference(namespaces)(v))(hbody)))
  }
  case hydra.core.Function.primitive(v_Function_primitive_name) => Right(hydra.ext.haskell.ast.Expression.variable(hydra.ext.haskell.utils.elementReference(namespaces)(v_Function_primitive_name)))

def encodeLiteral(l: hydra.core.Literal)(cx: hydra.context.Context): Either[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression] =
  l match
  case hydra.core.Literal.binary(v_Literal_binary_bs) => Right(hydra.ext.haskell.utils.hsapp(hydra.ext.haskell.utils.hsvar("Literals.stringToBinary"))(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.string(literals.binaryToString(v_Literal_binary_bs)))))
  case hydra.core.Literal.boolean(v_Literal_boolean_b) => Right(hydra.ext.haskell.utils.hsvar(logic.ifElse[scala.Predef.String](v_Literal_boolean_b)("True")("False")))
  case hydra.core.Literal.float(v_Literal_float_fv) => v_Literal_float_fv match
    case hydra.core.FloatValue.float32(v_FloatValue_float32_f) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.float(v_FloatValue_float32_f)))
    case hydra.core.FloatValue.float64(v_FloatValue_float64_f) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.double(v_FloatValue_float64_f)))
    case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_f) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.double(literals.bigfloatToFloat64(v_FloatValue_bigfloat_f))))
  case hydra.core.Literal.integer(v_Literal_integer_iv) => v_Literal_integer_iv match
    case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.integer(v_IntegerValue_bigint_i)))
    case hydra.core.IntegerValue.int8(v_IntegerValue_int8_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.integer(literals.int8ToBigint(v_IntegerValue_int8_i))))
    case hydra.core.IntegerValue.int16(v_IntegerValue_int16_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.integer(literals.int16ToBigint(v_IntegerValue_int16_i))))
    case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.int(v_IntegerValue_int32_i)))
    case hydra.core.IntegerValue.int64(v_IntegerValue_int64_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.integer(literals.int64ToBigint(v_IntegerValue_int64_i))))
    case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.integer(literals.uint8ToBigint(v_IntegerValue_uint8_i))))
    case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.integer(literals.uint16ToBigint(v_IntegerValue_uint16_i))))
    case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.integer(literals.uint32ToBigint(v_IntegerValue_uint32_i))))
    case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_i) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.integer(literals.uint64ToBigint(v_IntegerValue_uint64_i))))
  case hydra.core.Literal.string(v_Literal_string_s) => Right(hydra.ext.haskell.utils.hslit(hydra.ext.haskell.ast.Literal.string(v_Literal_string_s)))
  case _ => Left(hydra.context.InContext(hydra.error.Error.other(strings.cat2("literal value ")(hydra.show.core.literal(l))), cx))

def encodeTerm(depth: Int)(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(term: hydra.core.Term)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.haskell.ast.Expression] =
  {
  def encode(t: hydra.core.Term): Either[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression] = hydra.ext.haskell.coder.encodeTerm(depth)(namespaces)(t)(cx)(g)
  def nonemptyMap(m: Map[hydra.core.Term, hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression] =
    {
    val lhs: hydra.ext.haskell.ast.Expression = hydra.ext.haskell.utils.hsvar("M.fromList")
    def encodePair(pair: Tuple2[hydra.core.Term, hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression] =
      {
      val k: hydra.core.Term = pairs.first[hydra.core.Term, hydra.core.Term](pair)
      val v: hydra.core.Term = pairs.second[hydra.core.Term, hydra.core.Term](pair)
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(k))((hk: hydra.ext.haskell.ast.Expression) =>
        eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(v))((hv: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.ast.Expression.tuple(Seq(hk,
           hv)))))
    }
    eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](eithers.map[Seq[hydra.ext.haskell.ast.Expression],
       hydra.ext.haskell.ast.Expression, hydra.context.InContext[hydra.error.Error]]((x: Seq[hydra.ext.haskell.ast.Expression]) => hydra.ext.haskell.ast.Expression.list(x))(eithers.mapList[Tuple2[hydra.core.Term,
       hydra.core.Term], hydra.ext.haskell.ast.Expression, hydra.context.InContext[hydra.error.Error]](encodePair)(maps.toList[hydra.core.Term,
       hydra.core.Term](m))))((rhs: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.utils.hsapp(lhs)(rhs)))
  }
  def nonemptySet(s: scala.collection.immutable.Set[hydra.core.Term]): Either[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression] =
    {
    val lhs: hydra.ext.haskell.ast.Expression = hydra.ext.haskell.utils.hsvar("S.fromList")
    eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](hydra.ext.haskell.coder.encodeTerm(depth)(namespaces)(hydra.core.Term.list(sets.toList[hydra.core.Term](s)))(cx)(g))((rhs: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.utils.hsapp(lhs)(rhs)))
  }
  hydra.rewriting.deannotateTerm(term) match
    case hydra.core.Term.application(v_Term_application_app) => {
      val fun: hydra.core.Term = (v_Term_application_app.function)
      val arg: hydra.core.Term = (v_Term_application_app.argument)
      val deannotatedFun: hydra.core.Term = hydra.rewriting.deannotateTerm(fun)
      deannotatedFun match
        case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
          case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
            case hydra.core.Elimination.union(v_Elimination_union_stmt) => eithers.bind[hydra.context.InContext[hydra.error.Error],
               hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(arg))((harg: hydra.ext.haskell.ast.Expression) =>
              hydra.ext.haskell.coder.encodeCaseExpression(depth)(namespaces)(v_Elimination_union_stmt)(harg)(cx)(g))
            case _ => eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression,
               hydra.ext.haskell.ast.Expression](encode(fun))((hfun: hydra.ext.haskell.ast.Expression) =>
              eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression,
                 hydra.ext.haskell.ast.Expression](encode(arg))((harg: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.utils.hsapp(hfun)(harg))))
          case _ => eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression,
             hydra.ext.haskell.ast.Expression](encode(fun))((hfun: hydra.ext.haskell.ast.Expression) =>
            eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression,
               hydra.ext.haskell.ast.Expression](encode(arg))((harg: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.utils.hsapp(hfun)(harg))))
        case _ => eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression,
           hydra.ext.haskell.ast.Expression](encode(fun))((hfun: hydra.ext.haskell.ast.Expression) =>
          eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(arg))((harg: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.utils.hsapp(hfun)(harg))))
    }
    case hydra.core.Term.either(v_Term_either_e) => eithers.either[hydra.core.Term, hydra.core.Term, Either[hydra.context.InContext[hydra.error.Error],
       hydra.ext.haskell.ast.Expression]]((l: hydra.core.Term) =>
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(l))((hl: hydra.ext.haskell.ast.Expression) =>
      Right(hydra.ext.haskell.utils.hsapp(hydra.ext.haskell.utils.hsvar("Left"))(hl))))((r: hydra.core.Term) =>
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(r))((hr: hydra.ext.haskell.ast.Expression) =>
      Right(hydra.ext.haskell.utils.hsapp(hydra.ext.haskell.utils.hsvar("Right"))(hr))))(v_Term_either_e)
    case hydra.core.Term.function(v_Term_function_f) => hydra.ext.haskell.coder.encodeFunction(depth)(namespaces)(v_Term_function_f)(cx)(g)
    case hydra.core.Term.let(v_Term_let_letTerm) => {
      def collectBindings(lt: hydra.core.Let): Tuple2[Seq[hydra.core.Binding], hydra.core.Term] =
        {
        val bs: Seq[hydra.core.Binding] = (lt.bindings)
        val body: hydra.core.Term = (lt.body)
        hydra.rewriting.deannotateTerm(body) match
          case hydra.core.Term.let(v_Term_let_innerLt) => {
            val innerResult: Tuple2[Seq[hydra.core.Binding], hydra.core.Term] = collectBindings(v_Term_let_innerLt)
            Tuple2(lists.concat2[hydra.core.Binding](bs)(pairs.first[Seq[hydra.core.Binding], hydra.core.Term](innerResult)),
               pairs.second[Seq[hydra.core.Binding], hydra.core.Term](innerResult))
          }
          case _ => Tuple2(bs, body)
      }
      val collected: Tuple2[Seq[hydra.core.Binding], hydra.core.Term] = collectBindings(v_Term_let_letTerm)
      val allBindings: Seq[hydra.core.Binding] = pairs.first[Seq[hydra.core.Binding], hydra.core.Term](collected)
      val finalBody: hydra.core.Term = pairs.second[Seq[hydra.core.Binding], hydra.core.Term](collected)
      def encodeBinding(binding: hydra.core.Binding): Either[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.LocalBinding] =
        {
        val name: hydra.core.Name = (binding.name)
        val `term_`: hydra.core.Term = (binding.term)
        val hname: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.simpleName(name)
        eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.LocalBinding](encode(`term_`))((hexpr: hydra.ext.haskell.ast.Expression) =>
          Right(hydra.ext.haskell.ast.LocalBinding.value(hydra.ext.haskell.utils.simpleValueBinding(hname)(hexpr)(None))))
      }
      eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.haskell.ast.LocalBinding],
         hydra.ext.haskell.ast.Expression](eithers.mapList[hydra.core.Binding, hydra.ext.haskell.ast.LocalBinding,
         hydra.context.InContext[hydra.error.Error]](encodeBinding)(allBindings))((hbindings: Seq[hydra.ext.haskell.ast.LocalBinding]) =>
        eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(finalBody))((hinner: hydra.ext.haskell.ast.Expression) =>
        Right(hydra.ext.haskell.ast.Expression.let(hydra.ext.haskell.ast.LetExpression(hbindings, hinner)))))
    }
    case hydra.core.Term.list(v_Term_list_els) => eithers.bind[hydra.context.InContext[hydra.error.Error],
       Seq[hydra.ext.haskell.ast.Expression], hydra.ext.haskell.ast.Expression](eithers.mapList[hydra.core.Term,
       hydra.ext.haskell.ast.Expression, hydra.context.InContext[hydra.error.Error]](encode)(v_Term_list_els))((helems: Seq[hydra.ext.haskell.ast.Expression]) => Right(hydra.ext.haskell.ast.Expression.list(helems)))
    case hydra.core.Term.literal(v_Term_literal_v) => hydra.ext.haskell.coder.encodeLiteral(v_Term_literal_v)(cx)
    case hydra.core.Term.map(v_Term_map_m) => logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
       hydra.ext.haskell.ast.Expression]](maps.`null`[hydra.core.Term, hydra.core.Term](v_Term_map_m))(Right(hydra.ext.haskell.utils.hsvar("M.empty")))(nonemptyMap(v_Term_map_m))
    case hydra.core.Term.maybe(v_Term_maybe_m) => maybes.cases[hydra.core.Term, Either[hydra.context.InContext[hydra.error.Error],
       hydra.ext.haskell.ast.Expression]](v_Term_maybe_m)(Right(hydra.ext.haskell.utils.hsvar("Nothing")))((t: hydra.core.Term) =>
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(t))((ht: hydra.ext.haskell.ast.Expression) =>
      Right(hydra.ext.haskell.utils.hsapp(hydra.ext.haskell.utils.hsvar("Just"))(ht))))
    case hydra.core.Term.pair(v_Term_pair_p) => eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(pairs.first[hydra.core.Term,
       hydra.core.Term](v_Term_pair_p)))((f: hydra.ext.haskell.ast.Expression) =>
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(pairs.second[hydra.core.Term,
         hydra.core.Term](v_Term_pair_p)))((s: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.ast.Expression.tuple(Seq(f,
         s)))))
    case hydra.core.Term.record(v_Term_record_record) => {
      val sname: hydra.core.Name = (v_Term_record_record.typeName)
      val fields: Seq[hydra.core.Field] = (v_Term_record_record.fields)
      def toFieldUpdate(field: hydra.core.Field): Either[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.FieldUpdate] =
        {
        val fn: hydra.core.Name = (field.name)
        val ft: hydra.core.Term = (field.term)
        val fieldRef: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.recordFieldReference(namespaces)(sname)(fn)
        eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.FieldUpdate](encode(ft))((hft: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.ast.FieldUpdate(fieldRef,
           hft)))
      }
      val typeName: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.elementReference(namespaces)(sname)
      eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.haskell.ast.FieldUpdate],
         hydra.ext.haskell.ast.Expression](eithers.mapList[hydra.core.Field, hydra.ext.haskell.ast.FieldUpdate,
         hydra.context.InContext[hydra.error.Error]](toFieldUpdate)(fields))((updates: Seq[hydra.ext.haskell.ast.FieldUpdate]) =>
        Right(hydra.ext.haskell.ast.Expression.constructRecord(hydra.ext.haskell.ast.ConstructRecordExpression(typeName, updates))))
    }
    case hydra.core.Term.set(v_Term_set_s) => logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
       hydra.ext.haskell.ast.Expression]](sets.`null`[hydra.core.Term](v_Term_set_s))(Right(hydra.ext.haskell.utils.hsvar("S.empty")))(nonemptySet(v_Term_set_s))
    case hydra.core.Term.typeLambda(v_Term_typeLambda_abs) => {
      val term1: hydra.core.Term = (v_Term_typeLambda_abs.body)
      encode(term1)
    }
    case hydra.core.Term.typeApplication(v_Term_typeApplication_typed) => {
      val term1: hydra.core.Term = (v_Term_typeApplication_typed.body)
      encode(term1)
    }
    case hydra.core.Term.union(v_Term_union_injection) => {
      val sname: hydra.core.Name = (v_Term_union_injection.typeName)
      val field: hydra.core.Field = (v_Term_union_injection.field)
      val fn: hydra.core.Name = (field.name)
      val ft: hydra.core.Term = (field.term)
      val lhs: hydra.ext.haskell.ast.Expression = hydra.ext.haskell.ast.Expression.variable(hydra.ext.haskell.utils.unionFieldReference(sets.fromList[hydra.core.Name](maps.keys[hydra.core.Name,
         hydra.core.Term](g.boundTerms)))(namespaces)(sname)(fn))
      val dflt: Either[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression] = eithers.map[hydra.ext.haskell.ast.Expression,
         hydra.ext.haskell.ast.Expression, hydra.context.InContext[hydra.error.Error]]((v1: hydra.ext.haskell.ast.Expression) => hydra.ext.haskell.utils.hsapp(lhs)(v1))(encode(ft))
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Type, hydra.ext.haskell.ast.Expression](hydra.schemas.requireUnionField(cx)(g)(sname)(fn))((ftyp: hydra.core.Type) =>
        hydra.rewriting.deannotateType(ftyp) match
        case hydra.core.Type.unit => Right(lhs)
        case _ => dflt)
    }
    case hydra.core.Term.unit => Right(hydra.ext.haskell.ast.Expression.tuple(Seq()))
    case hydra.core.Term.variable(v_Term_variable_name) => Right(hydra.ext.haskell.ast.Expression.variable(hydra.ext.haskell.utils.elementReference(namespaces)(v_Term_variable_name)))
    case hydra.core.Term.wrap(v_Term_wrap_wrapped) => {
      val tname: hydra.core.Name = (v_Term_wrap_wrapped.typeName)
      val `term_`: hydra.core.Term = (v_Term_wrap_wrapped.body)
      val lhs: hydra.ext.haskell.ast.Expression = hydra.ext.haskell.ast.Expression.variable(hydra.ext.haskell.utils.elementReference(namespaces)(tname))
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.Expression](encode(`term_`))((rhs: hydra.ext.haskell.ast.Expression) => Right(hydra.ext.haskell.utils.hsapp(lhs)(rhs)))
    }
    case _ => Left(hydra.context.InContext(hydra.error.Error.other(strings.cat2("unexpected term: ")(hydra.show.core.term(term))), cx))
}

def encodeType[T0](namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(typ: hydra.core.Type)(cx: hydra.context.Context)(g: T0): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.haskell.ast.Type] =
  {
  def encode(t: hydra.core.Type): Either[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Type] = hydra.ext.haskell.coder.encodeType(namespaces)(t)(cx)(g)
  def ref[T1](name: hydra.core.Name): Either[T1, hydra.ext.haskell.ast.Type] =
    Right(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.elementReference(namespaces)(name)))
  val unitTuple: hydra.ext.haskell.ast.Type = hydra.ext.haskell.ast.Type.tuple(Seq())
  hydra.rewriting.deannotateType(typ) match
    case hydra.core.Type.application(v_Type_application_app) => {
      val lhs: hydra.core.Type = (v_Type_application_app.function)
      val rhs: hydra.core.Type = (v_Type_application_app.argument)
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(lhs))((hlhs: hydra.ext.haskell.ast.Type) =>
        eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(rhs))((hrhs: hydra.ext.haskell.ast.Type) =>
        Right(hydra.ext.haskell.utils.toTypeApplication(Seq(hlhs, hrhs)))))
    }
    case hydra.core.Type.either(v_Type_either_eitherType) => {
      val `left_`: hydra.core.Type = (v_Type_either_eitherType.left)
      val `right_`: hydra.core.Type = (v_Type_either_eitherType.right)
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(`left_`))((hleft: hydra.ext.haskell.ast.Type) =>
        eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(`right_`))((hright: hydra.ext.haskell.ast.Type) =>
        Right(hydra.ext.haskell.utils.toTypeApplication(Seq(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("Either")), hleft, hright)))))
    }
    case hydra.core.Type.function(v_Type_function_funType) => {
      val dom: hydra.core.Type = (v_Type_function_funType.domain)
      val cod: hydra.core.Type = (v_Type_function_funType.codomain)
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(dom))((hdom: hydra.ext.haskell.ast.Type) =>
        eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(cod))((hcod: hydra.ext.haskell.ast.Type) =>
        Right(hydra.ext.haskell.ast.Type.function(hydra.ext.haskell.ast.FunctionType(hdom, hcod)))))
    }
    case hydra.core.Type.forall(v_Type_forall_forallType) => {
      val v: hydra.core.Name = (v_Type_forall_forallType.parameter)
      val body: hydra.core.Type = (v_Type_forall_forallType.body)
      encode(body)
    }
    case hydra.core.Type.list(v_Type_list_lt) => eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(v_Type_list_lt))((hlt: hydra.ext.haskell.ast.Type) => Right(hydra.ext.haskell.ast.Type.list(hlt)))
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
        case _ => Left(hydra.context.InContext(hydra.error.Error.other(strings.cat2("unexpected integer type: ")(hydra.show.core.integerType(v_LiteralType_integer_it))),
           cx))
      case hydra.core.LiteralType.string => Right(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("String")))
      case _ => Left(hydra.context.InContext(hydra.error.Error.other(strings.cat2("unexpected literal type: ")(hydra.show.core.literalType(v_Type_literal_lt))),
         cx))
    case hydra.core.Type.map(v_Type_map_mapType) => {
      val kt: hydra.core.Type = (v_Type_map_mapType.keys)
      val vt: hydra.core.Type = (v_Type_map_mapType.values)
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(kt))((hkt: hydra.ext.haskell.ast.Type) =>
        eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(vt))((hvt: hydra.ext.haskell.ast.Type) =>
        Right(hydra.ext.haskell.utils.toTypeApplication(Seq(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("M.Map")), hkt, hvt)))))
    }
    case hydra.core.Type.maybe(v_Type_maybe_ot) => eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(v_Type_maybe_ot))((hot: hydra.ext.haskell.ast.Type) =>
      Right(hydra.ext.haskell.utils.toTypeApplication(Seq(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("Maybe")), hot))))
    case hydra.core.Type.pair(v_Type_pair_pt) => eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(v_Type_pair_pt.first))((f: hydra.ext.haskell.ast.Type) =>
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(v_Type_pair_pt.second))((s: hydra.ext.haskell.ast.Type) => Right(hydra.ext.haskell.ast.Type.tuple(Seq(f,
         s)))))
    case hydra.core.Type.record(v_Type_record__) => ref("placeholder")
    case hydra.core.Type.set(v_Type_set_st) => eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](encode(v_Type_set_st))((hst: hydra.ext.haskell.ast.Type) =>
      Right(hydra.ext.haskell.utils.toTypeApplication(Seq(hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName("S.Set")), hst))))
    case hydra.core.Type.union(v_Type_union__) => ref("placeholder")
    case hydra.core.Type.unit => Right(unitTuple)
    case hydra.core.Type.variable(v_Type_variable_v1) => ref(v_Type_variable_v1)
    case hydra.core.Type.wrap(v_Type_wrap__) => ref("placeholder")
    case _ => Left(hydra.context.InContext(hydra.error.Error.other(strings.cat2("unexpected type: ")(hydra.show.core.`type`(typ))), cx))
}

def encodeTypeWithClassAssertions[T0](namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(explicitClasses: Map[hydra.core.Name,
   scala.collection.immutable.Set[hydra.classes.TypeClass]])(typ: hydra.core.Type)(cx: hydra.context.Context)(g: T0): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.haskell.ast.Type] =
  {
  val classes: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]] = maps.union[hydra.core.Name,
     scala.collection.immutable.Set[hydra.classes.TypeClass]](explicitClasses)(hydra.ext.haskell.coder.getImplicitTypeClasses(typ))
  val implicitClasses: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]] = hydra.ext.haskell.coder.getImplicitTypeClasses(typ)
  def encodeAssertion(pair: Tuple2[hydra.core.Name, hydra.classes.TypeClass]): hydra.ext.haskell.ast.Assertion =
    {
    val name: hydra.core.Name = pairs.first[hydra.core.Name, hydra.classes.TypeClass](pair)
    val cls: hydra.classes.TypeClass = pairs.second[hydra.core.Name, hydra.classes.TypeClass](pair)
    val hname: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.rawName(cls match
      case hydra.classes.TypeClass.equality => "Eq"
      case hydra.classes.TypeClass.ordering => "Ord")
    val htype: hydra.ext.haskell.ast.Type = hydra.ext.haskell.ast.Type.variable(hydra.ext.haskell.utils.rawName(name))
    hydra.ext.haskell.ast.Assertion.`class`(hydra.ext.haskell.ast.ClassAssertion(hname, Seq(htype)))
  }
  val assertPairs: Seq[Tuple2[hydra.core.Name, hydra.classes.TypeClass]] = lists.concat[Tuple2[hydra.core.Name,
     hydra.classes.TypeClass]](lists.map[Tuple2[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]],
     Seq[Tuple2[hydra.core.Name, hydra.classes.TypeClass]]](toPairs)(maps.toList[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]](classes)))
  def toPairs[T1, T2](mapEntry: Tuple2[T1, scala.collection.immutable.Set[T2]]): Seq[Tuple2[T1, T2]] =
    {
    val name: T1 = pairs.first[T1, scala.collection.immutable.Set[T2]](mapEntry)
    val clsSet: scala.collection.immutable.Set[T2] = pairs.second[T1, scala.collection.immutable.Set[T2]](mapEntry)
    def toPair[T3](c: T3): Tuple2[T1, T3] = Tuple2(name, c)
    lists.map[T2, Tuple2[T1, T2]](toPair)(sets.toList[T2](clsSet))
  }
  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Type](hydra.ext.haskell.coder.adaptTypeToHaskellAndEncode(namespaces)(typ)(cx)(g))((htyp: hydra.ext.haskell.ast.Type) =>
    logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Type]](lists.`null`[Tuple2[hydra.core.Name,
       hydra.classes.TypeClass]](assertPairs))(Right(htyp))({
    val encoded: Seq[hydra.ext.haskell.ast.Assertion] = lists.map[Tuple2[hydra.core.Name, hydra.classes.TypeClass],
       hydra.ext.haskell.ast.Assertion](encodeAssertion)(assertPairs)
    val hassert: hydra.ext.haskell.ast.Assertion = logic.ifElse[hydra.ext.haskell.ast.Assertion](equality.equal[Int](lists.length[hydra.ext.haskell.ast.Assertion](encoded))(1))(lists.head[hydra.ext.haskell.ast.Assertion](encoded))(hydra.ext.haskell.ast.Assertion.tuple(encoded))
    Right(hydra.ext.haskell.ast.Type.ctx(hydra.ext.haskell.ast.ContextType(hassert, htyp)))
  }))
}

def findOrdVariables(typ: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
  {
  def fold(names: scala.collection.immutable.Set[hydra.core.Name])(`typ_`: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
    `typ_` match
    case hydra.core.Type.map(v_Type_map_mapType) => {
      val kt: hydra.core.Type = (v_Type_map_mapType.keys)
      tryType(names)(kt)
    }
    case hydra.core.Type.set(v_Type_set_et) => tryType(names)(v_Type_set_et)
    case _ => names
  def isTypeVariable(v: hydra.core.Name): Boolean = maybes.isNothing[hydra.module.Namespace](hydra.names.namespaceOf(v))
  def tryType(names: scala.collection.immutable.Set[hydra.core.Name])(t: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
    hydra.rewriting.deannotateType(t) match
    case hydra.core.Type.variable(v_Type_variable_v) => logic.ifElse[scala.collection.immutable.Set[hydra.core.Name]](isTypeVariable(v_Type_variable_v))(sets.insert[hydra.core.Name](v_Type_variable_v)(names))(names)
    case _ => names
  hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)(fold)(sets.empty[hydra.core.Name])(typ)
}

def getImplicitTypeClasses(typ: hydra.core.Type): Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]] =
  {
  def toPair[T0](name: T0): Tuple2[T0, scala.collection.immutable.Set[hydra.classes.TypeClass]] =
    Tuple2(name, sets.fromList[hydra.classes.TypeClass](Seq(hydra.classes.TypeClass.ordering)))
  maps.fromList[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]](lists.map[hydra.core.Name,
     Tuple2[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]]](toPair)(sets.toList[hydra.core.Name](hydra.ext.haskell.coder.findOrdVariables(typ))))
}

def moduleToHaskellModule(mod: hydra.module.Module)(defs: Seq[hydra.module.Definition])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.haskell.ast.Module] =
  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName],
     hydra.ext.haskell.ast.Module](hydra.ext.haskell.utils.namespacesForModule(mod)(cx)(g))((namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName]) =>
  hydra.ext.haskell.coder.constructModule(namespaces)(mod)(defs)(cx)(g))

def moduleToHaskell(mod: hydra.module.Module)(defs: Seq[hydra.module.Definition])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   Map[scala.Predef.String, scala.Predef.String]] =
  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Module, Map[scala.Predef.String,
     scala.Predef.String]](hydra.ext.haskell.coder.moduleToHaskellModule(mod)(defs)(cx)(g))((hsmod: hydra.ext.haskell.ast.Module) =>
  {
  val s: scala.Predef.String = hydra.serialization.printExpr(hydra.serialization.parenthesize(hydra.ext.haskell.serde.moduleToExpr(hsmod)))
  val filepath: scala.Predef.String = hydra.names.namespaceToFilePath(hydra.util.CaseConvention.pascal)("hs")(mod.namespace)
  Right(maps.singleton[scala.Predef.String, scala.Predef.String](filepath)(s))
})

def nameDecls(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(name: hydra.core.Name)(typ: hydra.core.Type): Seq[hydra.ext.haskell.ast.DeclarationWithComments] =
  {
  val nm: scala.Predef.String = name
  def toDecl(n: hydra.core.Name)(pair: Tuple2[scala.Predef.String, scala.Predef.String]): hydra.ext.haskell.ast.DeclarationWithComments =
    {
    val k: scala.Predef.String = pairs.first[scala.Predef.String, scala.Predef.String](pair)
    val v: scala.Predef.String = pairs.second[scala.Predef.String, scala.Predef.String](pair)
    val decl: hydra.ext.haskell.ast.Declaration = hydra.ext.haskell.ast.Declaration.valueBinding(hydra.ext.haskell.ast.ValueBinding.simple(hydra.ext.haskell.ast.SimpleValueBinding(hydra.ext.haskell.utils.applicationPattern(hydra.ext.haskell.utils.simpleName(k))(Seq()),
       hydra.ext.haskell.ast.Expression.application(hydra.ext.haskell.ast.ApplicationExpression(hydra.ext.haskell.ast.Expression.variable(hydra.ext.haskell.utils.elementReference(namespaces)(n)),
       hydra.ext.haskell.ast.Expression.literal(hydra.ext.haskell.ast.Literal.string(v)))), None)))
    hydra.ext.haskell.ast.DeclarationWithComments(decl, None)
  }
  val nameDecl: Tuple2[scala.Predef.String, scala.Predef.String] = Tuple2(hydra.ext.haskell.coder.constantForTypeName(name), nm)
  val fieldDecls: Seq[Tuple2[scala.Predef.String, scala.Predef.String]] = lists.map[hydra.core.FieldType,
     Tuple2[scala.Predef.String, scala.Predef.String]](toConstant)(hydra.lexical.fieldsOf(typ))
  def toConstant(fieldType: hydra.core.FieldType): Tuple2[scala.Predef.String, scala.Predef.String] =
    {
    val fname: hydra.core.Name = (fieldType.name)
    Tuple2(hydra.ext.haskell.coder.constantForFieldName(name)(fname), fname)
  }
  logic.ifElse[Seq[hydra.ext.haskell.ast.DeclarationWithComments]](hydra.ext.haskell.coder.useCoreImport)(lists.cons[hydra.ext.haskell.ast.DeclarationWithComments](toDecl("hydra.core.Name")(nameDecl))(lists.map[Tuple2[scala.Predef.String,
     scala.Predef.String], hydra.ext.haskell.ast.DeclarationWithComments]((v1: Tuple2[scala.Predef.String,
     scala.Predef.String]) => toDecl("hydra.core.Name")(v1))(fieldDecls)))(Seq())
}

def toDataDeclaration(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(`def`: hydra.module.TermDefinition)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.haskell.ast.DeclarationWithComments] =
  {
  val name: hydra.core.Name = (`def`.name)
  val term: hydra.core.Term = (`def`.term)
  val typ: hydra.core.TypeScheme = (`def`.`type`)
  val hname: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.simpleName(hydra.names.localNameOf(name))
  def rewriteValueBinding(vb: hydra.ext.haskell.ast.ValueBinding): hydra.ext.haskell.ast.ValueBinding =
    vb match
    case hydra.ext.haskell.ast.ValueBinding.simple(v_ValueBinding_simple_simple) => {
      val `pattern_`: hydra.ext.haskell.ast.Pattern = (v_ValueBinding_simple_simple.pattern)
      val rhs: hydra.ext.haskell.ast.RightHandSide = (v_ValueBinding_simple_simple.rhs)
      val bindings: Option[hydra.ext.haskell.ast.LocalBindings] = (v_ValueBinding_simple_simple.localBindings)
      `pattern_` match
        case hydra.ext.haskell.ast.Pattern.application(v_Pattern_application_appPat) => {
          val `name_`: hydra.ext.haskell.ast.Name = (v_Pattern_application_appPat.name)
          val args: Seq[hydra.ext.haskell.ast.Pattern] = (v_Pattern_application_appPat.args)
          val rhsExpr: hydra.ext.haskell.ast.Expression = rhs
          rhsExpr match
            case hydra.ext.haskell.ast.Expression.lambda(v_Expression_lambda_lambda_) => {
              val vars: Seq[hydra.ext.haskell.ast.Pattern] = (`v_Expression_lambda_lambda_`.bindings)
              val body: hydra.ext.haskell.ast.Expression = (`v_Expression_lambda_lambda_`.inner)
              val newPattern: hydra.ext.haskell.ast.Pattern = hydra.ext.haskell.utils.applicationPattern(`name_`)(lists.concat2[hydra.ext.haskell.ast.Pattern](args)(vars))
              val newRhs: hydra.ext.haskell.ast.RightHandSide = body
              rewriteValueBinding(hydra.ext.haskell.ast.ValueBinding.simple(hydra.ext.haskell.ast.SimpleValueBinding(newPattern, newRhs, bindings)))
            }
            case _ => vb
        }
        case _ => vb
    }
  def toDecl(comments: Option[scala.Predef.String])(`hname_`: hydra.ext.haskell.ast.Name)(`term_`: hydra.core.Term)(bindings: Option[hydra.ext.haskell.ast.LocalBindings]): Either[hydra.context.InContext[hydra.error.Error],
     hydra.ext.haskell.ast.DeclarationWithComments] =
    hydra.rewriting.deannotateTerm(`term_`) match
    case hydra.core.Term.let(v_Term_let_letTerm) => {
      val lbindings: Seq[hydra.core.Binding] = (v_Term_let_letTerm.bindings)
      val env: hydra.core.Term = (v_Term_let_letTerm.body)
      def toBinding(`hname__`: hydra.ext.haskell.ast.Name)(`hterm_`: hydra.ext.haskell.ast.Expression): hydra.ext.haskell.ast.LocalBinding =
        hydra.ext.haskell.ast.LocalBinding.value(hydra.ext.haskell.utils.simpleValueBinding(`hname__`)(`hterm_`)(None))
      val hnames: Seq[hydra.ext.haskell.ast.Name] = lists.map[hydra.core.Binding, hydra.ext.haskell.ast.Name]((binding: hydra.core.Binding) => hydra.ext.haskell.utils.simpleName(binding.name))(lbindings)
      val terms: Seq[hydra.core.Term] = lists.map[hydra.core.Binding, hydra.core.Term]((x: hydra.core.Binding) => (x.term))(lbindings)
      eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.haskell.ast.Expression],
         hydra.ext.haskell.ast.DeclarationWithComments](eithers.mapList[hydra.core.Term, hydra.ext.haskell.ast.Expression,
         hydra.context.InContext[hydra.error.Error]]((t: hydra.core.Term) => hydra.ext.haskell.coder.encodeTerm(0)(namespaces)(t)(cx)(g))(terms))((hterms: Seq[hydra.ext.haskell.ast.Expression]) =>
        {
        val hbindings: Seq[hydra.ext.haskell.ast.LocalBinding] = lists.zipWith[hydra.ext.haskell.ast.Name,
           hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.LocalBinding](toBinding)(hnames)(hterms)
        val prevBindings: Seq[hydra.ext.haskell.ast.LocalBinding] = maybes.maybe[Seq[hydra.ext.haskell.ast.LocalBinding],
           hydra.ext.haskell.ast.LocalBindings](Seq())((lb: hydra.ext.haskell.ast.LocalBindings) => lb)(bindings)
        val allBindings: Seq[hydra.ext.haskell.ast.LocalBinding] = lists.concat2[hydra.ext.haskell.ast.LocalBinding](prevBindings)(hbindings)
        toDecl(comments)(`hname_`)(env)(Some(allBindings))
      })
    }
    case _ => eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression,
       hydra.ext.haskell.ast.DeclarationWithComments](hydra.ext.haskell.coder.encodeTerm(0)(namespaces)(`term_`)(cx)(g))((hterm: hydra.ext.haskell.ast.Expression) =>
      {
      val vb: hydra.ext.haskell.ast.ValueBinding = hydra.ext.haskell.utils.simpleValueBinding(`hname_`)(hterm)(bindings)
      val schemeConstraints: Option[Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = (typ.constraints)
      val schemeClasses: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]] = hydra.ext.haskell.coder.typeSchemeConstraintsToClassMap(schemeConstraints)
      eithers.bind[hydra.context.InContext[hydra.error.Error], Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]],
         hydra.ext.haskell.ast.DeclarationWithComments](hydra.annotations.getTypeClasses(cx)(g)(hydra.rewriting.removeTypesFromTerm(term)))((explicitClasses: Map[hydra.core.Name,
         scala.collection.immutable.Set[hydra.classes.TypeClass]]) =>
        {
        val combinedClasses: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]] = maps.union[hydra.core.Name,
           scala.collection.immutable.Set[hydra.classes.TypeClass]](schemeClasses)(explicitClasses)
        eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.DeclarationWithComments](hydra.ext.haskell.coder.encodeTypeWithClassAssertions(namespaces)(combinedClasses)(typ.`type`)(cx)(g))((htype: hydra.ext.haskell.ast.Type) =>
          {
          val decl: hydra.ext.haskell.ast.Declaration = hydra.ext.haskell.ast.Declaration.typedBinding(hydra.ext.haskell.ast.TypedBinding(hydra.ext.haskell.ast.TypeSignature(`hname_`,
             htype), rewriteValueBinding(vb)))
          Right(hydra.ext.haskell.ast.DeclarationWithComments(decl, comments))
        })
      })
    })
  eithers.bind[hydra.context.InContext[hydra.error.Error], Option[scala.Predef.String], hydra.ext.haskell.ast.DeclarationWithComments](hydra.annotations.getTermDescription(cx)(g)(term))((comments: Option[scala.Predef.String]) => toDecl(comments)(hname)(term)(None))
}

def toTypeDeclarationsFrom(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(elementName: hydra.core.Name)(typ: hydra.core.Type)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   Seq[hydra.ext.haskell.ast.DeclarationWithComments]] =
  {
  val lname: scala.Predef.String = hydra.names.localNameOf(elementName)
  val hname: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.simpleName(lname)
  def declHead(name: hydra.ext.haskell.ast.Name)(`vars_`: Seq[hydra.core.Name]): hydra.ext.haskell.ast.DeclarationHead =
    logic.ifElse[hydra.ext.haskell.ast.DeclarationHead](lists.`null`[hydra.core.Name](`vars_`))(hydra.ext.haskell.ast.DeclarationHead.simple(name))({
    val h: hydra.core.Name = lists.head[hydra.core.Name](`vars_`)
    val rest: Seq[hydra.core.Name] = lists.tail[hydra.core.Name](`vars_`)
    val hvar: hydra.ext.haskell.ast.Variable = hydra.ext.haskell.utils.simpleName(h)
    hydra.ext.haskell.ast.DeclarationHead.application(hydra.ext.haskell.ast.ApplicationDeclarationHead(declHead(name)(rest), hvar))
  })
  def newtypeCons(tname: hydra.core.Name)(`typ_`: hydra.core.Type): Either[hydra.context.InContext[hydra.error.Error],
     hydra.ext.haskell.ast.ConstructorWithComments] =
    {
    val hname0: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.simpleName(hydra.ext.haskell.utils.newtypeAccessorName(tname))
    eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.ConstructorWithComments](hydra.ext.haskell.coder.adaptTypeToHaskellAndEncode(namespaces)(`typ_`)(cx)(g))((htype: hydra.ext.haskell.ast.Type) =>
      {
      val hfield: hydra.ext.haskell.ast.FieldWithComments = hydra.ext.haskell.ast.FieldWithComments(hydra.ext.haskell.ast.Field(hname0, htype), None)
      val constructorName: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.simpleName(hydra.names.localNameOf(tname))
      Right(hydra.ext.haskell.ast.ConstructorWithComments(hydra.ext.haskell.ast.Constructor.record(hydra.ext.haskell.ast.RecordConstructor(constructorName,
         Seq(hfield))), None))
    })
  }
  def recordCons(`lname_`: scala.Predef.String)(fields: Seq[hydra.core.FieldType]): Either[hydra.context.InContext[hydra.error.Error],
     hydra.ext.haskell.ast.ConstructorWithComments] =
    {
    def toField(fieldType: hydra.core.FieldType): Either[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.FieldWithComments] =
      {
      val fname: hydra.core.Name = (fieldType.name)
      val ftype: hydra.core.Type = (fieldType.`type`)
      val `hname_`: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.simpleName(strings.cat2(hydra.formatting.decapitalize(`lname_`))(hydra.formatting.capitalize(fname)))
      eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.FieldWithComments](hydra.ext.haskell.coder.adaptTypeToHaskellAndEncode(namespaces)(ftype)(cx)(g))((htype: hydra.ext.haskell.ast.Type) =>
        eithers.bind[hydra.context.InContext[hydra.error.Error], Option[scala.Predef.String], hydra.ext.haskell.ast.FieldWithComments](hydra.annotations.getTypeDescription(cx)(g)(ftype))((comments: Option[scala.Predef.String]) =>
        Right(hydra.ext.haskell.ast.FieldWithComments(hydra.ext.haskell.ast.Field(`hname_`, htype), comments))))
    }
    eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.haskell.ast.FieldWithComments],
       hydra.ext.haskell.ast.ConstructorWithComments](eithers.mapList[hydra.core.FieldType, hydra.ext.haskell.ast.FieldWithComments,
       hydra.context.InContext[hydra.error.Error]](toField)(fields))((hFields: Seq[hydra.ext.haskell.ast.FieldWithComments]) =>
      Right(hydra.ext.haskell.ast.ConstructorWithComments(hydra.ext.haskell.ast.Constructor.record(hydra.ext.haskell.ast.RecordConstructor(hydra.ext.haskell.utils.simpleName(`lname_`),
         hFields)), None)))
  }
  def unionCons(`boundNames_`: scala.collection.immutable.Set[hydra.core.Name])(`lname_`: scala.Predef.String)(fieldType: hydra.core.FieldType): Either[hydra.context.InContext[hydra.error.Error],
     hydra.ext.haskell.ast.ConstructorWithComments] =
    {
    val fname: hydra.core.Name = (fieldType.name)
    val ftype: hydra.core.Type = (fieldType.`type`)
    def deconflict(name: scala.Predef.String): scala.Predef.String =
      {
      val tname: hydra.core.Name = hydra.names.unqualifyName(hydra.module.QualifiedName(Some(pairs.first[hydra.module.Namespace,
         hydra.ext.haskell.ast.ModuleName](namespaces.focus)), name))
      logic.ifElse[scala.Predef.String](sets.member[hydra.core.Name](tname)(`boundNames_`))(deconflict(strings.cat2(name)("_")))(name)
    }
    eithers.bind[hydra.context.InContext[hydra.error.Error], Option[scala.Predef.String], hydra.ext.haskell.ast.ConstructorWithComments](hydra.annotations.getTypeDescription(cx)(g)(ftype))((comments: Option[scala.Predef.String]) =>
      {
      val nm: scala.Predef.String = deconflict(strings.cat2(hydra.formatting.capitalize(`lname_`))(hydra.formatting.capitalize(fname)))
      eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.haskell.ast.Type], hydra.ext.haskell.ast.ConstructorWithComments](logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
         Seq[hydra.ext.haskell.ast.Type]]](equality.equal[hydra.core.Type](hydra.rewriting.deannotateType(ftype))(hydra.core.Type.unit))(Right(Seq()))(eithers.bind[hydra.context.InContext[hydra.error.Error],
         hydra.ext.haskell.ast.Type, Seq[hydra.ext.haskell.ast.Type]](hydra.ext.haskell.coder.adaptTypeToHaskellAndEncode(namespaces)(ftype)(cx)(g))((htype: hydra.ext.haskell.ast.Type) => Right(Seq(htype)))))((typeList: Seq[hydra.ext.haskell.ast.Type]) =>
        Right(hydra.ext.haskell.ast.ConstructorWithComments(hydra.ext.haskell.ast.Constructor.ordinary(hydra.ext.haskell.ast.OrdinaryConstructor(hydra.ext.haskell.utils.simpleName(nm),
           typeList)), comments)))
    })
  }
  eithers.bind[hydra.context.InContext[hydra.error.Error], Boolean, Seq[hydra.ext.haskell.ast.DeclarationWithComments]](hydra.schemas.isSerializableByName(cx)(g)(elementName))((isSer: Boolean) =>
    {
    val deriv: hydra.ext.haskell.ast.Deriving = logic.ifElse[Seq[hydra.ext.haskell.ast.Name]](isSer)(lists.map[scala.Predef.String,
       hydra.ext.haskell.ast.Name](hydra.ext.haskell.utils.rawName)(Seq("Eq", "Ord", "Read", "Show")))(Seq())
    val unpackResult: Tuple2[Seq[hydra.core.Name], hydra.core.Type] = hydra.ext.haskell.utils.unpackForallType(typ)
    val vars: Seq[hydra.core.Name] = pairs.first[Seq[hydra.core.Name], hydra.core.Type](unpackResult)
    val `t_`: hydra.core.Type = pairs.second[Seq[hydra.core.Name], hydra.core.Type](unpackResult)
    val hd: hydra.ext.haskell.ast.DeclarationHead = declHead(hname)(lists.reverse[hydra.core.Name](vars))
    eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Declaration, Seq[hydra.ext.haskell.ast.DeclarationWithComments]](hydra.rewriting.deannotateType(`t_`) match
      case hydra.core.Type.record(v_Type_record_rt) => eithers.bind[hydra.context.InContext[hydra.error.Error],
         hydra.ext.haskell.ast.ConstructorWithComments, hydra.ext.haskell.ast.Declaration](recordCons(lname)(v_Type_record_rt))((cons: hydra.ext.haskell.ast.ConstructorWithComments) =>
        Right(hydra.ext.haskell.ast.Declaration.data(hydra.ext.haskell.ast.DataDeclaration(hydra.ext.haskell.ast.DataOrNewtype.data,
           Seq(), hd, Seq(cons), Seq(deriv)))))
      case hydra.core.Type.union(v_Type_union_rt) => eithers.bind[hydra.context.InContext[hydra.error.Error],
         Seq[hydra.ext.haskell.ast.ConstructorWithComments], hydra.ext.haskell.ast.Declaration](eithers.mapList[hydra.core.FieldType,
         hydra.ext.haskell.ast.ConstructorWithComments, hydra.context.InContext[hydra.error.Error]]((v1: hydra.core.FieldType) =>
        unionCons(sets.fromList[hydra.core.Name](maps.keys[hydra.core.Name, hydra.core.Term](g.boundTerms)))(lname)(v1))(v_Type_union_rt))((cons: Seq[hydra.ext.haskell.ast.ConstructorWithComments]) =>
        Right(hydra.ext.haskell.ast.Declaration.data(hydra.ext.haskell.ast.DataDeclaration(hydra.ext.haskell.ast.DataOrNewtype.data,
           Seq(), hd, cons, Seq(deriv)))))
      case hydra.core.Type.wrap(v_Type_wrap_wrapped) => eithers.bind[hydra.context.InContext[hydra.error.Error],
         hydra.ext.haskell.ast.ConstructorWithComments, hydra.ext.haskell.ast.Declaration](newtypeCons(elementName)(v_Type_wrap_wrapped))((cons: hydra.ext.haskell.ast.ConstructorWithComments) =>
        Right(hydra.ext.haskell.ast.Declaration.data(hydra.ext.haskell.ast.DataDeclaration(hydra.ext.haskell.ast.DataOrNewtype.newtype,
           Seq(), hd, Seq(cons), Seq(deriv)))))
      case _ => eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.Declaration](hydra.ext.haskell.coder.adaptTypeToHaskellAndEncode(namespaces)(typ)(cx)(g))((htype: hydra.ext.haskell.ast.Type) =>
        Right(hydra.ext.haskell.ast.Declaration.`type`(hydra.ext.haskell.ast.TypeDeclaration(hd, htype)))))((decl: hydra.ext.haskell.ast.Declaration) =>
      eithers.bind[hydra.context.InContext[hydra.error.Error], Option[scala.Predef.String], Seq[hydra.ext.haskell.ast.DeclarationWithComments]](hydra.annotations.getTypeDescription(cx)(g)(typ))((comments: Option[scala.Predef.String]) =>
      eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.ext.haskell.ast.DeclarationWithComments],
         Seq[hydra.ext.haskell.ast.DeclarationWithComments]](logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
         Seq[hydra.ext.haskell.ast.DeclarationWithComments]]](hydra.ext.haskell.coder.includeTypeDefinitions)(eithers.bind[hydra.context.InContext[hydra.error.Error],
         hydra.ext.haskell.ast.DeclarationWithComments, Seq[hydra.ext.haskell.ast.DeclarationWithComments]](hydra.ext.haskell.coder.typeDecl(namespaces)(elementName)(typ)(cx)(g))((`decl_`: hydra.ext.haskell.ast.DeclarationWithComments) => Right(Seq(`decl_`))))(Right(Seq())))((tdecls: Seq[hydra.ext.haskell.ast.DeclarationWithComments]) =>
      {
      val mainDecl: hydra.ext.haskell.ast.DeclarationWithComments = hydra.ext.haskell.ast.DeclarationWithComments(decl, comments)
      val `nameDecls_`: Seq[hydra.ext.haskell.ast.DeclarationWithComments] = hydra.ext.haskell.coder.nameDecls(namespaces)(elementName)(typ)
      Right(lists.concat[hydra.ext.haskell.ast.DeclarationWithComments](Seq(Seq(mainDecl), `nameDecls_`, tdecls)))
    })))
  })
}

def typeDecl(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName])(name: hydra.core.Name)(typ: hydra.core.Type)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.error.Error],
   hydra.ext.haskell.ast.DeclarationWithComments] =
  {
  def typeName(ns: hydra.module.Namespace)(`name_`: hydra.core.Name): hydra.core.Name = hydra.names.qname(ns)(typeNameLocal(`name_`))
  def typeNameLocal(`name_`: hydra.core.Name): scala.Predef.String = strings.cat(Seq("_", hydra.names.localNameOf(`name_`), "_type_"))
  val rawTerm: hydra.core.Term = hydra.encode.core.`type`(typ)
  def rewrite(recurse: (hydra.core.Term => hydra.core.Term))(term: hydra.core.Term): hydra.core.Term =
    {
    val variantResult: Option[hydra.core.Field] = hydra.rewriting.deannotateTerm(term) match
      case hydra.core.Term.union(v_Term_union_inj) => logic.ifElse[Option[hydra.core.Field]](equality.equal[hydra.core.Name](v_Term_union_inj.typeName)("hydra.core.Type"))(Some(v_Term_union_inj.field))(None)
      case _ => None
    def decodeString(term2: hydra.core.Term): Option[scala.Predef.String] =
      hydra.rewriting.deannotateTerm(term2) match
      case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
        case hydra.core.Literal.string(v_Literal_string_s) => Some(v_Literal_string_s)
        case _ => None
      case _ => None
    def decodeName(term2: hydra.core.Term): Option[hydra.core.Name] =
      hydra.rewriting.deannotateTerm(term2) match
      case hydra.core.Term.wrap(v_Term_wrap_wt) => logic.ifElse[Option[hydra.core.Name]](equality.equal[hydra.core.Name](v_Term_wrap_wt.typeName)("hydra.core.Name"))(maybes.map[scala.Predef.String,
         hydra.core.Name]((x: scala.Predef.String) => x)(decodeString(v_Term_wrap_wt.body)))(None)
      case _ => None
    def forType(field: hydra.core.Field): Option[hydra.core.Term] =
      {
      val fname: hydra.core.Name = (field.name)
      val fterm: hydra.core.Term = (field.term)
      logic.ifElse[Option[hydra.core.Term]](equality.equal[hydra.core.Name](fname)("record"))(None)(logic.ifElse[Option[hydra.core.Term]](equality.equal[hydra.core.Name](fname)("variable"))(maybes.bind[hydra.core.Name,
         hydra.core.Term](decodeName(fterm))(forVariableType))(None))
    }
    def forVariableType(vname: hydra.core.Name): Option[hydra.core.Term] =
      {
      val qname: hydra.module.QualifiedName = hydra.names.qualifyName(vname)
      val mns: Option[hydra.module.Namespace] = (qname.namespace)
      val local: scala.Predef.String = (qname.local)
      maybes.map[hydra.module.Namespace, hydra.core.Term]((ns: hydra.module.Namespace) =>
        hydra.core.Term.variable(hydra.names.qname(ns)(strings.cat(Seq("_", local, "_type_")))))(mns)
    }
    maybes.fromMaybe[hydra.core.Term](recurse(term))(maybes.bind[hydra.core.Field, hydra.core.Term](variantResult)(forType))
  }
  val finalTerm: hydra.core.Term = hydra.rewriting.rewriteTerm(rewrite)(rawTerm)
  eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.DeclarationWithComments](hydra.ext.haskell.coder.encodeTerm(0)(namespaces)(finalTerm)(cx)(g))((expr: hydra.ext.haskell.ast.Expression) =>
    {
    val rhs: hydra.ext.haskell.ast.RightHandSide = expr
    val hname: hydra.ext.haskell.ast.Name = hydra.ext.haskell.utils.simpleName(typeNameLocal(name))
    val pat: hydra.ext.haskell.ast.Pattern = hydra.ext.haskell.utils.applicationPattern(hname)(Seq())
    val decl: hydra.ext.haskell.ast.Declaration = hydra.ext.haskell.ast.Declaration.valueBinding(hydra.ext.haskell.ast.ValueBinding.simple(hydra.ext.haskell.ast.SimpleValueBinding(pat,
       rhs, None)))
    Right(hydra.ext.haskell.ast.DeclarationWithComments(decl, None))
  })
}

def typeSchemeConstraintsToClassMap[T0](maybeConstraints: Option[Map[T0, hydra.core.TypeVariableMetadata]]): Map[T0,
   scala.collection.immutable.Set[hydra.classes.TypeClass]] =
  {
  def nameToTypeClass(className: hydra.core.Name): Option[hydra.classes.TypeClass] =
    {
    val classNameStr: scala.Predef.String = className
    val isEq: Boolean = equality.equal[scala.Predef.String](classNameStr)("equality")
    val isOrd: Boolean = equality.equal[scala.Predef.String](classNameStr)("ordering")
    logic.ifElse[Option[hydra.classes.TypeClass]](isEq)(Some(hydra.classes.TypeClass.equality))(logic.ifElse[Option[hydra.classes.TypeClass]](isOrd)(Some(hydra.classes.TypeClass.ordering))(None))
  }
  maybes.maybe[Map[T0, scala.collection.immutable.Set[hydra.classes.TypeClass]], Map[T0, hydra.core.TypeVariableMetadata]](maps.empty[T0,
     scala.collection.immutable.Set[hydra.classes.TypeClass]])((constraints: Map[T0, hydra.core.TypeVariableMetadata]) =>
    maps.map[hydra.core.TypeVariableMetadata, scala.collection.immutable.Set[hydra.classes.TypeClass], T0]((meta: hydra.core.TypeVariableMetadata) =>
    sets.fromList[hydra.classes.TypeClass](maybes.cat[hydra.classes.TypeClass](lists.map[hydra.core.Name,
       Option[hydra.classes.TypeClass]](nameToTypeClass)(sets.toList[hydra.core.Name](meta.classes)))))(constraints))(maybeConstraints)
}
