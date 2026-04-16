package hydra.haskell.coder

import hydra.classes.*

import hydra.coders.*

import hydra.core.*

import hydra.errors.*

import hydra.graph.*

import hydra.haskell.environment.*

import hydra.haskell.syntax.*

import hydra.packaging.*

import hydra.util.*

def adaptTypeToHaskellAndEncode[T0, T1](namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(typ: hydra.core.Type)(cx: T0)(g: T1): Either[hydra.errors.Error,
   hydra.haskell.syntax.Type] =
  {
  def enc(t: hydra.core.Type): Either[hydra.errors.Error, hydra.haskell.syntax.Type] = hydra.haskell.coder.encodeType(namespaces)(t)(cx)(g)
  hydra.strip.deannotateType(typ) match
    case hydra.core.Type.variable(v_Type_variable__) => enc(typ)
    case _ => hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, hydra.haskell.syntax.Type](hydra.adapt.adaptTypeForLanguage(hydra.haskell.language.haskellLanguage)(typ))((adaptedType: hydra.core.Type) => enc(adaptedType))
}

def constantForFieldName(tname: hydra.core.Name)(fname: hydra.core.Name): scala.Predef.String = hydra.lib.strings.cat(Seq("_",
   hydra.names.localNameOf(tname), "_", fname))

def constantForTypeName(tname: hydra.core.Name): scala.Predef.String = hydra.lib.strings.cat2("_")(hydra.names.localNameOf(tname))

def constructModule(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(mod: hydra.packaging.Module)(defs: Seq[hydra.packaging.Definition])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.haskell.syntax.Module] =
  {
  def h(namespace: hydra.packaging.Namespace): scala.Predef.String = namespace
  def createDeclarations(`def`: hydra.packaging.Definition): Either[hydra.errors.Error,
     Seq[hydra.haskell.syntax.DeclarationWithComments]] =
    `def` match
    case hydra.packaging.Definition.`type`(v_Definition_type_type) => {
      lazy val name: hydra.core.Name = (v_Definition_type_type.name)
      lazy val typ: hydra.core.Type = (v_Definition_type_type.`type`.`type`)
      hydra.haskell.coder.toTypeDeclarationsFrom(namespaces)(name)(typ)(cx)(g)
    }
    case hydra.packaging.Definition.term(v_Definition_term_term) => hydra.lib.eithers.bind[hydra.errors.Error,
       hydra.haskell.syntax.DeclarationWithComments, Seq[hydra.haskell.syntax.DeclarationWithComments]](hydra.haskell.coder.toDataDeclaration(namespaces)(v_Definition_term_term)(cx)(g))((d: hydra.haskell.syntax.DeclarationWithComments) => Right(Seq(d)))
  def importName(name: scala.Predef.String): hydra.haskell.syntax.ModuleName =
    hydra.lib.strings.intercalate(".")(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String](hydra.formatting.capitalize)(hydra.lib.strings.splitOn(".")(name)))
  lazy val imports: Seq[hydra.haskell.syntax.Import] = hydra.lib.lists.concat2[hydra.haskell.syntax.Import](domainImports)(standardImports)
  lazy val domainImports: Seq[hydra.haskell.syntax.Import] = {
    def toImport(pair: Tuple2[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName]): hydra.haskell.syntax.Import =
      {
      lazy val namespace: hydra.packaging.Namespace = hydra.lib.pairs.first[hydra.packaging.Namespace,
         hydra.haskell.syntax.ModuleName](pair)
      lazy val alias: hydra.haskell.syntax.ModuleName = hydra.lib.pairs.second[hydra.packaging.Namespace,
         hydra.haskell.syntax.ModuleName](pair)
      lazy val name: scala.Predef.String = h(namespace)
      hydra.haskell.syntax.Import(true, importName(name), Some(alias), None)
    }
    hydra.lib.lists.map[Tuple2[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName],
       hydra.haskell.syntax.Import](toImport)(hydra.lib.maps.toList[hydra.packaging.Namespace,
       hydra.haskell.syntax.ModuleName](namespaces.mapping))
  }
  lazy val meta: hydra.haskell.environment.HaskellModuleMetadata = hydra.haskell.coder.gatherMetadata(defs)
  def condImport[T0](flag: Boolean)(triple: T0): Seq[T0] = hydra.lib.logic.ifElse[Seq[T0]](flag)(Seq(triple))(Seq())
  lazy val standardImports: Seq[hydra.haskell.syntax.Import] = {
    def toImport(triple: Tuple2[Tuple2[scala.Predef.String, Option[scala.Predef.String]],
       Seq[scala.Predef.String]]): hydra.haskell.syntax.Import =
      {
      lazy val name: scala.Predef.String = hydra.lib.pairs.first[scala.Predef.String,
         Option[scala.Predef.String]](hydra.lib.pairs.first[Tuple2[scala.Predef.String,
         Option[scala.Predef.String]], Seq[scala.Predef.String]](triple))
      lazy val malias: Option[scala.Predef.String] = hydra.lib.pairs.second[scala.Predef.String,
         Option[scala.Predef.String]](hydra.lib.pairs.first[Tuple2[scala.Predef.String,
         Option[scala.Predef.String]], Seq[scala.Predef.String]](triple))
      lazy val hidden: Seq[scala.Predef.String] = hydra.lib.pairs.second[Tuple2[scala.Predef.String,
         Option[scala.Predef.String]], Seq[scala.Predef.String]](triple)
      lazy val spec: Option[hydra.haskell.syntax.SpecImport] = hydra.lib.logic.ifElse[Option[hydra.haskell.syntax.SpecImport]](hydra.lib.lists.`null`[scala.Predef.String](hidden))(None)(Some(hydra.haskell.syntax.SpecImport.hiding(hydra.lib.lists.map[scala.Predef.String,
         hydra.haskell.syntax.ImportExportSpec]((n: scala.Predef.String) =>
        hydra.haskell.syntax.ImportExportSpec(None, hydra.haskell.utils.simpleName(n), None))(hidden))))
      hydra.haskell.syntax.Import(hydra.lib.maybes.isJust[scala.Predef.String](malias),
         name, hydra.lib.maybes.map[scala.Predef.String, hydra.haskell.syntax.ModuleName]((x: scala.Predef.String) => x)(malias),
         spec)
    }
    hydra.lib.lists.map[Tuple2[Tuple2[scala.Predef.String, Option[scala.Predef.String]],
       Seq[scala.Predef.String]], hydra.haskell.syntax.Import](toImport)(hydra.lib.lists.concat[Tuple2[Tuple2[scala.Predef.String,
       Option[scala.Predef.String]], Seq[scala.Predef.String]]](Seq(Seq(Tuple2(Tuple2("Prelude",
       None), Seq("Enum", "Ordering", "decodeFloat", "encodeFloat", "fail", "map",
       "pure", "sum"))), Seq(Tuple2(Tuple2("Data.Scientific", Some("Sci")), Seq())),
       condImport(meta.usesByteString)(Tuple2(Tuple2("Data.ByteString", Some("B")),
       Seq())), condImport(meta.usesInt)(Tuple2(Tuple2("Data.Int", Some("I")), Seq())),
       condImport(meta.usesMap)(Tuple2(Tuple2("Data.Map", Some("M")), Seq())), condImport(meta.usesSet)(Tuple2(Tuple2("Data.Set",
       Some("S")), Seq())), hydra.lib.logic.ifElse[Seq[Tuple2[Tuple2[scala.Predef.String,
       Option[scala.Predef.String]], Seq[scala.Predef.String]]]](hydra.lib.logic.or(hydra.analysis.moduleContainsBinaryLiterals(mod))(hydra.analysis.moduleContainsDecimalLiterals(mod)))(Seq(Tuple2(Tuple2("Hydra.Lib.Literals",
       Some("Literals")), Seq())))(Seq()))))
  }
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[Seq[hydra.haskell.syntax.DeclarationWithComments]],
     hydra.haskell.syntax.Module](hydra.lib.eithers.mapList[hydra.packaging.Definition,
     Seq[hydra.haskell.syntax.DeclarationWithComments], hydra.errors.Error](createDeclarations)(defs))((declLists: Seq[Seq[hydra.haskell.syntax.DeclarationWithComments]]) =>
    {
    lazy val decls: Seq[hydra.haskell.syntax.DeclarationWithComments] = hydra.lib.lists.concat[hydra.haskell.syntax.DeclarationWithComments](declLists)
    lazy val mc: Option[scala.Predef.String] = (mod.description)
    Right(hydra.haskell.syntax.Module(Some(hydra.haskell.syntax.ModuleHead(mc, importName(h(mod.namespace)),
       Seq())), imports, decls))
  })
}

lazy val emptyMetadata: hydra.haskell.environment.HaskellModuleMetadata = hydra.haskell.environment.HaskellModuleMetadata(false,
   false, false, false)

def encodeCaseExpression[T0](depth: Int)(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(stmt: hydra.core.CaseStatement)(scrutinee: hydra.haskell.syntax.Expression)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.haskell.syntax.Expression] =
  {
  lazy val dn: hydra.core.Name = (stmt.typeName)
  lazy val `def`: Option[hydra.core.Term] = (stmt.default)
  lazy val fields: Seq[hydra.core.Field] = (stmt.cases)
  def toAlt(fieldMap: Map[hydra.core.Name, hydra.core.FieldType])(field: hydra.core.Field): Either[hydra.errors.Error,
     hydra.haskell.syntax.Alternative] =
    {
    lazy val fn: hydra.core.Name = (field.name)
    lazy val `fun_`: hydra.core.Term = (field.term)
    lazy val v0: scala.Predef.String = hydra.lib.strings.cat2("v")(hydra.lib.literals.showInt32(depth))
    lazy val raw: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(`fun_`,
       hydra.core.Term.variable(v0)))
    lazy val rhsTerm: hydra.core.Term = hydra.dependencies.simplifyTerm(raw)
    lazy val v1: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](hydra.variables.isFreeVariableInTerm(v0)(rhsTerm))(hydra.constants.ignoredVariable)(v0)
    lazy val hname: hydra.haskell.syntax.Name = hydra.haskell.utils.unionFieldReference(hydra.lib.sets.union[hydra.core.Name](hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name,
       hydra.core.Term](g.boundTerms)))(hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name,
       hydra.core.TypeScheme](g.schemaTypes))))(namespaces)(dn)(fn)
    hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.haskell.syntax.Pattern],
       hydra.haskell.syntax.Alternative](hydra.lib.maybes.cases[hydra.core.FieldType,
       Either[hydra.errors.Error, Seq[hydra.haskell.syntax.Pattern]]](hydra.lib.maps.lookup[hydra.core.Name,
       hydra.core.FieldType](fn)(fieldMap))(Left(hydra.errors.Error.resolution(hydra.errors.ResolutionError.noMatchingField(hydra.errors.NoMatchingFieldError(fn)))))((fieldType: hydra.core.FieldType) =>
      {
      lazy val ft: hydra.core.Type = (fieldType.`type`)
      def noArgs[T1]: Seq[T1] = Seq()
      lazy val singleArg: Seq[hydra.haskell.syntax.Pattern] = Seq(hydra.haskell.syntax.Pattern.name(hydra.haskell.utils.rawName(v1)))
      hydra.strip.deannotateType(ft) match
        case hydra.core.Type.unit => Right(noArgs)
        case _ => Right(singleArg)
    }))((args: Seq[hydra.haskell.syntax.Pattern]) =>
      {
      lazy val lhs: hydra.haskell.syntax.Pattern = hydra.haskell.utils.applicationPattern(hname)(args)
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.CaseRhs, hydra.haskell.syntax.Alternative](hydra.lib.eithers.map[hydra.haskell.syntax.Expression,
         hydra.haskell.syntax.CaseRhs, hydra.errors.Error]((x: hydra.haskell.syntax.Expression) => x)(hydra.haskell.coder.encodeTerm(hydra.lib.math.add(depth)(1))(namespaces)(rhsTerm)(cx)(g)))((rhs: hydra.haskell.syntax.CaseRhs) => Right(hydra.haskell.syntax.Alternative(lhs,
         rhs, None)))
    })
  }
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.FieldType], hydra.haskell.syntax.Expression](hydra.resolution.requireUnionType(cx)(g)(dn))((rt: Seq[hydra.core.FieldType]) =>
    {
    def toFieldMapEntry(f: hydra.core.FieldType): Tuple2[hydra.core.Name, hydra.core.FieldType] = Tuple2(f.name, f)
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.FieldType] = hydra.lib.maps.fromList[hydra.core.Name,
       hydra.core.FieldType](hydra.lib.lists.map[hydra.core.FieldType, Tuple2[hydra.core.Name,
       hydra.core.FieldType]](toFieldMapEntry)(rt))
    hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.haskell.syntax.Alternative],
       hydra.haskell.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Field,
       hydra.haskell.syntax.Alternative, hydra.errors.Error]((v1: hydra.core.Field) => toAlt(fieldMap)(v1))(fields))((ecases: Seq[hydra.haskell.syntax.Alternative]) =>
      hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.haskell.syntax.Alternative],
         hydra.haskell.syntax.Expression](hydra.lib.maybes.cases[hydra.core.Term,
         Either[hydra.errors.Error, Seq[hydra.haskell.syntax.Alternative]]](`def`)(Right(Seq()))((d: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.CaseRhs, Seq[hydra.haskell.syntax.Alternative]](hydra.lib.eithers.map[hydra.haskell.syntax.Expression,
         hydra.haskell.syntax.CaseRhs, hydra.errors.Error]((x: hydra.haskell.syntax.Expression) => x)(hydra.haskell.coder.encodeTerm(depth)(namespaces)(d)(cx)(g)))((cs: hydra.haskell.syntax.CaseRhs) =>
      {
      lazy val lhs: hydra.haskell.syntax.Pattern = hydra.haskell.syntax.Pattern.name(hydra.haskell.utils.rawName(hydra.constants.ignoredVariable))
      lazy val alt: hydra.haskell.syntax.Alternative = hydra.haskell.syntax.Alternative(lhs, cs, None)
      Right(Seq(alt))
    })))((dcases: Seq[hydra.haskell.syntax.Alternative]) =>
      Right(hydra.haskell.syntax.Expression.`case`(hydra.haskell.syntax.CaseExpression(scrutinee,
         hydra.lib.lists.concat2[hydra.haskell.syntax.Alternative](ecases)(dcases))))))
  })
}

def encodeLambdaTerm[T0](depth: Int)(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(lam: hydra.core.Lambda)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.haskell.syntax.Expression] =
  {
  lazy val v: hydra.core.Name = (lam.parameter)
  lazy val body: hydra.core.Term = (lam.body)
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Expression, hydra.haskell.syntax.Expression](hydra.haskell.coder.encodeTerm(depth)(namespaces)(body)(cx)(g))((hbody: hydra.haskell.syntax.Expression) =>
    Right(hydra.haskell.utils.hslambda(hydra.haskell.utils.elementReference(namespaces)(v))(hbody)))
}

def encodeLiteral[T0](l: hydra.core.Literal)(cx: T0): Either[hydra.errors.Error, hydra.haskell.syntax.Expression] =
  l match
  case hydra.core.Literal.binary(v_Literal_binary_bs) => Right(hydra.haskell.utils.hsapp(hydra.haskell.utils.hsvar("Literals.stringToBinary"))(hydra.haskell.utils.hslit(hydra.haskell.syntax.Literal.string(hydra.lib.literals.binaryToString(v_Literal_binary_bs)))))
  case hydra.core.Literal.boolean(v_Literal_boolean_b) => Right(hydra.haskell.utils.hsvar(hydra.lib.logic.ifElse[scala.Predef.String](v_Literal_boolean_b)("True")("False")))
  case hydra.core.Literal.decimal(v_Literal_decimal_d) => Right(hydra.haskell.utils.hsapp(hydra.haskell.utils.hsvar("Literals.stringToDecimal"))(hydra.haskell.utils.hslit(hydra.haskell.syntax.Literal.string(hydra.lib.literals.showDecimal(v_Literal_decimal_d)))))
  case hydra.core.Literal.float(v_Literal_float_fv) => v_Literal_float_fv match
    case hydra.core.FloatValue.float32(v_FloatValue_float32_f) => Right(hydra.haskell.utils.hslit(hydra.haskell.syntax.Literal.float(v_FloatValue_float32_f)))
    case hydra.core.FloatValue.float64(v_FloatValue_float64_f) => Right(hydra.haskell.utils.hslit(hydra.haskell.syntax.Literal.double(v_FloatValue_float64_f)))
    case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_f) => Right(hydra.haskell.utils.hslit(hydra.haskell.syntax.Literal.double(hydra.lib.literals.bigfloatToFloat64(v_FloatValue_bigfloat_f))))
  case hydra.core.Literal.integer(v_Literal_integer_iv) => v_Literal_integer_iv match
    case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_i) => Right(hydra.haskell.utils.hslit(hydra.haskell.syntax.Literal.integer(v_IntegerValue_bigint_i)))
    case hydra.core.IntegerValue.int8(v_IntegerValue_int8_i) => Right(hydra.haskell.utils.hslit(hydra.haskell.syntax.Literal.integer(hydra.lib.literals.int8ToBigint(v_IntegerValue_int8_i))))
    case hydra.core.IntegerValue.int16(v_IntegerValue_int16_i) => Right(hydra.haskell.utils.hslit(hydra.haskell.syntax.Literal.integer(hydra.lib.literals.int16ToBigint(v_IntegerValue_int16_i))))
    case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(hydra.haskell.utils.hslit(hydra.haskell.syntax.Literal.int(v_IntegerValue_int32_i)))
    case hydra.core.IntegerValue.int64(v_IntegerValue_int64_i) => Right(hydra.haskell.utils.hslit(hydra.haskell.syntax.Literal.integer(hydra.lib.literals.int64ToBigint(v_IntegerValue_int64_i))))
    case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_i) => Right(hydra.haskell.utils.hslit(hydra.haskell.syntax.Literal.integer(hydra.lib.literals.uint8ToBigint(v_IntegerValue_uint8_i))))
    case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_i) => Right(hydra.haskell.utils.hslit(hydra.haskell.syntax.Literal.integer(hydra.lib.literals.uint16ToBigint(v_IntegerValue_uint16_i))))
    case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_i) => Right(hydra.haskell.utils.hslit(hydra.haskell.syntax.Literal.integer(hydra.lib.literals.uint32ToBigint(v_IntegerValue_uint32_i))))
    case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_i) => Right(hydra.haskell.utils.hslit(hydra.haskell.syntax.Literal.integer(hydra.lib.literals.uint64ToBigint(v_IntegerValue_uint64_i))))
  case hydra.core.Literal.string(v_Literal_string_s) => Right(hydra.haskell.utils.hslit(hydra.haskell.syntax.Literal.string(v_Literal_string_s)))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("supported literal",
     hydra.show.core.literal(l)))))

def encodeProjection[T0](namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(proj: hydra.core.Projection): Either[T0,
   hydra.haskell.syntax.Expression] =
  {
  lazy val dn: hydra.core.Name = (proj.typeName)
  lazy val fname: hydra.core.Name = (proj.field)
  Right(hydra.haskell.syntax.Expression.variable(hydra.haskell.utils.recordFieldReference(namespaces)(dn)(fname)))
}

def encodeStandaloneCases[T0](depth: Int)(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(stmt: hydra.core.CaseStatement)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.haskell.syntax.Expression] =
  hydra.lib.eithers.map[hydra.haskell.syntax.Expression, hydra.haskell.syntax.Expression,
     hydra.errors.Error]((v1: hydra.haskell.syntax.Expression) =>
  hydra.haskell.utils.hslambda(hydra.haskell.utils.rawName("x"))(v1))(hydra.haskell.coder.encodeCaseExpression(depth)(namespaces)(stmt)(hydra.haskell.utils.hsvar("x"))(cx)(g))

def encodeTerm[T0](depth: Int)(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(term: hydra.core.Term)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.haskell.syntax.Expression] =
  {
  def encode(t: hydra.core.Term): Either[hydra.errors.Error, hydra.haskell.syntax.Expression] = hydra.haskell.coder.encodeTerm(depth)(namespaces)(t)(cx)(g)
  def nonemptyMap(m: Map[hydra.core.Term, hydra.core.Term]): Either[hydra.errors.Error,
     hydra.haskell.syntax.Expression] =
    {
    lazy val lhs: hydra.haskell.syntax.Expression = hydra.haskell.utils.hsvar("M.fromList")
    def encodePair(pair: Tuple2[hydra.core.Term, hydra.core.Term]): Either[hydra.errors.Error,
       hydra.haskell.syntax.Expression] =
      {
      lazy val k: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](pair)
      lazy val v: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](pair)
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Expression,
         hydra.haskell.syntax.Expression](encode(k))((hk: hydra.haskell.syntax.Expression) =>
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Expression,
           hydra.haskell.syntax.Expression](encode(v))((hv: hydra.haskell.syntax.Expression) => Right(hydra.haskell.syntax.Expression.tuple(Seq(hk,
           hv)))))
    }
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Expression, hydra.haskell.syntax.Expression](hydra.lib.eithers.map[Seq[hydra.haskell.syntax.Expression],
       hydra.haskell.syntax.Expression, hydra.errors.Error]((x: Seq[hydra.haskell.syntax.Expression]) => hydra.haskell.syntax.Expression.list(x))(hydra.lib.eithers.mapList[Tuple2[hydra.core.Term,
       hydra.core.Term], hydra.haskell.syntax.Expression, hydra.errors.Error](encodePair)(hydra.lib.maps.toList[hydra.core.Term,
       hydra.core.Term](m))))((rhs: hydra.haskell.syntax.Expression) => Right(hydra.haskell.utils.hsapp(lhs)(rhs)))
  }
  def nonemptySet(s: scala.collection.immutable.Set[hydra.core.Term]): Either[hydra.errors.Error,
     hydra.haskell.syntax.Expression] =
    {
    lazy val lhs: hydra.haskell.syntax.Expression = hydra.haskell.utils.hsvar("S.fromList")
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Expression, hydra.haskell.syntax.Expression](hydra.haskell.coder.encodeTerm(depth)(namespaces)(hydra.core.Term.list(hydra.lib.sets.toList[hydra.core.Term](s)))(cx)(g))((rhs: hydra.haskell.syntax.Expression) => Right(hydra.haskell.utils.hsapp(lhs)(rhs)))
  }
  hydra.strip.deannotateTerm(term) match
    case hydra.core.Term.application(v_Term_application_app) => {
      lazy val fun: hydra.core.Term = (v_Term_application_app.function)
      lazy val arg: hydra.core.Term = (v_Term_application_app.argument)
      lazy val deannotatedFun: hydra.core.Term = hydra.strip.deannotateTerm(fun)
      deannotatedFun match
        case hydra.core.Term.cases(v_Term_cases_stmt) => hydra.lib.eithers.bind[hydra.errors.Error,
           hydra.haskell.syntax.Expression, hydra.haskell.syntax.Expression](encode(arg))((harg: hydra.haskell.syntax.Expression) =>
          hydra.haskell.coder.encodeCaseExpression(depth)(namespaces)(v_Term_cases_stmt)(harg)(cx)(g))
        case _ => hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Expression,
           hydra.haskell.syntax.Expression](encode(fun))((hfun: hydra.haskell.syntax.Expression) =>
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Expression,
             hydra.haskell.syntax.Expression](encode(arg))((harg: hydra.haskell.syntax.Expression) => Right(hydra.haskell.utils.hsapp(hfun)(harg))))
    }
    case hydra.core.Term.cases(v_Term_cases_stmt) => hydra.haskell.coder.encodeStandaloneCases(depth)(namespaces)(v_Term_cases_stmt)(cx)(g)
    case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term,
       hydra.core.Term, Either[hydra.errors.Error, hydra.haskell.syntax.Expression]]((l: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Expression,
         hydra.haskell.syntax.Expression](encode(l))((hl: hydra.haskell.syntax.Expression) =>
      Right(hydra.haskell.utils.hsapp(hydra.haskell.utils.hsvar("Left"))(hl))))((r: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Expression,
         hydra.haskell.syntax.Expression](encode(r))((hr: hydra.haskell.syntax.Expression) =>
      Right(hydra.haskell.utils.hsapp(hydra.haskell.utils.hsvar("Right"))(hr))))(v_Term_either_e)
    case hydra.core.Term.lambda(v_Term_lambda_lam) => hydra.haskell.coder.encodeLambdaTerm(depth)(namespaces)(v_Term_lambda_lam)(cx)(g)
    case hydra.core.Term.project(v_Term_project_proj) => hydra.haskell.coder.encodeProjection(namespaces)(v_Term_project_proj)
    case hydra.core.Term.unwrap(v_Term_unwrap_name) => hydra.haskell.coder.encodeUnwrap(namespaces)(v_Term_unwrap_name)
    case hydra.core.Term.let(v_Term_let_letTerm) => {
      def collectBindings(lt: hydra.core.Let): Tuple2[Seq[hydra.core.Binding], hydra.core.Term] =
        {
        lazy val bs: Seq[hydra.core.Binding] = (lt.bindings)
        lazy val body: hydra.core.Term = (lt.body)
        hydra.strip.deannotateTerm(body) match
          case hydra.core.Term.let(v_Term_let_innerLt) => {
            lazy val innerResult: Tuple2[Seq[hydra.core.Binding], hydra.core.Term] = collectBindings(v_Term_let_innerLt)
            Tuple2(hydra.lib.lists.concat2[hydra.core.Binding](bs)(hydra.lib.pairs.first[Seq[hydra.core.Binding],
               hydra.core.Term](innerResult)), hydra.lib.pairs.second[Seq[hydra.core.Binding],
               hydra.core.Term](innerResult))
          }
          case _ => Tuple2(bs, body)
      }
      lazy val collected: Tuple2[Seq[hydra.core.Binding], hydra.core.Term] = collectBindings(v_Term_let_letTerm)
      lazy val allBindings: Seq[hydra.core.Binding] = hydra.lib.pairs.first[Seq[hydra.core.Binding],
         hydra.core.Term](collected)
      lazy val finalBody: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Binding], hydra.core.Term](collected)
      def encodeBinding(binding: hydra.core.Binding): Either[hydra.errors.Error, hydra.haskell.syntax.LocalBinding] =
        {
        lazy val name: hydra.core.Name = (binding.name)
        lazy val `term_`: hydra.core.Term = (binding.term)
        lazy val hname: hydra.haskell.syntax.Name = hydra.haskell.utils.simpleName(name)
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Expression,
           hydra.haskell.syntax.LocalBinding](encode(`term_`))((hexpr: hydra.haskell.syntax.Expression) =>
          Right(hydra.haskell.syntax.LocalBinding.value(hydra.haskell.utils.simpleValueBinding(hname)(hexpr)(None))))
      }
      hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.haskell.syntax.LocalBinding],
         hydra.haskell.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Binding,
         hydra.haskell.syntax.LocalBinding, hydra.errors.Error](encodeBinding)(allBindings))((hbindings: Seq[hydra.haskell.syntax.LocalBinding]) =>
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Expression,
           hydra.haskell.syntax.Expression](encode(finalBody))((hinner: hydra.haskell.syntax.Expression) =>
        Right(hydra.haskell.syntax.Expression.let(hydra.haskell.syntax.LetExpression(hbindings, hinner)))))
    }
    case hydra.core.Term.list(v_Term_list_els) => hydra.lib.eithers.bind[hydra.errors.Error,
       Seq[hydra.haskell.syntax.Expression], hydra.haskell.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Term,
       hydra.haskell.syntax.Expression, hydra.errors.Error](encode)(v_Term_list_els))((helems: Seq[hydra.haskell.syntax.Expression]) => Right(hydra.haskell.syntax.Expression.list(helems)))
    case hydra.core.Term.literal(v_Term_literal_v) => hydra.haskell.coder.encodeLiteral(v_Term_literal_v)(cx)
    case hydra.core.Term.map(v_Term_map_m) => hydra.lib.logic.ifElse[Either[hydra.errors.Error,
       hydra.haskell.syntax.Expression]](hydra.lib.maps.`null`[hydra.core.Term, hydra.core.Term](v_Term_map_m))(Right(hydra.haskell.utils.hsvar("M.empty")))(nonemptyMap(v_Term_map_m))
    case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.lib.maybes.cases[hydra.core.Term,
       Either[hydra.errors.Error, hydra.haskell.syntax.Expression]](v_Term_maybe_m)(Right(hydra.haskell.utils.hsvar("Nothing")))((t: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Expression,
         hydra.haskell.syntax.Expression](encode(t))((ht: hydra.haskell.syntax.Expression) =>
      Right(hydra.haskell.utils.hsapp(hydra.haskell.utils.hsvar("Just"))(ht))))
    case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.eithers.bind[hydra.errors.Error,
       hydra.haskell.syntax.Expression, hydra.haskell.syntax.Expression](encode(hydra.lib.pairs.first[hydra.core.Term,
       hydra.core.Term](v_Term_pair_p)))((f: hydra.haskell.syntax.Expression) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Expression,
         hydra.haskell.syntax.Expression](encode(hydra.lib.pairs.second[hydra.core.Term,
         hydra.core.Term](v_Term_pair_p)))((s: hydra.haskell.syntax.Expression) => Right(hydra.haskell.syntax.Expression.tuple(Seq(f,
         s)))))
    case hydra.core.Term.record(v_Term_record_record) => {
      lazy val sname: hydra.core.Name = (v_Term_record_record.typeName)
      lazy val fields: Seq[hydra.core.Field] = (v_Term_record_record.fields)
      def toFieldUpdate(field: hydra.core.Field): Either[hydra.errors.Error, hydra.haskell.syntax.FieldUpdate] =
        {
        lazy val fn: hydra.core.Name = (field.name)
        lazy val ft: hydra.core.Term = (field.term)
        lazy val fieldRef: hydra.haskell.syntax.Name = hydra.haskell.utils.recordFieldReference(namespaces)(sname)(fn)
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Expression,
           hydra.haskell.syntax.FieldUpdate](encode(ft))((hft: hydra.haskell.syntax.Expression) => Right(hydra.haskell.syntax.FieldUpdate(fieldRef,
           hft)))
      }
      lazy val typeName: hydra.haskell.syntax.Name = hydra.haskell.utils.elementReference(namespaces)(sname)
      hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.haskell.syntax.FieldUpdate],
         hydra.haskell.syntax.Expression](hydra.lib.eithers.mapList[hydra.core.Field,
         hydra.haskell.syntax.FieldUpdate, hydra.errors.Error](toFieldUpdate)(fields))((updates: Seq[hydra.haskell.syntax.FieldUpdate]) =>
        Right(hydra.haskell.syntax.Expression.constructRecord(hydra.haskell.syntax.ConstructRecordExpression(typeName,
           updates))))
    }
    case hydra.core.Term.set(v_Term_set_s) => hydra.lib.logic.ifElse[Either[hydra.errors.Error,
       hydra.haskell.syntax.Expression]](hydra.lib.sets.`null`[hydra.core.Term](v_Term_set_s))(Right(hydra.haskell.utils.hsvar("S.empty")))(nonemptySet(v_Term_set_s))
    case hydra.core.Term.typeLambda(v_Term_typeLambda_abs) => {
      lazy val term1: hydra.core.Term = (v_Term_typeLambda_abs.body)
      encode(term1)
    }
    case hydra.core.Term.typeApplication(v_Term_typeApplication_typed) => {
      lazy val term1: hydra.core.Term = (v_Term_typeApplication_typed.body)
      encode(term1)
    }
    case hydra.core.Term.inject(v_Term_inject_injection) => {
      lazy val sname: hydra.core.Name = (v_Term_inject_injection.typeName)
      lazy val field: hydra.core.Field = (v_Term_inject_injection.field)
      lazy val fn: hydra.core.Name = (field.name)
      lazy val ft: hydra.core.Term = (field.term)
      lazy val lhs: hydra.haskell.syntax.Expression = hydra.haskell.syntax.Expression.variable(hydra.haskell.utils.unionFieldReference(hydra.lib.sets.union[hydra.core.Name](hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name,
         hydra.core.Term](g.boundTerms)))(hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name,
         hydra.core.TypeScheme](g.schemaTypes))))(namespaces)(sname)(fn))
      lazy val dflt: Either[hydra.errors.Error, hydra.haskell.syntax.Expression] = hydra.lib.eithers.map[hydra.haskell.syntax.Expression,
         hydra.haskell.syntax.Expression, hydra.errors.Error]((v1: hydra.haskell.syntax.Expression) => hydra.haskell.utils.hsapp(lhs)(v1))(encode(ft))
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, hydra.haskell.syntax.Expression](hydra.resolution.requireUnionField(cx)(g)(sname)(fn))((ftyp: hydra.core.Type) =>
        hydra.strip.deannotateType(ftyp) match
        case hydra.core.Type.unit => Right(lhs)
        case _ => dflt)
    }
    case hydra.core.Term.unit => Right(hydra.haskell.syntax.Expression.tuple(Seq()))
    case hydra.core.Term.variable(v_Term_variable_name) => Right(hydra.haskell.syntax.Expression.variable(hydra.haskell.utils.elementReference(namespaces)(v_Term_variable_name)))
    case hydra.core.Term.wrap(v_Term_wrap_wrapped) => {
      lazy val tname: hydra.core.Name = (v_Term_wrap_wrapped.typeName)
      lazy val `term_`: hydra.core.Term = (v_Term_wrap_wrapped.body)
      lazy val lhs: hydra.haskell.syntax.Expression = hydra.haskell.syntax.Expression.variable(hydra.haskell.utils.elementReference(namespaces)(tname))
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Expression,
         hydra.haskell.syntax.Expression](encode(`term_`))((rhs: hydra.haskell.syntax.Expression) => Right(hydra.haskell.utils.hsapp(lhs)(rhs)))
    }
    case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("supported term",
       hydra.show.core.term(term)))))
}

def encodeType[T0, T1](namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(typ: hydra.core.Type)(cx: T0)(g: T1): Either[hydra.errors.Error,
   hydra.haskell.syntax.Type] =
  {
  def encode(t: hydra.core.Type): Either[hydra.errors.Error, hydra.haskell.syntax.Type] = hydra.haskell.coder.encodeType(namespaces)(t)(cx)(g)
  def ref[T2](name: hydra.core.Name): Either[T2, hydra.haskell.syntax.Type] =
    Right(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.elementReference(namespaces)(name)))
  lazy val unitTuple: hydra.haskell.syntax.Type = hydra.haskell.syntax.Type.tuple(Seq())
  hydra.strip.deannotateType(typ) match
    case hydra.core.Type.application(v_Type_application_app) => {
      lazy val lhs: hydra.core.Type = (v_Type_application_app.function)
      lazy val rhs: hydra.core.Type = (v_Type_application_app.argument)
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Type, hydra.haskell.syntax.Type](encode(lhs))((hlhs: hydra.haskell.syntax.Type) =>
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Type, hydra.haskell.syntax.Type](encode(rhs))((hrhs: hydra.haskell.syntax.Type) =>
        Right(hydra.haskell.utils.toTypeApplication(Seq(hlhs, hrhs)))))
    }
    case hydra.core.Type.either(v_Type_either_eitherType) => {
      lazy val `left_`: hydra.core.Type = (v_Type_either_eitherType.left)
      lazy val `right_`: hydra.core.Type = (v_Type_either_eitherType.right)
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Type, hydra.haskell.syntax.Type](encode(`left_`))((hleft: hydra.haskell.syntax.Type) =>
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Type, hydra.haskell.syntax.Type](encode(`right_`))((hright: hydra.haskell.syntax.Type) =>
        Right(hydra.haskell.utils.toTypeApplication(Seq(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName("Either")),
           hleft, hright)))))
    }
    case hydra.core.Type.function(v_Type_function_funType) => {
      lazy val dom: hydra.core.Type = (v_Type_function_funType.domain)
      lazy val cod: hydra.core.Type = (v_Type_function_funType.codomain)
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Type, hydra.haskell.syntax.Type](encode(dom))((hdom: hydra.haskell.syntax.Type) =>
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Type, hydra.haskell.syntax.Type](encode(cod))((hcod: hydra.haskell.syntax.Type) =>
        Right(hydra.haskell.syntax.Type.function(hydra.haskell.syntax.FunctionType(hdom, hcod)))))
    }
    case hydra.core.Type.forall(v_Type_forall_forallType) => {
      lazy val v: hydra.core.Name = (v_Type_forall_forallType.parameter)
      lazy val body: hydra.core.Type = (v_Type_forall_forallType.body)
      encode(body)
    }
    case hydra.core.Type.list(v_Type_list_lt) => hydra.lib.eithers.bind[hydra.errors.Error,
       hydra.haskell.syntax.Type, hydra.haskell.syntax.Type](encode(v_Type_list_lt))((hlt: hydra.haskell.syntax.Type) => Right(hydra.haskell.syntax.Type.list(hlt)))
    case hydra.core.Type.literal(v_Type_literal_lt) => v_Type_literal_lt match
      case hydra.core.LiteralType.binary => Right(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName("B.ByteString")))
      case hydra.core.LiteralType.boolean => Right(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName("Bool")))
      case hydra.core.LiteralType.decimal => Right(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName("Sci.Scientific")))
      case hydra.core.LiteralType.float(v_LiteralType_float_ft) => v_LiteralType_float_ft match
        case hydra.core.FloatType.float32 => Right(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName("Float")))
        case hydra.core.FloatType.float64 => Right(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName("Double")))
        case hydra.core.FloatType.bigfloat => Right(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName("Double")))
      case hydra.core.LiteralType.integer(v_LiteralType_integer_it) => v_LiteralType_integer_it match
        case hydra.core.IntegerType.bigint => Right(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName("Integer")))
        case hydra.core.IntegerType.int8 => Right(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName("I.Int8")))
        case hydra.core.IntegerType.int16 => Right(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName("I.Int16")))
        case hydra.core.IntegerType.int32 => Right(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName("Int")))
        case hydra.core.IntegerType.int64 => Right(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName("I.Int64")))
        case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("supported integer type",
           hydra.show.core.integerType(v_LiteralType_integer_it)))))
      case hydra.core.LiteralType.string => Right(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName("String")))
      case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("supported literal type",
         hydra.show.core.literalType(v_Type_literal_lt)))))
    case hydra.core.Type.map(v_Type_map_mapType) => {
      lazy val kt: hydra.core.Type = (v_Type_map_mapType.keys)
      lazy val vt: hydra.core.Type = (v_Type_map_mapType.values)
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Type, hydra.haskell.syntax.Type](encode(kt))((hkt: hydra.haskell.syntax.Type) =>
        hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Type, hydra.haskell.syntax.Type](encode(vt))((hvt: hydra.haskell.syntax.Type) =>
        Right(hydra.haskell.utils.toTypeApplication(Seq(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName("M.Map")),
           hkt, hvt)))))
    }
    case hydra.core.Type.maybe(v_Type_maybe_ot) => hydra.lib.eithers.bind[hydra.errors.Error,
       hydra.haskell.syntax.Type, hydra.haskell.syntax.Type](encode(v_Type_maybe_ot))((hot: hydra.haskell.syntax.Type) =>
      Right(hydra.haskell.utils.toTypeApplication(Seq(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName("Maybe")),
         hot))))
    case hydra.core.Type.pair(v_Type_pair_pt) => hydra.lib.eithers.bind[hydra.errors.Error,
       hydra.haskell.syntax.Type, hydra.haskell.syntax.Type](encode(v_Type_pair_pt.first))((f: hydra.haskell.syntax.Type) =>
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Type, hydra.haskell.syntax.Type](encode(v_Type_pair_pt.second))((s: hydra.haskell.syntax.Type) => Right(hydra.haskell.syntax.Type.tuple(Seq(f,
         s)))))
    case hydra.core.Type.record(v_Type_record__) => ref("placeholder")
    case hydra.core.Type.set(v_Type_set_st) => hydra.lib.eithers.bind[hydra.errors.Error,
       hydra.haskell.syntax.Type, hydra.haskell.syntax.Type](encode(v_Type_set_st))((hst: hydra.haskell.syntax.Type) =>
      Right(hydra.haskell.utils.toTypeApplication(Seq(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName("S.Set")),
         hst))))
    case hydra.core.Type.union(v_Type_union__) => ref("placeholder")
    case hydra.core.Type.unit => Right(unitTuple)
    case hydra.core.Type.variable(v_Type_variable_v1) => ref(v_Type_variable_v1)
    case hydra.core.Type.void => Right(hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName("Void")))
    case hydra.core.Type.wrap(v_Type_wrap__) => ref("placeholder")
    case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("supported type",
       hydra.show.core.`type`(typ)))))
}

def encodeTypeWithClassAssertions[T0, T1](namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(explicitClasses: Map[hydra.core.Name,
   scala.collection.immutable.Set[hydra.classes.TypeClass]])(typ: hydra.core.Type)(cx: T0)(g: T1): Either[hydra.errors.Error,
   hydra.haskell.syntax.Type] =
  {
  lazy val classes: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]] = hydra.lib.maps.union[hydra.core.Name,
     scala.collection.immutable.Set[hydra.classes.TypeClass]](explicitClasses)(hydra.haskell.coder.getImplicitTypeClasses(typ))
  lazy val implicitClasses: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]] = hydra.haskell.coder.getImplicitTypeClasses(typ)
  def encodeAssertion(pair: Tuple2[hydra.core.Name, hydra.classes.TypeClass]): hydra.haskell.syntax.Assertion =
    {
    lazy val name: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.classes.TypeClass](pair)
    lazy val cls: hydra.classes.TypeClass = hydra.lib.pairs.second[hydra.core.Name, hydra.classes.TypeClass](pair)
    lazy val hname: hydra.haskell.syntax.Name = hydra.haskell.utils.rawName(cls match
      case hydra.classes.TypeClass.equality => "Eq"
      case hydra.classes.TypeClass.ordering => "Ord")
    lazy val htype: hydra.haskell.syntax.Type = hydra.haskell.syntax.Type.variable(hydra.haskell.utils.rawName(name))
    hydra.haskell.syntax.Assertion.`class`(hydra.haskell.syntax.ClassAssertion(hname, Seq(htype)))
  }
  lazy val assertPairs: Seq[Tuple2[hydra.core.Name, hydra.classes.TypeClass]] = hydra.lib.lists.concat[Tuple2[hydra.core.Name,
     hydra.classes.TypeClass]](hydra.lib.lists.map[Tuple2[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]],
     Seq[Tuple2[hydra.core.Name, hydra.classes.TypeClass]]](toPairs)(hydra.lib.maps.toList[hydra.core.Name,
     scala.collection.immutable.Set[hydra.classes.TypeClass]](classes)))
  def toPairs[T2, T3](mapEntry: Tuple2[T2, scala.collection.immutable.Set[T3]]): Seq[Tuple2[T2, T3]] =
    {
    lazy val name: T2 = hydra.lib.pairs.first[T2, scala.collection.immutable.Set[T3]](mapEntry)
    lazy val clsSet: scala.collection.immutable.Set[T3] = hydra.lib.pairs.second[T2,
       scala.collection.immutable.Set[T3]](mapEntry)
    def toPair[T4](c: T4): Tuple2[T2, T4] = Tuple2(name, c)
    hydra.lib.lists.map[T3, Tuple2[T2, T3]](toPair)(hydra.lib.sets.toList[T3](clsSet))
  }
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Type, hydra.haskell.syntax.Type](hydra.haskell.coder.adaptTypeToHaskellAndEncode(namespaces)(typ)(cx)(g))((htyp: hydra.haskell.syntax.Type) =>
    hydra.lib.logic.ifElse[Either[hydra.errors.Error, hydra.haskell.syntax.Type]](hydra.lib.lists.`null`[Tuple2[hydra.core.Name,
       hydra.classes.TypeClass]](assertPairs))(Right(htyp))({
    lazy val encoded: Seq[hydra.haskell.syntax.Assertion] = hydra.lib.lists.map[Tuple2[hydra.core.Name,
       hydra.classes.TypeClass], hydra.haskell.syntax.Assertion](encodeAssertion)(assertPairs)
    lazy val hassert: hydra.haskell.syntax.Assertion = hydra.lib.logic.ifElse[hydra.haskell.syntax.Assertion](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.haskell.syntax.Assertion](encoded))(1))(hydra.lib.lists.head[hydra.haskell.syntax.Assertion](encoded))(hydra.haskell.syntax.Assertion.tuple(encoded))
    Right(hydra.haskell.syntax.Type.ctx(hydra.haskell.syntax.ContextType(hassert, htyp)))
  }))
}

def encodeUnwrap[T0](namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(name: hydra.core.Name): Either[T0,
   hydra.haskell.syntax.Expression] =
  Right(hydra.haskell.syntax.Expression.variable(hydra.haskell.utils.elementReference(namespaces)(hydra.names.qname(hydra.lib.maybes.fromJust[hydra.packaging.Namespace](hydra.names.namespaceOf(name)))(hydra.haskell.utils.newtypeAccessorName(name)))))

def extendMetaForTerm(meta: hydra.haskell.environment.HaskellModuleMetadata)(term: hydra.core.Term): hydra.haskell.environment.HaskellModuleMetadata =
  term match
  case hydra.core.Term.map(v_Term_map__) => hydra.haskell.coder.setMetaUsesMap(true)(meta)
  case hydra.core.Term.set(v_Term_set__) => hydra.haskell.coder.setMetaUsesSet(true)(meta)
  case _ => meta

def extendMetaForType(meta: hydra.haskell.environment.HaskellModuleMetadata)(typ: hydra.core.Type): hydra.haskell.environment.HaskellModuleMetadata =
  hydra.strip.deannotateType(typ) match
  case hydra.core.Type.literal(v_Type_literal_lt) => v_Type_literal_lt match
    case hydra.core.LiteralType.binary => hydra.haskell.coder.setMetaUsesByteString(true)(meta)
    case hydra.core.LiteralType.integer(v_LiteralType_integer_it) => v_LiteralType_integer_it match
      case hydra.core.IntegerType.int8 => hydra.haskell.coder.setMetaUsesInt(true)(meta)
      case hydra.core.IntegerType.int16 => hydra.haskell.coder.setMetaUsesInt(true)(meta)
      case hydra.core.IntegerType.int64 => hydra.haskell.coder.setMetaUsesInt(true)(meta)
      case _ => meta
    case _ => meta
  case hydra.core.Type.map(v_Type_map__) => hydra.haskell.coder.setMetaUsesMap(true)(meta)
  case hydra.core.Type.set(v_Type_set__) => hydra.haskell.coder.setMetaUsesSet(true)(meta)
  case _ => meta

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
  def isTypeVariable(v: hydra.core.Name): Boolean =
    hydra.lib.maybes.isNothing[hydra.packaging.Namespace](hydra.names.namespaceOf(v))
  def tryType(names: scala.collection.immutable.Set[hydra.core.Name])(t: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
    hydra.strip.deannotateType(t) match
    case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.logic.ifElse[scala.collection.immutable.Set[hydra.core.Name]](isTypeVariable(v_Type_variable_v))(hydra.lib.sets.insert[hydra.core.Name](v_Type_variable_v)(names))(names)
    case _ => names
  hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)(fold)(hydra.lib.sets.empty[hydra.core.Name])(typ)
}

def gatherMetadata(defs: Seq[hydra.packaging.Definition]): hydra.haskell.environment.HaskellModuleMetadata =
  {
  def addDef(meta: hydra.haskell.environment.HaskellModuleMetadata)(`def`: hydra.packaging.Definition): hydra.haskell.environment.HaskellModuleMetadata =
    `def` match
    case hydra.packaging.Definition.term(v_Definition_term_termDef) => {
      lazy val term: hydra.core.Term = (v_Definition_term_termDef.term)
      {
        lazy val metaWithTerm: hydra.haskell.environment.HaskellModuleMetadata = hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)((m: hydra.haskell.environment.HaskellModuleMetadata) =>
          (t: hydra.core.Term) => hydra.haskell.coder.extendMetaForTerm(m)(t))(meta)(term)
        hydra.lib.maybes.maybe[hydra.haskell.environment.HaskellModuleMetadata, hydra.core.TypeScheme](metaWithTerm)((ts: hydra.core.TypeScheme) =>
          hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)((m: hydra.haskell.environment.HaskellModuleMetadata) =>
          (t: hydra.core.Type) => hydra.haskell.coder.extendMetaForType(m)(t))(metaWithTerm)(ts.`type`))(v_Definition_term_termDef.`type`)
      }
    }
    case hydra.packaging.Definition.`type`(v_Definition_type_typeDef) => {
      lazy val typ: hydra.core.Type = (v_Definition_type_typeDef.`type`.`type`)
      hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)((m: hydra.haskell.environment.HaskellModuleMetadata) =>
        (t: hydra.core.Type) => hydra.haskell.coder.extendMetaForType(m)(t))(meta)(typ)
    }
  hydra.lib.lists.foldl[hydra.haskell.environment.HaskellModuleMetadata, hydra.packaging.Definition](addDef)(hydra.haskell.coder.emptyMetadata)(defs)
}

def getImplicitTypeClasses(typ: hydra.core.Type): Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]] =
  {
  def toPair[T0](name: T0): Tuple2[T0, scala.collection.immutable.Set[hydra.classes.TypeClass]] =
    Tuple2(name, hydra.lib.sets.fromList[hydra.classes.TypeClass](Seq(hydra.classes.TypeClass.ordering)))
  hydra.lib.maps.fromList[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]](hydra.lib.lists.map[hydra.core.Name,
     Tuple2[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]]](toPair)(hydra.lib.sets.toList[hydra.core.Name](hydra.haskell.coder.findOrdVariables(typ))))
}

lazy val includeTypeDefinitions: Boolean = false

lazy val keyHaskellVar: hydra.core.Name = "haskellVar"

def moduleToHaskell(mod: hydra.packaging.Module)(defs: Seq[hydra.packaging.Definition])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   Map[scala.Predef.String, scala.Predef.String]] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Module, Map[scala.Predef.String,
     scala.Predef.String]](hydra.haskell.coder.moduleToHaskellModule(mod)(defs)(cx)(g))((hsmod: hydra.haskell.syntax.Module) =>
  {
  lazy val s: scala.Predef.String = hydra.serialization.printExpr(hydra.serialization.parenthesize(hydra.haskell.serde.moduleToExpr(hsmod)))
  lazy val filepath: scala.Predef.String = hydra.names.namespaceToFilePath(hydra.util.CaseConvention.pascal)("hs")(mod.namespace)
  Right(hydra.lib.maps.singleton[scala.Predef.String, scala.Predef.String](filepath)(s))
})

def moduleToHaskellModule(mod: hydra.packaging.Module)(defs: Seq[hydra.packaging.Definition])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.haskell.syntax.Module] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName],
     hydra.haskell.syntax.Module](hydra.haskell.utils.namespacesForModule(mod)(cx)(g))((namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName]) =>
  hydra.haskell.coder.constructModule(namespaces)(mod)(defs)(cx)(g))

def nameDecls(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(name: hydra.core.Name)(typ: hydra.core.Type): Seq[hydra.haskell.syntax.DeclarationWithComments] =
  {
  lazy val nm: scala.Predef.String = name
  def toDecl(n: hydra.core.Name)(pair: Tuple2[scala.Predef.String, scala.Predef.String]): hydra.haskell.syntax.DeclarationWithComments =
    {
    lazy val k: scala.Predef.String = hydra.lib.pairs.first[scala.Predef.String, scala.Predef.String](pair)
    lazy val v: scala.Predef.String = hydra.lib.pairs.second[scala.Predef.String, scala.Predef.String](pair)
    lazy val decl: hydra.haskell.syntax.Declaration = hydra.haskell.syntax.Declaration.valueBinding(hydra.haskell.syntax.ValueBinding.simple(hydra.haskell.syntax.SimpleValueBinding(hydra.haskell.utils.applicationPattern(hydra.haskell.utils.simpleName(k))(Seq()),
       hydra.haskell.syntax.Expression.application(hydra.haskell.syntax.ApplicationExpression(hydra.haskell.syntax.Expression.variable(hydra.haskell.utils.elementReference(namespaces)(n)),
       hydra.haskell.syntax.Expression.literal(hydra.haskell.syntax.Literal.string(v)))),
       None)))
    hydra.haskell.syntax.DeclarationWithComments(decl, None)
  }
  lazy val nameDecl: Tuple2[scala.Predef.String, scala.Predef.String] = Tuple2(hydra.haskell.coder.constantForTypeName(name),
     nm)
  lazy val fieldDecls: Seq[Tuple2[scala.Predef.String, scala.Predef.String]] = hydra.lib.lists.map[hydra.core.FieldType,
     Tuple2[scala.Predef.String, scala.Predef.String]](toConstant)(hydra.lexical.fieldsOf(typ))
  def toConstant(fieldType: hydra.core.FieldType): Tuple2[scala.Predef.String, scala.Predef.String] =
    {
    lazy val fname: hydra.core.Name = (fieldType.name)
    Tuple2(hydra.haskell.coder.constantForFieldName(name)(fname), fname)
  }
  hydra.lib.logic.ifElse[Seq[hydra.haskell.syntax.DeclarationWithComments]](hydra.haskell.coder.useCoreImport)(hydra.lib.lists.cons[hydra.haskell.syntax.DeclarationWithComments](toDecl("hydra.core.Name")(nameDecl))(hydra.lib.lists.map[Tuple2[scala.Predef.String,
     scala.Predef.String], hydra.haskell.syntax.DeclarationWithComments]((v1: Tuple2[scala.Predef.String,
     scala.Predef.String]) => toDecl("hydra.core.Name")(v1))(fieldDecls)))(Seq())
}

def setMetaUsesByteString(b: Boolean)(m: hydra.haskell.environment.HaskellModuleMetadata): hydra.haskell.environment.HaskellModuleMetadata =
  hydra.haskell.environment.HaskellModuleMetadata(b, (m.usesInt), (m.usesMap), (m.usesSet))

def setMetaUsesInt(b: Boolean)(m: hydra.haskell.environment.HaskellModuleMetadata): hydra.haskell.environment.HaskellModuleMetadata =
  hydra.haskell.environment.HaskellModuleMetadata(m.usesByteString, b, (m.usesMap), (m.usesSet))

def setMetaUsesMap(b: Boolean)(m: hydra.haskell.environment.HaskellModuleMetadata): hydra.haskell.environment.HaskellModuleMetadata =
  hydra.haskell.environment.HaskellModuleMetadata(m.usesByteString, (m.usesInt), b, (m.usesSet))

def setMetaUsesSet(b: Boolean)(m: hydra.haskell.environment.HaskellModuleMetadata): hydra.haskell.environment.HaskellModuleMetadata =
  hydra.haskell.environment.HaskellModuleMetadata(m.usesByteString, (m.usesInt), (m.usesMap), b)

def toDataDeclaration[T0](namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(`def`: hydra.packaging.TermDefinition)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.haskell.syntax.DeclarationWithComments] =
  {
  lazy val name: hydra.core.Name = (`def`.name)
  lazy val term: hydra.core.Term = (`def`.term)
  lazy val typ: Option[hydra.core.TypeScheme] = (`def`.`type`)
  lazy val hname: hydra.haskell.syntax.Name = hydra.haskell.utils.simpleName(hydra.names.localNameOf(name))
  def rewriteValueBinding(vb: hydra.haskell.syntax.ValueBinding): hydra.haskell.syntax.ValueBinding =
    vb match
    case hydra.haskell.syntax.ValueBinding.simple(v_ValueBinding_simple_simple) => {
      lazy val `pattern_`: hydra.haskell.syntax.Pattern = (v_ValueBinding_simple_simple.pattern)
      lazy val rhs: hydra.haskell.syntax.RightHandSide = (v_ValueBinding_simple_simple.rhs)
      lazy val bindings: Option[hydra.haskell.syntax.LocalBindings] = (v_ValueBinding_simple_simple.localBindings)
      `pattern_` match
        case hydra.haskell.syntax.Pattern.application(v_Pattern_application_appPat) => {
          lazy val `name_`: hydra.haskell.syntax.Name = (v_Pattern_application_appPat.name)
          lazy val args: Seq[hydra.haskell.syntax.Pattern] = (v_Pattern_application_appPat.args)
          lazy val rhsExpr: hydra.haskell.syntax.Expression = rhs
          rhsExpr match
            case hydra.haskell.syntax.Expression.lambda(v_Expression_lambda_lambda_) => {
              lazy val vars: Seq[hydra.haskell.syntax.Pattern] = (`v_Expression_lambda_lambda_`.bindings)
              lazy val body: hydra.haskell.syntax.Expression = (`v_Expression_lambda_lambda_`.inner)
              lazy val newPattern: hydra.haskell.syntax.Pattern = hydra.haskell.utils.applicationPattern(`name_`)(hydra.lib.lists.concat2[hydra.haskell.syntax.Pattern](args)(vars))
              lazy val newRhs: hydra.haskell.syntax.RightHandSide = body
              rewriteValueBinding(hydra.haskell.syntax.ValueBinding.simple(hydra.haskell.syntax.SimpleValueBinding(newPattern,
                 newRhs, bindings)))
            }
            case _ => vb
        }
        case _ => vb
    }
  def toDecl(comments: Option[scala.Predef.String])(`hname_`: hydra.haskell.syntax.Name)(`term_`: hydra.core.Term)(bindings: Option[hydra.haskell.syntax.LocalBindings]): Either[hydra.errors.Error,
     hydra.haskell.syntax.DeclarationWithComments] =
    hydra.strip.deannotateTerm(`term_`) match
    case hydra.core.Term.let(v_Term_let_letTerm) => {
      lazy val lbindings: Seq[hydra.core.Binding] = (v_Term_let_letTerm.bindings)
      lazy val env: hydra.core.Term = (v_Term_let_letTerm.body)
      def toTermDefinition(`hname__`: hydra.haskell.syntax.Name)(`hterm_`: hydra.haskell.syntax.Expression): hydra.haskell.syntax.LocalBinding =
        hydra.haskell.syntax.LocalBinding.value(hydra.haskell.utils.simpleValueBinding(`hname__`)(`hterm_`)(None))
      lazy val hnames: Seq[hydra.haskell.syntax.Name] = hydra.lib.lists.map[hydra.core.Binding,
         hydra.haskell.syntax.Name]((binding: hydra.core.Binding) => hydra.haskell.utils.simpleName(binding.name))(lbindings)
      lazy val terms: Seq[hydra.core.Term] = hydra.lib.lists.map[hydra.core.Binding,
         hydra.core.Term]((x: hydra.core.Binding) => (x.term))(lbindings)
      hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.haskell.syntax.Expression],
         hydra.haskell.syntax.DeclarationWithComments](hydra.lib.eithers.mapList[hydra.core.Term,
         hydra.haskell.syntax.Expression, hydra.errors.Error]((t: hydra.core.Term) => hydra.haskell.coder.encodeTerm(0)(namespaces)(t)(cx)(g))(terms))((hterms: Seq[hydra.haskell.syntax.Expression]) =>
        {
        lazy val hbindings: Seq[hydra.haskell.syntax.LocalBinding] = hydra.lib.lists.zipWith[hydra.haskell.syntax.Name,
           hydra.haskell.syntax.Expression, hydra.haskell.syntax.LocalBinding](toTermDefinition)(hnames)(hterms)
        lazy val prevBindings: Seq[hydra.haskell.syntax.LocalBinding] = hydra.lib.maybes.maybe[Seq[hydra.haskell.syntax.LocalBinding],
           hydra.haskell.syntax.LocalBindings](Seq())((lb: hydra.haskell.syntax.LocalBindings) => lb)(bindings)
        lazy val allBindings: Seq[hydra.haskell.syntax.LocalBinding] = hydra.lib.lists.concat2[hydra.haskell.syntax.LocalBinding](prevBindings)(hbindings)
        toDecl(comments)(`hname_`)(env)(Some(allBindings))
      })
    }
    case _ => hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Expression,
       hydra.haskell.syntax.DeclarationWithComments](hydra.haskell.coder.encodeTerm(0)(namespaces)(`term_`)(cx)(g))((hterm: hydra.haskell.syntax.Expression) =>
      {
      lazy val vb: hydra.haskell.syntax.ValueBinding = hydra.haskell.utils.simpleValueBinding(`hname_`)(hterm)(bindings)
      lazy val schemeConstraints: Option[Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = hydra.lib.maybes.maybe[Option[Map[hydra.core.Name,
         hydra.core.TypeVariableMetadata]], hydra.core.TypeScheme](None)((ts: hydra.core.TypeScheme) => (ts.constraints))(typ)
      lazy val schemeClasses: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]] = hydra.haskell.coder.typeSchemeConstraintsToClassMap(schemeConstraints)
      hydra.lib.eithers.bind[hydra.errors.Error, Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]],
         hydra.haskell.syntax.DeclarationWithComments](hydra.annotations.getTypeClasses(cx)(g)(hydra.strip.removeTypesFromTerm(term)))((explicitClasses: Map[hydra.core.Name,
         scala.collection.immutable.Set[hydra.classes.TypeClass]]) =>
        {
        lazy val combinedClasses: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.classes.TypeClass]] = hydra.lib.maps.union[hydra.core.Name,
           scala.collection.immutable.Set[hydra.classes.TypeClass]](schemeClasses)(explicitClasses)
        {
          lazy val schemeType: hydra.core.Type = hydra.lib.maybes.maybe[hydra.core.Type,
             hydra.core.TypeScheme](hydra.core.Type.unit)((ts: hydra.core.TypeScheme) => (ts.`type`))(typ)
          hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Type, hydra.haskell.syntax.DeclarationWithComments](hydra.haskell.coder.encodeTypeWithClassAssertions(namespaces)(combinedClasses)(schemeType)(cx)(g))((htype: hydra.haskell.syntax.Type) =>
            {
            lazy val decl: hydra.haskell.syntax.Declaration = hydra.haskell.syntax.Declaration.typedBinding(hydra.haskell.syntax.TypedBinding(hydra.haskell.syntax.TypeSignature(`hname_`,
               htype), rewriteValueBinding(vb)))
            Right(hydra.haskell.syntax.DeclarationWithComments(decl, comments))
          })
        }
      })
    })
  hydra.lib.eithers.bind[hydra.errors.Error, Option[scala.Predef.String], hydra.haskell.syntax.DeclarationWithComments](hydra.annotations.getTermDescription(cx)(g)(term))((comments: Option[scala.Predef.String]) => toDecl(comments)(hname)(term)(None))
}

def toTypeDeclarationsFrom(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(elementName: hydra.core.Name)(typ: hydra.core.Type)(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   Seq[hydra.haskell.syntax.DeclarationWithComments]] =
  {
  lazy val lname: scala.Predef.String = hydra.names.localNameOf(elementName)
  lazy val hname: hydra.haskell.syntax.Name = hydra.haskell.utils.simpleName(lname)
  def declHead(name: hydra.haskell.syntax.Name)(`vars_`: Seq[hydra.core.Name]): hydra.haskell.syntax.DeclarationHead =
    hydra.lib.logic.ifElse[hydra.haskell.syntax.DeclarationHead](hydra.lib.lists.`null`[hydra.core.Name](`vars_`))(hydra.haskell.syntax.DeclarationHead.simple(name))({
    lazy val h: hydra.core.Name = hydra.lib.lists.head[hydra.core.Name](`vars_`)
    lazy val rest: Seq[hydra.core.Name] = hydra.lib.lists.tail[hydra.core.Name](`vars_`)
    lazy val hvar: hydra.haskell.syntax.Variable = hydra.haskell.utils.simpleName(h)
    hydra.haskell.syntax.DeclarationHead.application(hydra.haskell.syntax.ApplicationDeclarationHead(declHead(name)(rest),
       hvar))
  })
  def newtypeCons(tname: hydra.core.Name)(`typ_`: hydra.core.Type): Either[hydra.errors.Error,
     hydra.haskell.syntax.ConstructorWithComments] =
    {
    lazy val hname0: hydra.haskell.syntax.Name = hydra.haskell.utils.simpleName(hydra.haskell.utils.newtypeAccessorName(tname))
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Type, hydra.haskell.syntax.ConstructorWithComments](hydra.haskell.coder.adaptTypeToHaskellAndEncode(namespaces)(`typ_`)(cx)(g))((htype: hydra.haskell.syntax.Type) =>
      {
      lazy val hfield: hydra.haskell.syntax.FieldWithComments = hydra.haskell.syntax.FieldWithComments(hydra.haskell.syntax.Field(hname0,
         htype), None)
      lazy val constructorName: hydra.haskell.syntax.Name = hydra.haskell.utils.simpleName(hydra.names.localNameOf(tname))
      Right(hydra.haskell.syntax.ConstructorWithComments(hydra.haskell.syntax.Constructor.record(hydra.haskell.syntax.RecordConstructor(constructorName,
         Seq(hfield))), None))
    })
  }
  def recordCons(`lname_`: scala.Predef.String)(fields: Seq[hydra.core.FieldType]): Either[hydra.errors.Error,
     hydra.haskell.syntax.ConstructorWithComments] =
    {
    def toField(fieldType: hydra.core.FieldType): Either[hydra.errors.Error, hydra.haskell.syntax.FieldWithComments] =
      {
      lazy val fname: hydra.core.Name = (fieldType.name)
      lazy val ftype: hydra.core.Type = (fieldType.`type`)
      lazy val `hname_`: hydra.haskell.syntax.Name = hydra.haskell.utils.simpleName(hydra.lib.strings.cat2(hydra.formatting.decapitalize(`lname_`))(hydra.formatting.capitalize(fname)))
      hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Type, hydra.haskell.syntax.FieldWithComments](hydra.haskell.coder.adaptTypeToHaskellAndEncode(namespaces)(ftype)(cx)(g))((htype: hydra.haskell.syntax.Type) =>
        hydra.lib.eithers.bind[hydra.errors.Error, Option[scala.Predef.String], hydra.haskell.syntax.FieldWithComments](hydra.annotations.getTypeDescription(cx)(g)(ftype))((comments: Option[scala.Predef.String]) =>
        Right(hydra.haskell.syntax.FieldWithComments(hydra.haskell.syntax.Field(`hname_`, htype), comments))))
    }
    hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.haskell.syntax.FieldWithComments],
       hydra.haskell.syntax.ConstructorWithComments](hydra.lib.eithers.mapList[hydra.core.FieldType,
       hydra.haskell.syntax.FieldWithComments, hydra.errors.Error](toField)(fields))((hFields: Seq[hydra.haskell.syntax.FieldWithComments]) =>
      Right(hydra.haskell.syntax.ConstructorWithComments(hydra.haskell.syntax.Constructor.record(hydra.haskell.syntax.RecordConstructor(hydra.haskell.utils.simpleName(`lname_`),
         hFields)), None)))
  }
  def unionCons(`boundNames_`: scala.collection.immutable.Set[hydra.core.Name])(`lname_`: scala.Predef.String)(fieldType: hydra.core.FieldType): Either[hydra.errors.Error,
     hydra.haskell.syntax.ConstructorWithComments] =
    {
    lazy val fname: hydra.core.Name = (fieldType.name)
    lazy val ftype: hydra.core.Type = (fieldType.`type`)
    def deconflict(name: scala.Predef.String): scala.Predef.String =
      {
      lazy val tname: hydra.core.Name = hydra.names.unqualifyName(hydra.packaging.QualifiedName(Some(hydra.lib.pairs.first[hydra.packaging.Namespace,
         hydra.haskell.syntax.ModuleName](namespaces.focus)), name))
      hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.sets.member[hydra.core.Name](tname)(`boundNames_`))(deconflict(hydra.lib.strings.cat2(name)("_")))(name)
    }
    hydra.lib.eithers.bind[hydra.errors.Error, Option[scala.Predef.String], hydra.haskell.syntax.ConstructorWithComments](hydra.annotations.getTypeDescription(cx)(g)(ftype))((comments: Option[scala.Predef.String]) =>
      {
      lazy val nm: scala.Predef.String = deconflict(hydra.lib.strings.cat2(hydra.formatting.capitalize(`lname_`))(hydra.formatting.capitalize(fname)))
      hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.haskell.syntax.Type], hydra.haskell.syntax.ConstructorWithComments](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
         Seq[hydra.haskell.syntax.Type]]](hydra.lib.equality.equal[hydra.core.Type](hydra.strip.deannotateType(ftype))(hydra.core.Type.unit))(Right(Seq()))(hydra.lib.eithers.bind[hydra.errors.Error,
         hydra.haskell.syntax.Type, Seq[hydra.haskell.syntax.Type]](hydra.haskell.coder.adaptTypeToHaskellAndEncode(namespaces)(ftype)(cx)(g))((htype: hydra.haskell.syntax.Type) => Right(Seq(htype)))))((typeList: Seq[hydra.haskell.syntax.Type]) =>
        Right(hydra.haskell.syntax.ConstructorWithComments(hydra.haskell.syntax.Constructor.ordinary(hydra.haskell.syntax.OrdinaryConstructor(hydra.haskell.utils.simpleName(nm),
           typeList)), comments)))
    })
  }
  hydra.lib.eithers.bind[hydra.errors.Error, Boolean, Seq[hydra.haskell.syntax.DeclarationWithComments]](hydra.predicates.isSerializableByName(cx)(g)(elementName))((isSer: Boolean) =>
    {
    lazy val deriv: hydra.haskell.syntax.Deriving = hydra.lib.logic.ifElse[Seq[hydra.haskell.syntax.Name]](isSer)(hydra.lib.lists.map[scala.Predef.String,
       hydra.haskell.syntax.Name](hydra.haskell.utils.rawName)(Seq("Eq", "Ord", "Read",
       "Show")))(Seq())
    lazy val unpackResult: Tuple2[Seq[hydra.core.Name], hydra.core.Type] = hydra.haskell.utils.unpackForallType(typ)
    lazy val vars: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name], hydra.core.Type](unpackResult)
    lazy val `t_`: hydra.core.Type = hydra.lib.pairs.second[Seq[hydra.core.Name], hydra.core.Type](unpackResult)
    lazy val hd: hydra.haskell.syntax.DeclarationHead = declHead(hname)(hydra.lib.lists.reverse[hydra.core.Name](vars))
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Declaration, Seq[hydra.haskell.syntax.DeclarationWithComments]](hydra.strip.deannotateType(`t_`) match
      case hydra.core.Type.record(v_Type_record_rt) => hydra.lib.eithers.bind[hydra.errors.Error,
         hydra.haskell.syntax.ConstructorWithComments, hydra.haskell.syntax.Declaration](recordCons(lname)(v_Type_record_rt))((cons: hydra.haskell.syntax.ConstructorWithComments) =>
        Right(hydra.haskell.syntax.Declaration.data(hydra.haskell.syntax.DataDeclaration(hydra.haskell.syntax.DataOrNewtype.data,
           Seq(), hd, Seq(cons), Seq(deriv)))))
      case hydra.core.Type.union(v_Type_union_rt) => hydra.lib.eithers.bind[hydra.errors.Error,
         Seq[hydra.haskell.syntax.ConstructorWithComments], hydra.haskell.syntax.Declaration](hydra.lib.eithers.mapList[hydra.core.FieldType,
         hydra.haskell.syntax.ConstructorWithComments, hydra.errors.Error]((v1: hydra.core.FieldType) =>
        unionCons(hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name,
           hydra.core.Term](g.boundTerms)))(lname)(v1))(v_Type_union_rt))((cons: Seq[hydra.haskell.syntax.ConstructorWithComments]) =>
        Right(hydra.haskell.syntax.Declaration.data(hydra.haskell.syntax.DataDeclaration(hydra.haskell.syntax.DataOrNewtype.data,
           Seq(), hd, cons, Seq(deriv)))))
      case hydra.core.Type.wrap(v_Type_wrap_wrapped) => hydra.lib.eithers.bind[hydra.errors.Error,
         hydra.haskell.syntax.ConstructorWithComments, hydra.haskell.syntax.Declaration](newtypeCons(elementName)(v_Type_wrap_wrapped))((cons: hydra.haskell.syntax.ConstructorWithComments) =>
        Right(hydra.haskell.syntax.Declaration.data(hydra.haskell.syntax.DataDeclaration(hydra.haskell.syntax.DataOrNewtype.newtype,
           Seq(), hd, Seq(cons), Seq(deriv)))))
      case _ => hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Type,
         hydra.haskell.syntax.Declaration](hydra.haskell.coder.adaptTypeToHaskellAndEncode(namespaces)(typ)(cx)(g))((htype: hydra.haskell.syntax.Type) =>
        Right(hydra.haskell.syntax.Declaration.`type`(hydra.haskell.syntax.TypeDeclaration(hd,
           htype)))))((decl: hydra.haskell.syntax.Declaration) =>
      hydra.lib.eithers.bind[hydra.errors.Error, Option[scala.Predef.String], Seq[hydra.haskell.syntax.DeclarationWithComments]](hydra.annotations.getTypeDescription(cx)(g)(typ))((comments: Option[scala.Predef.String]) =>
      hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.haskell.syntax.DeclarationWithComments],
         Seq[hydra.haskell.syntax.DeclarationWithComments]](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
         Seq[hydra.haskell.syntax.DeclarationWithComments]]](hydra.haskell.coder.includeTypeDefinitions)(hydra.lib.eithers.bind[hydra.errors.Error,
         hydra.haskell.syntax.DeclarationWithComments, Seq[hydra.haskell.syntax.DeclarationWithComments]](hydra.haskell.coder.typeDecl(namespaces)(elementName)(typ)(cx)(g))((`decl_`: hydra.haskell.syntax.DeclarationWithComments) => Right(Seq(`decl_`))))(Right(Seq())))((tdecls: Seq[hydra.haskell.syntax.DeclarationWithComments]) =>
      {
      lazy val mainDecl: hydra.haskell.syntax.DeclarationWithComments = hydra.haskell.syntax.DeclarationWithComments(decl,
         comments)
      lazy val `nameDecls_`: Seq[hydra.haskell.syntax.DeclarationWithComments] = hydra.haskell.coder.nameDecls(namespaces)(elementName)(typ)
      Right(hydra.lib.lists.concat[hydra.haskell.syntax.DeclarationWithComments](Seq(Seq(mainDecl),
         `nameDecls_`, tdecls)))
    })))
  })
}

def typeDecl[T0](namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName])(name: hydra.core.Name)(typ: hydra.core.Type)(cx: T0)(g: hydra.graph.Graph): Either[hydra.errors.Error,
   hydra.haskell.syntax.DeclarationWithComments] =
  {
  def typeName(ns: hydra.packaging.Namespace)(`name_`: hydra.core.Name): hydra.core.Name = hydra.names.qname(ns)(typeNameLocal(`name_`))
  def typeNameLocal(`name_`: hydra.core.Name): scala.Predef.String = hydra.lib.strings.cat(Seq("_",
     hydra.names.localNameOf(`name_`), "_type_"))
  lazy val rawTerm: hydra.core.Term = hydra.encode.core.`type`(typ)
  def rewrite(recurse: (hydra.core.Term => hydra.core.Term))(term: hydra.core.Term): hydra.core.Term =
    {
    lazy val variantResult: Option[hydra.core.Field] = hydra.strip.deannotateTerm(term) match
      case hydra.core.Term.inject(v_Term_inject_inj) => hydra.lib.logic.ifElse[Option[hydra.core.Field]](hydra.lib.equality.equal[hydra.core.Name](v_Term_inject_inj.typeName)("hydra.core.Type"))(Some(v_Term_inject_inj.field))(None)
      case _ => None
    def decodeString(term2: hydra.core.Term): Option[scala.Predef.String] =
      hydra.strip.deannotateTerm(term2) match
      case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
        case hydra.core.Literal.string(v_Literal_string_s) => Some(v_Literal_string_s)
        case _ => None
      case _ => None
    def decodeName(term2: hydra.core.Term): Option[hydra.core.Name] =
      hydra.strip.deannotateTerm(term2) match
      case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.lib.logic.ifElse[Option[hydra.core.Name]](hydra.lib.equality.equal[hydra.core.Name](v_Term_wrap_wt.typeName)("hydra.core.Name"))(hydra.lib.maybes.map[scala.Predef.String,
         hydra.core.Name]((x: scala.Predef.String) => x)(decodeString(v_Term_wrap_wt.body)))(None)
      case _ => None
    def forType(field: hydra.core.Field): Option[hydra.core.Term] =
      {
      lazy val fname: hydra.core.Name = (field.name)
      lazy val fterm: hydra.core.Term = (field.term)
      hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](fname)("record"))(None)(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](fname)("variable"))(hydra.lib.maybes.bind[hydra.core.Name,
         hydra.core.Term](decodeName(fterm))(forVariableType))(None))
    }
    def forVariableType(vname: hydra.core.Name): Option[hydra.core.Term] =
      {
      lazy val qname: hydra.packaging.QualifiedName = hydra.names.qualifyName(vname)
      lazy val mns: Option[hydra.packaging.Namespace] = (qname.namespace)
      lazy val local: scala.Predef.String = (qname.local)
      hydra.lib.maybes.map[hydra.packaging.Namespace, hydra.core.Term]((ns: hydra.packaging.Namespace) =>
        hydra.core.Term.variable(hydra.names.qname(ns)(hydra.lib.strings.cat(Seq("_", local, "_type_")))))(mns)
    }
    hydra.lib.maybes.fromMaybe[hydra.core.Term](recurse(term))(hydra.lib.maybes.bind[hydra.core.Field,
       hydra.core.Term](variantResult)(forType))
  }
  lazy val finalTerm: hydra.core.Term = hydra.rewriting.rewriteTerm(rewrite)(rawTerm)
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.haskell.syntax.Expression, hydra.haskell.syntax.DeclarationWithComments](hydra.haskell.coder.encodeTerm(0)(namespaces)(finalTerm)(cx)(g))((expr: hydra.haskell.syntax.Expression) =>
    {
    lazy val rhs: hydra.haskell.syntax.RightHandSide = expr
    lazy val hname: hydra.haskell.syntax.Name = hydra.haskell.utils.simpleName(typeNameLocal(name))
    lazy val pat: hydra.haskell.syntax.Pattern = hydra.haskell.utils.applicationPattern(hname)(Seq())
    lazy val decl: hydra.haskell.syntax.Declaration = hydra.haskell.syntax.Declaration.valueBinding(hydra.haskell.syntax.ValueBinding.simple(hydra.haskell.syntax.SimpleValueBinding(pat,
       rhs, None)))
    Right(hydra.haskell.syntax.DeclarationWithComments(decl, None))
  })
}

def typeSchemeConstraintsToClassMap[T0](maybeConstraints: Option[Map[T0, hydra.core.TypeVariableMetadata]]): Map[T0,
   scala.collection.immutable.Set[hydra.classes.TypeClass]] =
  {
  def nameToTypeClass(className: hydra.core.Name): Option[hydra.classes.TypeClass] =
    {
    lazy val classNameStr: scala.Predef.String = className
    lazy val isEq: Boolean = hydra.lib.equality.equal[scala.Predef.String](classNameStr)("equality")
    lazy val isOrd: Boolean = hydra.lib.equality.equal[scala.Predef.String](classNameStr)("ordering")
    hydra.lib.logic.ifElse[Option[hydra.classes.TypeClass]](isEq)(Some(hydra.classes.TypeClass.equality))(hydra.lib.logic.ifElse[Option[hydra.classes.TypeClass]](isOrd)(Some(hydra.classes.TypeClass.ordering))(None))
  }
  hydra.lib.maybes.maybe[Map[T0, scala.collection.immutable.Set[hydra.classes.TypeClass]],
     Map[T0, hydra.core.TypeVariableMetadata]](hydra.lib.maps.empty[T0, scala.collection.immutable.Set[hydra.classes.TypeClass]])((constraints: Map[T0,
     hydra.core.TypeVariableMetadata]) =>
    hydra.lib.maps.map[hydra.core.TypeVariableMetadata, scala.collection.immutable.Set[hydra.classes.TypeClass],
       T0]((meta: hydra.core.TypeVariableMetadata) =>
    hydra.lib.sets.fromList[hydra.classes.TypeClass](hydra.lib.maybes.cat[hydra.classes.TypeClass](hydra.lib.lists.map[hydra.core.Name,
       Option[hydra.classes.TypeClass]](nameToTypeClass)(hydra.lib.sets.toList[hydra.core.Name](meta.classes)))))(constraints))(maybeConstraints)
}

lazy val useCoreImport: Boolean = true
