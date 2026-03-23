package hydra.adapt

import hydra.coders.*

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.graph.*

import hydra.module.*

import hydra.util.*

import hydra.lib.eithers

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.literals

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

def adaptFloatType(constraints: hydra.coders.LanguageConstraints)(ft: hydra.core.FloatType): Option[hydra.core.FloatType] =
  {
  lazy val supported: Boolean = hydra.lib.sets.member[hydra.core.FloatType](ft)(constraints.floatTypes)
  def alt(v1: hydra.core.FloatType): Option[hydra.core.FloatType] = hydra.adapt.adaptFloatType(constraints)(v1)
  def forUnsupported(ft2: hydra.core.FloatType): Option[hydra.core.FloatType] =
    ft2 match
    case hydra.core.FloatType.bigfloat => alt(hydra.core.FloatType.float64)
    case hydra.core.FloatType.float32 => alt(hydra.core.FloatType.float64)
    case hydra.core.FloatType.float64 => alt(hydra.core.FloatType.bigfloat)
  hydra.lib.logic.ifElse[Option[hydra.core.FloatType]](supported)(Some(ft))(forUnsupported(ft))
}

def adaptDataGraph(constraints: hydra.coders.LanguageConstraints)(doExpand: Boolean)(els0: Seq[hydra.core.Binding])(cx: hydra.context.Context)(graph0: hydra.graph.Graph): Either[scala.Predef.String, Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]]] =
  {
  def transform(g: hydra.graph.Graph)(gterm: hydra.core.Term): hydra.core.Term =
    {
    lazy val tx: hydra.graph.Graph = g
    lazy val gterm1: hydra.core.Term = hydra.rewriting.unshadowVariables(hydra.adapt.pushTypeAppsInward(gterm))
    lazy val gterm2: hydra.core.Term = hydra.rewriting.unshadowVariables(hydra.lib.logic.ifElse[hydra.core.Term](doExpand)(hydra.adapt.pushTypeAppsInward(hydra.reduction.etaExpandTermNew(tx)(gterm1)))(gterm1))
    hydra.rewriting.liftLambdaAboveLet(gterm2)
  }
  lazy val litmap: Map[hydra.core.LiteralType, hydra.core.LiteralType] = hydra.adapt.adaptLiteralTypesMap(constraints)
  lazy val prims0: Map[hydra.core.Name, hydra.graph.Primitive] = (graph0.primitives)
  lazy val schemaTypes0: Map[hydra.core.Name, hydra.core.TypeScheme] = (graph0.schemaTypes)
  lazy val schemaBindings: Seq[hydra.core.Binding] = hydra.schemas.typesToElements(hydra.lib.maps.map[hydra.core.TypeScheme, hydra.core.Type, hydra.core.Name]((ts: hydra.core.TypeScheme) => hydra.rewriting.typeSchemeToFType(ts))(schemaTypes0))
  hydra.lib.eithers.bind[scala.Predef.String, Map[hydra.core.Name, hydra.core.TypeScheme], Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]]](hydra.lib.logic.ifElse[Either[scala.Predef.String, Map[hydra.core.Name, hydra.core.TypeScheme]]](hydra.lib.maps.`null`[hydra.core.Name, hydra.core.TypeScheme](schemaTypes0))(Right(hydra.lib.maps.empty[hydra.core.Name, hydra.core.TypeScheme]))(hydra.lib.eithers.bind[scala.Predef.String, Map[hydra.core.Name, hydra.core.Type], Map[hydra.core.Name, hydra.core.TypeScheme]](hydra.lib.eithers.bimap[hydra.context.InContext[hydra.errors.DecodingError], Map[hydra.core.Name, hydra.core.Type], scala.Predef.String, Map[hydra.core.Name, hydra.core.Type]]((ic: hydra.context.InContext[hydra.errors.DecodingError]) => (ic.`object`))((x: Map[hydra.core.Name, hydra.core.Type]) => x)(hydra.schemas.graphAsTypes(cx)(graph0)(schemaBindings)))((tmap0: Map[hydra.core.Name, hydra.core.Type]) =>
    hydra.lib.eithers.bind[scala.Predef.String, Map[hydra.core.Name, hydra.core.Type], Map[hydra.core.Name, hydra.core.TypeScheme]](hydra.adapt.adaptGraphSchema(constraints)(litmap)(tmap0))((tmap1: Map[hydra.core.Name, hydra.core.Type]) =>
    Right(hydra.lib.maps.map[hydra.core.Type, hydra.core.TypeScheme, hydra.core.Name]((t: hydra.core.Type) => hydra.schemas.typeToTypeScheme(t))(tmap1))))))((schemaResult: Map[hydra.core.Name, hydra.core.TypeScheme]) =>
    {
    lazy val adaptedSchemaTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = schemaResult
    {
      lazy val gterm0: hydra.core.Term = hydra.core.Term.let(hydra.core.Let(els0, hydra.core.Term.unit))
      {
        lazy val gterm1: hydra.core.Term = hydra.lib.logic.ifElse[hydra.core.Term](doExpand)(transform(graph0)(gterm0))(gterm0)
        hydra.lib.eithers.bind[scala.Predef.String, hydra.core.Term, Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]]](hydra.adapt.adaptTerm(constraints)(litmap)(cx)(graph0)(gterm1))((gterm2: hydra.core.Term) =>
          hydra.lib.eithers.bind[scala.Predef.String, hydra.core.Term, Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]]](hydra.rewriting.rewriteTermM((v1: (hydra.core.Term => Either[scala.Predef.String, hydra.core.Term])) =>
          (v2: hydra.core.Term) => hydra.adapt.adaptLambdaDomains(constraints)(litmap)(v1)(v2))(gterm2))((gterm3: hydra.core.Term) =>
          {
          lazy val els1Raw: Seq[hydra.core.Binding] = hydra.schemas.termAsBindings(gterm3)
          {
            def processBinding(el: hydra.core.Binding): Either[scala.Predef.String, hydra.core.Binding] =
              hydra.lib.eithers.bind[scala.Predef.String, hydra.core.Term, hydra.core.Binding](hydra.rewriting.rewriteTermM((v1: (hydra.core.Term => Either[scala.Predef.String, hydra.core.Term])) =>
              (v2: hydra.core.Term) => hydra.adapt.adaptNestedTypes(constraints)(litmap)(v1)(v2))(el.term))((newTerm: hydra.core.Term) =>
              hydra.lib.eithers.bind[scala.Predef.String, Option[hydra.core.TypeScheme], hydra.core.Binding](hydra.lib.maybes.maybe[Either[scala.Predef.String, Option[hydra.core.TypeScheme]], hydra.core.TypeScheme](Right(None))((ts: hydra.core.TypeScheme) =>
              hydra.lib.eithers.bind[scala.Predef.String, hydra.core.TypeScheme, Option[hydra.core.TypeScheme]](hydra.adapt.adaptTypeScheme(constraints)(litmap)(ts))((ts1: hydra.core.TypeScheme) => Right(Some(ts1))))(el.`type`))((adaptedType: Option[hydra.core.TypeScheme]) => Right(hydra.core.Binding(el.name, newTerm, adaptedType))))
            hydra.lib.eithers.bind[scala.Predef.String, Seq[hydra.core.Binding], Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]]](hydra.lib.eithers.mapList[hydra.core.Binding, hydra.core.Binding, scala.Predef.String](processBinding)(els1Raw))((els1: Seq[hydra.core.Binding]) =>
              hydra.lib.eithers.bind[scala.Predef.String, Seq[Tuple2[hydra.core.Name, hydra.graph.Primitive]], Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]]](hydra.lib.eithers.mapList[Tuple2[hydra.core.Name, hydra.graph.Primitive], Tuple2[hydra.core.Name, hydra.graph.Primitive], scala.Predef.String]((kv: Tuple2[hydra.core.Name, hydra.graph.Primitive]) =>
              hydra.lib.eithers.bind[scala.Predef.String, hydra.graph.Primitive, Tuple2[hydra.core.Name, hydra.graph.Primitive]](hydra.adapt.adaptPrimitive(constraints)(litmap)(hydra.lib.pairs.second[hydra.core.Name, hydra.graph.Primitive](kv)))((prim1: hydra.graph.Primitive) =>
              Right(Tuple2(hydra.lib.pairs.first[hydra.core.Name, hydra.graph.Primitive](kv), prim1))))(hydra.lib.maps.toList[hydra.core.Name, hydra.graph.Primitive](prims0)))((primPairs: Seq[Tuple2[hydra.core.Name, hydra.graph.Primitive]]) =>
              {
              lazy val prims1: Map[hydra.core.Name, hydra.graph.Primitive] = hydra.lib.maps.fromList[hydra.core.Name, hydra.graph.Primitive](primPairs)
              {
                lazy val adaptedGraph: hydra.graph.Graph = hydra.graph.Graph(hydra.lexical.buildGraph(els1)(hydra.lib.maps.empty[hydra.core.Name, Option[hydra.core.Term]])(prims1).boundTerms, (hydra.lexical.buildGraph(els1)(hydra.lib.maps.empty[hydra.core.Name, Option[hydra.core.Term]])(prims1).boundTypes), (hydra.lexical.buildGraph(els1)(hydra.lib.maps.empty[hydra.core.Name, Option[hydra.core.Term]])(prims1).classConstraints), (hydra.lexical.buildGraph(els1)(hydra.lib.maps.empty[hydra.core.Name, Option[hydra.core.Term]])(prims1).lambdaVariables), (hydra.lexical.buildGraph(els1)(hydra.lib.maps.empty[hydra.core.Name, Option[hydra.core.Term]])(prims1).metadata), (hydra.lexical.buildGraph(els1)(hydra.lib.maps.empty[hydra.core.Name, Option[hydra.core.Term]])(prims1).primitives), adaptedSchemaTypes, (hydra.lexical.buildGraph(els1)(hydra.lib.maps.empty[hydra.core.Name, Option[hydra.core.Term]])(prims1).typeVariables))
                Right(Tuple2(adaptedGraph, els1))
              }
            }))
          }
        }))
      }
    }
  })
}

def adaptGraphSchema[T0](constraints: hydra.coders.LanguageConstraints)(litmap: Map[hydra.core.LiteralType, hydra.core.LiteralType])(types0: Map[T0, hydra.core.Type]): Either[scala.Predef.String, Map[T0, hydra.core.Type]] =
  {
  def mapPair[T1](pair: Tuple2[T1, hydra.core.Type]): Either[scala.Predef.String, Tuple2[T1, hydra.core.Type]] =
    {
    lazy val name: T1 = hydra.lib.pairs.first[T1, hydra.core.Type](pair)
    lazy val typ: hydra.core.Type = hydra.lib.pairs.second[T1, hydra.core.Type](pair)
    hydra.lib.eithers.bind[scala.Predef.String, hydra.core.Type, Tuple2[T1, hydra.core.Type]](hydra.adapt.adaptType(constraints)(litmap)(typ))((typ1: hydra.core.Type) => Right(Tuple2(name, typ1)))
  }
  hydra.lib.eithers.bind[scala.Predef.String, Seq[Tuple2[T0, hydra.core.Type]], Map[T0, hydra.core.Type]](hydra.lib.eithers.mapList[Tuple2[T0, hydra.core.Type], Tuple2[T0, hydra.core.Type], scala.Predef.String](mapPair)(hydra.lib.maps.toList[T0, hydra.core.Type](types0)))((pairs: Seq[Tuple2[T0, hydra.core.Type]]) => Right(hydra.lib.maps.fromList[T0, hydra.core.Type](pairs)))
}

def adaptIntegerType(constraints: hydra.coders.LanguageConstraints)(it: hydra.core.IntegerType): Option[hydra.core.IntegerType] =
  {
  lazy val supported: Boolean = hydra.lib.sets.member[hydra.core.IntegerType](it)(constraints.integerTypes)
  def alt(v1: hydra.core.IntegerType): Option[hydra.core.IntegerType] = hydra.adapt.adaptIntegerType(constraints)(v1)
  def forUnsupported(it2: hydra.core.IntegerType): Option[hydra.core.IntegerType] =
    it2 match
    case hydra.core.IntegerType.bigint => None
    case hydra.core.IntegerType.int8 => alt(hydra.core.IntegerType.uint16)
    case hydra.core.IntegerType.int16 => alt(hydra.core.IntegerType.uint32)
    case hydra.core.IntegerType.int32 => alt(hydra.core.IntegerType.uint64)
    case hydra.core.IntegerType.int64 => alt(hydra.core.IntegerType.bigint)
    case hydra.core.IntegerType.uint8 => alt(hydra.core.IntegerType.int16)
    case hydra.core.IntegerType.uint16 => alt(hydra.core.IntegerType.int32)
    case hydra.core.IntegerType.uint32 => alt(hydra.core.IntegerType.int64)
    case hydra.core.IntegerType.uint64 => alt(hydra.core.IntegerType.bigint)
  hydra.lib.logic.ifElse[Option[hydra.core.IntegerType]](supported)(Some(it))(forUnsupported(it))
}

def adaptLambdaDomains[T0](constraints: hydra.coders.LanguageConstraints)(litmap: Map[hydra.core.LiteralType, hydra.core.LiteralType])(recurse: (T0 => Either[scala.Predef.String, hydra.core.Term]))(term: T0): Either[scala.Predef.String, hydra.core.Term] =
  hydra.lib.eithers.bind[scala.Predef.String, hydra.core.Term, hydra.core.Term](recurse(term))((rewritten: hydra.core.Term) =>
  rewritten match
  case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
    case hydra.core.Function.lambda(v_Function_lambda_l) => hydra.lib.eithers.bind[scala.Predef.String, Option[hydra.core.Type], hydra.core.Term](hydra.lib.maybes.maybe[Either[scala.Predef.String, Option[hydra.core.Type]], hydra.core.Type](Right(None))((dom: hydra.core.Type) =>
      hydra.lib.eithers.bind[scala.Predef.String, hydra.core.Type, Option[hydra.core.Type]](hydra.adapt.adaptType(constraints)(litmap)(dom))((dom1: hydra.core.Type) => Right(Some(dom1))))(v_Function_lambda_l.domain))((adaptedDomain: Option[hydra.core.Type]) =>
      Right(hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(v_Function_lambda_l.parameter, adaptedDomain, (v_Function_lambda_l.body))))))
    case _ => Right(hydra.core.Term.function(v_Term_function_f))
  case _ => Right(rewritten))

def adaptLiteral(lt: hydra.core.LiteralType)(l: hydra.core.Literal): hydra.core.Literal =
  l match
  case hydra.core.Literal.binary(v_Literal_binary_b) => lt match
    case hydra.core.LiteralType.string => hydra.core.Literal.string(hydra.lib.literals.binaryToString(v_Literal_binary_b))
  case hydra.core.Literal.boolean(v_Literal_boolean_b) => lt match
    case hydra.core.LiteralType.integer(v_LiteralType_integer_it) => hydra.core.Literal.integer(hydra.literals.bigintToIntegerValue(v_LiteralType_integer_it)(hydra.lib.logic.ifElse[BigInt](v_Literal_boolean_b)(BigInt(1L))(BigInt(0L))))
  case hydra.core.Literal.float(v_Literal_float_f) => lt match
    case hydra.core.LiteralType.float(v_LiteralType_float_ft) => hydra.core.Literal.float(hydra.literals.bigfloatToFloatValue(v_LiteralType_float_ft)(hydra.literals.floatValueToBigfloat(v_Literal_float_f)))
  case hydra.core.Literal.integer(v_Literal_integer_i) => lt match
    case hydra.core.LiteralType.integer(v_LiteralType_integer_it) => hydra.core.Literal.integer(hydra.literals.bigintToIntegerValue(v_LiteralType_integer_it)(hydra.literals.integerValueToBigint(v_Literal_integer_i)))

def adaptLiteralType(constraints: hydra.coders.LanguageConstraints)(lt: hydra.core.LiteralType): Option[hydra.core.LiteralType] =
  {
  def forUnsupported(lt2: hydra.core.LiteralType): Option[hydra.core.LiteralType] =
    lt2 match
    case hydra.core.LiteralType.binary => Some(hydra.core.LiteralType.string)
    case hydra.core.LiteralType.boolean => hydra.lib.maybes.map[hydra.core.IntegerType, hydra.core.LiteralType]((x: hydra.core.IntegerType) => hydra.core.LiteralType.integer(x))(hydra.adapt.adaptIntegerType(constraints)(hydra.core.IntegerType.int8))
    case hydra.core.LiteralType.float(v_LiteralType_float_ft) => hydra.lib.maybes.map[hydra.core.FloatType, hydra.core.LiteralType]((x: hydra.core.FloatType) => hydra.core.LiteralType.float(x))(hydra.adapt.adaptFloatType(constraints)(v_LiteralType_float_ft))
    case hydra.core.LiteralType.integer(v_LiteralType_integer_it) => hydra.lib.maybes.map[hydra.core.IntegerType, hydra.core.LiteralType]((x: hydra.core.IntegerType) => hydra.core.LiteralType.integer(x))(hydra.adapt.adaptIntegerType(constraints)(v_LiteralType_integer_it))
    case _ => None
  hydra.lib.logic.ifElse[Option[hydra.core.LiteralType]](hydra.adapt.literalTypeSupported(constraints)(lt))(None)(forUnsupported(lt))
}

def adaptLiteralTypesMap(constraints: hydra.coders.LanguageConstraints): Map[hydra.core.LiteralType, hydra.core.LiteralType] =
  {
  def tryType(lt: hydra.core.LiteralType): Option[Tuple2[hydra.core.LiteralType, hydra.core.LiteralType]] =
    hydra.lib.maybes.maybe[Option[Tuple2[hydra.core.LiteralType, hydra.core.LiteralType]], hydra.core.LiteralType](None)((lt2: hydra.core.LiteralType) => Some(Tuple2(lt, lt2)))(hydra.adapt.adaptLiteralType(constraints)(lt))
  hydra.lib.maps.fromList[hydra.core.LiteralType, hydra.core.LiteralType](hydra.lib.maybes.cat[Tuple2[hydra.core.LiteralType, hydra.core.LiteralType]](hydra.lib.lists.map[hydra.core.LiteralType, Option[Tuple2[hydra.core.LiteralType, hydra.core.LiteralType]]](tryType)(hydra.reflect.literalTypes)))
}

def adaptLiteralValue[T0](litmap: Map[T0, hydra.core.LiteralType])(lt: T0)(l: hydra.core.Literal): hydra.core.Literal =
  hydra.lib.maybes.maybe[hydra.core.Literal, hydra.core.LiteralType](hydra.core.Literal.string(hydra.show.core.literal(l)))((lt2: hydra.core.LiteralType) => hydra.adapt.adaptLiteral(lt2)(l))(hydra.lib.maps.lookup[T0, hydra.core.LiteralType](lt)(litmap))

def adaptNestedTypes[T0](constraints: hydra.coders.LanguageConstraints)(litmap: Map[hydra.core.LiteralType, hydra.core.LiteralType])(recurse: (T0 => Either[scala.Predef.String, hydra.core.Term]))(term: T0): Either[scala.Predef.String, hydra.core.Term] =
  hydra.lib.eithers.bind[scala.Predef.String, hydra.core.Term, hydra.core.Term](recurse(term))((rewritten: hydra.core.Term) =>
  rewritten match
  case hydra.core.Term.let(v_Term_let_lt) => {
    def adaptB(b: hydra.core.Binding): Either[scala.Predef.String, hydra.core.Binding] =
      hydra.lib.eithers.bind[scala.Predef.String, Option[hydra.core.TypeScheme], hydra.core.Binding](hydra.lib.maybes.maybe[Either[scala.Predef.String, Option[hydra.core.TypeScheme]], hydra.core.TypeScheme](Right(None))((ts: hydra.core.TypeScheme) =>
      hydra.lib.eithers.bind[scala.Predef.String, hydra.core.TypeScheme, Option[hydra.core.TypeScheme]](hydra.adapt.adaptTypeScheme(constraints)(litmap)(ts))((ts1: hydra.core.TypeScheme) => Right(Some(ts1))))(b.`type`))((adaptedBType: Option[hydra.core.TypeScheme]) => Right(hydra.core.Binding(b.name, (b.term), adaptedBType)))
    hydra.lib.eithers.bind[scala.Predef.String, Seq[hydra.core.Binding], hydra.core.Term](hydra.lib.eithers.mapList[hydra.core.Binding, hydra.core.Binding, scala.Predef.String](adaptB)(v_Term_let_lt.bindings))((adaptedBindings: Seq[hydra.core.Binding]) =>
      Right(hydra.core.Term.let(hydra.core.Let(adaptedBindings, (v_Term_let_lt.body)))))
  }
  case _ => Right(rewritten))

def adaptPrimitive(constraints: hydra.coders.LanguageConstraints)(litmap: Map[hydra.core.LiteralType, hydra.core.LiteralType])(prim0: hydra.graph.Primitive): Either[scala.Predef.String, hydra.graph.Primitive] =
  {
  lazy val ts0: hydra.core.TypeScheme = (prim0.`type`)
  hydra.lib.eithers.bind[scala.Predef.String, hydra.core.TypeScheme, hydra.graph.Primitive](hydra.adapt.adaptTypeScheme(constraints)(litmap)(ts0))((ts1: hydra.core.TypeScheme) =>
    Right(hydra.graph.Primitive(prim0.name, ts1, (prim0.implementation))))
}

def adaptTerm(constraints: hydra.coders.LanguageConstraints)(litmap: Map[hydra.core.LiteralType, hydra.core.LiteralType])(cx: hydra.context.Context)(graph: hydra.graph.Graph)(term0: hydra.core.Term): Either[scala.Predef.String, hydra.core.Term] =
  {
  def rewrite[T0](recurse: (T0 => Either[scala.Predef.String, hydra.core.Term]))(term02: T0): Either[scala.Predef.String, hydra.core.Term] =
    {
    def forSupported[T1](term: hydra.core.Term): Either[T1, Option[hydra.core.Term]] =
      term match
      case hydra.core.Term.literal(v_Term_literal_l) => {
        lazy val lt: hydra.core.LiteralType = hydra.reflect.literalType(v_Term_literal_l)
        Right(Some(hydra.lib.logic.ifElse[hydra.core.Term](hydra.adapt.literalTypeSupported(constraints)(lt))(term)(hydra.core.Term.literal(hydra.adapt.adaptLiteralValue(litmap)(lt)(v_Term_literal_l)))))
      }
      case _ => Right(Some(term))
    def forUnsupported(term: hydra.core.Term): Either[scala.Predef.String, Option[hydra.core.Term]] =
      {
      def forNonNull(alts: Seq[hydra.core.Term]): Either[scala.Predef.String, Option[hydra.core.Term]] =
        hydra.lib.eithers.bind[scala.Predef.String, Option[hydra.core.Term], Option[hydra.core.Term]](tryTerm(hydra.lib.lists.head[hydra.core.Term](alts)))((mterm: Option[hydra.core.Term]) =>
        hydra.lib.maybes.maybe[Either[scala.Predef.String, Option[hydra.core.Term]], hydra.core.Term](tryAlts(hydra.lib.lists.tail[hydra.core.Term](alts)))((t: hydra.core.Term) => Right(Some(t)))(mterm))
      def tryAlts(alts: Seq[hydra.core.Term]): Either[scala.Predef.String, Option[hydra.core.Term]] =
        hydra.lib.logic.ifElse[Either[scala.Predef.String, Option[hydra.core.Term]]](hydra.lib.lists.`null`[hydra.core.Term](alts))(Right(None))(forNonNull(alts))
      hydra.lib.eithers.bind[scala.Predef.String, Seq[hydra.core.Term], Option[hydra.core.Term]](hydra.adapt.termAlternatives(cx)(graph)(term))((alts0: Seq[hydra.core.Term]) => tryAlts(alts0))
    }
    def tryTerm(term: hydra.core.Term): Either[scala.Predef.String, Option[hydra.core.Term]] =
      {
      lazy val supportedVariant: Boolean = hydra.lib.sets.member[hydra.variants.TermVariant](hydra.reflect.termVariant(term))(constraints.termVariants)
      hydra.lib.logic.ifElse[Either[scala.Predef.String, Option[hydra.core.Term]]](supportedVariant)(forSupported(term))(forUnsupported(term))
    }
    hydra.lib.eithers.bind[scala.Predef.String, hydra.core.Term, hydra.core.Term](recurse(term02))((term1: hydra.core.Term) =>
      term1 match
      case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.lib.eithers.bind[scala.Predef.String, hydra.core.Type, hydra.core.Term](hydra.adapt.adaptType(constraints)(litmap)(v_Term_typeApplication_ta.`type`))((atyp: hydra.core.Type) =>
        Right(hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(v_Term_typeApplication_ta.body, atyp))))
      case hydra.core.Term.typeLambda(v_Term_typeLambda__) => Right(term1)
      case _ => hydra.lib.eithers.bind[scala.Predef.String, Option[hydra.core.Term], hydra.core.Term](tryTerm(term1))((mterm: Option[hydra.core.Term]) =>
        hydra.lib.maybes.maybe[Either[scala.Predef.String, hydra.core.Term], hydra.core.Term](Left(hydra.lib.strings.cat2("no alternatives for term: ")(hydra.show.core.term(term1))))((term2: hydra.core.Term) => Right(term2))(mterm)))
  }
  hydra.rewriting.rewriteTermM(rewrite)(term0)
}

def adaptTermForLanguage(lang: hydra.coders.Language)(cx: hydra.context.Context)(g: hydra.graph.Graph)(term: hydra.core.Term): Either[scala.Predef.String, hydra.core.Term] =
  {
  lazy val constraints: hydra.coders.LanguageConstraints = (lang.constraints)
  lazy val litmap: Map[hydra.core.LiteralType, hydra.core.LiteralType] = hydra.adapt.adaptLiteralTypesMap(constraints)
  hydra.adapt.adaptTerm(constraints)(litmap)(cx)(g)(term)
}

def adaptType(constraints: hydra.coders.LanguageConstraints)(litmap: Map[hydra.core.LiteralType, hydra.core.LiteralType])(type0: hydra.core.Type): Either[scala.Predef.String, hydra.core.Type] =
  {
  def forSupported(typ: hydra.core.Type): Option[hydra.core.Type] =
    typ match
    case hydra.core.Type.literal(v_Type_literal_lt) => hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.adapt.literalTypeSupported(constraints)(v_Type_literal_lt))(Some(typ))(hydra.lib.maybes.maybe[Option[hydra.core.Type], hydra.core.LiteralType](Some(hydra.core.Type.literal(hydra.core.LiteralType.string)))((lt2: hydra.core.LiteralType) => Some(hydra.core.Type.literal(lt2)))(hydra.lib.maps.lookup[hydra.core.LiteralType, hydra.core.LiteralType](v_Type_literal_lt)(litmap)))
    case _ => Some(typ)
  def forUnsupported(typ: hydra.core.Type): Option[hydra.core.Type] =
    {
    def tryAlts(alts: Seq[hydra.core.Type]): Option[hydra.core.Type] =
      hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.lib.lists.`null`[hydra.core.Type](alts))(None)(hydra.lib.maybes.maybe[Option[hydra.core.Type], hydra.core.Type](tryAlts(hydra.lib.lists.tail[hydra.core.Type](alts)))((t: hydra.core.Type) => Some(t))(tryType(hydra.lib.lists.head[hydra.core.Type](alts))))
    lazy val alts0: Seq[hydra.core.Type] = hydra.adapt.typeAlternatives(typ)
    tryAlts(alts0)
  }
  def tryType(typ: hydra.core.Type): Option[hydra.core.Type] =
    {
    lazy val supportedVariant: Boolean = hydra.lib.sets.member[hydra.variants.TypeVariant](hydra.reflect.typeVariant(typ))(constraints.typeVariants)
    hydra.lib.logic.ifElse[Option[hydra.core.Type]](supportedVariant)(forSupported(typ))(forUnsupported(typ))
  }
  def rewrite(recurse: (hydra.core.Type => Either[scala.Predef.String, hydra.core.Type]))(typ: hydra.core.Type): Either[scala.Predef.String, hydra.core.Type] =
    hydra.lib.eithers.bind[scala.Predef.String, hydra.core.Type, hydra.core.Type](recurse(typ))((type1: hydra.core.Type) =>
    hydra.lib.maybes.maybe[Either[scala.Predef.String, hydra.core.Type], hydra.core.Type](Left(hydra.lib.strings.cat2("no alternatives for type: ")(hydra.show.core.`type`(typ))))((type2: hydra.core.Type) => Right(type2))(tryType(type1)))
  hydra.rewriting.rewriteTypeM(rewrite)(type0)
}

def adaptTypeForLanguage(lang: hydra.coders.Language)(typ: hydra.core.Type): Either[scala.Predef.String, hydra.core.Type] =
  {
  lazy val constraints: hydra.coders.LanguageConstraints = (lang.constraints)
  lazy val litmap: Map[hydra.core.LiteralType, hydra.core.LiteralType] = hydra.adapt.adaptLiteralTypesMap(constraints)
  hydra.adapt.adaptType(constraints)(litmap)(typ)
}

def adaptTypeScheme(constraints: hydra.coders.LanguageConstraints)(litmap: Map[hydra.core.LiteralType, hydra.core.LiteralType])(ts0: hydra.core.TypeScheme): Either[scala.Predef.String, hydra.core.TypeScheme] =
  {
  lazy val vars0: Seq[hydra.core.Name] = (ts0.variables)
  lazy val t0: hydra.core.Type = (ts0.`type`)
  hydra.lib.eithers.bind[scala.Predef.String, hydra.core.Type, hydra.core.TypeScheme](hydra.adapt.adaptType(constraints)(litmap)(t0))((t1: hydra.core.Type) => Right(hydra.core.TypeScheme(vars0, t1, (ts0.constraints))))
}

def composeCoders[T0, T1, T2](c1: hydra.util.Coder[T0, T1])(c2: hydra.util.Coder[T1, T2]): hydra.util.Coder[T0, T2] =
  hydra.util.Coder((cx: hydra.context.Context) =>
  (a: T0) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], T1, T2](c1.encode(cx)(a))((b1: T1) => c2.encode(cx)(b1)), (cx: hydra.context.Context) =>
  (c: T2) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], T1, T0](c2.decode(cx)(c))((b2: T1) => c1.decode(cx)(b2)))

def dataGraphToDefinitions(constraints: hydra.coders.LanguageConstraints)(doInfer: Boolean)(doExpand: Boolean)(doHoistCaseStatements: Boolean)(doHoistPolymorphicLetBindings: Boolean)(originalBindings: Seq[hydra.core.Binding])(graph0: hydra.graph.Graph)(namespaces: Seq[hydra.module.Namespace])(cx: hydra.context.Context): Either[scala.Predef.String, Tuple2[hydra.graph.Graph, Seq[Seq[hydra.module.TermDefinition]]]] =
  {
  lazy val namespacesSet: scala.collection.immutable.Set[hydra.module.Namespace] = hydra.lib.sets.fromList[hydra.module.Namespace](namespaces)
  def isParentBinding(b: hydra.core.Binding): Boolean =
    hydra.lib.maybes.maybe[Boolean, hydra.module.Namespace](false)((ns: hydra.module.Namespace) =>
    hydra.lib.sets.member[hydra.module.Namespace](ns)(namespacesSet))(hydra.names.namespaceOf(b.name))
  def hoistCases(bindings: Seq[hydra.core.Binding]): Seq[hydra.core.Binding] =
    {
    lazy val stripped: Seq[hydra.core.Binding] = hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding]((b: hydra.core.Binding) =>
      hydra.core.Binding(b.name, hydra.rewriting.stripTypeLambdas(b.term), (b.`type`)))(bindings)
    lazy val term0: hydra.core.Term = hydra.core.Term.let(hydra.core.Let(stripped, hydra.core.Term.unit))
    lazy val unshadowed0: Seq[hydra.core.Binding] = hydra.schemas.termAsBindings(hydra.rewriting.unshadowVariables(term0))
    lazy val hoisted: Seq[hydra.core.Binding] = hydra.hoisting.hoistCaseStatementsInGraph(unshadowed0)
    lazy val term1: hydra.core.Term = hydra.core.Term.let(hydra.core.Let(hoisted, hydra.core.Term.unit))
    hydra.schemas.termAsBindings(hydra.rewriting.unshadowVariables(term1))
  }
  def hoistPoly(bindings: Seq[hydra.core.Binding]): Seq[hydra.core.Binding] =
    {
    lazy val letBefore: hydra.core.Let = hydra.core.Let(bindings, hydra.core.Term.unit)
    lazy val letAfter: hydra.core.Let = hydra.hoisting.hoistPolymorphicLetBindings(isParentBinding)(letBefore)
    (letAfter.bindings)
  }
  def checkBindingsTyped(debugLabel: scala.Predef.String)(bindings: Seq[hydra.core.Binding]): Either[scala.Predef.String, Seq[hydra.core.Binding]] =
    {
    lazy val untypedBindings: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.core.Binding, scala.Predef.String]((b: hydra.core.Binding) => (b.name))(hydra.lib.lists.filter[hydra.core.Binding]((b: hydra.core.Binding) =>
      hydra.lib.logic.not(hydra.lib.maybes.isJust[hydra.core.TypeScheme](b.`type`)))(bindings))
    hydra.lib.logic.ifElse[Either[scala.Predef.String, Seq[hydra.core.Binding]]](hydra.lib.lists.`null`[scala.Predef.String](untypedBindings))(Right(bindings))(Left(hydra.lib.strings.cat(Seq("Found untyped bindings (", debugLabel, "): ", hydra.lib.strings.intercalate(", ")(untypedBindings)))))
  }
  def normalizeBindings(bindings: Seq[hydra.core.Binding]): Seq[hydra.core.Binding] =
    hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding]((b: hydra.core.Binding) =>
    hydra.core.Binding(b.name, hydra.adapt.pushTypeAppsInward(b.term), (b.`type`)))(bindings)
  def rebuildGraph(bindings: Seq[hydra.core.Binding]): hydra.graph.Graph =
    {
    lazy val g: hydra.graph.Graph = hydra.lexical.buildGraph(bindings)(hydra.lib.maps.empty[hydra.core.Name, Option[hydra.core.Term]])(graph0.primitives)
    hydra.graph.Graph(g.boundTerms, (g.boundTypes), (g.classConstraints), (g.lambdaVariables), (g.metadata), (g.primitives), (graph0.schemaTypes), (g.typeVariables))
  }
  lazy val bins0: Seq[hydra.core.Binding] = originalBindings
  lazy val bins1: Seq[hydra.core.Binding] = hydra.lib.logic.ifElse[Seq[hydra.core.Binding]](doHoistCaseStatements)(hoistCases(bins0))(bins0)
  hydra.lib.eithers.bind[scala.Predef.String, Seq[hydra.core.Binding], Tuple2[hydra.graph.Graph, Seq[Seq[hydra.module.TermDefinition]]]](hydra.lib.logic.ifElse[Either[scala.Predef.String, Seq[hydra.core.Binding]]](doInfer)(hydra.lib.eithers.map[Tuple2[Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]], hydra.context.Context], Seq[hydra.core.Binding], scala.Predef.String]((result: Tuple2[Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]], hydra.context.Context]) =>
    hydra.lib.pairs.second[hydra.graph.Graph, Seq[hydra.core.Binding]](hydra.lib.pairs.first[Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]], hydra.context.Context](result)))(hydra.lib.eithers.bimap[hydra.context.InContext[hydra.errors.Error], Tuple2[Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]], hydra.context.Context], scala.Predef.String, Tuple2[Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]], hydra.context.Context]]((ic: hydra.context.InContext[hydra.errors.Error]) => hydra.show.errors.error(ic.`object`))((x: Tuple2[Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]], hydra.context.Context]) => x)(hydra.inference.inferGraphTypes(cx)(bins1)(rebuildGraph(bins1)))))(checkBindingsTyped("after case hoisting")(bins1)))((bins2: Seq[hydra.core.Binding]) =>
    hydra.lib.eithers.bind[scala.Predef.String, Seq[hydra.core.Binding], Tuple2[hydra.graph.Graph, Seq[Seq[hydra.module.TermDefinition]]]](hydra.lib.logic.ifElse[Either[scala.Predef.String, Seq[hydra.core.Binding]]](doHoistPolymorphicLetBindings)(checkBindingsTyped("after let hoisting")(hoistPoly(bins2)))(Right(bins2)))((bins3: Seq[hydra.core.Binding]) =>
    hydra.lib.eithers.bind[scala.Predef.String, Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]], Tuple2[hydra.graph.Graph, Seq[Seq[hydra.module.TermDefinition]]]](hydra.adapt.adaptDataGraph(constraints)(doExpand)(bins3)(cx)(rebuildGraph(bins3)))((adaptResult: Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]]) =>
    {
    lazy val adapted: hydra.graph.Graph = hydra.lib.pairs.first[hydra.graph.Graph, Seq[hydra.core.Binding]](adaptResult)
    {
      lazy val adaptedBindings: Seq[hydra.core.Binding] = hydra.lib.pairs.second[hydra.graph.Graph, Seq[hydra.core.Binding]](adaptResult)
      hydra.lib.eithers.bind[scala.Predef.String, Seq[hydra.core.Binding], Tuple2[hydra.graph.Graph, Seq[Seq[hydra.module.TermDefinition]]]](checkBindingsTyped("after adaptation")(adaptedBindings))((bins4: Seq[hydra.core.Binding]) =>
        {
        lazy val bins5: Seq[hydra.core.Binding] = normalizeBindings(bins4)
        {
          def toDef(el: hydra.core.Binding): Option[hydra.module.TermDefinition] =
            hydra.lib.maybes.map[hydra.core.TypeScheme, hydra.module.TermDefinition]((ts: hydra.core.TypeScheme) => hydra.module.TermDefinition(el.name, (el.term), ts))(el.`type`)
          {
            lazy val selectedElements: Seq[hydra.core.Binding] = hydra.lib.lists.filter[hydra.core.Binding]((el: hydra.core.Binding) =>
              hydra.lib.maybes.maybe[Boolean, hydra.module.Namespace](false)((ns: hydra.module.Namespace) =>
              hydra.lib.sets.member[hydra.module.Namespace](ns)(namespacesSet))(hydra.names.namespaceOf(el.name)))(bins5)
            {
              lazy val elementsByNamespace: Map[hydra.module.Namespace, Seq[hydra.core.Binding]] = hydra.lib.lists.foldl[Map[hydra.module.Namespace, Seq[hydra.core.Binding]], hydra.core.Binding]((acc: Map[hydra.module.Namespace, Seq[hydra.core.Binding]]) =>
                (el: hydra.core.Binding) =>
                hydra.lib.maybes.maybe[Map[hydra.module.Namespace, Seq[hydra.core.Binding]], hydra.module.Namespace](acc)((ns: hydra.module.Namespace) =>
                {
                lazy val existing: Seq[hydra.core.Binding] = hydra.lib.maybes.maybe[Seq[hydra.core.Binding], Seq[hydra.core.Binding]](Seq())(hydra.lib.equality.identity[Seq[hydra.core.Binding]])(hydra.lib.maps.lookup[hydra.module.Namespace, Seq[hydra.core.Binding]](ns)(acc))
                hydra.lib.maps.insert[hydra.module.Namespace, Seq[hydra.core.Binding]](ns)(hydra.lib.lists.concat2[hydra.core.Binding](existing)(Seq(el)))(acc)
              })(hydra.names.namespaceOf(el.name)))(hydra.lib.maps.empty[hydra.module.Namespace, Seq[hydra.core.Binding]])(selectedElements)
              {
                lazy val defsGrouped: Seq[Seq[hydra.module.TermDefinition]] = hydra.lib.lists.map[hydra.module.Namespace, Seq[hydra.module.TermDefinition]]((ns: hydra.module.Namespace) =>
                  {
                  lazy val elsForNs: Seq[hydra.core.Binding] = hydra.lib.maybes.maybe[Seq[hydra.core.Binding], Seq[hydra.core.Binding]](Seq())(hydra.lib.equality.identity[Seq[hydra.core.Binding]])(hydra.lib.maps.lookup[hydra.module.Namespace, Seq[hydra.core.Binding]](ns)(elementsByNamespace))
                  hydra.lib.maybes.cat[hydra.module.TermDefinition](hydra.lib.lists.map[hydra.core.Binding, Option[hydra.module.TermDefinition]](toDef)(elsForNs))
                })(namespaces)
                {
                  lazy val g: hydra.graph.Graph = hydra.lexical.buildGraph(bins5)(hydra.lib.maps.empty[hydra.core.Name, Option[hydra.core.Term]])(adapted.primitives)
                  Right(Tuple2(hydra.graph.Graph(g.boundTerms, (g.boundTypes), (g.classConstraints), (g.lambdaVariables), (g.metadata), (g.primitives), (adapted.schemaTypes), (g.typeVariables)), defsGrouped))
                }
              }
            }
          }
        }
      })
    }
  })))
}

def literalTypeSupported(constraints: hydra.coders.LanguageConstraints)(lt: hydra.core.LiteralType): Boolean =
  {
  def forType(lt2: hydra.core.LiteralType): Boolean =
    lt2 match
    case hydra.core.LiteralType.float(v_LiteralType_float_ft) => hydra.lib.sets.member[hydra.core.FloatType](v_LiteralType_float_ft)(constraints.floatTypes)
    case hydra.core.LiteralType.integer(v_LiteralType_integer_it) => hydra.lib.sets.member[hydra.core.IntegerType](v_LiteralType_integer_it)(constraints.integerTypes)
    case _ => true
  hydra.lib.logic.ifElse[Boolean](hydra.lib.sets.member[hydra.variants.LiteralVariant](hydra.reflect.literalTypeVariant(lt))(constraints.literalVariants))(forType(lt))(false)
}

def pushTypeAppsInward(term: hydra.core.Term): hydra.core.Term =
  {
  def push(body: hydra.core.Term)(typ: hydra.core.Type): hydra.core.Term =
    body match
    case hydra.core.Term.application(v_Term_application_a) => go(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(v_Term_application_a.function, typ)), (v_Term_application_a.argument))))
    case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
      case hydra.core.Function.lambda(v_Function_lambda_l) => go(hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(v_Function_lambda_l.parameter, (v_Function_lambda_l.domain), hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(v_Function_lambda_l.body, typ))))))
      case _ => hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(hydra.core.Term.function(v_Term_function_f), typ))
    case hydra.core.Term.let(v_Term_let_lt) => go(hydra.core.Term.let(hydra.core.Let(v_Term_let_lt.bindings, hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(v_Term_let_lt.body, typ)))))
    case _ => hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(body, typ))
  def go(t: hydra.core.Term): hydra.core.Term =
    {
    def forField(fld: hydra.core.Field): hydra.core.Field = hydra.core.Field(fld.name, go(fld.term))
    def forElimination(elm: hydra.core.Elimination): hydra.core.Elimination =
      elm match
      case hydra.core.Elimination.record(v_Elimination_record_p) => hydra.core.Elimination.record(v_Elimination_record_p)
      case hydra.core.Elimination.union(v_Elimination_union_cs) => hydra.core.Elimination.union(hydra.core.CaseStatement(v_Elimination_union_cs.typeName, hydra.lib.maybes.map[hydra.core.Term, hydra.core.Term](go)(v_Elimination_union_cs.default), hydra.lib.lists.map[hydra.core.Field, hydra.core.Field](forField)(v_Elimination_union_cs.cases)))
      case hydra.core.Elimination.wrap(v_Elimination_wrap_name) => hydra.core.Elimination.wrap(v_Elimination_wrap_name)
    def forFunction(fun: hydra.core.Function): hydra.core.Function =
      fun match
      case hydra.core.Function.elimination(v_Function_elimination_elm) => hydra.core.Function.elimination(forElimination(v_Function_elimination_elm))
      case hydra.core.Function.lambda(v_Function_lambda_l) => hydra.core.Function.lambda(hydra.core.Lambda(v_Function_lambda_l.parameter, (v_Function_lambda_l.domain), go(v_Function_lambda_l.body)))
      case hydra.core.Function.primitive(v_Function_primitive_name) => hydra.core.Function.primitive(v_Function_primitive_name)
    def forLet(lt: hydra.core.Let): hydra.core.Let =
      {
      def mapBinding(b: hydra.core.Binding): hydra.core.Binding = hydra.core.Binding(b.name, go(b.term), (b.`type`))
      hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding](mapBinding)(lt.bindings), go(lt.body))
    }
    def forMap(m: Map[hydra.core.Term, hydra.core.Term]): Map[hydra.core.Term, hydra.core.Term] =
      {
      def forPair(p: Tuple2[hydra.core.Term, hydra.core.Term]): Tuple2[hydra.core.Term, hydra.core.Term] =
        Tuple2(go(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](p)), go(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](p)))
      hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](hydra.lib.lists.map[Tuple2[hydra.core.Term, hydra.core.Term], Tuple2[hydra.core.Term, hydra.core.Term]](forPair)(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](m)))
    }
    t match
      case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.core.Term.annotated(hydra.core.AnnotatedTerm(go(v_Term_annotated_at.body), (v_Term_annotated_at.annotation)))
      case hydra.core.Term.application(v_Term_application_a) => hydra.core.Term.application(hydra.core.Application(go(v_Term_application_a.function), go(v_Term_application_a.argument)))
      case hydra.core.Term.either(v_Term_either_e) => hydra.core.Term.either(hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, Either[hydra.core.Term, hydra.core.Term]]((l: hydra.core.Term) => Left(go(l)))((r: hydra.core.Term) => Right(go(r)))(v_Term_either_e))
      case hydra.core.Term.function(v_Term_function_fun) => hydra.core.Term.function(forFunction(v_Term_function_fun))
      case hydra.core.Term.let(v_Term_let_lt) => hydra.core.Term.let(forLet(v_Term_let_lt))
      case hydra.core.Term.list(v_Term_list_els) => hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Term, hydra.core.Term](go)(v_Term_list_els))
      case hydra.core.Term.literal(v_Term_literal_v) => hydra.core.Term.literal(v_Term_literal_v)
      case hydra.core.Term.map(v_Term_map_m) => hydra.core.Term.map(forMap(v_Term_map_m))
      case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.core.Term, hydra.core.Term](go)(v_Term_maybe_m))
      case hydra.core.Term.pair(v_Term_pair_p) => hydra.core.Term.pair(Tuple2(go(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)), go(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p))))
      case hydra.core.Term.record(v_Term_record_r) => hydra.core.Term.record(hydra.core.Record(v_Term_record_r.typeName, hydra.lib.lists.map[hydra.core.Field, hydra.core.Field](forField)(v_Term_record_r.fields)))
      case hydra.core.Term.set(v_Term_set_s) => hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](hydra.lib.lists.map[hydra.core.Term, hydra.core.Term](go)(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s))))
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => {
        lazy val body1: hydra.core.Term = go(v_Term_typeApplication_tt.body)
        push(body1)(v_Term_typeApplication_tt.`type`)
      }
      case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_ta.parameter, go(v_Term_typeLambda_ta.body)))
      case hydra.core.Term.union(v_Term_union_i) => hydra.core.Term.union(hydra.core.Injection(v_Term_union_i.typeName, forField(v_Term_union_i.field)))
      case hydra.core.Term.unit => hydra.core.Term.unit
      case hydra.core.Term.variable(v_Term_variable_v) => hydra.core.Term.variable(v_Term_variable_v)
      case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.core.Term.wrap(hydra.core.WrappedTerm(v_Term_wrap_wt.typeName, go(v_Term_wrap_wt.body)))
  }
  go(term)
}

def schemaGraphToDefinitions(constraints: hydra.coders.LanguageConstraints)(graph: hydra.graph.Graph)(nameLists: Seq[Seq[hydra.core.Name]])(cx: hydra.context.Context): Either[scala.Predef.String, Tuple2[Map[hydra.core.Name, hydra.core.Type], Seq[Seq[hydra.module.TypeDefinition]]]] =
  {
  lazy val litmap: Map[hydra.core.LiteralType, hydra.core.LiteralType] = hydra.adapt.adaptLiteralTypesMap(constraints)
  hydra.lib.eithers.bind[scala.Predef.String, Map[hydra.core.Name, hydra.core.Type], Tuple2[Map[hydra.core.Name, hydra.core.Type], Seq[Seq[hydra.module.TypeDefinition]]]](hydra.lib.eithers.bimap[hydra.context.InContext[hydra.errors.DecodingError], Map[hydra.core.Name, hydra.core.Type], scala.Predef.String, Map[hydra.core.Name, hydra.core.Type]]((ic: hydra.context.InContext[hydra.errors.DecodingError]) => (ic.`object`))((x: Map[hydra.core.Name, hydra.core.Type]) => x)(hydra.schemas.graphAsTypes(cx)(graph)(hydra.lexical.graphToBindings(graph))))((tmap0: Map[hydra.core.Name, hydra.core.Type]) =>
    hydra.lib.eithers.bind[scala.Predef.String, Map[hydra.core.Name, hydra.core.Type], Tuple2[Map[hydra.core.Name, hydra.core.Type], Seq[Seq[hydra.module.TypeDefinition]]]](hydra.adapt.adaptGraphSchema(constraints)(litmap)(tmap0))((tmap1: Map[hydra.core.Name, hydra.core.Type]) =>
    {
    def toDef(pair: Tuple2[hydra.core.Name, hydra.core.Type]): hydra.module.TypeDefinition =
      hydra.module.TypeDefinition(hydra.lib.pairs.first[hydra.core.Name, hydra.core.Type](pair), hydra.lib.pairs.second[hydra.core.Name, hydra.core.Type](pair))
    Right(Tuple2(tmap1, hydra.lib.lists.map[Seq[hydra.core.Name], Seq[hydra.module.TypeDefinition]]((names: Seq[hydra.core.Name]) =>
      hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.Type], hydra.module.TypeDefinition](toDef)(hydra.lib.lists.map[hydra.core.Name, Tuple2[hydra.core.Name, hydra.core.Type]]((n: hydra.core.Name) =>
      Tuple2(n, hydra.lib.maybes.fromJust[hydra.core.Type](hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Type](n)(tmap1))))(names)))(nameLists)))
  }))
}

def simpleLanguageAdapter[T0](lang: hydra.coders.Language)(cx: T0)(g: hydra.graph.Graph)(typ: hydra.core.Type): Either[scala.Predef.String, hydra.util.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]] =
  {
  lazy val constraints: hydra.coders.LanguageConstraints = (lang.constraints)
  lazy val litmap: Map[hydra.core.LiteralType, hydra.core.LiteralType] = hydra.adapt.adaptLiteralTypesMap(constraints)
  hydra.lib.eithers.bind[scala.Predef.String, hydra.core.Type, hydra.util.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]](hydra.adapt.adaptType(constraints)(litmap)(typ))((adaptedType: hydra.core.Type) =>
    Right(hydra.util.Adapter(false, typ, adaptedType, hydra.util.Coder((cx2: hydra.context.Context) =>
    (term: hydra.core.Term) =>
    hydra.lib.eithers.bimap[scala.Predef.String, hydra.core.Term, hydra.context.InContext[hydra.errors.Error], hydra.core.Term]((_s: scala.Predef.String) => hydra.context.InContext(hydra.errors.Error.other(_s), cx2))((_x: hydra.core.Term) => _x)(hydra.adapt.adaptTerm(constraints)(litmap)(cx2)(g)(term)), (cx2: hydra.context.Context) => (term: hydra.core.Term) => Right(term)))))
}

def termAlternatives(cx: hydra.context.Context)(graph: hydra.graph.Graph)(term: hydra.core.Term): Either[scala.Predef.String, Seq[hydra.core.Term]] =
  term match
  case hydra.core.Term.annotated(v_Term_annotated_at) => {
    lazy val term2: hydra.core.Term = (v_Term_annotated_at.body)
    Right(Seq(term2))
  }
  case hydra.core.Term.maybe(v_Term_maybe_ot) => Right(Seq(hydra.core.Term.list(hydra.lib.maybes.maybe[Seq[hydra.core.Term], hydra.core.Term](Seq())((term2: hydra.core.Term) => Seq(term2))(v_Term_maybe_ot))))
  case hydra.core.Term.typeLambda(v_Term_typeLambda_abs) => {
    lazy val term2: hydra.core.Term = (v_Term_typeLambda_abs.body)
    Right(Seq(term2))
  }
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => {
    lazy val term2: hydra.core.Term = (v_Term_typeApplication_ta.body)
    Right(Seq(term2))
  }
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val tname: hydra.core.Name = (v_Term_union_inj.typeName)
    {
      lazy val field: hydra.core.Field = (v_Term_union_inj.field)
      {
        lazy val fname: hydra.core.Name = (field.name)
        {
          lazy val fterm: hydra.core.Term = (field.term)
          {
            def forFieldType(ft: hydra.core.FieldType): hydra.core.Field =
              {
              lazy val ftname: hydra.core.Name = (ft.name)
              hydra.core.Field(fname, hydra.core.Term.maybe(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](ftname)(fname))(Some(fterm))(None)))
            }
            hydra.lib.eithers.bind[scala.Predef.String, Seq[hydra.core.FieldType], Seq[hydra.core.Term]](hydra.lib.eithers.bimap[hydra.context.InContext[hydra.errors.Error], Seq[hydra.core.FieldType], scala.Predef.String, Seq[hydra.core.FieldType]]((ic: hydra.context.InContext[hydra.errors.Error]) => hydra.show.errors.error(ic.`object`))((x: Seq[hydra.core.FieldType]) => x)(hydra.schemas.requireUnionType(cx)(graph)(tname)))((rt: Seq[hydra.core.FieldType]) =>
              Right(Seq(hydra.core.Term.record(hydra.core.Record(tname, hydra.lib.lists.map[hydra.core.FieldType, hydra.core.Field](forFieldType)(rt))))))
          }
        }
      }
    }
  }
  case hydra.core.Term.unit => Right(Seq(hydra.core.Term.literal(hydra.core.Literal.boolean(true))))
  case hydra.core.Term.wrap(v_Term_wrap_wt) => {
    lazy val term2: hydra.core.Term = (v_Term_wrap_wt.body)
    Right(Seq(term2))
  }
  case _ => Right(Seq())

def typeAlternatives(`type`: hydra.core.Type): Seq[hydra.core.Type] =
  `type` match
  case hydra.core.Type.annotated(v_Type_annotated_at) => {
    lazy val type2: hydra.core.Type = (v_Type_annotated_at.body)
    Seq(type2)
  }
  case hydra.core.Type.maybe(v_Type_maybe_ot) => Seq(hydra.core.Type.list(v_Type_maybe_ot))
  case hydra.core.Type.union(v_Type_union_rt) => {
    def toOptField(f: hydra.core.FieldType): hydra.core.FieldType = hydra.core.FieldType(f.name, hydra.core.Type.maybe(f.`type`))
    {
      lazy val optFields: Seq[hydra.core.FieldType] = hydra.lib.lists.map[hydra.core.FieldType, hydra.core.FieldType](toOptField)(v_Type_union_rt)
      Seq(hydra.core.Type.record(optFields))
    }
  }
  case hydra.core.Type.unit => Seq(hydra.core.Type.literal(hydra.core.LiteralType.boolean))
  case hydra.core.Type.void => Seq(hydra.core.Type.unit)
  case _ => Seq()
