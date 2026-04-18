package hydra.codegen

import hydra.coders.*

import hydra.core.*

import hydra.errors.*

import hydra.graph.*

import hydra.packaging.*

def buildSchemaMap(g: hydra.graph.Graph): Map[hydra.core.Name, hydra.core.Type] =
  hydra.lib.maps.map[hydra.core.TypeScheme, hydra.core.Type, hydra.core.Name]((ts: hydra.core.TypeScheme) => hydra.strip.deannotateType(ts.`type`))(g.schemaTypes)

def decodeModuleFromJson(bsGraph: hydra.graph.Graph)(universeModules: Seq[hydra.packaging.Module])(jsonVal: hydra.json.model.Value): Either[hydra.errors.Error,
   hydra.packaging.Module] =
  {
  lazy val graph: hydra.graph.Graph = hydra.codegen.modulesToGraph(bsGraph)(universeModules)(universeModules)
  lazy val schemaMap: Map[hydra.core.Name, hydra.core.Type] = hydra.codegen.buildSchemaMap(graph)
  lazy val modType: hydra.core.Type = hydra.core.Type.variable("hydra.packaging.Module")
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[hydra.errors.Error,
     hydra.packaging.Module]]((err: scala.Predef.String) => Left(hydra.errors.Error.other(err)))((term: hydra.core.Term) =>
    hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.packaging.Module, Either[hydra.errors.Error,
       hydra.packaging.Module]]((decErr: hydra.errors.DecodingError) => Left(hydra.errors.Error.decoding(decErr)))((mod: hydra.packaging.Module) => Right(mod))(hydra.decode.packaging.module(graph)(term)))(hydra.json.decode.fromJson(schemaMap)("hydra.packaging.Module")(modType)(jsonVal))
}

def escapeControlCharsInJson(input: Seq[Int]): Seq[Int] =
  {
  def hexDigit(n: Int): Int =
    hydra.lib.logic.ifElse[Int](hydra.lib.equality.lt[Int](n)(10))(hydra.lib.math.add(48)(n))(hydra.lib.math.add(97)(hydra.lib.math.sub(n)(10)))
  def escapeToUnicode(b: Int): Seq[Int] =
    Seq(92, 117, 48, 48, hexDigit(hydra.lib.maybes.fromMaybe[Int](0)(hydra.lib.math.maybeDiv(b)(16))),
       hexDigit(hydra.lib.maybes.fromMaybe[Int](0)(hydra.lib.math.maybeMod(b)(16))))
  def go(inStr: Boolean)(esc: Boolean)(bytes: Seq[Int]): Seq[Int] =
    hydra.lib.maybes.maybe[Seq[Int], Tuple2[Int, Seq[Int]]](Seq())((uc: Tuple2[Int, Seq[Int]]) =>
    {
    lazy val b: Int = hydra.lib.pairs.first[Int, Seq[Int]](uc)
    {
      lazy val bs: Seq[Int] = hydra.lib.pairs.second[Int, Seq[Int]](uc)
      hydra.lib.logic.ifElse[Seq[Int]](esc)(hydra.lib.lists.cons[Int](b)(go(inStr)(false)(bs)))(hydra.lib.logic.ifElse[Seq[Int]](hydra.lib.logic.and(hydra.lib.equality.equal[Int](b)(92))(inStr))(hydra.lib.lists.cons[Int](b)(go(inStr)(true)(bs)))(hydra.lib.logic.ifElse[Seq[Int]](hydra.lib.equality.equal[Int](b)(34))(hydra.lib.lists.cons[Int](b)(go(hydra.lib.logic.not(inStr))(false)(bs)))(hydra.lib.logic.ifElse[Seq[Int]](hydra.lib.logic.and(inStr)(hydra.lib.equality.lt[Int](b)(32)))(hydra.lib.lists.concat2[Int](escapeToUnicode(b))(go(inStr)(false)(bs)))(hydra.lib.lists.cons[Int](b)(go(inStr)(false)(bs))))))
    }
  })(hydra.lib.lists.uncons[Int](bytes))
  go(false)(false)(input)
}

def formatPrimitive(prim: hydra.graph.Primitive): scala.Predef.String =
  {
  lazy val name: scala.Predef.String = (prim.name)
  lazy val typeStr: scala.Predef.String = hydra.show.core.typeScheme(prim.`type`)
  hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("  ")(name))(" : "))(typeStr)
}

def formatTermBinding(binding: hydra.core.Binding): scala.Predef.String =
  {
  lazy val name: scala.Predef.String = (binding.name)
  lazy val typeStr: scala.Predef.String = hydra.lib.maybes.maybe[scala.Predef.String,
     hydra.core.TypeScheme]("?")((scheme: hydra.core.TypeScheme) => hydra.show.core.typeScheme(scheme))(binding.`type`)
  hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("  ")(name))(" : "))(typeStr)
}

def formatTypeBinding(graph: hydra.graph.Graph)(binding: hydra.core.Binding): Either[hydra.errors.Error,
   scala.Predef.String] =
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Type, scala.Predef.String](hydra.lib.eithers.bimap[hydra.errors.DecodingError,
     hydra.core.Type, hydra.errors.Error, hydra.core.Type]((_e: hydra.errors.DecodingError) => hydra.errors.Error.decoding(_e))((_a: hydra.core.Type) => _a)(hydra.decode.core.`type`(graph)(binding.term)))((typ: hydra.core.Type) =>
  Right(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("  ")(binding.name))(" = "))(hydra.show.core.`type`(typ))))

def generateCoderModules[T0, T1, T2, T3](codec: (T0 => hydra.graph.Graph => T1 => Either[T2,
   Option[T3]]))(bsGraph: hydra.graph.Graph)(universeModules: Seq[hydra.packaging.Module])(typeModules: Seq[T1])(cx: T0): Either[T2,
   Seq[T3]] =
  {
  lazy val universe: Map[hydra.packaging.Namespace, hydra.packaging.Module] = hydra.lib.maps.fromList[hydra.packaging.Namespace,
     hydra.packaging.Module](hydra.lib.lists.map[hydra.packaging.Module, Tuple2[hydra.packaging.Namespace,
     hydra.packaging.Module]]((m: hydra.packaging.Module) => Tuple2(m.namespace, m))(hydra.lib.lists.concat2[hydra.packaging.Module](universeModules)(universeModules)))
  lazy val schemaModules: Seq[hydra.packaging.Module] = hydra.codegen.moduleTypeDepsTransitive(universe)(universeModules)
  lazy val dataModules: Seq[hydra.packaging.Module] = hydra.codegen.moduleTermDepsTransitive(universe)(universeModules)
  lazy val schemaElements: Seq[hydra.core.Binding] = hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Module,
     Seq[hydra.core.Binding]]((m: hydra.packaging.Module) =>
    hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Definition,
       Option[hydra.core.Binding]]((d: hydra.packaging.Definition) =>
    d match
    case hydra.packaging.Definition.`type`(v_Definition_type_td) => Some({
      lazy val schemaTerm: hydra.core.Term = hydra.core.Term.variable("hydra.core.Type")
      {
        lazy val dataTerm: hydra.core.Term = hydra.annotations.normalizeTermAnnotations(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.encode.core.`type`(v_Definition_type_td.`type`.`type`),
           hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Term](Seq(Tuple2(hydra.constants.key_type,
           schemaTerm))))))
        hydra.core.Binding(v_Definition_type_td.name, dataTerm, Some(hydra.core.TypeScheme(Seq(),
           hydra.core.Type.variable("hydra.core.Type"), None)))
      }
    })
    case _ => None)(m.definitions)))(hydra.lib.lists.concat2[hydra.packaging.Module](schemaModules)(universeModules)))
  lazy val dataElements: Seq[hydra.core.Binding] = hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Module,
     Seq[hydra.core.Binding]]((m: hydra.packaging.Module) =>
    hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Definition,
       Option[hydra.core.Binding]]((d: hydra.packaging.Definition) =>
    d match
    case hydra.packaging.Definition.term(v_Definition_term_td) => Some(hydra.core.Binding(v_Definition_term_td.name,
       (v_Definition_term_td.term), (v_Definition_term_td.`type`)))
    case _ => None)(m.definitions)))(dataModules))
  lazy val schemaGraph: hydra.graph.Graph = hydra.lexical.elementsToGraph(bsGraph)(hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeScheme])(schemaElements)
  lazy val schemaTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = hydra.lib.eithers.either[hydra.errors.Error,
     Map[hydra.core.Name, hydra.core.TypeScheme], Map[hydra.core.Name, hydra.core.TypeScheme]]((_x: hydra.errors.Error) => hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeScheme])((_r: Map[hydra.core.Name, hydra.core.TypeScheme]) => _r)(hydra.environment.schemaGraphToTypingEnvironment(schemaGraph))
  lazy val allElements: Seq[hydra.core.Binding] = hydra.lib.lists.concat2[hydra.core.Binding](schemaElements)(dataElements)
  lazy val graph: hydra.graph.Graph = hydra.lexical.elementsToGraph(bsGraph)(schemaTypes)(allElements)
  hydra.lib.eithers.map[Seq[Option[T3]], Seq[T3], T2]((results: Seq[Option[T3]]) => hydra.lib.maybes.cat[T3](results))(hydra.lib.eithers.mapList[T1,
     Option[T3], T2]((m: T1) => codec(cx)(graph)(m))(typeModules))
}

def generateLexicon(graph: hydra.graph.Graph): Either[hydra.errors.Error, scala.Predef.String] =
  {
  lazy val bindings: Seq[hydra.core.Binding] = hydra.lexical.graphToBindings(graph)
  lazy val primitives: Seq[hydra.graph.Primitive] = hydra.lib.maps.elems[hydra.core.Name,
     hydra.graph.Primitive](graph.primitives)
  lazy val partitioned: Tuple2[Seq[hydra.core.Binding], Seq[hydra.core.Binding]] = hydra.lib.lists.partition[hydra.core.Binding]((b: hydra.core.Binding) => hydra.annotations.isNativeType(b))(bindings)
  lazy val typeBindings: Seq[hydra.core.Binding] = hydra.lib.pairs.first[Seq[hydra.core.Binding],
     Seq[hydra.core.Binding]](partitioned)
  lazy val termBindings: Seq[hydra.core.Binding] = hydra.lib.pairs.second[Seq[hydra.core.Binding],
     Seq[hydra.core.Binding]](partitioned)
  lazy val sortedPrimitives: Seq[hydra.graph.Primitive] = hydra.lib.lists.sortOn[hydra.graph.Primitive,
     hydra.core.Name]((p: hydra.graph.Primitive) => (p.name))(primitives)
  lazy val sortedTypes: Seq[hydra.core.Binding] = hydra.lib.lists.sortOn[hydra.core.Binding,
     hydra.core.Name]((b: hydra.core.Binding) => (b.name))(typeBindings)
  lazy val sortedTerms: Seq[hydra.core.Binding] = hydra.lib.lists.sortOn[hydra.core.Binding,
     hydra.core.Name]((b: hydra.core.Binding) => (b.name))(termBindings)
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[scala.Predef.String], scala.Predef.String](hydra.lib.eithers.mapList[hydra.core.Binding,
     scala.Predef.String, hydra.errors.Error]((b: hydra.core.Binding) => hydra.codegen.formatTypeBinding(graph)(b))(sortedTypes))((typeLines: Seq[scala.Predef.String]) =>
    {
    lazy val termLines: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.core.Binding,
       scala.Predef.String]((b: hydra.core.Binding) => hydra.codegen.formatTermBinding(b))(sortedTerms)
    {
      lazy val primitiveLines: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.graph.Primitive,
         scala.Predef.String]((p: hydra.graph.Primitive) => hydra.codegen.formatPrimitive(p))(sortedPrimitives)
      Right(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("Primitives:\n")(hydra.lib.strings.unlines(primitiveLines)))("\nTypes:\n"))(hydra.lib.strings.unlines(typeLines)))("\nTerms:\n"))(hydra.lib.strings.unlines(termLines)))
    }
  })
}

def generateSourceFiles[T0, T1](printDefinitions: (hydra.packaging.Module => Seq[hydra.packaging.Definition] => hydra.context.Context => hydra.graph.Graph => Either[hydra.errors.Error,
   Map[T0, T1]]))(lang: hydra.coders.Language)(doInfer: Boolean)(doExpand: Boolean)(doHoistCaseStatements: Boolean)(doHoistPolymorphicLetBindings: Boolean)(bsGraph: hydra.graph.Graph)(universeModules: Seq[hydra.packaging.Module])(modsToGenerate: Seq[hydra.packaging.Module])(cx: hydra.context.Context): Either[hydra.errors.Error,
   Seq[Tuple2[T0, T1]]] =
  {
  lazy val namespaceMap: Map[hydra.packaging.Namespace, hydra.packaging.Module] = hydra.lib.maps.fromList[hydra.packaging.Namespace,
     hydra.packaging.Module](hydra.lib.lists.map[hydra.packaging.Module, Tuple2[hydra.packaging.Namespace,
     hydra.packaging.Module]]((m: hydra.packaging.Module) => Tuple2(m.namespace, m))(hydra.lib.lists.concat2[hydra.packaging.Module](universeModules)(modsToGenerate)))
  lazy val constraints: hydra.coders.LanguageConstraints = (lang.constraints)
  lazy val typeModulesToGenerate: Seq[hydra.packaging.Module] = hydra.lib.lists.filter[hydra.packaging.Module]((mod: hydra.packaging.Module) =>
    hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Binding](hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Definition,
       Option[hydra.core.Binding]]((d: hydra.packaging.Definition) =>
    d match
    case hydra.packaging.Definition.`type`(v_Definition_type_td) => Some({
      lazy val schemaTerm: hydra.core.Term = hydra.core.Term.variable("hydra.core.Type")
      {
        lazy val dataTerm: hydra.core.Term = hydra.annotations.normalizeTermAnnotations(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.encode.core.`type`(v_Definition_type_td.`type`.`type`),
           hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Term](Seq(Tuple2(hydra.constants.key_type,
           schemaTerm))))))
        hydra.core.Binding(v_Definition_type_td.name, dataTerm, Some(hydra.core.TypeScheme(Seq(),
           hydra.core.Type.variable("hydra.core.Type"), None)))
      }
    })
    case _ => None)(mod.definitions)))))(modsToGenerate)
  lazy val termModulesToGenerate: Seq[hydra.packaging.Module] = hydra.lib.lists.filter[hydra.packaging.Module]((mod: hydra.packaging.Module) =>
    hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Binding](hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Definition,
       Option[hydra.core.Binding]]((d: hydra.packaging.Definition) =>
    d match
    case hydra.packaging.Definition.term(v_Definition_term_td) => Some(hydra.core.Binding(v_Definition_term_td.name,
       (v_Definition_term_td.term), (v_Definition_term_td.`type`)))
    case _ => None)(mod.definitions)))))(modsToGenerate)
  lazy val schemaMods: Seq[hydra.packaging.Module] = hydra.codegen.moduleTypeDepsTransitive(namespaceMap)(modsToGenerate)
  lazy val schemaElements: Seq[hydra.core.Binding] = hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Module,
     Seq[hydra.core.Binding]]((m: hydra.packaging.Module) =>
    hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Definition,
       Option[hydra.core.Binding]]((d: hydra.packaging.Definition) =>
    d match
    case hydra.packaging.Definition.`type`(v_Definition_type_td) => Some({
      lazy val schemaTerm: hydra.core.Term = hydra.core.Term.variable("hydra.core.Type")
      {
        lazy val dataTerm: hydra.core.Term = hydra.annotations.normalizeTermAnnotations(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.encode.core.`type`(v_Definition_type_td.`type`.`type`),
           hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Term](Seq(Tuple2(hydra.constants.key_type,
           schemaTerm))))))
        hydra.core.Binding(v_Definition_type_td.name, dataTerm, Some(hydra.core.TypeScheme(Seq(),
           hydra.core.Type.variable("hydra.core.Type"), None)))
      }
    })
    case _ => None)(m.definitions)))(hydra.lib.lists.concat2[hydra.packaging.Module](schemaMods)(typeModulesToGenerate)))
  lazy val dataMods: Seq[hydra.packaging.Module] = hydra.codegen.moduleTermDepsTransitive(namespaceMap)(modsToGenerate)
  lazy val dataElements: Seq[hydra.core.Binding] = hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Module,
     Seq[hydra.core.Binding]]((m: hydra.packaging.Module) =>
    hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Definition,
       Option[hydra.core.Binding]]((d: hydra.packaging.Definition) =>
    d match
    case hydra.packaging.Definition.term(v_Definition_term_td) => Some(hydra.core.Binding(v_Definition_term_td.name,
       (v_Definition_term_td.term), (v_Definition_term_td.`type`)))
    case _ => None)(m.definitions)))(dataMods))
  lazy val schemaGraph: hydra.graph.Graph = hydra.lexical.elementsToGraph(bsGraph)(hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeScheme])(schemaElements)
  lazy val schemaTypes2: Map[hydra.core.Name, hydra.core.TypeScheme] = hydra.lib.eithers.either[hydra.errors.Error,
     Map[hydra.core.Name, hydra.core.TypeScheme], Map[hydra.core.Name, hydra.core.TypeScheme]]((_x: hydra.errors.Error) => hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeScheme])((_r: Map[hydra.core.Name, hydra.core.TypeScheme]) => _r)(hydra.environment.schemaGraphToTypingEnvironment(schemaGraph))
  lazy val dataGraph: hydra.graph.Graph = hydra.lexical.elementsToGraph(bsGraph)(schemaTypes2)(dataElements)
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[Tuple2[T0, T1]], Seq[Tuple2[T0, T1]]](hydra.lib.logic.ifElse[Either[hydra.errors.Error,
     Seq[Tuple2[T0, T1]]]](hydra.lib.lists.`null`[hydra.packaging.Module](typeModulesToGenerate))(Right(Seq()))({
    lazy val nameLists: Seq[Seq[hydra.core.Name]] = hydra.lib.lists.map[hydra.packaging.Module,
       Seq[hydra.core.Name]]((m: hydra.packaging.Module) =>
      hydra.lib.maybes.cat[hydra.core.Name](hydra.lib.lists.map[hydra.packaging.Definition,
         Option[hydra.core.Name]]((d: hydra.packaging.Definition) =>
      d match
      case hydra.packaging.Definition.`type`(v_Definition_type_td) => Some(v_Definition_type_td.name)
      case _ => None)(m.definitions)))(typeModulesToGenerate)
    hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[Map[hydra.core.Name, hydra.core.Type],
       Seq[Seq[hydra.packaging.TypeDefinition]]], Seq[Tuple2[T0, T1]]](hydra.adapt.schemaGraphToDefinitions(constraints)(schemaGraph)(nameLists)(cx))((schemaResult: Tuple2[Map[hydra.core.Name,
       hydra.core.Type], Seq[Seq[hydra.packaging.TypeDefinition]]]) =>
      {
      lazy val defLists: Seq[Seq[hydra.packaging.TypeDefinition]] = hydra.lib.pairs.second[Map[hydra.core.Name,
         hydra.core.Type], Seq[Seq[hydra.packaging.TypeDefinition]]](schemaResult)
      {
        lazy val schemaGraphWithTypes: hydra.graph.Graph = hydra.graph.Graph(schemaGraph.boundTerms,
           (schemaGraph.boundTypes), (schemaGraph.classConstraints), (schemaGraph.lambdaVariables),
           (schemaGraph.metadata), (schemaGraph.primitives), schemaTypes2, (schemaGraph.typeVariables))
        hydra.lib.eithers.map[Seq[Seq[Tuple2[T0, T1]]], Seq[Tuple2[T0, T1]], hydra.errors.Error]((xs: Seq[Seq[Tuple2[T0,
           T1]]]) => hydra.lib.lists.concat[Tuple2[T0, T1]](xs))(hydra.lib.eithers.mapList[Tuple2[hydra.packaging.Module,
           Seq[hydra.packaging.TypeDefinition]], Seq[Tuple2[T0, T1]], hydra.errors.Error]((p: Tuple2[hydra.packaging.Module,
           Seq[hydra.packaging.TypeDefinition]]) =>
          {
          lazy val mod: hydra.packaging.Module = hydra.lib.pairs.first[hydra.packaging.Module,
             Seq[hydra.packaging.TypeDefinition]](p)
          {
            lazy val defs: Seq[hydra.packaging.TypeDefinition] = hydra.lib.pairs.second[hydra.packaging.Module,
               Seq[hydra.packaging.TypeDefinition]](p)
            hydra.lib.eithers.map[Map[T0, T1], Seq[Tuple2[T0, T1]], hydra.errors.Error]((m: Map[T0,
               T1]) => hydra.lib.maps.toList[T0, T1](m))(printDefinitions(mod)(hydra.lib.lists.map[hydra.packaging.TypeDefinition,
               hydra.packaging.Definition]((d: hydra.packaging.TypeDefinition) => hydra.packaging.Definition.`type`(d))(defs))(cx)(schemaGraphWithTypes))
          }
        })(hydra.lib.lists.zip[hydra.packaging.Module, Seq[hydra.packaging.TypeDefinition]](typeModulesToGenerate)(defLists)))
      }
    })
  }))((schemaFiles: Seq[Tuple2[T0, T1]]) =>
    hydra.lib.eithers.bind[hydra.errors.Error, Seq[Tuple2[T0, T1]], Seq[Tuple2[T0,
       T1]]](hydra.lib.logic.ifElse[Either[hydra.errors.Error, Seq[Tuple2[T0, T1]]]](hydra.lib.lists.`null`[hydra.packaging.Module](termModulesToGenerate))(Right(Seq()))({
    lazy val namespaces: Seq[hydra.packaging.Namespace] = hydra.lib.lists.map[hydra.packaging.Module,
       hydra.packaging.Namespace]((m: hydra.packaging.Module) => (m.namespace))(termModulesToGenerate)
    hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[hydra.graph.Graph, Seq[Seq[hydra.packaging.TermDefinition]]],
       Seq[Tuple2[T0, T1]]](hydra.adapt.dataGraphToDefinitions(constraints)(doInfer)(doExpand)(doHoistCaseStatements)(doHoistPolymorphicLetBindings)(dataElements)(dataGraph)(namespaces)(cx))((dataResult: Tuple2[hydra.graph.Graph,
       Seq[Seq[hydra.packaging.TermDefinition]]]) =>
      {
      lazy val g1: hydra.graph.Graph = hydra.lib.pairs.first[hydra.graph.Graph, Seq[Seq[hydra.packaging.TermDefinition]]](dataResult)
      {
        lazy val defLists: Seq[Seq[hydra.packaging.TermDefinition]] = hydra.lib.pairs.second[hydra.graph.Graph,
           Seq[Seq[hydra.packaging.TermDefinition]]](dataResult)
        {
          def defName(d: hydra.packaging.Definition): hydra.core.Name =
            d match
            case hydra.packaging.Definition.term(v_Definition_term_td) => (v_Definition_term_td.name)
            case hydra.packaging.Definition.`type`(v_Definition_type_td) => (v_Definition_type_td.name)
          {
            def refreshModule(els: Seq[hydra.core.Binding])(m: hydra.packaging.Module): hydra.packaging.Module =
              hydra.packaging.Module(m.namespace, hydra.lib.maybes.cat[hydra.packaging.Definition](hydra.lib.lists.map[hydra.packaging.Definition,
                 Option[hydra.packaging.Definition]]((d: hydra.packaging.Definition) =>
              d match
              case hydra.packaging.Definition.`type`(v_Definition_type_td) => Some(hydra.packaging.Definition.`type`(v_Definition_type_td))
              case hydra.packaging.Definition.term(v_Definition_term_td) => hydra.lib.maybes.map[hydra.core.Binding,
                 hydra.packaging.Definition]((b: hydra.core.Binding) =>
                hydra.packaging.Definition.term(hydra.packaging.TermDefinition(b.name,
                   (b.term), (b.`type`))))(hydra.lib.lists.find[hydra.core.Binding]((b: hydra.core.Binding) =>
                hydra.lib.equality.equal[hydra.core.Name](b.name)(v_Definition_term_td.name))(els)))(m.definitions)),
                   (m.termDependencies), (m.typeDependencies), (m.description))
            {
              lazy val allBindings: Seq[hydra.core.Binding] = hydra.lexical.graphToBindings(g1)
              {
                lazy val refreshedMods: Seq[hydra.packaging.Module] = hydra.lib.lists.map[hydra.packaging.Module,
                   hydra.packaging.Module]((m: hydra.packaging.Module) => refreshModule(allBindings)(m))(termModulesToGenerate)
                {
                  def dedupDefs(defs: Seq[hydra.packaging.TermDefinition]): Seq[hydra.packaging.TermDefinition] =
                    hydra.lib.maps.elems[hydra.core.Name, hydra.packaging.TermDefinition](hydra.lib.maps.fromList[hydra.core.Name,
                       hydra.packaging.TermDefinition](hydra.lib.lists.map[hydra.packaging.TermDefinition,
                       Tuple2[hydra.core.Name, hydra.packaging.TermDefinition]]((d: hydra.packaging.TermDefinition) => Tuple2(d.name,
                       d))(defs)))
                  {
                    lazy val dedupedDefLists: Seq[Seq[hydra.packaging.TermDefinition]] = hydra.lib.lists.map[Seq[hydra.packaging.TermDefinition],
                       Seq[hydra.packaging.TermDefinition]](dedupDefs)(defLists)
                    hydra.lib.eithers.map[Seq[Seq[Tuple2[T0, T1]]], Seq[Tuple2[T0,
                       T1]], hydra.errors.Error]((xs: Seq[Seq[Tuple2[T0, T1]]]) => hydra.lib.lists.concat[Tuple2[T0,
                       T1]](xs))(hydra.lib.eithers.mapList[Tuple2[hydra.packaging.Module,
                       Seq[hydra.packaging.TermDefinition]], Seq[Tuple2[T0, T1]],
                       hydra.errors.Error]((p: Tuple2[hydra.packaging.Module, Seq[hydra.packaging.TermDefinition]]) =>
                      {
                      lazy val mod: hydra.packaging.Module = hydra.lib.pairs.first[hydra.packaging.Module,
                         Seq[hydra.packaging.TermDefinition]](p)
                      {
                        lazy val defs: Seq[hydra.packaging.TermDefinition] = hydra.lib.pairs.second[hydra.packaging.Module,
                           Seq[hydra.packaging.TermDefinition]](p)
                        hydra.lib.eithers.map[Map[T0, T1], Seq[Tuple2[T0, T1]], hydra.errors.Error]((m: Map[T0,
                           T1]) => hydra.lib.maps.toList[T0, T1](m))(printDefinitions(mod)(hydra.lib.lists.map[hydra.packaging.TermDefinition,
                           hydra.packaging.Definition]((d: hydra.packaging.TermDefinition) => hydra.packaging.Definition.term(d))(defs))(cx)(g1))
                      }
                    })(hydra.lib.lists.zip[hydra.packaging.Module, Seq[hydra.packaging.TermDefinition]](refreshedMods)(dedupedDefLists)))
                  }
                }
              }
            }
          }
        }
      }
    })
  }))((termFiles: Seq[Tuple2[T0, T1]]) =>
    Right(hydra.lib.lists.concat2[Tuple2[T0, T1]](schemaFiles)(termFiles))))
}

def inferAndGenerateLexicon(cx: hydra.context.Context)(bsGraph: hydra.graph.Graph)(kernelModules: Seq[hydra.packaging.Module]): Either[hydra.errors.Error,
   scala.Predef.String] =
  {
  lazy val g0: hydra.graph.Graph = hydra.codegen.modulesToGraph(bsGraph)(kernelModules)(kernelModules)
  lazy val dataElements: Seq[hydra.core.Binding] = hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Module,
     Seq[hydra.core.Binding]]((m: hydra.packaging.Module) =>
    hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Definition,
       Option[hydra.core.Binding]]((d: hydra.packaging.Definition) =>
    d match
    case hydra.packaging.Definition.term(v_Definition_term_td) => Some(hydra.core.Binding(v_Definition_term_td.name,
       (v_Definition_term_td.term), (v_Definition_term_td.`type`)))
    case _ => None)(m.definitions)))(kernelModules))
  hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]],
     hydra.context.Context], scala.Predef.String](hydra.inference.inferGraphTypes(cx)(dataElements)(g0))((inferResultWithCx: Tuple2[Tuple2[hydra.graph.Graph,
     Seq[hydra.core.Binding]], hydra.context.Context]) =>
    {
    lazy val g1: hydra.graph.Graph = hydra.lib.pairs.first[hydra.graph.Graph, Seq[hydra.core.Binding]](hydra.lib.pairs.first[Tuple2[hydra.graph.Graph,
       Seq[hydra.core.Binding]], hydra.context.Context](inferResultWithCx))
    hydra.codegen.generateLexicon(g1)
  })
}

def inferModules(cx: hydra.context.Context)(bsGraph: hydra.graph.Graph)(universeMods: Seq[hydra.packaging.Module])(targetMods: Seq[hydra.packaging.Module]): Either[hydra.errors.Error,
   Seq[hydra.packaging.Module]] =
  {
  lazy val g0: hydra.graph.Graph = hydra.codegen.modulesToGraph(bsGraph)(universeMods)(universeMods)
  lazy val dataElements: Seq[hydra.core.Binding] = hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Module,
     Seq[hydra.core.Binding]]((m: hydra.packaging.Module) =>
    hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Definition,
       Option[hydra.core.Binding]]((d: hydra.packaging.Definition) =>
    d match
    case hydra.packaging.Definition.term(v_Definition_term_td) => Some(hydra.core.Binding(v_Definition_term_td.name,
       (v_Definition_term_td.term), (v_Definition_term_td.`type`)))
    case _ => None)(m.definitions)))(universeMods))
  hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]],
     hydra.context.Context], Seq[hydra.packaging.Module]](hydra.inference.inferGraphTypes(cx)(dataElements)(g0))((inferResultWithCx: Tuple2[Tuple2[hydra.graph.Graph,
     Seq[hydra.core.Binding]], hydra.context.Context]) =>
    {
    lazy val inferResult: Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]] = hydra.lib.pairs.first[Tuple2[hydra.graph.Graph,
       Seq[hydra.core.Binding]], hydra.context.Context](inferResultWithCx)
    {
      lazy val inferredElements: Seq[hydra.core.Binding] = hydra.lib.pairs.second[hydra.graph.Graph,
         Seq[hydra.core.Binding]](inferResult)
      Right(hydra.lib.lists.map[hydra.packaging.Module, hydra.packaging.Module]((v1: hydra.packaging.Module) => hydra.codegen.refreshModule(inferredElements)(v1))(targetMods))
    }
  })
}

def inferModulesGiven(cx: hydra.context.Context)(bsGraph: hydra.graph.Graph)(universeMods: Seq[hydra.packaging.Module])(targetMods: Seq[hydra.packaging.Module]): Either[hydra.errors.Error,
   Seq[hydra.packaging.Module]] =
  {
  lazy val g0: hydra.graph.Graph = hydra.codegen.modulesToGraph(bsGraph)(universeMods)(universeMods)
  lazy val dataElements: Seq[hydra.core.Binding] = hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Module,
     Seq[hydra.core.Binding]]((m: hydra.packaging.Module) =>
    hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Definition,
       Option[hydra.core.Binding]]((d: hydra.packaging.Definition) =>
    d match
    case hydra.packaging.Definition.term(v_Definition_term_td) => Some(hydra.core.Binding(v_Definition_term_td.name,
       (v_Definition_term_td.term), (v_Definition_term_td.`type`)))
    case _ => None)(m.definitions)))(universeMods))
  hydra.lib.eithers.bind[hydra.errors.Error, Tuple2[Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]],
     hydra.context.Context], Seq[hydra.packaging.Module]](hydra.inference.inferGraphTypes(cx)(dataElements)(g0))((inferResultWithCx: Tuple2[Tuple2[hydra.graph.Graph,
     Seq[hydra.core.Binding]], hydra.context.Context]) =>
    {
    lazy val inferResult: Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]] = hydra.lib.pairs.first[Tuple2[hydra.graph.Graph,
       Seq[hydra.core.Binding]], hydra.context.Context](inferResultWithCx)
    {
      lazy val inferredElements: Seq[hydra.core.Binding] = hydra.lib.pairs.second[hydra.graph.Graph,
         Seq[hydra.core.Binding]](inferResult)
      Right(hydra.lib.lists.map[hydra.packaging.Module, hydra.packaging.Module]((v1: hydra.packaging.Module) => hydra.codegen.refreshModule(inferredElements)(v1))(targetMods))
    }
  })
}

def moduleTermDepsTransitive(nsMap: Map[hydra.packaging.Namespace, hydra.packaging.Module])(modules: Seq[hydra.packaging.Module]): Seq[hydra.packaging.Module] =
  {
  lazy val closure: scala.collection.immutable.Set[hydra.packaging.Namespace] = hydra.lib.sets.union[hydra.packaging.Namespace](hydra.codegen.transitiveDeps((m: hydra.packaging.Module) => (m.termDependencies))(nsMap)(modules))(hydra.lib.sets.fromList[hydra.packaging.Namespace](hydra.lib.lists.map[hydra.packaging.Module,
     hydra.packaging.Namespace]((m: hydra.packaging.Module) => (m.namespace))(modules)))
  hydra.lib.maybes.cat[hydra.packaging.Module](hydra.lib.lists.map[hydra.packaging.Namespace,
     Option[hydra.packaging.Module]]((n: hydra.packaging.Namespace) =>
    hydra.lib.maps.lookup[hydra.packaging.Namespace, hydra.packaging.Module](n)(nsMap))(hydra.lib.sets.toList[hydra.packaging.Namespace](closure)))
}

def moduleToJson(schemaMap: Map[hydra.core.Name, hydra.core.Type])(m: hydra.packaging.Module): Either[hydra.errors.Error,
   scala.Predef.String] =
  {
  lazy val term: hydra.core.Term = hydra.encode.packaging.module(m)
  lazy val modType: hydra.core.Type = hydra.core.Type.variable("hydra.packaging.Module")
  hydra.lib.eithers.map[hydra.json.model.Value, scala.Predef.String, hydra.errors.Error]((json: hydra.json.model.Value) => hydra.json.writer.printJson(json))(hydra.lib.eithers.bimap[scala.Predef.String,
     hydra.json.model.Value, hydra.errors.Error, hydra.json.model.Value]((_e: scala.Predef.String) => hydra.errors.Error.other(_e))((_a: hydra.json.model.Value) => _a)(hydra.json.encode.toJson(schemaMap)("hydra.packaging.Module")(modType)(term)))
}

def moduleToSourceModule(m: hydra.packaging.Module): hydra.packaging.Module =
  {
  lazy val sourceNs: hydra.packaging.Namespace = hydra.lib.strings.cat2("hydra.sources.")(hydra.lib.strings.intercalate(".")(hydra.lib.lists.drop[scala.Predef.String](1)(hydra.lib.strings.splitOn(".")(m.namespace))))
  lazy val modTypeNs: hydra.packaging.Namespace = "hydra.packaging"
  lazy val moduleDef: hydra.packaging.Definition = hydra.packaging.Definition.term(hydra.packaging.TermDefinition(hydra.lib.strings.cat2(sourceNs)(".module_"),
     hydra.encode.packaging.module(m), None))
  hydra.packaging.Module(sourceNs, Seq(moduleDef), Seq(modTypeNs), Seq(modTypeNs),
     Some(hydra.lib.strings.cat2("Source module for ")(m.namespace)))
}

def moduleTypeDepsTransitive(nsMap: Map[hydra.packaging.Namespace, hydra.packaging.Module])(modules: Seq[hydra.packaging.Module]): Seq[hydra.packaging.Module] =
  {
  lazy val termMods: Seq[hydra.packaging.Module] = hydra.codegen.moduleTermDepsTransitive(nsMap)(modules)
  lazy val typeNamespaces: Seq[hydra.packaging.Namespace] = hydra.lib.sets.toList[hydra.packaging.Namespace](hydra.codegen.transitiveDeps((m: hydra.packaging.Module) => (m.typeDependencies))(nsMap)(termMods))
  hydra.lib.maybes.cat[hydra.packaging.Module](hydra.lib.lists.map[hydra.packaging.Namespace,
     Option[hydra.packaging.Module]]((n: hydra.packaging.Namespace) =>
    hydra.lib.maps.lookup[hydra.packaging.Namespace, hydra.packaging.Module](n)(nsMap))(typeNamespaces))
}

def modulesToGraph(bsGraph: hydra.graph.Graph)(universeModules: Seq[hydra.packaging.Module])(modules: Seq[hydra.packaging.Module]): hydra.graph.Graph =
  {
  lazy val universe: Map[hydra.packaging.Namespace, hydra.packaging.Module] = hydra.lib.maps.fromList[hydra.packaging.Namespace,
     hydra.packaging.Module](hydra.lib.lists.map[hydra.packaging.Module, Tuple2[hydra.packaging.Namespace,
     hydra.packaging.Module]]((m: hydra.packaging.Module) => Tuple2(m.namespace, m))(hydra.lib.lists.concat2[hydra.packaging.Module](universeModules)(modules)))
  lazy val schemaModules: Seq[hydra.packaging.Module] = hydra.codegen.moduleTypeDepsTransitive(universe)(modules)
  lazy val dataModules: Seq[hydra.packaging.Module] = hydra.codegen.moduleTermDepsTransitive(universe)(modules)
  lazy val schemaElements: Seq[hydra.core.Binding] = hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Module,
     Seq[hydra.core.Binding]]((m: hydra.packaging.Module) =>
    hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Definition,
       Option[hydra.core.Binding]]((d: hydra.packaging.Definition) =>
    d match
    case hydra.packaging.Definition.`type`(v_Definition_type_td) => Some({
      lazy val schemaTerm: hydra.core.Term = hydra.core.Term.variable("hydra.core.Type")
      {
        lazy val dataTerm: hydra.core.Term = hydra.annotations.normalizeTermAnnotations(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.encode.core.`type`(v_Definition_type_td.`type`.`type`),
           hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Term](Seq(Tuple2(hydra.constants.key_type,
           schemaTerm))))))
        hydra.core.Binding(v_Definition_type_td.name, dataTerm, Some(hydra.core.TypeScheme(Seq(),
           hydra.core.Type.variable("hydra.core.Type"), None)))
      }
    })
    case _ => None)(m.definitions)))(hydra.lib.lists.concat2[hydra.packaging.Module](schemaModules)(modules)))
  lazy val dataElements: Seq[hydra.core.Binding] = hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Module,
     Seq[hydra.core.Binding]]((m: hydra.packaging.Module) =>
    hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Definition,
       Option[hydra.core.Binding]]((d: hydra.packaging.Definition) =>
    d match
    case hydra.packaging.Definition.term(v_Definition_term_td) => Some(hydra.core.Binding(v_Definition_term_td.name,
       (v_Definition_term_td.term), (v_Definition_term_td.`type`)))
    case _ => None)(m.definitions)))(dataModules))
  lazy val schemaGraph: hydra.graph.Graph = hydra.lexical.elementsToGraph(bsGraph)(hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeScheme])(schemaElements)
  lazy val schemaTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = hydra.lib.eithers.either[hydra.errors.Error,
     Map[hydra.core.Name, hydra.core.TypeScheme], Map[hydra.core.Name, hydra.core.TypeScheme]]((_x: hydra.errors.Error) => hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeScheme])((_r: Map[hydra.core.Name, hydra.core.TypeScheme]) => _r)(hydra.environment.schemaGraphToTypingEnvironment(schemaGraph))
  lazy val baseGraph: hydra.graph.Graph = hydra.lexical.elementsToGraph(bsGraph)(schemaTypes)(dataElements)
  lazy val universeDataElements: Seq[hydra.core.Binding] = hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Module,
     Seq[hydra.core.Binding]]((m: hydra.packaging.Module) =>
    hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Definition,
       Option[hydra.core.Binding]]((d: hydra.packaging.Definition) =>
    d match
    case hydra.packaging.Definition.term(v_Definition_term_td) => Some(hydra.core.Binding(v_Definition_term_td.name,
       (v_Definition_term_td.term), (v_Definition_term_td.`type`)))
    case _ => None)(m.definitions)))(universeModules))
  lazy val universeBoundTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = hydra.lib.maps.fromList[hydra.core.Name,
     hydra.core.TypeScheme](hydra.lib.maybes.cat[Tuple2[hydra.core.Name, hydra.core.TypeScheme]](hydra.lib.lists.map[hydra.core.Binding,
     Option[Tuple2[hydra.core.Name, hydra.core.TypeScheme]]]((b: hydra.core.Binding) =>
    hydra.lib.maybes.map[hydra.core.TypeScheme, Tuple2[hydra.core.Name, hydra.core.TypeScheme]]((ts: hydra.core.TypeScheme) => Tuple2(b.name,
       ts))(b.`type`))(universeDataElements)))
  hydra.graph.Graph(baseGraph.boundTerms, universeBoundTypes, (baseGraph.classConstraints),
     (baseGraph.lambdaVariables), (baseGraph.metadata), (baseGraph.primitives), (baseGraph.schemaTypes),
     (baseGraph.typeVariables))
}

def namespaceToPath(ns: hydra.packaging.Namespace): scala.Predef.String = hydra.lib.strings.intercalate("/")(hydra.lib.strings.splitOn(".")(ns))

def refreshModule(inferredElements: Seq[hydra.core.Binding])(m: hydra.packaging.Module): hydra.packaging.Module =
  hydra.lib.logic.ifElse[hydra.packaging.Module](hydra.lib.logic.not(hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Binding](hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.packaging.Definition,
     Option[hydra.core.Binding]]((d: hydra.packaging.Definition) =>
  d match
  case hydra.packaging.Definition.term(v_Definition_term_td) => Some(hydra.core.Binding(v_Definition_term_td.name,
     (v_Definition_term_td.term), (v_Definition_term_td.`type`)))
  case _ => None)(m.definitions))))))(m)(hydra.packaging.Module(m.namespace, hydra.lib.maybes.cat[hydra.packaging.Definition](hydra.lib.lists.map[hydra.packaging.Definition,
     Option[hydra.packaging.Definition]]((d: hydra.packaging.Definition) =>
  d match
  case hydra.packaging.Definition.`type`(v_Definition_type_td) => Some(hydra.packaging.Definition.`type`(v_Definition_type_td))
  case hydra.packaging.Definition.term(v_Definition_term_td) => hydra.lib.maybes.map[hydra.core.Binding,
     hydra.packaging.Definition]((b: hydra.core.Binding) =>
    hydra.packaging.Definition.term(hydra.packaging.TermDefinition(b.name, (b.term),
       (b.`type`))))(hydra.lib.lists.find[hydra.core.Binding]((b: hydra.core.Binding) =>
    hydra.lib.equality.equal[hydra.core.Name](b.name)(v_Definition_term_td.name))(inferredElements)))(m.definitions)),
       (m.termDependencies), (m.typeDependencies), (m.description)))

def transitiveDeps(getDeps: (hydra.packaging.Module => Seq[hydra.packaging.Namespace]))(nsMap: Map[hydra.packaging.Namespace,
   hydra.packaging.Module])(startMods: Seq[hydra.packaging.Module]): scala.collection.immutable.Set[hydra.packaging.Namespace] =
  {
  lazy val initialDeps: scala.collection.immutable.Set[hydra.packaging.Namespace] = hydra.lib.sets.fromList[hydra.packaging.Namespace](hydra.lib.lists.concat[hydra.packaging.Namespace](hydra.lib.lists.map[hydra.packaging.Module,
     Seq[hydra.packaging.Namespace]]((m: hydra.packaging.Module) =>
    hydra.lib.lists.filter[hydra.packaging.Namespace]((dep: hydra.packaging.Namespace) =>
    hydra.lib.logic.not(hydra.lib.equality.equal[hydra.packaging.Namespace](dep)(m.namespace)))(getDeps(m)))(startMods)))
  def go(pending: scala.collection.immutable.Set[hydra.packaging.Namespace])(visited: scala.collection.immutable.Set[hydra.packaging.Namespace]): scala.collection.immutable.Set[hydra.packaging.Namespace] =
    hydra.lib.logic.ifElse[scala.collection.immutable.Set[hydra.packaging.Namespace]](hydra.lib.sets.`null`[hydra.packaging.Namespace](pending))(visited)({
    lazy val newVisited: scala.collection.immutable.Set[hydra.packaging.Namespace] = hydra.lib.sets.union[hydra.packaging.Namespace](visited)(pending)
    {
      lazy val nextDeps: scala.collection.immutable.Set[hydra.packaging.Namespace] = hydra.lib.sets.fromList[hydra.packaging.Namespace](hydra.lib.lists.concat[hydra.packaging.Namespace](hydra.lib.lists.map[hydra.packaging.Namespace,
         Seq[hydra.packaging.Namespace]]((nsv: hydra.packaging.Namespace) =>
        hydra.lib.maybes.maybe[Seq[hydra.packaging.Namespace], hydra.packaging.Module](Seq())((depMod: hydra.packaging.Module) => getDeps(depMod))(hydra.lib.maps.lookup[hydra.packaging.Namespace,
           hydra.packaging.Module](nsv)(nsMap)))(hydra.lib.sets.toList[hydra.packaging.Namespace](pending))))
      {
        lazy val newPending: scala.collection.immutable.Set[hydra.packaging.Namespace] = hydra.lib.sets.difference[hydra.packaging.Namespace](nextDeps)(newVisited)
        go(newPending)(newVisited)
      }
    }
  })
  go(initialDeps)(hydra.lib.sets.empty[hydra.packaging.Namespace])
}
