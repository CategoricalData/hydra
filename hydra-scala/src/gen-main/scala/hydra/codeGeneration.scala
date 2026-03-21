package hydra.codeGeneration

import hydra.coders.*

import hydra.context.*

import hydra.core.*

import hydra.error.*

import hydra.graph.*

import hydra.module.*

import hydra.lib.eithers

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.math

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

def namespaceToPath(ns: hydra.module.Namespace): scala.Predef.String = strings.intercalate("/")(strings.splitOn(".")(ns))

def stripModuleTypeSchemes(m: hydra.module.Module): hydra.module.Module =
  {
  def stripIfTerm(b: hydra.core.Binding): hydra.core.Binding =
    logic.ifElse[hydra.core.Binding](hydra.annotations.isNativeType(b))(b)(hydra.core.Binding(b.name, (b.term), None))
  hydra.module.Module(m.namespace, lists.map[hydra.core.Binding, hydra.core.Binding](stripIfTerm)(m.elements),
     (m.termDependencies), (m.typeDependencies), (m.description))
}

def transitiveDeps(getDeps: (hydra.module.Module => Seq[hydra.module.Namespace]))(nsMap: Map[hydra.module.Namespace,
   hydra.module.Module])(startMods: Seq[hydra.module.Module]): scala.collection.immutable.Set[hydra.module.Namespace] =
  {
  val initialDeps: scala.collection.immutable.Set[hydra.module.Namespace] = sets.fromList[hydra.module.Namespace](lists.concat[hydra.module.Namespace](lists.map[hydra.module.Module,
     Seq[hydra.module.Namespace]]((m: hydra.module.Module) =>
    lists.filter[hydra.module.Namespace]((dep: hydra.module.Namespace) =>
    logic.not(equality.equal[hydra.module.Namespace](dep)(m.namespace)))(getDeps(m)))(startMods)))
  def go(pending: scala.collection.immutable.Set[hydra.module.Namespace])(visited: scala.collection.immutable.Set[hydra.module.Namespace]): scala.collection.immutable.Set[hydra.module.Namespace] =
    logic.ifElse[scala.collection.immutable.Set[hydra.module.Namespace]](sets.`null`[hydra.module.Namespace](pending))(visited)({
    val newVisited: scala.collection.immutable.Set[hydra.module.Namespace] = sets.union[hydra.module.Namespace](visited)(pending)
    {
      val nextDeps: scala.collection.immutable.Set[hydra.module.Namespace] = sets.fromList[hydra.module.Namespace](lists.concat[hydra.module.Namespace](lists.map[hydra.module.Namespace,
         Seq[hydra.module.Namespace]]((nsv: hydra.module.Namespace) =>
        maybes.maybe[Seq[hydra.module.Namespace], hydra.module.Module](Seq())((depMod: hydra.module.Module) => getDeps(depMod))(maps.lookup[hydra.module.Namespace,
           hydra.module.Module](nsv)(nsMap)))(sets.toList[hydra.module.Namespace](pending))))
      {
        val newPending: scala.collection.immutable.Set[hydra.module.Namespace] = sets.difference[hydra.module.Namespace](nextDeps)(newVisited)
        go(newPending)(newVisited)
      }
    }
  })
  go(initialDeps)(sets.empty[hydra.module.Namespace])
}

def moduleTermDepsTransitive(nsMap: Map[hydra.module.Namespace, hydra.module.Module])(modules: Seq[hydra.module.Module]): Seq[hydra.module.Module] =
  {
  val closure: scala.collection.immutable.Set[hydra.module.Namespace] = sets.union[hydra.module.Namespace](hydra.codeGeneration.transitiveDeps((m: hydra.module.Module) => (m.termDependencies))(nsMap)(modules))(sets.fromList[hydra.module.Namespace](lists.map[hydra.module.Module,
     hydra.module.Namespace]((m: hydra.module.Module) => (m.namespace))(modules)))
  maybes.cat[hydra.module.Module](lists.map[hydra.module.Namespace, Option[hydra.module.Module]]((n: hydra.module.Namespace) =>
    maps.lookup[hydra.module.Namespace, hydra.module.Module](n)(nsMap))(sets.toList[hydra.module.Namespace](closure)))
}

def moduleTypeDepsTransitive(nsMap: Map[hydra.module.Namespace, hydra.module.Module])(modules: Seq[hydra.module.Module]): Seq[hydra.module.Module] =
  {
  val termMods: Seq[hydra.module.Module] = hydra.codeGeneration.moduleTermDepsTransitive(nsMap)(modules)
  val typeNamespaces: Seq[hydra.module.Namespace] = sets.toList[hydra.module.Namespace](hydra.codeGeneration.transitiveDeps((m: hydra.module.Module) => (m.typeDependencies))(nsMap)(termMods))
  maybes.cat[hydra.module.Module](lists.map[hydra.module.Namespace, Option[hydra.module.Module]]((n: hydra.module.Namespace) =>
    maps.lookup[hydra.module.Namespace, hydra.module.Module](n)(nsMap))(typeNamespaces))
}

def modulesToGraph(bsGraph: hydra.graph.Graph)(universeModules: Seq[hydra.module.Module])(modules: Seq[hydra.module.Module]): hydra.graph.Graph =
  {
  val universe: Map[hydra.module.Namespace, hydra.module.Module] = maps.fromList[hydra.module.Namespace,
     hydra.module.Module](lists.map[hydra.module.Module, Tuple2[hydra.module.Namespace, hydra.module.Module]]((m: hydra.module.Module) => Tuple2(m.namespace,
     m))(lists.concat2[hydra.module.Module](universeModules)(modules)))
  val schemaModules: Seq[hydra.module.Module] = hydra.codeGeneration.moduleTypeDepsTransitive(universe)(modules)
  val dataModules: Seq[hydra.module.Module] = hydra.codeGeneration.moduleTermDepsTransitive(universe)(modules)
  val schemaElements: Seq[hydra.core.Binding] = lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => hydra.annotations.isNativeType(e))(lists.concat[hydra.core.Binding](lists.map[hydra.module.Module,
     Seq[hydra.core.Binding]]((m: hydra.module.Module) => (m.elements))(lists.concat2[hydra.module.Module](schemaModules)(modules))))
  val dataElements: Seq[hydra.core.Binding] = lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => logic.not(hydra.annotations.isNativeType(e)))(lists.concat[hydra.core.Binding](lists.map[hydra.module.Module,
     Seq[hydra.core.Binding]]((m: hydra.module.Module) => (m.elements))(dataModules)))
  val schemaGraph: hydra.graph.Graph = hydra.lexical.elementsToGraph(bsGraph)(maps.empty[hydra.core.Name, hydra.core.TypeScheme])(schemaElements)
  val schemaTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = eithers.either[hydra.context.InContext[hydra.error.Error],
     Map[hydra.core.Name, hydra.core.TypeScheme], Map[hydra.core.Name, hydra.core.TypeScheme]]((_x: hydra.context.InContext[hydra.error.Error]) => maps.empty[hydra.core.Name,
     hydra.core.TypeScheme])((_r: Map[hydra.core.Name, hydra.core.TypeScheme]) => _r)(hydra.schemas.schemaGraphToTypingEnvironment(hydra.lexical.emptyContext)(schemaGraph))
  hydra.lexical.elementsToGraph(bsGraph)(schemaTypes)(dataElements)
}

def generateSourceFiles[T0, T1](printDefinitions: (hydra.module.Module => Seq[hydra.module.Definition] => hydra.context.Context => hydra.graph.Graph => Either[hydra.context.InContext[hydra.error.Error],
   Map[T0, T1]]))(lang: hydra.coders.Language)(doInfer: Boolean)(doExpand: Boolean)(doHoistCaseStatements: Boolean)(doHoistPolymorphicLetBindings: Boolean)(bsGraph: hydra.graph.Graph)(universeModules: Seq[hydra.module.Module])(modsToGenerate: Seq[hydra.module.Module])(cx: hydra.context.Context): Either[hydra.context.InContext[hydra.error.Error],
   Seq[Tuple2[T0, T1]]] =
  {
  val namespaceMap: Map[hydra.module.Namespace, hydra.module.Module] = maps.fromList[hydra.module.Namespace,
     hydra.module.Module](lists.map[hydra.module.Module, Tuple2[hydra.module.Namespace, hydra.module.Module]]((m: hydra.module.Module) => Tuple2(m.namespace,
     m))(lists.concat2[hydra.module.Module](universeModules)(modsToGenerate)))
  val constraints: hydra.coders.LanguageConstraints = (lang.constraints)
  def isTypeModule(mod: hydra.module.Module): Boolean =
    logic.not(lists.`null`[hydra.core.Binding](lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => hydra.annotations.isNativeType(e))(mod.elements)))
  val partitioned: Tuple2[Seq[hydra.module.Module], Seq[hydra.module.Module]] = lists.partition[hydra.module.Module](isTypeModule)(modsToGenerate)
  val typeModulesToGenerate: Seq[hydra.module.Module] = pairs.first[Seq[hydra.module.Module], Seq[hydra.module.Module]](partitioned)
  val termModulesToGenerate: Seq[hydra.module.Module] = pairs.second[Seq[hydra.module.Module], Seq[hydra.module.Module]](partitioned)
  val schemaMods: Seq[hydra.module.Module] = hydra.codeGeneration.moduleTypeDepsTransitive(namespaceMap)(modsToGenerate)
  val schemaElements: Seq[hydra.core.Binding] = lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => hydra.annotations.isNativeType(e))(lists.concat[hydra.core.Binding](lists.map[hydra.module.Module,
     Seq[hydra.core.Binding]]((m: hydra.module.Module) => (m.elements))(lists.concat2[hydra.module.Module](schemaMods)(typeModulesToGenerate))))
  val dataMods: Seq[hydra.module.Module] = hydra.codeGeneration.moduleTermDepsTransitive(namespaceMap)(modsToGenerate)
  val dataElements: Seq[hydra.core.Binding] = lists.concat[hydra.core.Binding](lists.map[hydra.module.Module,
     Seq[hydra.core.Binding]]((m: hydra.module.Module) => (m.elements))(dataMods))
  val schemaGraph: hydra.graph.Graph = hydra.lexical.elementsToGraph(bsGraph)(maps.empty[hydra.core.Name, hydra.core.TypeScheme])(schemaElements)
  val schemaTypes2: Map[hydra.core.Name, hydra.core.TypeScheme] = eithers.either[hydra.context.InContext[hydra.error.Error],
     Map[hydra.core.Name, hydra.core.TypeScheme], Map[hydra.core.Name, hydra.core.TypeScheme]]((_x: hydra.context.InContext[hydra.error.Error]) => maps.empty[hydra.core.Name,
     hydra.core.TypeScheme])((_r: Map[hydra.core.Name, hydra.core.TypeScheme]) => _r)(hydra.schemas.schemaGraphToTypingEnvironment(hydra.lexical.emptyContext)(schemaGraph))
  val dataGraph: hydra.graph.Graph = hydra.lexical.elementsToGraph(bsGraph)(schemaTypes2)(dataElements)
  eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[Tuple2[T0, T1]], Seq[Tuple2[T0, T1]]](logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
     Seq[Tuple2[T0, T1]]]](lists.`null`[hydra.module.Module](typeModulesToGenerate))(Right(Seq()))({
    val nameLists: Seq[Seq[hydra.core.Name]] = lists.map[hydra.module.Module, Seq[hydra.core.Name]]((m: hydra.module.Module) =>
      lists.map[hydra.core.Binding, hydra.core.Name]((e: hydra.core.Binding) => (e.name))(lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => hydra.annotations.isNativeType(e))(m.elements)))(typeModulesToGenerate)
    eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Map[hydra.core.Name, hydra.core.Type],
       Seq[Seq[hydra.module.TypeDefinition]]], Seq[Tuple2[T0, T1]]](eithers.bimap[scala.Predef.String,
       Tuple2[Map[hydra.core.Name, hydra.core.Type], Seq[Seq[hydra.module.TypeDefinition]]], hydra.context.InContext[hydra.error.Error],
       Tuple2[Map[hydra.core.Name, hydra.core.Type], Seq[Seq[hydra.module.TypeDefinition]]]]((s: scala.Predef.String) => hydra.context.InContext(hydra.error.Error.other(s),
       cx))((r: Tuple2[Map[hydra.core.Name, hydra.core.Type], Seq[Seq[hydra.module.TypeDefinition]]]) => r)(hydra.adapt.schemaGraphToDefinitions(constraints)(schemaGraph)(nameLists)(cx)))((schemaResult: Tuple2[Map[hydra.core.Name,
       hydra.core.Type], Seq[Seq[hydra.module.TypeDefinition]]]) =>
      {
      val defLists: Seq[Seq[hydra.module.TypeDefinition]] = pairs.second[Map[hydra.core.Name, hydra.core.Type],
         Seq[Seq[hydra.module.TypeDefinition]]](schemaResult)
      {
        val schemaGraphWithTypes: hydra.graph.Graph = hydra.graph.Graph(schemaGraph.boundTerms, (schemaGraph.boundTypes),
           (schemaGraph.classConstraints), (schemaGraph.lambdaVariables), (schemaGraph.metadata), (schemaGraph.primitives),
           schemaTypes2, (schemaGraph.typeVariables))
        eithers.map[Seq[Seq[Tuple2[T0, T1]]], Seq[Tuple2[T0, T1]], hydra.context.InContext[hydra.error.Error]]((xs: Seq[Seq[Tuple2[T0,
           T1]]]) => lists.concat[Tuple2[T0, T1]](xs))(eithers.mapList[Tuple2[hydra.module.Module, Seq[hydra.module.TypeDefinition]],
           Seq[Tuple2[T0, T1]], hydra.context.InContext[hydra.error.Error]]((p: Tuple2[hydra.module.Module,
           Seq[hydra.module.TypeDefinition]]) =>
          {
          val mod: hydra.module.Module = pairs.first[hydra.module.Module, Seq[hydra.module.TypeDefinition]](p)
          {
            val defs: Seq[hydra.module.TypeDefinition] = pairs.second[hydra.module.Module, Seq[hydra.module.TypeDefinition]](p)
            eithers.map[Map[T0, T1], Seq[Tuple2[T0, T1]], hydra.context.InContext[hydra.error.Error]]((m: Map[T0,
               T1]) => maps.toList[T0, T1](m))(printDefinitions(mod)(lists.map[hydra.module.TypeDefinition,
               hydra.module.Definition]((d: hydra.module.TypeDefinition) => hydra.module.Definition.`type`(d))(defs))(cx)(schemaGraphWithTypes))
          }
        })(lists.zip[hydra.module.Module, Seq[hydra.module.TypeDefinition]](typeModulesToGenerate)(defLists)))
      }
    })
  }))((schemaFiles: Seq[Tuple2[T0, T1]]) =>
    eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[Tuple2[T0, T1]], Seq[Tuple2[T0, T1]]](logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
       Seq[Tuple2[T0, T1]]]](lists.`null`[hydra.module.Module](termModulesToGenerate))(Right(Seq()))({
    val namespaces: Seq[hydra.module.Namespace] = lists.map[hydra.module.Module, hydra.module.Namespace]((m: hydra.module.Module) => (m.namespace))(termModulesToGenerate)
    eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[hydra.graph.Graph, Seq[Seq[hydra.module.TermDefinition]]],
       Seq[Tuple2[T0, T1]]](eithers.bimap[scala.Predef.String, Tuple2[hydra.graph.Graph, Seq[Seq[hydra.module.TermDefinition]]],
       hydra.context.InContext[hydra.error.Error], Tuple2[hydra.graph.Graph, Seq[Seq[hydra.module.TermDefinition]]]]((s: scala.Predef.String) => hydra.context.InContext(hydra.error.Error.other(s),
       cx))((r: Tuple2[hydra.graph.Graph, Seq[Seq[hydra.module.TermDefinition]]]) => r)(hydra.adapt.dataGraphToDefinitions(constraints)(doInfer)(doExpand)(doHoistCaseStatements)(doHoistPolymorphicLetBindings)(dataElements)(dataGraph)(namespaces)(cx)))((dataResult: Tuple2[hydra.graph.Graph,
       Seq[Seq[hydra.module.TermDefinition]]]) =>
      {
      val g1: hydra.graph.Graph = pairs.first[hydra.graph.Graph, Seq[Seq[hydra.module.TermDefinition]]](dataResult)
      {
        val defLists: Seq[Seq[hydra.module.TermDefinition]] = pairs.second[hydra.graph.Graph, Seq[Seq[hydra.module.TermDefinition]]](dataResult)
        {
          def refreshModule(els: Seq[hydra.core.Binding])(m: hydra.module.Module): hydra.module.Module =
            hydra.module.Module(m.namespace, maybes.cat[hydra.core.Binding](lists.map[hydra.core.Binding, Option[hydra.core.Binding]]((e: hydra.core.Binding) =>
            lists.find[hydra.core.Binding]((b: hydra.core.Binding) => equality.equal[hydra.core.Name](b.name)(e.name))(els))(m.elements)),
               (m.termDependencies), (m.typeDependencies), (m.description))
          {
            val allBindings: Seq[hydra.core.Binding] = hydra.lexical.graphToBindings(g1)
            {
              val refreshedMods: Seq[hydra.module.Module] = lists.map[hydra.module.Module, hydra.module.Module]((m: hydra.module.Module) => refreshModule(allBindings)(m))(termModulesToGenerate)
              eithers.map[Seq[Seq[Tuple2[T0, T1]]], Seq[Tuple2[T0, T1]], hydra.context.InContext[hydra.error.Error]]((xs: Seq[Seq[Tuple2[T0,
                 T1]]]) => lists.concat[Tuple2[T0, T1]](xs))(eithers.mapList[Tuple2[hydra.module.Module,
                 Seq[hydra.module.TermDefinition]], Seq[Tuple2[T0, T1]], hydra.context.InContext[hydra.error.Error]]((p: Tuple2[hydra.module.Module,
                 Seq[hydra.module.TermDefinition]]) =>
                {
                val mod: hydra.module.Module = pairs.first[hydra.module.Module, Seq[hydra.module.TermDefinition]](p)
                {
                  val defs: Seq[hydra.module.TermDefinition] = pairs.second[hydra.module.Module, Seq[hydra.module.TermDefinition]](p)
                  eithers.map[Map[T0, T1], Seq[Tuple2[T0, T1]], hydra.context.InContext[hydra.error.Error]]((m: Map[T0,
                     T1]) => maps.toList[T0, T1](m))(printDefinitions(mod)(lists.map[hydra.module.TermDefinition,
                     hydra.module.Definition]((d: hydra.module.TermDefinition) => hydra.module.Definition.term(d))(defs))(cx)(g1))
                }
              })(lists.zip[hydra.module.Module, Seq[hydra.module.TermDefinition]](refreshedMods)(defLists)))
            }
          }
        }
      }
    })
  }))((termFiles: Seq[Tuple2[T0, T1]]) => Right(lists.concat2[Tuple2[T0, T1]](schemaFiles)(termFiles))))
}

def formatTermBinding(binding: hydra.core.Binding): scala.Predef.String =
  {
  val name: scala.Predef.String = (binding.name)
  val typeStr: scala.Predef.String = maybes.maybe[scala.Predef.String, hydra.core.TypeScheme]("?")((scheme: hydra.core.TypeScheme) => hydra.show.core.typeScheme(scheme))(binding.`type`)
  strings.cat2(strings.cat2(strings.cat2("  ")(name))(" : "))(typeStr)
}

def formatPrimitive(prim: hydra.graph.Primitive): scala.Predef.String =
  {
  val name: scala.Predef.String = (prim.name)
  val typeStr: scala.Predef.String = hydra.show.core.typeScheme(prim.`type`)
  strings.cat2(strings.cat2(strings.cat2("  ")(name))(" : "))(typeStr)
}

def formatTypeBinding(graph: hydra.graph.Graph)(binding: hydra.core.Binding): Either[hydra.error.DecodingError, scala.Predef.String] =
  eithers.bind[hydra.error.DecodingError, hydra.core.Type, scala.Predef.String](hydra.decode.core.`type`(graph)(binding.term))((typ: hydra.core.Type) =>
  Right(strings.cat2(strings.cat2(strings.cat2("  ")(binding.name))(" = "))(hydra.show.core.`type`(typ))))

def buildSchemaMap(g: hydra.graph.Graph): Map[hydra.core.Name, hydra.core.Type] =
  maps.map[hydra.core.TypeScheme, hydra.core.Type, hydra.core.Name]((ts: hydra.core.TypeScheme) => hydra.rewriting.deannotateType(ts.`type`))(g.schemaTypes)

def moduleToSourceModule(m: hydra.module.Module): hydra.module.Module =
  {
  val sourceNs: hydra.module.Namespace = strings.cat2("hydra.sources.")(strings.intercalate(".")(lists.drop[scala.Predef.String](1)(strings.splitOn(".")(m.namespace))))
  val modTypeNs: hydra.module.Namespace = "hydra.module"
  val moduleBinding: hydra.core.Binding = hydra.core.Binding(strings.cat2(sourceNs)(".module_"), hydra.encode.module.module(m), None)
  hydra.module.Module(sourceNs, Seq(moduleBinding), Seq(modTypeNs), Seq(modTypeNs), Some(strings.cat2("Source module for ")(m.namespace)))
}

def generateLexicon(graph: hydra.graph.Graph): Either[hydra.error.DecodingError, scala.Predef.String] =
  {
  val bindings: Seq[hydra.core.Binding] = hydra.lexical.graphToBindings(graph)
  val primitives: Seq[hydra.graph.Primitive] = maps.elems[hydra.core.Name, hydra.graph.Primitive](graph.primitives)
  val partitioned: Tuple2[Seq[hydra.core.Binding], Seq[hydra.core.Binding]] = lists.partition[hydra.core.Binding]((b: hydra.core.Binding) => hydra.annotations.isNativeType(b))(bindings)
  val typeBindings: Seq[hydra.core.Binding] = pairs.first[Seq[hydra.core.Binding], Seq[hydra.core.Binding]](partitioned)
  val termBindings: Seq[hydra.core.Binding] = pairs.second[Seq[hydra.core.Binding], Seq[hydra.core.Binding]](partitioned)
  val sortedPrimitives: Seq[hydra.graph.Primitive] = lists.sortOn[hydra.graph.Primitive, hydra.core.Name]((p: hydra.graph.Primitive) => (p.name))(primitives)
  val sortedTypes: Seq[hydra.core.Binding] = lists.sortOn[hydra.core.Binding, hydra.core.Name]((b: hydra.core.Binding) => (b.name))(typeBindings)
  val sortedTerms: Seq[hydra.core.Binding] = lists.sortOn[hydra.core.Binding, hydra.core.Name]((b: hydra.core.Binding) => (b.name))(termBindings)
  eithers.bind[hydra.error.DecodingError, Seq[scala.Predef.String], scala.Predef.String](eithers.mapList[hydra.core.Binding,
     scala.Predef.String, hydra.error.DecodingError]((b: hydra.core.Binding) => hydra.codeGeneration.formatTypeBinding(graph)(b))(sortedTypes))((typeLines: Seq[scala.Predef.String]) =>
    {
    val termLines: Seq[scala.Predef.String] = lists.map[hydra.core.Binding, scala.Predef.String]((b: hydra.core.Binding) => hydra.codeGeneration.formatTermBinding(b))(sortedTerms)
    {
      val primitiveLines: Seq[scala.Predef.String] = lists.map[hydra.graph.Primitive, scala.Predef.String]((p: hydra.graph.Primitive) => hydra.codeGeneration.formatPrimitive(p))(sortedPrimitives)
      Right(strings.cat2(strings.cat2(strings.cat2(strings.cat2(strings.cat2("Primitives:\n")(strings.unlines(primitiveLines)))("\nTypes:\n"))(strings.unlines(typeLines)))("\nTerms:\n"))(strings.unlines(termLines)))
    }
  })
}

def moduleToJson(m: hydra.module.Module): Either[scala.Predef.String, scala.Predef.String] =
  {
  val term: hydra.core.Term = hydra.encode.module.module(m)
  eithers.map[hydra.json.model.Value, scala.Predef.String, scala.Predef.String]((json: hydra.json.model.Value) => hydra.json.writer.printJson(json))(hydra.json.encode.toJson(term))
}

def inferModules(cx: hydra.context.Context)(bsGraph: hydra.graph.Graph)(universeMods: Seq[hydra.module.Module])(targetMods: Seq[hydra.module.Module]): Either[hydra.context.InContext[hydra.error.Error],
   Seq[hydra.module.Module]] =
  {
  val g0: hydra.graph.Graph = hydra.codeGeneration.modulesToGraph(bsGraph)(universeMods)(universeMods)
  val dataElements: Seq[hydra.core.Binding] = lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => logic.not(hydra.annotations.isNativeType(e)))(lists.concat[hydra.core.Binding](lists.map[hydra.module.Module,
     Seq[hydra.core.Binding]]((m: hydra.module.Module) => (m.elements))(universeMods)))
  eithers.bind[hydra.context.InContext[hydra.error.Error], Tuple2[Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]],
     hydra.context.Context], Seq[hydra.module.Module]](hydra.inference.inferGraphTypes(cx)(dataElements)(g0))((inferResultWithCx: Tuple2[Tuple2[hydra.graph.Graph,
     Seq[hydra.core.Binding]], hydra.context.Context]) =>
    {
    val inferResult: Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]] = pairs.first[Tuple2[hydra.graph.Graph,
       Seq[hydra.core.Binding]], hydra.context.Context](inferResultWithCx)
    {
      val g1: hydra.graph.Graph = pairs.first[hydra.graph.Graph, Seq[hydra.core.Binding]](inferResult)
      {
        val inferredElements: Seq[hydra.core.Binding] = pairs.second[hydra.graph.Graph, Seq[hydra.core.Binding]](inferResult)
        {
          def isTypeModule(mod: hydra.module.Module): Boolean =
            lists.`null`[hydra.core.Binding](lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => logic.not(hydra.annotations.isNativeType(e)))(mod.elements))
          {
            def refreshModule(m: hydra.module.Module): hydra.module.Module =
              logic.ifElse[hydra.module.Module](isTypeModule(m))(m)(hydra.module.Module(m.namespace, maybes.cat[hydra.core.Binding](lists.map[hydra.core.Binding,
                 Option[hydra.core.Binding]]((e: hydra.core.Binding) =>
              lists.find[hydra.core.Binding]((b: hydra.core.Binding) => equality.equal[hydra.core.Name](b.name)(e.name))(inferredElements))(m.elements)),
                 (m.termDependencies), (m.typeDependencies), (m.description)))
            Right(lists.map[hydra.module.Module, hydra.module.Module](refreshModule)(targetMods))
          }
        }
      }
    }
  })
}

def generateCoderModules[T0, T1, T2, T3](codec: (T0 => hydra.graph.Graph => T1 => Either[T2, Option[T3]]))(bsGraph: hydra.graph.Graph)(universeModules: Seq[hydra.module.Module])(typeModules: Seq[T1])(cx: T0): Either[T2,
   Seq[T3]] =
  {
  val universe: Map[hydra.module.Namespace, hydra.module.Module] = maps.fromList[hydra.module.Namespace,
     hydra.module.Module](lists.map[hydra.module.Module, Tuple2[hydra.module.Namespace, hydra.module.Module]]((m: hydra.module.Module) => Tuple2(m.namespace,
     m))(lists.concat2[hydra.module.Module](universeModules)(universeModules)))
  val schemaModules: Seq[hydra.module.Module] = hydra.codeGeneration.moduleTypeDepsTransitive(universe)(universeModules)
  val dataModules: Seq[hydra.module.Module] = hydra.codeGeneration.moduleTermDepsTransitive(universe)(universeModules)
  val schemaElements: Seq[hydra.core.Binding] = lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => hydra.annotations.isNativeType(e))(lists.concat[hydra.core.Binding](lists.map[hydra.module.Module,
     Seq[hydra.core.Binding]]((m: hydra.module.Module) => (m.elements))(lists.concat2[hydra.module.Module](schemaModules)(universeModules))))
  val dataElements: Seq[hydra.core.Binding] = lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => logic.not(hydra.annotations.isNativeType(e)))(lists.concat[hydra.core.Binding](lists.map[hydra.module.Module,
     Seq[hydra.core.Binding]]((m: hydra.module.Module) => (m.elements))(dataModules)))
  val schemaGraph: hydra.graph.Graph = hydra.lexical.elementsToGraph(bsGraph)(maps.empty[hydra.core.Name, hydra.core.TypeScheme])(schemaElements)
  val schemaTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = eithers.either[hydra.context.InContext[hydra.error.Error],
     Map[hydra.core.Name, hydra.core.TypeScheme], Map[hydra.core.Name, hydra.core.TypeScheme]]((_x: hydra.context.InContext[hydra.error.Error]) => maps.empty[hydra.core.Name,
     hydra.core.TypeScheme])((_r: Map[hydra.core.Name, hydra.core.TypeScheme]) => _r)(hydra.schemas.schemaGraphToTypingEnvironment(hydra.lexical.emptyContext)(schemaGraph))
  val allElements: Seq[hydra.core.Binding] = lists.concat2[hydra.core.Binding](schemaElements)(dataElements)
  val graph: hydra.graph.Graph = hydra.lexical.elementsToGraph(bsGraph)(schemaTypes)(allElements)
  eithers.map[Seq[Option[T3]], Seq[T3], T2]((results: Seq[Option[T3]]) => maybes.cat[T3](results))(eithers.mapList[T1,
     Option[T3], T2]((m: T1) => codec(cx)(graph)(m))(typeModules))
}

def inferAndGenerateLexicon(cx: hydra.context.Context)(bsGraph: hydra.graph.Graph)(kernelModules: Seq[hydra.module.Module]): Either[scala.Predef.String,
   scala.Predef.String] =
  {
  val g0: hydra.graph.Graph = hydra.codeGeneration.modulesToGraph(bsGraph)(kernelModules)(kernelModules)
  val dataElements: Seq[hydra.core.Binding] = lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => logic.not(hydra.annotations.isNativeType(e)))(lists.concat[hydra.core.Binding](lists.map[hydra.module.Module,
     Seq[hydra.core.Binding]]((m: hydra.module.Module) => (m.elements))(kernelModules)))
  eithers.bind[scala.Predef.String, Tuple2[Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]], hydra.context.Context],
     scala.Predef.String](eithers.bimap[hydra.context.InContext[hydra.error.Error], Tuple2[Tuple2[hydra.graph.Graph,
     Seq[hydra.core.Binding]], hydra.context.Context], scala.Predef.String, Tuple2[Tuple2[hydra.graph.Graph,
     Seq[hydra.core.Binding]], hydra.context.Context]]((ic: hydra.context.InContext[hydra.error.Error]) => hydra.show.error.error(ic.`object`))((x: Tuple2[Tuple2[hydra.graph.Graph,
     Seq[hydra.core.Binding]], hydra.context.Context]) => x)(hydra.inference.inferGraphTypes(cx)(dataElements)(g0)))((inferResultWithCx: Tuple2[Tuple2[hydra.graph.Graph,
     Seq[hydra.core.Binding]], hydra.context.Context]) =>
    {
    val g1: hydra.graph.Graph = pairs.first[hydra.graph.Graph, Seq[hydra.core.Binding]](pairs.first[Tuple2[hydra.graph.Graph,
       Seq[hydra.core.Binding]], hydra.context.Context](inferResultWithCx))
    eithers.bimap[hydra.error.DecodingError, scala.Predef.String, scala.Predef.String, scala.Predef.String]((x) => x)((x: scala.Predef.String) => x)(hydra.codeGeneration.generateLexicon(g1))
  })
}

def escapeControlCharsInJson(input: Seq[Int]): Seq[Int] =
  {
  def hexDigit(n: Int): Int =
    logic.ifElse[Int](equality.lt[Int](n)(10))(math.add(48)(n))(math.add(97)(math.sub(n)(10)))
  def escapeToUnicode(b: Int): Seq[Int] = Seq(92, 117, 48, 48, hexDigit(math.div(b)(16)), hexDigit(math.mod(b)(16)))
  def go(inStr: Boolean)(esc: Boolean)(bytes: Seq[Int]): Seq[Int] =
    logic.ifElse[Seq[Int]](lists.`null`[Int](bytes))(Seq())({
    val b: Int = lists.head[Int](bytes)
    {
      val bs: Seq[Int] = lists.tail[Int](bytes)
      logic.ifElse[Seq[Int]](esc)(lists.cons[Int](b)(go(inStr)(false)(bs)))(logic.ifElse[Seq[Int]](logic.and(equality.equal[Int](b)(92))(inStr))(lists.cons[Int](b)(go(inStr)(true)(bs)))(logic.ifElse[Seq[Int]](equality.equal[Int](b)(34))(lists.cons[Int](b)(go(logic.not(inStr))(false)(bs)))(logic.ifElse[Seq[Int]](logic.and(inStr)(equality.lt[Int](b)(32)))(lists.concat2[Int](escapeToUnicode(b))(go(inStr)(false)(bs)))(lists.cons[Int](b)(go(inStr)(false)(bs))))))
    }
  })
  go(false)(false)(input)
}

def decodeModuleFromJson(bsGraph: hydra.graph.Graph)(universeModules: Seq[hydra.module.Module])(doStripTypeSchemes: Boolean)(jsonVal: hydra.json.model.Value): Either[scala.Predef.String,
   hydra.module.Module] =
  {
  val graph: hydra.graph.Graph = hydra.codeGeneration.modulesToGraph(bsGraph)(universeModules)(universeModules)
  val schemaMap: Map[hydra.core.Name, hydra.core.Type] = hydra.codeGeneration.buildSchemaMap(graph)
  val modType: hydra.core.Type = hydra.core.Type.variable("hydra.module.Module")
  eithers.either[scala.Predef.String, hydra.core.Term, Either[scala.Predef.String, hydra.module.Module]]((err: scala.Predef.String) => Left(err))((term: hydra.core.Term) =>
    eithers.either[hydra.error.DecodingError, hydra.module.Module, Either[scala.Predef.String, hydra.module.Module]]((decErr: hydra.error.DecodingError) => Left(decErr))((mod: hydra.module.Module) =>
    Right(logic.ifElse[hydra.module.Module](doStripTypeSchemes)(hydra.codeGeneration.stripModuleTypeSchemes(mod))(mod)))(hydra.decode.module.module(graph)(term)))(hydra.json.decode.fromJson(schemaMap)("hydra.module.Module")(modType)(jsonVal))
}
