package hydra.codeGeneration

import hydra.coders.*

import hydra.context.*

import hydra.core.*

import hydra.errors.*

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

def namespaceToPath(ns: hydra.module.Namespace): scala.Predef.String = hydra.lib.strings.intercalate("/")(hydra.lib.strings.splitOn(".")(ns))

def stripModuleTypeSchemes(m: hydra.module.Module): hydra.module.Module =
  {
  def stripIfTerm(b: hydra.core.Binding): hydra.core.Binding =
    hydra.lib.logic.ifElse[hydra.core.Binding](hydra.annotations.isNativeType(b))(b)(hydra.core.Binding(b.name, (b.term), None))
  hydra.module.Module(m.namespace, hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding](stripIfTerm)(m.elements),
     (m.termDependencies), (m.typeDependencies), (m.description))
}

def transitiveDeps(getDeps: (hydra.module.Module => Seq[hydra.module.Namespace]))(nsMap: Map[hydra.module.Namespace,
   hydra.module.Module])(startMods: Seq[hydra.module.Module]): scala.collection.immutable.Set[hydra.module.Namespace] =
  {
  val initialDeps: scala.collection.immutable.Set[hydra.module.Namespace] = hydra.lib.sets.fromList[hydra.module.Namespace](hydra.lib.lists.concat[hydra.module.Namespace](hydra.lib.lists.map[hydra.module.Module,
     Seq[hydra.module.Namespace]]((m: hydra.module.Module) =>
    hydra.lib.lists.filter[hydra.module.Namespace]((dep: hydra.module.Namespace) =>
    hydra.lib.logic.not(hydra.lib.equality.equal[hydra.module.Namespace](dep)(m.namespace)))(getDeps(m)))(startMods)))
  def go(pending: scala.collection.immutable.Set[hydra.module.Namespace])(visited: scala.collection.immutable.Set[hydra.module.Namespace]): scala.collection.immutable.Set[hydra.module.Namespace] =
    hydra.lib.logic.ifElse[scala.collection.immutable.Set[hydra.module.Namespace]](hydra.lib.sets.`null`[hydra.module.Namespace](pending))(visited)({
    val newVisited: scala.collection.immutable.Set[hydra.module.Namespace] = hydra.lib.sets.union[hydra.module.Namespace](visited)(pending)
    {
      val nextDeps: scala.collection.immutable.Set[hydra.module.Namespace] = hydra.lib.sets.fromList[hydra.module.Namespace](hydra.lib.lists.concat[hydra.module.Namespace](hydra.lib.lists.map[hydra.module.Namespace,
         Seq[hydra.module.Namespace]]((nsv: hydra.module.Namespace) =>
        hydra.lib.maybes.maybe[Seq[hydra.module.Namespace], hydra.module.Module](Seq())((depMod: hydra.module.Module) => getDeps(depMod))(hydra.lib.maps.lookup[hydra.module.Namespace,
           hydra.module.Module](nsv)(nsMap)))(hydra.lib.sets.toList[hydra.module.Namespace](pending))))
      {
        val newPending: scala.collection.immutable.Set[hydra.module.Namespace] = hydra.lib.sets.difference[hydra.module.Namespace](nextDeps)(newVisited)
        go(newPending)(newVisited)
      }
    }
  })
  go(initialDeps)(hydra.lib.sets.empty[hydra.module.Namespace])
}

def moduleTermDepsTransitive(nsMap: Map[hydra.module.Namespace, hydra.module.Module])(modules: Seq[hydra.module.Module]): Seq[hydra.module.Module] =
  {
  val closure: scala.collection.immutable.Set[hydra.module.Namespace] = hydra.lib.sets.union[hydra.module.Namespace](hydra.codeGeneration.transitiveDeps((m: hydra.module.Module) => (m.termDependencies))(nsMap)(modules))(hydra.lib.sets.fromList[hydra.module.Namespace](hydra.lib.lists.map[hydra.module.Module,
     hydra.module.Namespace]((m: hydra.module.Module) => (m.namespace))(modules)))
  hydra.lib.maybes.cat[hydra.module.Module](hydra.lib.lists.map[hydra.module.Namespace, Option[hydra.module.Module]]((n: hydra.module.Namespace) =>
    hydra.lib.maps.lookup[hydra.module.Namespace, hydra.module.Module](n)(nsMap))(hydra.lib.sets.toList[hydra.module.Namespace](closure)))
}

def moduleTypeDepsTransitive(nsMap: Map[hydra.module.Namespace, hydra.module.Module])(modules: Seq[hydra.module.Module]): Seq[hydra.module.Module] =
  {
  val termMods: Seq[hydra.module.Module] = hydra.codeGeneration.moduleTermDepsTransitive(nsMap)(modules)
  val typeNamespaces: Seq[hydra.module.Namespace] = hydra.lib.sets.toList[hydra.module.Namespace](hydra.codeGeneration.transitiveDeps((m: hydra.module.Module) => (m.typeDependencies))(nsMap)(termMods))
  hydra.lib.maybes.cat[hydra.module.Module](hydra.lib.lists.map[hydra.module.Namespace, Option[hydra.module.Module]]((n: hydra.module.Namespace) =>
    hydra.lib.maps.lookup[hydra.module.Namespace, hydra.module.Module](n)(nsMap))(typeNamespaces))
}

def modulesToGraph(bsGraph: hydra.graph.Graph)(universeModules: Seq[hydra.module.Module])(modules: Seq[hydra.module.Module]): hydra.graph.Graph =
  {
  val universe: Map[hydra.module.Namespace, hydra.module.Module] = hydra.lib.maps.fromList[hydra.module.Namespace,
     hydra.module.Module](hydra.lib.lists.map[hydra.module.Module, Tuple2[hydra.module.Namespace, hydra.module.Module]]((m: hydra.module.Module) => Tuple2(m.namespace,
     m))(hydra.lib.lists.concat2[hydra.module.Module](universeModules)(modules)))
  val schemaModules: Seq[hydra.module.Module] = hydra.codeGeneration.moduleTypeDepsTransitive(universe)(modules)
  val dataModules: Seq[hydra.module.Module] = hydra.codeGeneration.moduleTermDepsTransitive(universe)(modules)
  val schemaElements: Seq[hydra.core.Binding] = hydra.lib.lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => hydra.annotations.isNativeType(e))(hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.module.Module,
     Seq[hydra.core.Binding]]((m: hydra.module.Module) => (m.elements))(hydra.lib.lists.concat2[hydra.module.Module](schemaModules)(modules))))
  val dataElements: Seq[hydra.core.Binding] = hydra.lib.lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => hydra.lib.logic.not(hydra.annotations.isNativeType(e)))(hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.module.Module,
     Seq[hydra.core.Binding]]((m: hydra.module.Module) => (m.elements))(dataModules)))
  val schemaGraph: hydra.graph.Graph = hydra.lexical.elementsToGraph(bsGraph)(hydra.lib.maps.empty[hydra.core.Name, hydra.core.TypeScheme])(schemaElements)
  val schemaTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = hydra.lib.eithers.either[hydra.context.InContext[hydra.errors.Error],
     Map[hydra.core.Name, hydra.core.TypeScheme], Map[hydra.core.Name, hydra.core.TypeScheme]]((_x: hydra.context.InContext[hydra.errors.Error]) => hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeScheme])((_r: Map[hydra.core.Name, hydra.core.TypeScheme]) => _r)(hydra.schemas.schemaGraphToTypingEnvironment(hydra.lexical.emptyContext)(schemaGraph))
  hydra.lexical.elementsToGraph(bsGraph)(schemaTypes)(dataElements)
}

def generateSourceFiles[T0, T1](printDefinitions: (hydra.module.Module => Seq[hydra.module.Definition] => hydra.context.Context => hydra.graph.Graph => Either[hydra.context.InContext[hydra.errors.Error],
   Map[T0, T1]]))(lang: hydra.coders.Language)(doInfer: Boolean)(doExpand: Boolean)(doHoistCaseStatements: Boolean)(doHoistPolymorphicLetBindings: Boolean)(bsGraph: hydra.graph.Graph)(universeModules: Seq[hydra.module.Module])(modsToGenerate: Seq[hydra.module.Module])(cx: hydra.context.Context): Either[hydra.context.InContext[hydra.errors.Error],
   Seq[Tuple2[T0, T1]]] =
  {
  val namespaceMap: Map[hydra.module.Namespace, hydra.module.Module] = hydra.lib.maps.fromList[hydra.module.Namespace,
     hydra.module.Module](hydra.lib.lists.map[hydra.module.Module, Tuple2[hydra.module.Namespace, hydra.module.Module]]((m: hydra.module.Module) => Tuple2(m.namespace,
     m))(hydra.lib.lists.concat2[hydra.module.Module](universeModules)(modsToGenerate)))
  val constraints: hydra.coders.LanguageConstraints = (lang.constraints)
  def isTypeModule(mod: hydra.module.Module): Boolean =
    hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Binding](hydra.lib.lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => hydra.annotations.isNativeType(e))(mod.elements)))
  val partitioned: Tuple2[Seq[hydra.module.Module], Seq[hydra.module.Module]] = hydra.lib.lists.partition[hydra.module.Module](isTypeModule)(modsToGenerate)
  val typeModulesToGenerate: Seq[hydra.module.Module] = hydra.lib.pairs.first[Seq[hydra.module.Module], Seq[hydra.module.Module]](partitioned)
  val termModulesToGenerate: Seq[hydra.module.Module] = hydra.lib.pairs.second[Seq[hydra.module.Module], Seq[hydra.module.Module]](partitioned)
  val schemaMods: Seq[hydra.module.Module] = hydra.codeGeneration.moduleTypeDepsTransitive(namespaceMap)(modsToGenerate)
  val schemaElements: Seq[hydra.core.Binding] = hydra.lib.lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => hydra.annotations.isNativeType(e))(hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.module.Module,
     Seq[hydra.core.Binding]]((m: hydra.module.Module) => (m.elements))(hydra.lib.lists.concat2[hydra.module.Module](schemaMods)(typeModulesToGenerate))))
  val dataMods: Seq[hydra.module.Module] = hydra.codeGeneration.moduleTermDepsTransitive(namespaceMap)(modsToGenerate)
  val dataElements: Seq[hydra.core.Binding] = hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.module.Module,
     Seq[hydra.core.Binding]]((m: hydra.module.Module) => (m.elements))(dataMods))
  val schemaGraph: hydra.graph.Graph = hydra.lexical.elementsToGraph(bsGraph)(hydra.lib.maps.empty[hydra.core.Name, hydra.core.TypeScheme])(schemaElements)
  val schemaTypes2: Map[hydra.core.Name, hydra.core.TypeScheme] = hydra.lib.eithers.either[hydra.context.InContext[hydra.errors.Error],
     Map[hydra.core.Name, hydra.core.TypeScheme], Map[hydra.core.Name, hydra.core.TypeScheme]]((_x: hydra.context.InContext[hydra.errors.Error]) => hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeScheme])((_r: Map[hydra.core.Name, hydra.core.TypeScheme]) => _r)(hydra.schemas.schemaGraphToTypingEnvironment(hydra.lexical.emptyContext)(schemaGraph))
  val dataGraph: hydra.graph.Graph = hydra.lexical.elementsToGraph(bsGraph)(schemaTypes2)(dataElements)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[Tuple2[T0, T1]], Seq[Tuple2[T0,
     T1]]](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Seq[Tuple2[T0, T1]]]](hydra.lib.lists.`null`[hydra.module.Module](typeModulesToGenerate))(Right(Seq()))({
    val nameLists: Seq[Seq[hydra.core.Name]] = hydra.lib.lists.map[hydra.module.Module, Seq[hydra.core.Name]]((m: hydra.module.Module) =>
      hydra.lib.lists.map[hydra.core.Binding, hydra.core.Name]((e: hydra.core.Binding) => (e.name))(hydra.lib.lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => hydra.annotations.isNativeType(e))(m.elements)))(typeModulesToGenerate)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Tuple2[Map[hydra.core.Name, hydra.core.Type],
       Seq[Seq[hydra.module.TypeDefinition]]], Seq[Tuple2[T0, T1]]](hydra.lib.eithers.bimap[scala.Predef.String,
       Tuple2[Map[hydra.core.Name, hydra.core.Type], Seq[Seq[hydra.module.TypeDefinition]]], hydra.context.InContext[hydra.errors.Error],
       Tuple2[Map[hydra.core.Name, hydra.core.Type], Seq[Seq[hydra.module.TypeDefinition]]]]((s: scala.Predef.String) => hydra.context.InContext(hydra.errors.Error.other(s),
       cx))((r: Tuple2[Map[hydra.core.Name, hydra.core.Type], Seq[Seq[hydra.module.TypeDefinition]]]) => r)(hydra.adapt.schemaGraphToDefinitions(constraints)(schemaGraph)(nameLists)(cx)))((schemaResult: Tuple2[Map[hydra.core.Name,
       hydra.core.Type], Seq[Seq[hydra.module.TypeDefinition]]]) =>
      {
      val defLists: Seq[Seq[hydra.module.TypeDefinition]] = hydra.lib.pairs.second[Map[hydra.core.Name,
         hydra.core.Type], Seq[Seq[hydra.module.TypeDefinition]]](schemaResult)
      {
        val schemaGraphWithTypes: hydra.graph.Graph = hydra.graph.Graph(schemaGraph.boundTerms, (schemaGraph.boundTypes),
           (schemaGraph.classConstraints), (schemaGraph.lambdaVariables), (schemaGraph.metadata), (schemaGraph.primitives),
           schemaTypes2, (schemaGraph.typeVariables))
        hydra.lib.eithers.map[Seq[Seq[Tuple2[T0, T1]]], Seq[Tuple2[T0, T1]], hydra.context.InContext[hydra.errors.Error]]((xs: Seq[Seq[Tuple2[T0,
           T1]]]) => hydra.lib.lists.concat[Tuple2[T0, T1]](xs))(hydra.lib.eithers.mapList[Tuple2[hydra.module.Module,
           Seq[hydra.module.TypeDefinition]], Seq[Tuple2[T0, T1]], hydra.context.InContext[hydra.errors.Error]]((p: Tuple2[hydra.module.Module,
           Seq[hydra.module.TypeDefinition]]) =>
          {
          val mod: hydra.module.Module = hydra.lib.pairs.first[hydra.module.Module, Seq[hydra.module.TypeDefinition]](p)
          {
            val defs: Seq[hydra.module.TypeDefinition] = hydra.lib.pairs.second[hydra.module.Module, Seq[hydra.module.TypeDefinition]](p)
            hydra.lib.eithers.map[Map[T0, T1], Seq[Tuple2[T0, T1]], hydra.context.InContext[hydra.errors.Error]]((m: Map[T0,
               T1]) => hydra.lib.maps.toList[T0, T1](m))(printDefinitions(mod)(hydra.lib.lists.map[hydra.module.TypeDefinition,
               hydra.module.Definition]((d: hydra.module.TypeDefinition) => hydra.module.Definition.`type`(d))(defs))(cx)(schemaGraphWithTypes))
          }
        })(hydra.lib.lists.zip[hydra.module.Module, Seq[hydra.module.TypeDefinition]](typeModulesToGenerate)(defLists)))
      }
    })
  }))((schemaFiles: Seq[Tuple2[T0, T1]]) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[Tuple2[T0, T1]], Seq[Tuple2[T0,
       T1]]](hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Seq[Tuple2[T0,
       T1]]]](hydra.lib.lists.`null`[hydra.module.Module](termModulesToGenerate))(Right(Seq()))({
    val namespaces: Seq[hydra.module.Namespace] = hydra.lib.lists.map[hydra.module.Module, hydra.module.Namespace]((m: hydra.module.Module) => (m.namespace))(termModulesToGenerate)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Tuple2[hydra.graph.Graph, Seq[Seq[hydra.module.TermDefinition]]],
       Seq[Tuple2[T0, T1]]](hydra.lib.eithers.bimap[scala.Predef.String, Tuple2[hydra.graph.Graph, Seq[Seq[hydra.module.TermDefinition]]],
       hydra.context.InContext[hydra.errors.Error], Tuple2[hydra.graph.Graph, Seq[Seq[hydra.module.TermDefinition]]]]((s: scala.Predef.String) => hydra.context.InContext(hydra.errors.Error.other(s),
       cx))((r: Tuple2[hydra.graph.Graph, Seq[Seq[hydra.module.TermDefinition]]]) => r)(hydra.adapt.dataGraphToDefinitions(constraints)(doInfer)(doExpand)(doHoistCaseStatements)(doHoistPolymorphicLetBindings)(dataElements)(dataGraph)(namespaces)(cx)))((dataResult: Tuple2[hydra.graph.Graph,
       Seq[Seq[hydra.module.TermDefinition]]]) =>
      {
      val g1: hydra.graph.Graph = hydra.lib.pairs.first[hydra.graph.Graph, Seq[Seq[hydra.module.TermDefinition]]](dataResult)
      {
        val defLists: Seq[Seq[hydra.module.TermDefinition]] = hydra.lib.pairs.second[hydra.graph.Graph, Seq[Seq[hydra.module.TermDefinition]]](dataResult)
        {
          def refreshModule(els: Seq[hydra.core.Binding])(m: hydra.module.Module): hydra.module.Module =
            hydra.module.Module(m.namespace, hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.core.Binding,
               Option[hydra.core.Binding]]((e: hydra.core.Binding) =>
            hydra.lib.lists.find[hydra.core.Binding]((b: hydra.core.Binding) => hydra.lib.equality.equal[hydra.core.Name](b.name)(e.name))(els))(m.elements)),
               (m.termDependencies), (m.typeDependencies), (m.description))
          {
            val allBindings: Seq[hydra.core.Binding] = hydra.lexical.graphToBindings(g1)
            {
              val refreshedMods: Seq[hydra.module.Module] = hydra.lib.lists.map[hydra.module.Module, hydra.module.Module]((m: hydra.module.Module) => refreshModule(allBindings)(m))(termModulesToGenerate)
              hydra.lib.eithers.map[Seq[Seq[Tuple2[T0, T1]]], Seq[Tuple2[T0, T1]], hydra.context.InContext[hydra.errors.Error]]((xs: Seq[Seq[Tuple2[T0,
                 T1]]]) => hydra.lib.lists.concat[Tuple2[T0, T1]](xs))(hydra.lib.eithers.mapList[Tuple2[hydra.module.Module,
                 Seq[hydra.module.TermDefinition]], Seq[Tuple2[T0, T1]], hydra.context.InContext[hydra.errors.Error]]((p: Tuple2[hydra.module.Module,
                 Seq[hydra.module.TermDefinition]]) =>
                {
                val mod: hydra.module.Module = hydra.lib.pairs.first[hydra.module.Module, Seq[hydra.module.TermDefinition]](p)
                {
                  val defs: Seq[hydra.module.TermDefinition] = hydra.lib.pairs.second[hydra.module.Module, Seq[hydra.module.TermDefinition]](p)
                  hydra.lib.eithers.map[Map[T0, T1], Seq[Tuple2[T0, T1]], hydra.context.InContext[hydra.errors.Error]]((m: Map[T0,
                     T1]) => hydra.lib.maps.toList[T0, T1](m))(printDefinitions(mod)(hydra.lib.lists.map[hydra.module.TermDefinition,
                     hydra.module.Definition]((d: hydra.module.TermDefinition) => hydra.module.Definition.term(d))(defs))(cx)(g1))
                }
              })(hydra.lib.lists.zip[hydra.module.Module, Seq[hydra.module.TermDefinition]](refreshedMods)(defLists)))
            }
          }
        }
      }
    })
  }))((termFiles: Seq[Tuple2[T0, T1]]) =>
    Right(hydra.lib.lists.concat2[Tuple2[T0, T1]](schemaFiles)(termFiles))))
}

def formatTermBinding(binding: hydra.core.Binding): scala.Predef.String =
  {
  val name: scala.Predef.String = (binding.name)
  val typeStr: scala.Predef.String = hydra.lib.maybes.maybe[scala.Predef.String, hydra.core.TypeScheme]("?")((scheme: hydra.core.TypeScheme) => hydra.show.core.typeScheme(scheme))(binding.`type`)
  hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("  ")(name))(" : "))(typeStr)
}

def formatPrimitive(prim: hydra.graph.Primitive): scala.Predef.String =
  {
  val name: scala.Predef.String = (prim.name)
  val typeStr: scala.Predef.String = hydra.show.core.typeScheme(prim.`type`)
  hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("  ")(name))(" : "))(typeStr)
}

def formatTypeBinding(graph: hydra.graph.Graph)(binding: hydra.core.Binding): Either[hydra.errors.DecodingError, scala.Predef.String] =
  hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Type, scala.Predef.String](hydra.decode.core.`type`(graph)(binding.term))((typ: hydra.core.Type) =>
  Right(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("  ")(binding.name))(" = "))(hydra.show.core.`type`(typ))))

def buildSchemaMap(g: hydra.graph.Graph): Map[hydra.core.Name, hydra.core.Type] =
  hydra.lib.maps.map[hydra.core.TypeScheme, hydra.core.Type, hydra.core.Name]((ts: hydra.core.TypeScheme) => hydra.rewriting.deannotateType(ts.`type`))(g.schemaTypes)

def moduleToSourceModule(m: hydra.module.Module): hydra.module.Module =
  {
  val sourceNs: hydra.module.Namespace = hydra.lib.strings.cat2("hydra.sources.")(hydra.lib.strings.intercalate(".")(hydra.lib.lists.drop[scala.Predef.String](1)(hydra.lib.strings.splitOn(".")(m.namespace))))
  val modTypeNs: hydra.module.Namespace = "hydra.module"
  val moduleBinding: hydra.core.Binding = hydra.core.Binding(hydra.lib.strings.cat2(sourceNs)(".module_"), hydra.encode.module.module(m), None)
  hydra.module.Module(sourceNs, Seq(moduleBinding), Seq(modTypeNs), Seq(modTypeNs), Some(hydra.lib.strings.cat2("Source module for ")(m.namespace)))
}

def generateLexicon(graph: hydra.graph.Graph): Either[hydra.errors.DecodingError, scala.Predef.String] =
  {
  val bindings: Seq[hydra.core.Binding] = hydra.lexical.graphToBindings(graph)
  val primitives: Seq[hydra.graph.Primitive] = hydra.lib.maps.elems[hydra.core.Name, hydra.graph.Primitive](graph.primitives)
  val partitioned: Tuple2[Seq[hydra.core.Binding], Seq[hydra.core.Binding]] = hydra.lib.lists.partition[hydra.core.Binding]((b: hydra.core.Binding) => hydra.annotations.isNativeType(b))(bindings)
  val typeBindings: Seq[hydra.core.Binding] = hydra.lib.pairs.first[Seq[hydra.core.Binding], Seq[hydra.core.Binding]](partitioned)
  val termBindings: Seq[hydra.core.Binding] = hydra.lib.pairs.second[Seq[hydra.core.Binding], Seq[hydra.core.Binding]](partitioned)
  val sortedPrimitives: Seq[hydra.graph.Primitive] = hydra.lib.lists.sortOn[hydra.graph.Primitive, hydra.core.Name]((p: hydra.graph.Primitive) => (p.name))(primitives)
  val sortedTypes: Seq[hydra.core.Binding] = hydra.lib.lists.sortOn[hydra.core.Binding, hydra.core.Name]((b: hydra.core.Binding) => (b.name))(typeBindings)
  val sortedTerms: Seq[hydra.core.Binding] = hydra.lib.lists.sortOn[hydra.core.Binding, hydra.core.Name]((b: hydra.core.Binding) => (b.name))(termBindings)
  hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[scala.Predef.String], scala.Predef.String](hydra.lib.eithers.mapList[hydra.core.Binding,
     scala.Predef.String, hydra.errors.DecodingError]((b: hydra.core.Binding) => hydra.codeGeneration.formatTypeBinding(graph)(b))(sortedTypes))((typeLines: Seq[scala.Predef.String]) =>
    {
    val termLines: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.core.Binding, scala.Predef.String]((b: hydra.core.Binding) => hydra.codeGeneration.formatTermBinding(b))(sortedTerms)
    {
      val primitiveLines: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.graph.Primitive, scala.Predef.String]((p: hydra.graph.Primitive) => hydra.codeGeneration.formatPrimitive(p))(sortedPrimitives)
      Right(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("Primitives:\n")(hydra.lib.strings.unlines(primitiveLines)))("\nTypes:\n"))(hydra.lib.strings.unlines(typeLines)))("\nTerms:\n"))(hydra.lib.strings.unlines(termLines)))
    }
  })
}

def moduleToJson(m: hydra.module.Module): Either[scala.Predef.String, scala.Predef.String] =
  {
  val term: hydra.core.Term = hydra.encode.module.module(m)
  hydra.lib.eithers.map[hydra.json.model.Value, scala.Predef.String, scala.Predef.String]((json: hydra.json.model.Value) => hydra.json.writer.printJson(json))(hydra.json.encode.toJson(term))
}

def inferModules(cx: hydra.context.Context)(bsGraph: hydra.graph.Graph)(universeMods: Seq[hydra.module.Module])(targetMods: Seq[hydra.module.Module]): Either[hydra.context.InContext[hydra.errors.Error],
   Seq[hydra.module.Module]] =
  {
  val g0: hydra.graph.Graph = hydra.codeGeneration.modulesToGraph(bsGraph)(universeMods)(universeMods)
  val dataElements: Seq[hydra.core.Binding] = hydra.lib.lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => hydra.lib.logic.not(hydra.annotations.isNativeType(e)))(hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.module.Module,
     Seq[hydra.core.Binding]]((m: hydra.module.Module) => (m.elements))(universeMods)))
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Tuple2[Tuple2[hydra.graph.Graph,
     Seq[hydra.core.Binding]], hydra.context.Context], Seq[hydra.module.Module]](hydra.inference.inferGraphTypes(cx)(dataElements)(g0))((inferResultWithCx: Tuple2[Tuple2[hydra.graph.Graph,
     Seq[hydra.core.Binding]], hydra.context.Context]) =>
    {
    val inferResult: Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]] = hydra.lib.pairs.first[Tuple2[hydra.graph.Graph,
       Seq[hydra.core.Binding]], hydra.context.Context](inferResultWithCx)
    {
      val g1: hydra.graph.Graph = hydra.lib.pairs.first[hydra.graph.Graph, Seq[hydra.core.Binding]](inferResult)
      {
        val inferredElements: Seq[hydra.core.Binding] = hydra.lib.pairs.second[hydra.graph.Graph, Seq[hydra.core.Binding]](inferResult)
        {
          def isTypeModule(mod: hydra.module.Module): Boolean =
            hydra.lib.lists.`null`[hydra.core.Binding](hydra.lib.lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => hydra.lib.logic.not(hydra.annotations.isNativeType(e)))(mod.elements))
          {
            def refreshModule(m: hydra.module.Module): hydra.module.Module =
              hydra.lib.logic.ifElse[hydra.module.Module](isTypeModule(m))(m)(hydra.module.Module(m.namespace,
                 hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.core.Binding, Option[hydra.core.Binding]]((e: hydra.core.Binding) =>
              hydra.lib.lists.find[hydra.core.Binding]((b: hydra.core.Binding) => hydra.lib.equality.equal[hydra.core.Name](b.name)(e.name))(inferredElements))(m.elements)),
                 (m.termDependencies), (m.typeDependencies), (m.description)))
            Right(hydra.lib.lists.map[hydra.module.Module, hydra.module.Module](refreshModule)(targetMods))
          }
        }
      }
    }
  })
}

def generateCoderModules[T0, T1, T2, T3](codec: (T0 => hydra.graph.Graph => T1 => Either[T2, Option[T3]]))(bsGraph: hydra.graph.Graph)(universeModules: Seq[hydra.module.Module])(typeModules: Seq[T1])(cx: T0): Either[T2,
   Seq[T3]] =
  {
  val universe: Map[hydra.module.Namespace, hydra.module.Module] = hydra.lib.maps.fromList[hydra.module.Namespace,
     hydra.module.Module](hydra.lib.lists.map[hydra.module.Module, Tuple2[hydra.module.Namespace, hydra.module.Module]]((m: hydra.module.Module) => Tuple2(m.namespace,
     m))(hydra.lib.lists.concat2[hydra.module.Module](universeModules)(universeModules)))
  val schemaModules: Seq[hydra.module.Module] = hydra.codeGeneration.moduleTypeDepsTransitive(universe)(universeModules)
  val dataModules: Seq[hydra.module.Module] = hydra.codeGeneration.moduleTermDepsTransitive(universe)(universeModules)
  val schemaElements: Seq[hydra.core.Binding] = hydra.lib.lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => hydra.annotations.isNativeType(e))(hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.module.Module,
     Seq[hydra.core.Binding]]((m: hydra.module.Module) => (m.elements))(hydra.lib.lists.concat2[hydra.module.Module](schemaModules)(universeModules))))
  val dataElements: Seq[hydra.core.Binding] = hydra.lib.lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => hydra.lib.logic.not(hydra.annotations.isNativeType(e)))(hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.module.Module,
     Seq[hydra.core.Binding]]((m: hydra.module.Module) => (m.elements))(dataModules)))
  val schemaGraph: hydra.graph.Graph = hydra.lexical.elementsToGraph(bsGraph)(hydra.lib.maps.empty[hydra.core.Name, hydra.core.TypeScheme])(schemaElements)
  val schemaTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = hydra.lib.eithers.either[hydra.context.InContext[hydra.errors.Error],
     Map[hydra.core.Name, hydra.core.TypeScheme], Map[hydra.core.Name, hydra.core.TypeScheme]]((_x: hydra.context.InContext[hydra.errors.Error]) => hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeScheme])((_r: Map[hydra.core.Name, hydra.core.TypeScheme]) => _r)(hydra.schemas.schemaGraphToTypingEnvironment(hydra.lexical.emptyContext)(schemaGraph))
  val allElements: Seq[hydra.core.Binding] = hydra.lib.lists.concat2[hydra.core.Binding](schemaElements)(dataElements)
  val graph: hydra.graph.Graph = hydra.lexical.elementsToGraph(bsGraph)(schemaTypes)(allElements)
  hydra.lib.eithers.map[Seq[Option[T3]], Seq[T3], T2]((results: Seq[Option[T3]]) => hydra.lib.maybes.cat[T3](results))(hydra.lib.eithers.mapList[T1,
     Option[T3], T2]((m: T1) => codec(cx)(graph)(m))(typeModules))
}

def inferAndGenerateLexicon(cx: hydra.context.Context)(bsGraph: hydra.graph.Graph)(kernelModules: Seq[hydra.module.Module]): Either[scala.Predef.String,
   scala.Predef.String] =
  {
  val g0: hydra.graph.Graph = hydra.codeGeneration.modulesToGraph(bsGraph)(kernelModules)(kernelModules)
  val dataElements: Seq[hydra.core.Binding] = hydra.lib.lists.filter[hydra.core.Binding]((e: hydra.core.Binding) => hydra.lib.logic.not(hydra.annotations.isNativeType(e)))(hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.module.Module,
     Seq[hydra.core.Binding]]((m: hydra.module.Module) => (m.elements))(kernelModules)))
  hydra.lib.eithers.bind[scala.Predef.String, Tuple2[Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]],
     hydra.context.Context], scala.Predef.String](hydra.lib.eithers.bimap[hydra.context.InContext[hydra.errors.Error],
     Tuple2[Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]], hydra.context.Context], scala.Predef.String,
     Tuple2[Tuple2[hydra.graph.Graph, Seq[hydra.core.Binding]], hydra.context.Context]]((ic: hydra.context.InContext[hydra.errors.Error]) => hydra.show.errors.error(ic.`object`))((x: Tuple2[Tuple2[hydra.graph.Graph,
     Seq[hydra.core.Binding]], hydra.context.Context]) => x)(hydra.inference.inferGraphTypes(cx)(dataElements)(g0)))((inferResultWithCx: Tuple2[Tuple2[hydra.graph.Graph,
     Seq[hydra.core.Binding]], hydra.context.Context]) =>
    {
    val g1: hydra.graph.Graph = hydra.lib.pairs.first[hydra.graph.Graph, Seq[hydra.core.Binding]](hydra.lib.pairs.first[Tuple2[hydra.graph.Graph,
       Seq[hydra.core.Binding]], hydra.context.Context](inferResultWithCx))
    hydra.lib.eithers.bimap[hydra.errors.DecodingError, scala.Predef.String, scala.Predef.String, scala.Predef.String]((x) => x)((x: scala.Predef.String) => x)(hydra.codeGeneration.generateLexicon(g1))
  })
}

def escapeControlCharsInJson(input: Seq[Int]): Seq[Int] =
  {
  def hexDigit(n: Int): Int =
    hydra.lib.logic.ifElse[Int](hydra.lib.equality.lt[Int](n)(10))(hydra.lib.math.add(48)(n))(hydra.lib.math.add(97)(hydra.lib.math.sub(n)(10)))
  def escapeToUnicode(b: Int): Seq[Int] =
    Seq(92, 117, 48, 48, hexDigit(hydra.lib.math.div(b)(16)), hexDigit(hydra.lib.math.mod(b)(16)))
  def go(inStr: Boolean)(esc: Boolean)(bytes: Seq[Int]): Seq[Int] =
    hydra.lib.logic.ifElse[Seq[Int]](hydra.lib.lists.`null`[Int](bytes))(Seq())({
    val b: Int = hydra.lib.lists.head[Int](bytes)
    {
      val bs: Seq[Int] = hydra.lib.lists.tail[Int](bytes)
      hydra.lib.logic.ifElse[Seq[Int]](esc)(hydra.lib.lists.cons[Int](b)(go(inStr)(false)(bs)))(hydra.lib.logic.ifElse[Seq[Int]](hydra.lib.logic.and(hydra.lib.equality.equal[Int](b)(92))(inStr))(hydra.lib.lists.cons[Int](b)(go(inStr)(true)(bs)))(hydra.lib.logic.ifElse[Seq[Int]](hydra.lib.equality.equal[Int](b)(34))(hydra.lib.lists.cons[Int](b)(go(hydra.lib.logic.not(inStr))(false)(bs)))(hydra.lib.logic.ifElse[Seq[Int]](hydra.lib.logic.and(inStr)(hydra.lib.equality.lt[Int](b)(32)))(hydra.lib.lists.concat2[Int](escapeToUnicode(b))(go(inStr)(false)(bs)))(hydra.lib.lists.cons[Int](b)(go(inStr)(false)(bs))))))
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
  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[scala.Predef.String, hydra.module.Module]]((err: scala.Predef.String) => Left(err))((term: hydra.core.Term) =>
    hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.module.Module, Either[scala.Predef.String,
       hydra.module.Module]]((decErr: hydra.errors.DecodingError) => Left(decErr))((mod: hydra.module.Module) =>
    Right(hydra.lib.logic.ifElse[hydra.module.Module](doStripTypeSchemes)(hydra.codeGeneration.stripModuleTypeSchemes(mod))(mod)))(hydra.decode.module.module(graph)(term)))(hydra.json.decode.fromJson(schemaMap)("hydra.module.Module")(modType)(jsonVal))
}
