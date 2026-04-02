package hydra.analysis

import hydra.coders.*

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.module.*

import hydra.lib.eithers

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.sets

def addNamesToNamespaces[T0](encodeNamespace: (hydra.module.Namespace => T0))(names: scala.collection.immutable.Set[hydra.core.Name])(ns0: hydra.module.Namespaces[T0]): hydra.module.Namespaces[T0] =
  {
  lazy val nss: scala.collection.immutable.Set[hydra.module.Namespace] = hydra.lib.sets.fromList[hydra.module.Namespace](hydra.lib.maybes.cat[hydra.module.Namespace](hydra.lib.lists.map[hydra.core.Name,
     Option[hydra.module.Namespace]](hydra.names.namespaceOf)(hydra.lib.sets.toList[hydra.core.Name](names))))
  def toPair(ns: hydra.module.Namespace): Tuple2[hydra.module.Namespace, T0] = Tuple2(ns, encodeNamespace(ns))
  hydra.module.Namespaces(ns0.focus, hydra.lib.maps.union[hydra.module.Namespace, T0](ns0.mapping)(hydra.lib.maps.fromList[hydra.module.Namespace,
     T0](hydra.lib.lists.map[hydra.module.Namespace, Tuple2[hydra.module.Namespace, T0]](toPair)(hydra.lib.sets.toList[hydra.module.Namespace](nss)))))
}

def definitionDependencyNamespaces(defs: Seq[hydra.module.Definition]): scala.collection.immutable.Set[hydra.module.Namespace] =
  {
  def defNames(`def`: hydra.module.Definition): scala.collection.immutable.Set[hydra.core.Name] =
    `def` match
    case hydra.module.Definition.`type`(v_Definition_type_typeDef) => hydra.dependencies.typeDependencyNames(true)(v_Definition_type_typeDef.`type`)
    case hydra.module.Definition.term(v_Definition_term_termDef) => hydra.dependencies.termDependencyNames(true)(true)(true)(v_Definition_term_termDef.term)
  lazy val allNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.unions[hydra.core.Name](hydra.lib.lists.map[hydra.module.Definition,
     scala.collection.immutable.Set[hydra.core.Name]](defNames)(defs))
  hydra.lib.sets.fromList[hydra.module.Namespace](hydra.lib.maybes.cat[hydra.module.Namespace](hydra.lib.lists.map[hydra.core.Name,
     Option[hydra.module.Namespace]](hydra.names.namespaceOf)(hydra.lib.sets.toList[hydra.core.Name](allNames))))
}

def dependencyNamespaces(cx: hydra.context.Context)(graph: hydra.graph.Graph)(binds: Boolean)(withPrims: Boolean)(withNoms: Boolean)(withSchema: Boolean)(els: Seq[hydra.core.Binding]): Either[hydra.context.InContext[hydra.errors.Error],
   scala.collection.immutable.Set[hydra.module.Namespace]] =
  {
  def depNames(el: hydra.core.Binding): Either[hydra.context.InContext[hydra.errors.Error], scala.collection.immutable.Set[hydra.core.Name]] =
    {
    lazy val term: hydra.core.Term = (el.term)
    lazy val deannotatedTerm: hydra.core.Term = hydra.strip.deannotateTerm(term)
    lazy val dataNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.dependencies.termDependencyNames(binds)(withPrims)(withNoms)(term)
    lazy val schemaNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.logic.ifElse[scala.collection.immutable.Set[hydra.core.Name]](withSchema)(hydra.lib.maybes.maybe[scala.collection.immutable.Set[hydra.core.Name],
       hydra.core.TypeScheme](hydra.lib.sets.empty[hydra.core.Name])((ts: hydra.core.TypeScheme) => hydra.dependencies.typeDependencyNames(true)(ts.`type`))(el.`type`))(hydra.lib.sets.empty[hydra.core.Name])
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], scala.collection.immutable.Set[hydra.core.Name]]](hydra.predicates.isEncodedType(deannotatedTerm))(hydra.lib.eithers.map[hydra.core.Type,
       scala.collection.immutable.Set[hydra.core.Name], hydra.context.InContext[hydra.errors.Error]]((typ: hydra.core.Type) =>
      hydra.lib.sets.unions[hydra.core.Name](Seq(dataNames, schemaNames, hydra.dependencies.typeDependencyNames(true)(typ))))(hydra.lib.eithers.bimap[hydra.errors.Error,
         hydra.core.Type, hydra.context.InContext[hydra.errors.Error], hydra.core.Type]((_wc_e: hydra.errors.Error) =>
      hydra.context.InContext(_wc_e, hydra.context.Context(hydra.lib.lists.cons[scala.Predef.String]("dependency namespace (type)")(cx.trace),
         (cx.messages), (cx.other))))((_wc_a: hydra.core.Type) => _wc_a)(hydra.lib.eithers.bimap[hydra.errors.DecodingError,
         hydra.core.Type, hydra.errors.Error, hydra.core.Type]((_e: hydra.errors.DecodingError) => hydra.errors.Error.other(_e))((_a: hydra.core.Type) => _a)(hydra.decode.core.`type`(graph)(term)))))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error],
         scala.collection.immutable.Set[hydra.core.Name]]](hydra.predicates.isEncodedTerm(deannotatedTerm))(hydra.lib.eithers.map[hydra.core.Term,
         scala.collection.immutable.Set[hydra.core.Name], hydra.context.InContext[hydra.errors.Error]]((decodedTerm: hydra.core.Term) =>
      hydra.lib.sets.unions[hydra.core.Name](Seq(dataNames, schemaNames, hydra.dependencies.termDependencyNames(binds)(withPrims)(withNoms)(decodedTerm))))(hydra.lib.eithers.bimap[hydra.errors.Error,
         hydra.core.Term, hydra.context.InContext[hydra.errors.Error], hydra.core.Term]((_wc_e: hydra.errors.Error) =>
      hydra.context.InContext(_wc_e, hydra.context.Context(hydra.lib.lists.cons[scala.Predef.String]("dependency namespace (term)")(cx.trace),
         (cx.messages), (cx.other))))((_wc_a: hydra.core.Term) => _wc_a)(hydra.lib.eithers.bimap[hydra.errors.DecodingError,
         hydra.core.Term, hydra.errors.Error, hydra.core.Term]((_e: hydra.errors.DecodingError) => hydra.errors.Error.other(_e))((_a: hydra.core.Term) => _a)(hydra.decode.core.term(graph)(term)))))(Right(hydra.lib.sets.unions[hydra.core.Name](Seq(dataNames,
         schemaNames)))))
  }
  hydra.lib.eithers.map[Seq[scala.collection.immutable.Set[hydra.core.Name]], scala.collection.immutable.Set[hydra.module.Namespace],
     hydra.context.InContext[hydra.errors.Error]]((namesList: Seq[scala.collection.immutable.Set[hydra.core.Name]]) =>
    hydra.lib.sets.fromList[hydra.module.Namespace](hydra.lib.maybes.cat[hydra.module.Namespace](hydra.lib.lists.map[hydra.core.Name,
       Option[hydra.module.Namespace]](hydra.names.namespaceOf)(hydra.lib.sets.toList[hydra.core.Name](hydra.lib.sets.unions[hydra.core.Name](namesList))))))(hydra.lib.eithers.mapList[hydra.core.Binding,
       scala.collection.immutable.Set[hydra.core.Name], hydra.context.InContext[hydra.errors.Error]](depNames)(els))
}

def moduleContainsBinaryLiterals(mod: hydra.module.Module): Boolean =
  {
  def checkTerm(found: Boolean)(term: hydra.core.Term): Boolean =
    hydra.lib.logic.or(found)(term match
    case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
      case hydra.core.Literal.binary(v_Literal_binary__) => true
      case _ => false
    case _ => false)
  def termContainsBinary(term: hydra.core.Term): Boolean =
    hydra.rewriting.foldOverTerm(hydra.coders.TraversalOrder.pre)(checkTerm)(false)(term)
  lazy val defTerms: Seq[hydra.core.Term] = hydra.lib.maybes.cat[hydra.core.Term](hydra.lib.lists.map[hydra.module.Definition,
     Option[hydra.core.Term]]((d: hydra.module.Definition) =>
    d match
    case hydra.module.Definition.term(v_Definition_term_td) => Some(v_Definition_term_td.term)
    case _ => None)(mod.definitions))
  hydra.lib.lists.foldl[Boolean, hydra.core.Term]((acc: Boolean) =>
    (t: hydra.core.Term) => hydra.lib.logic.or(acc)(termContainsBinary(t)))(false)(defTerms)
}

def moduleDependencyNamespaces(cx: hydra.context.Context)(graph: hydra.graph.Graph)(binds: Boolean)(withPrims: Boolean)(withNoms: Boolean)(withSchema: Boolean)(mod: hydra.module.Module): Either[hydra.context.InContext[hydra.errors.Error],
   scala.collection.immutable.Set[hydra.module.Namespace]] =
  {
  lazy val allBindings: Seq[hydra.core.Binding] = hydra.lib.maybes.cat[hydra.core.Binding](hydra.lib.lists.map[hydra.module.Definition,
     Option[hydra.core.Binding]]((d: hydra.module.Definition) =>
    d match
    case hydra.module.Definition.`type`(v_Definition_type_td) => Some(hydra.annotations.typeElement(v_Definition_type_td.name)(v_Definition_type_td.`type`))
    case hydra.module.Definition.term(v_Definition_term_td) => Some(hydra.core.Binding(v_Definition_term_td.name,
       (v_Definition_term_td.term), (v_Definition_term_td.`type`)))
    case _ => None)(mod.definitions))
  hydra.lib.eithers.map[scala.collection.immutable.Set[hydra.module.Namespace], scala.collection.immutable.Set[hydra.module.Namespace],
     hydra.context.InContext[hydra.errors.Error]]((deps: scala.collection.immutable.Set[hydra.module.Namespace]) =>
    hydra.lib.sets.delete[hydra.module.Namespace](mod.namespace)(deps))(hydra.analysis.dependencyNamespaces(cx)(graph)(binds)(withPrims)(withNoms)(withSchema)(allBindings))
}

def namespacesForDefinitions[T0](encodeNamespace: (hydra.module.Namespace => T0))(focusNs: hydra.module.Namespace)(defs: Seq[hydra.module.Definition]): hydra.module.Namespaces[T0] =
  {
  lazy val nss: scala.collection.immutable.Set[hydra.module.Namespace] = hydra.lib.sets.delete[hydra.module.Namespace](focusNs)(hydra.analysis.definitionDependencyNamespaces(defs))
  def toPair(ns: hydra.module.Namespace): Tuple2[hydra.module.Namespace, T0] = Tuple2(ns, encodeNamespace(ns))
  hydra.module.Namespaces(toPair(focusNs), hydra.lib.maps.fromList[hydra.module.Namespace, T0](hydra.lib.lists.map[hydra.module.Namespace,
     Tuple2[hydra.module.Namespace, T0]](toPair)(hydra.lib.sets.toList[hydra.module.Namespace](nss))))
}
