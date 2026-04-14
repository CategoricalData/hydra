package hydra.environment

import hydra.core.*

import hydra.errors.*

import hydra.packaging.*

def definitionAsTypeApplicationTerm(el: hydra.core.Binding): Either[hydra.errors.Error, hydra.core.TypeApplicationTerm] =
  hydra.lib.maybes.maybe[Either[hydra.errors.Error, hydra.core.TypeApplicationTerm], hydra.core.TypeScheme](Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("typed binding",
     "untyped binding")))))((ts: hydra.core.TypeScheme) => Right(hydra.core.TypeApplicationTerm(el.term,
     (ts.`type`))))(el.`type`)

def graphAsLet(bindings: Seq[hydra.core.Binding])(body: hydra.core.Term): hydra.core.Let = hydra.core.Let(bindings, body)

def graphAsTerm(bindings: Seq[hydra.core.Binding])(body: hydra.core.Term): hydra.core.Term = hydra.core.Term.let(hydra.environment.graphAsLet(bindings)(body))

def graphAsTypes(graph: hydra.graph.Graph)(els: Seq[hydra.core.Binding]): Either[hydra.errors.DecodingError, Map[hydra.core.Name, hydra.core.Type]] =
  {
  def toPair(el: hydra.core.Binding): Either[hydra.errors.DecodingError, Tuple2[hydra.core.Name, hydra.core.Type]] =
    hydra.lib.eithers.map[hydra.core.Type, Tuple2[hydra.core.Name, hydra.core.Type], hydra.errors.DecodingError]((typ: hydra.core.Type) => Tuple2(el.name,
       typ))(hydra.decode.core.`type`(graph)(el.term))
  hydra.lib.eithers.map[Seq[Tuple2[hydra.core.Name, hydra.core.Type]], Map[hydra.core.Name, hydra.core.Type],
     hydra.errors.DecodingError](hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Type])(hydra.lib.eithers.mapList[hydra.core.Binding,
     Tuple2[hydra.core.Name, hydra.core.Type], hydra.errors.DecodingError](toPair)(els))
}

def partitionDefinitions(defs: Seq[hydra.packaging.Definition]): Tuple2[Seq[hydra.packaging.TypeDefinition], Seq[hydra.packaging.TermDefinition]] =
  {
  def getType(`def`: hydra.packaging.Definition): Option[hydra.packaging.TypeDefinition] =
    `def` match
    case hydra.packaging.Definition.`type`(v_Definition_type_td) => Some(v_Definition_type_td)
    case hydra.packaging.Definition.term(v_Definition_term__) => None
  def getTerm(`def`: hydra.packaging.Definition): Option[hydra.packaging.TermDefinition] =
    `def` match
    case hydra.packaging.Definition.`type`(v_Definition_type__) => None
    case hydra.packaging.Definition.term(v_Definition_term_td) => Some(v_Definition_term_td)
  Tuple2(hydra.lib.maybes.cat[hydra.packaging.TypeDefinition](hydra.lib.lists.map[hydra.packaging.Definition,
     Option[hydra.packaging.TypeDefinition]](getType)(defs)), hydra.lib.maybes.cat[hydra.packaging.TermDefinition](hydra.lib.lists.map[hydra.packaging.Definition,
     Option[hydra.packaging.TermDefinition]](getTerm)(defs)))
}

def reorderDefs(defs: Seq[hydra.packaging.Definition]): Seq[hydra.packaging.Definition] =
  {
  lazy val partitioned: Tuple2[Seq[hydra.packaging.TypeDefinition], Seq[hydra.packaging.TermDefinition]] = hydra.environment.partitionDefinitions(defs)
  lazy val typeDefsRaw: Seq[hydra.packaging.TypeDefinition] = hydra.lib.pairs.first[Seq[hydra.packaging.TypeDefinition],
     Seq[hydra.packaging.TermDefinition]](partitioned)
  lazy val nameFirst: Seq[hydra.packaging.TypeDefinition] = hydra.lib.lists.filter[hydra.packaging.TypeDefinition]((td: hydra.packaging.TypeDefinition) =>
    hydra.lib.equality.equal[hydra.core.Name](td.name)("hydra.core.Name"))(typeDefsRaw)
  lazy val nameRest: Seq[hydra.packaging.TypeDefinition] = hydra.lib.lists.filter[hydra.packaging.TypeDefinition]((td: hydra.packaging.TypeDefinition) =>
    hydra.lib.logic.not(hydra.lib.equality.equal[hydra.core.Name](td.name)("hydra.core.Name")))(typeDefsRaw)
  lazy val typeDefs: Seq[hydra.packaging.Definition] = hydra.lib.lists.concat[hydra.packaging.Definition](Seq(hydra.lib.lists.map[hydra.packaging.TypeDefinition,
     hydra.packaging.Definition]((td: hydra.packaging.TypeDefinition) => hydra.packaging.Definition.`type`(td))(nameFirst),
     hydra.lib.lists.map[hydra.packaging.TypeDefinition, hydra.packaging.Definition]((td: hydra.packaging.TypeDefinition) => hydra.packaging.Definition.`type`(td))(nameRest)))
  lazy val termDefsWrapped: Seq[hydra.packaging.Definition] = hydra.lib.lists.map[hydra.packaging.TermDefinition,
     hydra.packaging.Definition]((td: hydra.packaging.TermDefinition) => hydra.packaging.Definition.term(td))(hydra.lib.pairs.second[Seq[hydra.packaging.TypeDefinition],
     Seq[hydra.packaging.TermDefinition]](partitioned))
  lazy val sortedTermDefs: Seq[hydra.packaging.Definition] = hydra.lib.lists.concat[hydra.packaging.Definition](hydra.sorting.topologicalSortNodes((d: hydra.packaging.Definition) =>
    d match
    case hydra.packaging.Definition.term(v_Definition_term_td) => (v_Definition_term_td.name))((d: hydra.packaging.Definition) =>
    d match
    case hydra.packaging.Definition.term(v_Definition_term_td) => hydra.lib.sets.toList[hydra.core.Name](hydra.variables.freeVariablesInTerm(v_Definition_term_td.term))
    case _ => Seq())(termDefsWrapped))
  hydra.lib.lists.concat[hydra.packaging.Definition](Seq(typeDefs, sortedTermDefs))
}

def schemaGraphToTypingEnvironment(g: hydra.graph.Graph): Either[hydra.errors.Error, Map[hydra.core.Name, hydra.core.TypeScheme]] =
  {
  def toTypeScheme(vars: Seq[hydra.core.Name])(typ: hydra.core.Type): hydra.core.TypeScheme =
    hydra.strip.deannotateType(typ) match
    case hydra.core.Type.forall(v_Type_forall_ft) => toTypeScheme(hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_ft.parameter)(vars))(v_Type_forall_ft.body)
    case _ => hydra.core.TypeScheme(hydra.lib.lists.reverse[hydra.core.Name](vars), typ, None)
  def decodeType(term: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Type] =
    hydra.lib.eithers.bimap[hydra.errors.DecodingError, hydra.core.Type, hydra.errors.Error, hydra.core.Type]((_e: hydra.errors.DecodingError) => hydra.errors.Error.decoding(_e))((_a: hydra.core.Type) => _a)(hydra.decode.core.`type`(g)(term))
  def decodeTypeScheme(term: hydra.core.Term): Either[hydra.errors.Error, hydra.core.TypeScheme] =
    hydra.lib.eithers.bimap[hydra.errors.DecodingError, hydra.core.TypeScheme, hydra.errors.Error, hydra.core.TypeScheme]((_e: hydra.errors.DecodingError) => hydra.errors.Error.decoding(_e))((_a: hydra.core.TypeScheme) => _a)(hydra.decode.core.typeScheme(g)(term))
  def toPair(el: hydra.core.Binding): Either[hydra.errors.Error, Option[Tuple2[hydra.core.Name, hydra.core.TypeScheme]]] =
    {
    def forTerm(term: hydra.core.Term): Either[hydra.errors.Error, Option[hydra.core.TypeScheme]] =
      term match
      case hydra.core.Term.record(v_Term_record_r) => hydra.lib.logic.ifElse[Either[hydra.errors.Error,
         Option[hydra.core.TypeScheme]]](hydra.lib.equality.equal[hydra.core.Name](v_Term_record_r.typeName)("hydra.core.TypeScheme"))(hydra.lib.eithers.map[hydra.core.TypeScheme,
         Option[hydra.core.TypeScheme], hydra.errors.Error](hydra.lib.maybes.pure[hydra.core.TypeScheme])(decodeTypeScheme(el.term)))(Right(None))
      case hydra.core.Term.inject(v_Term_inject_i) => hydra.lib.logic.ifElse[Either[hydra.errors.Error,
         Option[hydra.core.TypeScheme]]](hydra.lib.equality.equal[hydra.core.Name](v_Term_inject_i.typeName)("hydra.core.Type"))(hydra.lib.eithers.map[hydra.core.Type,
         Option[hydra.core.TypeScheme], hydra.errors.Error]((decoded: hydra.core.Type) => Some(toTypeScheme(Seq())(decoded)))(decodeType(el.term)))(Right(None))
      case _ => Right(None)
    hydra.lib.eithers.bind[hydra.errors.Error, Option[hydra.core.TypeScheme], Option[Tuple2[hydra.core.Name,
       hydra.core.TypeScheme]]](hydra.lib.maybes.maybe[Either[hydra.errors.Error, Option[hydra.core.TypeScheme]],
       hydra.core.TypeScheme](hydra.lib.eithers.map[hydra.core.Type, Option[hydra.core.TypeScheme], hydra.errors.Error]((typ: hydra.core.Type) => Some(hydra.scoping.fTypeToTypeScheme(typ)))(decodeType(el.term)))((ts: hydra.core.TypeScheme) =>
      hydra.lib.logic.ifElse[Either[hydra.errors.Error, Option[hydra.core.TypeScheme]]](hydra.lib.equality.equal[hydra.core.TypeScheme](ts)(hydra.core.TypeScheme(Seq(),
         hydra.core.Type.variable("hydra.core.TypeScheme"), None)))(hydra.lib.eithers.map[hydra.core.TypeScheme,
         Option[hydra.core.TypeScheme], hydra.errors.Error](hydra.lib.maybes.pure[hydra.core.TypeScheme])(decodeTypeScheme(el.term)))(hydra.lib.logic.ifElse[Either[hydra.errors.Error,
         Option[hydra.core.TypeScheme]]](hydra.lib.equality.equal[hydra.core.TypeScheme](ts)(hydra.core.TypeScheme(Seq(),
         hydra.core.Type.variable("hydra.core.Type"), None)))(hydra.lib.eithers.map[hydra.core.Type, Option[hydra.core.TypeScheme],
         hydra.errors.Error]((decoded: hydra.core.Type) => Some(toTypeScheme(Seq())(decoded)))(decodeType(el.term)))(forTerm(hydra.strip.deannotateTerm(el.term)))))(el.`type`))((mts: Option[hydra.core.TypeScheme]) =>
      Right(hydra.lib.maybes.map[hydra.core.TypeScheme, Tuple2[hydra.core.Name, hydra.core.TypeScheme]]((ts: hydra.core.TypeScheme) => Tuple2(el.name,
         ts))(mts)))
  }
  hydra.lib.eithers.map[Seq[Option[Tuple2[hydra.core.Name, hydra.core.TypeScheme]]], Map[hydra.core.Name,
     hydra.core.TypeScheme], hydra.errors.Error]((mpairs: Seq[Option[Tuple2[hydra.core.Name, hydra.core.TypeScheme]]]) =>
    hydra.lib.maps.fromList[hydra.core.Name, hydra.core.TypeScheme](hydra.lib.maybes.cat[Tuple2[hydra.core.Name,
       hydra.core.TypeScheme]](mpairs)))(hydra.lib.eithers.mapList[hydra.core.Binding, Option[Tuple2[hydra.core.Name,
       hydra.core.TypeScheme]], hydra.errors.Error](toPair)(hydra.lexical.graphToBindings(g)))
}

def termAsBindings(term: hydra.core.Term): Seq[hydra.core.Binding] =
  hydra.strip.deannotateTerm(term) match
  case hydra.core.Term.let(v_Term_let_lt) => (v_Term_let_lt.bindings)
  case _ => Seq()

def typesToDefinitions(typeMap: Map[hydra.core.Name, hydra.core.Type]): Seq[hydra.core.Binding] =
  {
  def toElement(pair: Tuple2[hydra.core.Name, hydra.core.Type]): hydra.core.Binding =
    {
    lazy val name: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.core.Type](pair)
    hydra.core.Binding(name, hydra.encode.core.`type`(hydra.lib.pairs.second[hydra.core.Name, hydra.core.Type](pair)), None)
  }
  hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.Type], hydra.core.Binding](toElement)(hydra.lib.maps.toList[hydra.core.Name, hydra.core.Type](typeMap))
}

def withLambdaContext[T0, T1, T2](getContext: (T0 => hydra.graph.Graph))(setContext: (hydra.graph.Graph => T0 => T1))(env: T0)(lam: hydra.core.Lambda)(body: (T1 => T2)): T2 =
  {
  lazy val newContext: hydra.graph.Graph = hydra.scoping.extendGraphForLambda(getContext(env))(lam)
  body(setContext(newContext)(env))
}

def withLetContext[T0, T1, T2](getContext: (T0 => hydra.graph.Graph))(setContext: (hydra.graph.Graph => T0 => T1))(forBinding: (hydra.graph.Graph => hydra.core.Binding => Option[hydra.core.Term]))(env: T0)(letrec: hydra.core.Let)(body: (T1 => T2)): T2 =
  {
  lazy val newContext: hydra.graph.Graph = hydra.scoping.extendGraphForLet(forBinding)(getContext(env))(letrec)
  body(setContext(newContext)(env))
}

def withTypeLambdaContext[T0, T1, T2](getContext: (T0 => hydra.graph.Graph))(setContext: (hydra.graph.Graph => T0 => T1))(env: T0)(tlam: hydra.core.TypeLambda)(body: (T1 => T2)): T2 =
  {
  lazy val newContext: hydra.graph.Graph = hydra.scoping.extendGraphForTypeLambda(getContext(env))(tlam)
  body(setContext(newContext)(env))
}
