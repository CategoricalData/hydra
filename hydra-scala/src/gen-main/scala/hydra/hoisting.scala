package hydra.hoisting

import hydra.accessors.*

import hydra.core.*

import hydra.graph.*

import hydra.typing.*

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

def augmentBindingsWithNewFreeVars(cx: hydra.graph.Graph)(boundVars: scala.collection.immutable.Set[hydra.core.Name])(bindings: Seq[hydra.core.Binding]): Tuple2[Seq[hydra.core.Binding],
   hydra.typing.TermSubst] =
  {
  val types: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maps.map[hydra.core.TypeScheme, hydra.core.Type,
     hydra.core.Name](hydra.rewriting.typeSchemeToFType)(cx.boundTypes)
  def wrapAfterTypeLambdas(vars: Seq[Tuple2[hydra.core.Name, Option[hydra.core.Type]]])(term: hydra.core.Term): hydra.core.Term =
    term match
    case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_tl.parameter,
       wrapAfterTypeLambdas(vars)(v_Term_typeLambda_tl.body)))
    case _ => hydra.lib.lists.foldl[hydra.core.Term, Tuple2[hydra.core.Name, Option[hydra.core.Type]]]((t: hydra.core.Term) =>
      (p: Tuple2[hydra.core.Name, Option[hydra.core.Type]]) =>
      hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(hydra.lib.pairs.first[hydra.core.Name,
         Option[hydra.core.Type]](p), hydra.lib.pairs.second[hydra.core.Name, Option[hydra.core.Type]](p),
         t))))(term)(hydra.lib.lists.reverse[Tuple2[hydra.core.Name, Option[hydra.core.Type]]](vars))
  def augment(b: hydra.core.Binding): Tuple2[hydra.core.Binding, Option[Tuple2[hydra.core.Name, hydra.core.Term]]] =
    {
    val freeVars: Seq[hydra.core.Name] = hydra.lib.sets.toList[hydra.core.Name](hydra.lib.sets.intersection[hydra.core.Name](boundVars)(hydra.rewriting.freeVariablesInTerm(b.term)))
    val varTypePairs: Seq[Tuple2[hydra.core.Name, Option[hydra.core.Type]]] = hydra.lib.lists.map[hydra.core.Name,
       Tuple2[hydra.core.Name, Option[hydra.core.Type]]]((v: hydra.core.Name) =>
      Tuple2(v, hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Type](v)(types)))(freeVars)
    val varTypes: Seq[hydra.core.Type] = hydra.lib.maybes.cat[hydra.core.Type](hydra.lib.lists.map[Tuple2[hydra.core.Name,
       Option[hydra.core.Type]], Option[hydra.core.Type]](hydra.lib.pairs.second[hydra.core.Name, Option[hydra.core.Type]])(varTypePairs))
    hydra.lib.logic.ifElse[Tuple2[hydra.core.Binding, Option[Tuple2[hydra.core.Name, hydra.core.Term]]]](hydra.lib.logic.or(hydra.lib.lists.`null`[hydra.core.Name](freeVars))(hydra.lib.logic.not(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Type](varTypes))(hydra.lib.lists.length[Tuple2[hydra.core.Name,
       Option[hydra.core.Type]]](varTypePairs)))))(Tuple2(b, None))(Tuple2(hydra.core.Binding(b.name,
       wrapAfterTypeLambdas(varTypePairs)(b.term), hydra.lib.maybes.map[hydra.core.TypeScheme, hydra.core.TypeScheme]((ts: hydra.core.TypeScheme) =>
      hydra.core.TypeScheme(ts.variables, hydra.lib.lists.foldl[hydra.core.Type, hydra.core.Type]((acc: hydra.core.Type) =>
      (t: hydra.core.Type) => hydra.core.Type.function(hydra.core.FunctionType(t, acc)))(ts.`type`)(hydra.lib.lists.reverse[hydra.core.Type](varTypes)),
         (ts.constraints)))(b.`type`)), Some(Tuple2(b.name, hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Name]((t: hydra.core.Term) =>
      (v: hydra.core.Name) =>
      hydra.core.Term.application(hydra.core.Application(t, hydra.core.Term.variable(v))))(hydra.core.Term.variable(b.name))(freeVars)))))
  }
  val results: Seq[Tuple2[hydra.core.Binding, Option[Tuple2[hydra.core.Name, hydra.core.Term]]]] = hydra.lib.lists.map[hydra.core.Binding,
     Tuple2[hydra.core.Binding, Option[Tuple2[hydra.core.Name, hydra.core.Term]]]](augment)(bindings)
  Tuple2(hydra.lib.lists.map[Tuple2[hydra.core.Binding, Option[Tuple2[hydra.core.Name, hydra.core.Term]]],
     hydra.core.Binding](hydra.lib.pairs.first[hydra.core.Binding, Option[Tuple2[hydra.core.Name, hydra.core.Term]]])(results),
     hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Term](hydra.lib.maybes.cat[Tuple2[hydra.core.Name,
     hydra.core.Term]](hydra.lib.lists.map[Tuple2[hydra.core.Binding, Option[Tuple2[hydra.core.Name, hydra.core.Term]]],
     Option[Tuple2[hydra.core.Name, hydra.core.Term]]](hydra.lib.pairs.second[hydra.core.Binding, Option[Tuple2[hydra.core.Name,
     hydra.core.Term]]])(results))))
}

def bindingIsPolymorphic(binding: hydra.core.Binding): Boolean =
  hydra.lib.maybes.maybe[Boolean, hydra.core.TypeScheme](false)((ts: hydra.core.TypeScheme) =>
  hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Name](ts.variables)))(binding.`type`)

def bindingUsesContextTypeVars(cx: hydra.graph.Graph)(binding: hydra.core.Binding): Boolean =
  hydra.lib.maybes.maybe[Boolean, hydra.core.TypeScheme](false)((ts: hydra.core.TypeScheme) =>
  {
  val freeInType: scala.collection.immutable.Set[hydra.core.Name] = hydra.rewriting.freeVariablesInType(ts.`type`)
  {
    val contextTypeVars: scala.collection.immutable.Set[hydra.core.Name] = (cx.typeVariables)
    hydra.lib.logic.not(hydra.lib.sets.`null`[hydra.core.Name](hydra.lib.sets.intersection[hydra.core.Name](freeInType)(contextTypeVars)))
  }
})(binding.`type`)

def countVarOccurrences(name: hydra.core.Name)(term: hydra.core.Term): Int =
  {
  val childCount: Int = hydra.lib.lists.foldl[Int, hydra.core.Term]((acc: Int) =>
    (t: hydra.core.Term) =>
    hydra.lib.math.add(acc)(hydra.hoisting.countVarOccurrences(name)(t)))(0)(hydra.rewriting.subterms(term))
  term match
    case hydra.core.Term.variable(v_Term_variable_v) => hydra.lib.logic.ifElse[Int](hydra.lib.equality.equal[hydra.core.Name](v_Term_variable_v)(name))(hydra.lib.math.add(1)(childCount))(childCount)
    case _ => childCount
}

def hoistAllLetBindings(let0: hydra.core.Let): hydra.core.Let =
  {
  val emptyCx: hydra.graph.Graph = hydra.graph.Graph(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term],
     hydra.lib.maps.empty[hydra.core.Name, hydra.core.TypeScheme], hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeVariableMetadata], hydra.lib.sets.empty[hydra.core.Name], hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.Term], hydra.lib.maps.empty[hydra.core.Name, hydra.graph.Primitive], hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeScheme], hydra.lib.sets.empty[hydra.core.Name])
  hydra.hoisting.hoistLetBindingsWithPredicate((_x: hydra.core.Binding) => true)(hydra.hoisting.shouldHoistAll)(emptyCx)(let0)
}

def hoistCaseStatements(v1: hydra.graph.Graph)(v2: hydra.core.Term): hydra.core.Term = hydra.hoisting.hoistSubterms(hydra.hoisting.shouldHoistCaseStatement)(v1)(v2)

def hoistCaseStatementsInGraph(bindings: Seq[hydra.core.Binding]): Seq[hydra.core.Binding] =
  {
  val emptyTx: hydra.graph.Graph = hydra.graph.Graph(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term],
     hydra.lib.maps.empty[hydra.core.Name, hydra.core.TypeScheme], hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeVariableMetadata], hydra.lib.sets.empty[hydra.core.Name], hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.Term], hydra.lib.maps.empty[hydra.core.Name, hydra.graph.Primitive], hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeScheme], hydra.lib.sets.empty[hydra.core.Name])
  val term0: hydra.core.Term = hydra.core.Term.let(hydra.core.Let(bindings, hydra.core.Term.unit))
  val term1: hydra.core.Term = hydra.hoisting.hoistCaseStatements(emptyTx)(term0)
  hydra.schemas.termAsBindings(term1)
}

def hoistLetBindingsWithContext(isParentBinding: (hydra.core.Binding => Boolean))(cx: hydra.graph.Graph)(let0: hydra.core.Let): hydra.core.Let =
  hydra.hoisting.hoistLetBindingsWithPredicate(isParentBinding)(hydra.hoisting.shouldHoistPolymorphic)(cx)(let0)

def hoistLetBindingsWithPredicate(isParentBinding: (hydra.core.Binding => Boolean))(shouldHoistBinding: (hydra.graph.Graph => hydra.core.Binding => Boolean))(cx0: hydra.graph.Graph)(let0: hydra.core.Let): hydra.core.Let =
  {
  def hoistOne(prefix: scala.Predef.String)(cx: hydra.graph.Graph)(pair: Tuple2[Seq[Tuple2[hydra.core.Binding,
     hydra.core.Term]], scala.collection.immutable.Set[hydra.core.Name]])(bindingWithCapturedVars: Tuple2[hydra.core.Binding,
     Seq[hydra.core.Name]]): Tuple2[Seq[Tuple2[hydra.core.Binding, hydra.core.Term]], scala.collection.immutable.Set[hydra.core.Name]] =
    {
    val bindingAndReplacementPairs: Seq[Tuple2[hydra.core.Binding, hydra.core.Term]] = hydra.lib.pairs.first[Seq[Tuple2[hydra.core.Binding,
       hydra.core.Term]], scala.collection.immutable.Set[hydra.core.Name]](pair)
    val alreadyUsedNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.pairs.second[Seq[Tuple2[hydra.core.Binding,
       hydra.core.Term]], scala.collection.immutable.Set[hydra.core.Name]](pair)
    val b: hydra.core.Binding = hydra.lib.pairs.first[hydra.core.Binding, Seq[hydra.core.Name]](bindingWithCapturedVars)
    val capturedTermVars: Seq[hydra.core.Name] = hydra.lib.pairs.second[hydra.core.Binding, Seq[hydra.core.Name]](bindingWithCapturedVars)
    val types: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maps.map[hydra.core.TypeScheme, hydra.core.Type,
       hydra.core.Name](hydra.rewriting.typeSchemeToFType)(cx.boundTypes)
    val capturedTermVarTypePairs: Seq[Tuple2[hydra.core.Name, Option[hydra.core.Type]]] = hydra.lib.lists.map[hydra.core.Name,
       Tuple2[hydra.core.Name, Option[hydra.core.Type]]]((v: hydra.core.Name) =>
      Tuple2(v, hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Type](v)(types)))(capturedTermVars)
    val capturedTermVarTypes: Seq[hydra.core.Type] = hydra.lib.lists.map[hydra.core.Type, hydra.core.Type]((typ: hydra.core.Type) => hydra.rewriting.deannotateTypeParameters(typ))(hydra.lib.maybes.cat[hydra.core.Type](hydra.lib.lists.map[Tuple2[hydra.core.Name,
       Option[hydra.core.Type]], Option[hydra.core.Type]](hydra.lib.pairs.second[hydra.core.Name, Option[hydra.core.Type]])(capturedTermVarTypePairs)))
    val freeInBindingType: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.maybes.maybe[scala.collection.immutable.Set[hydra.core.Name],
       hydra.core.TypeScheme](hydra.lib.sets.empty[hydra.core.Name])((ts: hydra.core.TypeScheme) => hydra.rewriting.freeVariablesInType(ts.`type`))(b.`type`)
    val freeInCapturedVarTypes: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.unions[hydra.core.Name](hydra.lib.lists.map[hydra.core.Type,
       scala.collection.immutable.Set[hydra.core.Name]]((t: hydra.core.Type) => hydra.rewriting.freeVariablesInType(t))(capturedTermVarTypes))
    val capturedTypeVars: Seq[hydra.core.Name] = hydra.lib.sets.toList[hydra.core.Name](hydra.lib.sets.intersection[hydra.core.Name](cx.typeVariables)(hydra.lib.sets.union[hydra.core.Name](freeInBindingType)(freeInCapturedVarTypes)))
    val globalBindingName: hydra.core.Name = hydra.lexical.chooseUniqueName(alreadyUsedNames)(hydra.lib.strings.cat2(prefix)(b.name))
    val newUsedNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.insert[hydra.core.Name](globalBindingName)(alreadyUsedNames)
    val newTypeScheme: Option[hydra.core.TypeScheme] = hydra.lib.logic.ifElse[Option[hydra.core.TypeScheme]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Type](capturedTermVarTypes))(hydra.lib.lists.length[Tuple2[hydra.core.Name,
       Option[hydra.core.Type]]](capturedTermVarTypePairs)))(hydra.lib.maybes.map[hydra.core.TypeScheme,
       hydra.core.TypeScheme]((ts: hydra.core.TypeScheme) =>
      hydra.core.TypeScheme(hydra.lib.lists.nub[hydra.core.Name](hydra.lib.lists.concat2[hydra.core.Name](capturedTypeVars)(ts.variables)),
         hydra.lib.lists.foldl[hydra.core.Type, hydra.core.Type]((t: hydra.core.Type) =>
      (a: hydra.core.Type) => hydra.core.Type.function(hydra.core.FunctionType(a, t)))(ts.`type`)(hydra.lib.lists.reverse[hydra.core.Type](capturedTermVarTypes)),
         (ts.constraints)))(b.`type`))(None)
    val strippedTerm: hydra.core.Term = hydra.rewriting.stripTypeLambdas(b.term)
    val termWithLambdas: hydra.core.Term = hydra.lib.lists.foldl[hydra.core.Term, Tuple2[hydra.core.Name, Option[hydra.core.Type]]]((t: hydra.core.Term) =>
      (p: Tuple2[hydra.core.Name, Option[hydra.core.Type]]) =>
      hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(hydra.lib.pairs.first[hydra.core.Name,
         Option[hydra.core.Type]](p), hydra.lib.maybes.map[hydra.core.Type, hydra.core.Type]((dom: hydra.core.Type) => hydra.rewriting.deannotateTypeParameters(dom))(hydra.lib.pairs.second[hydra.core.Name,
         Option[hydra.core.Type]](p)), t))))(strippedTerm)(hydra.lib.lists.reverse[Tuple2[hydra.core.Name,
         Option[hydra.core.Type]]](capturedTermVarTypePairs))
    val termWithTypeLambdas: hydra.core.Term = hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Name]((t: hydra.core.Term) =>
      (v: hydra.core.Name) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(v, t)))(termWithLambdas)(hydra.lib.lists.reverse[hydra.core.Name](hydra.lib.maybes.maybe[Seq[hydra.core.Name],
         hydra.core.TypeScheme](Seq())((x: hydra.core.TypeScheme) => (x.variables))(newTypeScheme)))
    val withTypeApps: hydra.core.Term = hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Name]((t: hydra.core.Term) =>
      (v: hydra.core.Name) =>
      hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(t, hydra.core.Type.variable(v))))(hydra.core.Term.variable(globalBindingName))(capturedTypeVars)
    val replacement: hydra.core.Term = hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Name]((t: hydra.core.Term) =>
      (v: hydra.core.Name) =>
      hydra.core.Term.application(hydra.core.Application(t, hydra.core.Term.variable(v))))(withTypeApps)(capturedTermVars)
    val newBindingAndReplacement: Tuple2[hydra.core.Binding, hydra.core.Term] = Tuple2(hydra.core.Binding(globalBindingName,
       termWithTypeLambdas, newTypeScheme), replacement)
    val newPairs: Seq[Tuple2[hydra.core.Binding, hydra.core.Term]] = hydra.lib.lists.cons[Tuple2[hydra.core.Binding,
       hydra.core.Term]](newBindingAndReplacement)(bindingAndReplacementPairs)
    Tuple2(newPairs, newUsedNames)
  }
  def rewrite[T0, T1, T2](prefix: scala.Predef.String)(recurse: (Tuple2[Seq[T0], T1] => T2 => Tuple2[Tuple2[Seq[hydra.core.Binding],
     scala.collection.immutable.Set[hydra.core.Name]], hydra.core.Term]))(cx: hydra.graph.Graph)(bindingsAndNames: Tuple2[Seq[hydra.core.Binding],
     T1])(term: T2): Tuple2[Tuple2[Seq[hydra.core.Binding], scala.collection.immutable.Set[hydra.core.Name]],
     hydra.core.Term] =
    {
    val previouslyFinishedBindings: Seq[hydra.core.Binding] = hydra.lib.pairs.first[Seq[hydra.core.Binding], T1](bindingsAndNames)
    def emptyBindingsAndNames[T3]: Tuple2[Seq[T3], T1] =
      Tuple2(Seq(), hydra.lib.pairs.second[Seq[hydra.core.Binding], T1](bindingsAndNames))
    val result: Tuple2[Tuple2[Seq[hydra.core.Binding], scala.collection.immutable.Set[hydra.core.Name]], hydra.core.Term] = recurse(emptyBindingsAndNames)(term)
    val newBindingsAndNames: Tuple2[Seq[hydra.core.Binding], scala.collection.immutable.Set[hydra.core.Name]] = hydra.lib.pairs.first[Tuple2[Seq[hydra.core.Binding],
       scala.collection.immutable.Set[hydra.core.Name]], hydra.core.Term](result)
    val bindingsSoFar: Seq[hydra.core.Binding] = hydra.lib.pairs.first[Seq[hydra.core.Binding], scala.collection.immutable.Set[hydra.core.Name]](newBindingsAndNames)
    val alreadyUsedNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.pairs.second[Seq[hydra.core.Binding],
       scala.collection.immutable.Set[hydra.core.Name]](newBindingsAndNames)
    val newTerm: hydra.core.Term = hydra.lib.pairs.second[Tuple2[Seq[hydra.core.Binding], scala.collection.immutable.Set[hydra.core.Name]],
       hydra.core.Term](result)
    newTerm match
      case hydra.core.Term.let(v_Term_let_l) => {
        val body: hydra.core.Term = (v_Term_let_l.body)
        {
          val partitionPair: Tuple2[Seq[hydra.core.Binding], Seq[hydra.core.Binding]] = hydra.lib.lists.partition[hydra.core.Binding]((v1: hydra.core.Binding) => shouldHoistBinding(cx)(v1))(v_Term_let_l.bindings)
          {
            val hoistUs: Seq[hydra.core.Binding] = hydra.lib.pairs.first[Seq[hydra.core.Binding], Seq[hydra.core.Binding]](partitionPair)
            {
              val keepUs: Seq[hydra.core.Binding] = hydra.lib.pairs.second[Seq[hydra.core.Binding], Seq[hydra.core.Binding]](partitionPair)
              {
                val hoistedBindingNames: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.Binding,
                   hydra.core.Name]((x: hydra.core.Binding) => (x.name))(hoistUs)
                {
                  val polyLetVariables: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) =>
                    hydra.lib.maybes.maybe[Boolean, hydra.core.Type](false)(hydra.schemas.fTypeIsPolymorphic)(hydra.lib.maybes.map[hydra.core.TypeScheme,
                       hydra.core.Type](hydra.rewriting.typeSchemeToFType)(hydra.lib.maps.lookup[hydra.core.Name,
                       hydra.core.TypeScheme](v)(cx.boundTypes))))(hydra.lib.sets.toList[hydra.core.Name](hydra.lib.sets.difference[hydra.core.Name](hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name,
                       hydra.core.Term](cx.boundTerms)))(cx.lambdaVariables))))
                  {
                    val boundTermVariables: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.union[hydra.core.Name](cx.lambdaVariables)(hydra.lib.sets.difference[hydra.core.Name](hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name,
                       hydra.core.Term](cx.boundTerms)))(cx.lambdaVariables))
                    {
                      val freeVariablesInEachBinding: Seq[Seq[hydra.core.Name]] = hydra.lib.lists.map[hydra.core.Binding,
                         Seq[hydra.core.Name]]((b: hydra.core.Binding) =>
                        hydra.lib.sets.toList[hydra.core.Name](hydra.lib.sets.intersection[hydra.core.Name](boundTermVariables)(hydra.rewriting.freeVariablesInTerm(b.term))))(hoistUs)
                      {
                        val bindingDependencies: Seq[Tuple2[Seq[hydra.core.Name], Seq[hydra.core.Name]]] = hydra.lib.lists.map[Seq[hydra.core.Name],
                           Tuple2[Seq[hydra.core.Name], Seq[hydra.core.Name]]]((vars: Seq[hydra.core.Name]) =>
                          hydra.lib.lists.partition[hydra.core.Name]((v: hydra.core.Name) =>
                          hydra.lib.sets.member[hydra.core.Name](v)(hydra.lib.sets.fromList[hydra.core.Name](hoistedBindingNames)))(vars))(freeVariablesInEachBinding)
                        {
                          val bindingEdges: Seq[Tuple2[hydra.core.Name, Seq[hydra.core.Name]]] = hydra.lib.lists.zip[hydra.core.Name,
                             Seq[hydra.core.Name]](hoistedBindingNames)(hydra.lib.lists.map[Tuple2[Seq[hydra.core.Name],
                             Seq[hydra.core.Name]], Seq[hydra.core.Name]](hydra.lib.pairs.first[Seq[hydra.core.Name],
                             Seq[hydra.core.Name]])(bindingDependencies))
                          {
                            val bindingImmediateCapturedVars: Seq[Tuple2[hydra.core.Name, Seq[hydra.core.Name]]] = hydra.lib.lists.zip[hydra.core.Name,
                               Seq[hydra.core.Name]](hoistedBindingNames)(hydra.lib.lists.map[Tuple2[Seq[hydra.core.Name],
                               Seq[hydra.core.Name]], Seq[hydra.core.Name]](hydra.lib.pairs.second[Seq[hydra.core.Name],
                               Seq[hydra.core.Name]])(bindingDependencies))
                            {
                              val capturedVarsMap: Map[hydra.core.Name, scala.collection.immutable.Set[hydra.core.Name]] = hydra.lib.maps.fromList[hydra.core.Name,
                                 scala.collection.immutable.Set[hydra.core.Name]](hydra.sorting.propagateTags(bindingEdges)(bindingImmediateCapturedVars))
                              {
                                val bindingsWithCapturedVars: Seq[Tuple2[hydra.core.Binding, Seq[hydra.core.Name]]] = hydra.lib.lists.map[hydra.core.Binding,
                                   Tuple2[hydra.core.Binding, Seq[hydra.core.Name]]]((b: hydra.core.Binding) =>
                                  Tuple2(b, hydra.lib.maybes.maybe[Seq[hydra.core.Name], scala.collection.immutable.Set[hydra.core.Name]](Seq())((vars: scala.collection.immutable.Set[hydra.core.Name]) =>
                                  hydra.lib.sets.toList[hydra.core.Name](hydra.lib.sets.difference[hydra.core.Name](vars)(polyLetVariables)))(hydra.lib.maps.lookup[hydra.core.Name,
                                     scala.collection.immutable.Set[hydra.core.Name]](b.name)(capturedVarsMap))))(hoistUs)
                                {
                                  val hoistPairsAndNames: Tuple2[Seq[Tuple2[hydra.core.Binding, hydra.core.Term]],
                                     scala.collection.immutable.Set[hydra.core.Name]] = hydra.lib.lists.foldl[Tuple2[Seq[Tuple2[hydra.core.Binding,
                                     hydra.core.Term]], scala.collection.immutable.Set[hydra.core.Name]],
                                     Tuple2[hydra.core.Binding, Seq[hydra.core.Name]]]((v1: Tuple2[Seq[Tuple2[hydra.core.Binding,
                                     hydra.core.Term]], scala.collection.immutable.Set[hydra.core.Name]]) =>
                                    (v2: Tuple2[hydra.core.Binding, Seq[hydra.core.Name]]) => hoistOne(prefix)(cx)(v1)(v2))(Tuple2(Seq(),
                                       alreadyUsedNames))(bindingsWithCapturedVars)
                                  {
                                    val hoistPairs: Seq[Tuple2[hydra.core.Binding, hydra.core.Term]] = hydra.lib.lists.reverse[Tuple2[hydra.core.Binding,
                                       hydra.core.Term]](hydra.lib.pairs.first[Seq[Tuple2[hydra.core.Binding,
                                       hydra.core.Term]], scala.collection.immutable.Set[hydra.core.Name]](hoistPairsAndNames))
                                    {
                                      val hoistedBindings: Seq[hydra.core.Binding] = hydra.lib.lists.map[Tuple2[hydra.core.Binding,
                                         hydra.core.Term], hydra.core.Binding](hydra.lib.pairs.first[hydra.core.Binding,
                                         hydra.core.Term])(hoistPairs)
                                      {
                                        val replacements: Seq[hydra.core.Term] = hydra.lib.lists.map[Tuple2[hydra.core.Binding,
                                           hydra.core.Term], hydra.core.Term](hydra.lib.pairs.second[hydra.core.Binding,
                                           hydra.core.Term])(hoistPairs)
                                        {
                                          val finalUsedNames: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.pairs.second[Seq[Tuple2[hydra.core.Binding,
                                             hydra.core.Term]], scala.collection.immutable.Set[hydra.core.Name]](hoistPairsAndNames)
                                          {
                                            val hoistNameReplacementPairs: Seq[Tuple2[hydra.core.Name,
                                               hydra.core.Term]] = hydra.lib.lists.zip[hydra.core.Name,
                                               hydra.core.Term](hydra.lib.lists.map[hydra.core.Binding,
                                               hydra.core.Name]((x: hydra.core.Binding) => (x.name))(hoistUs))(replacements)
                                            {
                                              val hoistBindingMap: Map[hydra.core.Name, hydra.core.Binding] = hydra.lib.maps.fromList[hydra.core.Name,
                                                 hydra.core.Binding](hydra.lib.lists.map[hydra.core.Binding,
                                                 Tuple2[hydra.core.Name, hydra.core.Binding]]((b: hydra.core.Binding) => Tuple2(b.name,
                                                 b))(hoistUs))
                                              {
                                                def isCacheable(name: hydra.core.Name): Boolean =
                                                  {
                                                  val multiRef: Boolean = hydra.lib.equality.gte[Int](hydra.hoisting.countVarOccurrences(name)(body))(2)
                                                  val isPoly: Boolean = hydra.lib.maybes.maybe[Boolean,
                                                     hydra.core.Binding](false)((b: hydra.core.Binding) => hydra.hoisting.bindingIsPolymorphic(b))(hydra.lib.maps.lookup[hydra.core.Name,
                                                     hydra.core.Binding](name)(hoistBindingMap))
                                                  hydra.lib.logic.and(multiRef)(hydra.lib.logic.not(isPoly))
                                                }
                                                {
                                                  val singleRefPairs: Seq[Tuple2[hydra.core.Name, hydra.core.Term]] = hydra.lib.lists.filter[Tuple2[hydra.core.Name,
                                                     hydra.core.Term]]((p: Tuple2[hydra.core.Name, hydra.core.Term]) =>
                                                    hydra.lib.logic.not(isCacheable(hydra.lib.pairs.first[hydra.core.Name,
                                                       hydra.core.Term](p))))(hoistNameReplacementPairs)
                                                  {
                                                    val multiRefPairs: Seq[Tuple2[hydra.core.Name, hydra.core.Term]] = hydra.lib.lists.filter[Tuple2[hydra.core.Name,
                                                       hydra.core.Term]]((p: Tuple2[hydra.core.Name, hydra.core.Term]) =>
                                                      isCacheable(hydra.lib.pairs.first[hydra.core.Name, hydra.core.Term](p)))(hoistNameReplacementPairs)
                                                    {
                                                      val fullSubst: hydra.typing.TermSubst = hydra.lib.maps.fromList[hydra.core.Name,
                                                         hydra.core.Term](hoistNameReplacementPairs)
                                                      {
                                                        val bodyOnlySubst: hydra.typing.TermSubst = hydra.lib.maps.fromList[hydra.core.Name,
                                                           hydra.core.Term](singleRefPairs)
                                                        {
                                                          val bodySubst: hydra.core.Term = hydra.substitution.substituteInTerm(bodyOnlySubst)(body)
                                                          {
                                                            val cacheBindings: Seq[hydra.core.Binding] = hydra.lib.lists.map[Tuple2[hydra.core.Name,
                                                               hydra.core.Term], hydra.core.Binding]((p: Tuple2[hydra.core.Name,
                                                               hydra.core.Term]) =>
                                                              {
                                                              val origType: Option[hydra.core.TypeScheme] = hydra.lib.maybes.maybe[Option[hydra.core.TypeScheme],
                                                                 hydra.core.Binding](None)((b: hydra.core.Binding) => (b.`type`))(hydra.lib.maps.lookup[hydra.core.Name,
                                                                 hydra.core.Binding](hydra.lib.pairs.first[hydra.core.Name,
                                                                 hydra.core.Term](p))(hoistBindingMap))
                                                              hydra.core.Binding(hydra.lib.pairs.first[hydra.core.Name,
                                                                 hydra.core.Term](p), hydra.lib.pairs.second[hydra.core.Name,
                                                                 hydra.core.Term](p), origType)
                                                            })(multiRefPairs)
                                                            {
                                                              val bodyWithCache: hydra.core.Term = hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.lists.`null`[hydra.core.Binding](cacheBindings))(bodySubst)(hydra.core.Term.let(hydra.core.Let(cacheBindings,
                                                                 bodySubst)))
                                                              {
                                                                val keepUsSubst: Seq[hydra.core.Binding] = hydra.lib.lists.map[hydra.core.Binding,
                                                                   hydra.core.Binding]((v1: hydra.core.Binding) => hydra.substitution.substituteInBinding(fullSubst)(v1))(keepUs)
                                                                {
                                                                  val hoistedBindingsSubst: Seq[hydra.core.Binding] = hydra.lib.lists.map[hydra.core.Binding,
                                                                     hydra.core.Binding]((v1: hydra.core.Binding) => hydra.substitution.substituteInBinding(fullSubst)(v1))(hoistedBindings)
                                                                  {
                                                                    val bindingsSoFarSubst: Seq[hydra.core.Binding] = hydra.lib.lists.map[hydra.core.Binding,
                                                                       hydra.core.Binding]((v1: hydra.core.Binding) => hydra.substitution.substituteInBinding(fullSubst)(v1))(bindingsSoFar)
                                                                    {
                                                                      val augmentResult: Tuple2[Seq[hydra.core.Binding],
                                                                         hydra.typing.TermSubst] = hydra.hoisting.augmentBindingsWithNewFreeVars(cx)(hydra.lib.sets.difference[hydra.core.Name](boundTermVariables)(polyLetVariables))(bindingsSoFarSubst)
                                                                      {
                                                                        val bindingsSoFarAugmented: Seq[hydra.core.Binding] = hydra.lib.pairs.first[Seq[hydra.core.Binding],
                                                                           hydra.typing.TermSubst](augmentResult)
                                                                        {
                                                                          val augmentSubst: hydra.typing.TermSubst = hydra.lib.pairs.second[Seq[hydra.core.Binding],
                                                                             hydra.typing.TermSubst](augmentResult)
                                                                          {
                                                                            val hoistedBindingsFinal: Seq[hydra.core.Binding] = hydra.lib.lists.map[hydra.core.Binding,
                                                                               hydra.core.Binding]((v1: hydra.core.Binding) => hydra.substitution.substituteInBinding(augmentSubst)(v1))(hoistedBindingsSubst)
                                                                            {
                                                                              val bindingsSoFarFinal: Seq[hydra.core.Binding] = hydra.lib.lists.map[hydra.core.Binding,
                                                                                 hydra.core.Binding]((v1: hydra.core.Binding) => hydra.substitution.substituteInBinding(augmentSubst)(v1))(bindingsSoFarAugmented)
                                                                              {
                                                                                val bodyFinal: hydra.core.Term = hydra.substitution.substituteInTerm(augmentSubst)(bodyWithCache)
                                                                                {
                                                                                  val keepUsFinal: Seq[hydra.core.Binding] = hydra.lib.lists.map[hydra.core.Binding,
                                                                                     hydra.core.Binding]((v1: hydra.core.Binding) => hydra.substitution.substituteInBinding(augmentSubst)(v1))(keepUsSubst)
                                                                                  {
                                                                                    val finalTerm: hydra.core.Term = hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.lists.`null`[hydra.core.Binding](keepUsFinal))(bodyFinal)(hydra.core.Term.let(hydra.core.Let(keepUsFinal,
                                                                                       bodyFinal)))
                                                                                    Tuple2(Tuple2(hydra.lib.lists.concat[hydra.core.Binding](Seq(previouslyFinishedBindings,
                                                                                       hoistedBindingsFinal,
                                                                                       bindingsSoFarFinal)),
                                                                                       finalUsedNames),
                                                                                       finalTerm)
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
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      case _ => Tuple2(Tuple2(hydra.lib.lists.concat2[hydra.core.Binding](previouslyFinishedBindings)(bindingsSoFar), alreadyUsedNames), newTerm)
  }
  val cx1: hydra.graph.Graph = hydra.rewriting.extendGraphForLet((c: hydra.graph.Graph) => (b: hydra.core.Binding) => None)(cx0)(let0)
  def forActiveBinding(b: hydra.core.Binding): Seq[hydra.core.Binding] =
    {
    val prefix: scala.Predef.String = hydra.lib.strings.cat2(b.name)("_")
    def init[T0]: Tuple2[Seq[T0], scala.collection.immutable.Set[hydra.core.Name]] = Tuple2(Seq(), hydra.lib.sets.singleton[hydra.core.Name](b.name))
    val resultPair: Tuple2[Tuple2[Seq[hydra.core.Binding], scala.collection.immutable.Set[hydra.core.Name]],
       hydra.core.Term] = hydra.rewriting.rewriteAndFoldTermWithGraph((v1: (Tuple2[Seq[hydra.core.Binding],
       scala.collection.immutable.Set[hydra.core.Name]] => hydra.core.Term => Tuple2[Tuple2[Seq[hydra.core.Binding],
       scala.collection.immutable.Set[hydra.core.Name]], hydra.core.Term])) =>
      (v2: hydra.graph.Graph) =>
      (v3: Tuple2[Seq[hydra.core.Binding], scala.collection.immutable.Set[hydra.core.Name]]) => (v4: hydra.core.Term) => rewrite(prefix)(v1)(v2)(v3)(v4))(cx1)(init)(b.term)
    val resultBindings: Seq[hydra.core.Binding] = hydra.lib.pairs.first[Seq[hydra.core.Binding], scala.collection.immutable.Set[hydra.core.Name]](hydra.lib.pairs.first[Tuple2[Seq[hydra.core.Binding],
       scala.collection.immutable.Set[hydra.core.Name]], hydra.core.Term](resultPair))
    val resultTerm: hydra.core.Term = hydra.lib.pairs.second[Tuple2[Seq[hydra.core.Binding], scala.collection.immutable.Set[hydra.core.Name]],
       hydra.core.Term](resultPair)
    hydra.lib.lists.cons[hydra.core.Binding](hydra.core.Binding(b.name, resultTerm, (b.`type`)))(resultBindings)
  }
  def forBinding(b: hydra.core.Binding): Seq[hydra.core.Binding] =
    hydra.lib.logic.ifElse[Seq[hydra.core.Binding]](isParentBinding(b))(forActiveBinding(b))(Seq(b))
  hydra.core.Let(hydra.lib.lists.concat[hydra.core.Binding](hydra.lib.lists.map[hydra.core.Binding, Seq[hydra.core.Binding]](forBinding)(let0.bindings)),
     (let0.body))
}

def hoistPolymorphicLetBindings(isParentBinding: (hydra.core.Binding => Boolean))(let0: hydra.core.Let): hydra.core.Let =
  {
  val emptyCx: hydra.graph.Graph = hydra.graph.Graph(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Term],
     hydra.lib.maps.empty[hydra.core.Name, hydra.core.TypeScheme], hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeVariableMetadata], hydra.lib.sets.empty[hydra.core.Name], hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.Term], hydra.lib.maps.empty[hydra.core.Name, hydra.graph.Primitive], hydra.lib.maps.empty[hydra.core.Name,
     hydra.core.TypeScheme], hydra.lib.sets.empty[hydra.core.Name])
  hydra.hoisting.hoistLetBindingsWithPredicate(isParentBinding)(hydra.hoisting.shouldHoistPolymorphic)(emptyCx)(let0)
}

def hoistSubterms(shouldHoist: (Tuple2[Seq[hydra.accessors.TermAccessor], hydra.core.Term] => Boolean))(cx0: hydra.graph.Graph)(term0: hydra.core.Term): hydra.core.Term =
  {
  def processImmediateSubterm(cx: hydra.graph.Graph)(counter: Int)(namePrefix: scala.Predef.String)(pathPrefix: Seq[hydra.accessors.TermAccessor])(subterm: hydra.core.Term): Tuple2[Int,
     hydra.core.Term] =
    {
    val baselineLambdaVars: scala.collection.immutable.Set[hydra.core.Name] = (cx.lambdaVariables)
    def collectAndReplace(recurse: (Tuple2[Int, Seq[hydra.core.Binding]] => hydra.core.Term => Tuple2[Tuple2[Int,
       Seq[hydra.core.Binding]], hydra.core.Term]))(path: Seq[hydra.accessors.TermAccessor])(cxInner: hydra.graph.Graph)(acc: Tuple2[Int,
       Seq[hydra.core.Binding]])(term: hydra.core.Term): Tuple2[Tuple2[Int, Seq[hydra.core.Binding]],
       hydra.core.Term] =
      {
      val currentCounter: Int = hydra.lib.pairs.first[Int, Seq[hydra.core.Binding]](acc)
      val collectedBindings: Seq[hydra.core.Binding] = hydra.lib.pairs.second[Int, Seq[hydra.core.Binding]](acc)
      term match
        case hydra.core.Term.let(v_Term_let__) => Tuple2(acc, term)
        case hydra.core.Term.typeLambda(v_Term_typeLambda__) => Tuple2(acc, term)
        case _ => {
          val result: Tuple2[Tuple2[Int, Seq[hydra.core.Binding]], hydra.core.Term] = recurse(acc)(term)
          {
            val newAcc: Tuple2[Int, Seq[hydra.core.Binding]] = hydra.lib.pairs.first[Tuple2[Int, Seq[hydra.core.Binding]], hydra.core.Term](result)
            {
              val processedTerm: hydra.core.Term = hydra.lib.pairs.second[Tuple2[Int, Seq[hydra.core.Binding]], hydra.core.Term](result)
              {
                val newCounter: Int = hydra.lib.pairs.first[Int, Seq[hydra.core.Binding]](newAcc)
                {
                  val newBindings: Seq[hydra.core.Binding] = hydra.lib.pairs.second[Int, Seq[hydra.core.Binding]](newAcc)
                  {
                    val fullPath: Seq[hydra.accessors.TermAccessor] = hydra.lib.lists.concat2[hydra.accessors.TermAccessor](pathPrefix)(path)
                    hydra.lib.logic.ifElse[Tuple2[Tuple2[Int, Seq[hydra.core.Binding]], hydra.core.Term]](shouldHoist(Tuple2(fullPath, processedTerm)))({
                      val bindingName: hydra.core.Name = hydra.lib.strings.cat(Seq("_hoist_", namePrefix, "_", hydra.lib.literals.showInt32(newCounter)))
                      {
                        val allLambdaVars: scala.collection.immutable.Set[hydra.core.Name] = (cxInner.lambdaVariables)
                        {
                          val newLambdaVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.difference[hydra.core.Name](allLambdaVars)(baselineLambdaVars)
                          {
                            val freeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.rewriting.freeVariablesInTerm(processedTerm)
                            {
                              val capturedVars: Seq[hydra.core.Name] = hydra.lib.sets.toList[hydra.core.Name](hydra.lib.sets.intersection[hydra.core.Name](newLambdaVars)(freeVars))
                              {
                                val typeMap: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maps.map[hydra.core.TypeScheme,
                                   hydra.core.Type, hydra.core.Name](hydra.rewriting.typeSchemeToFType)(cxInner.boundTypes)
                                {
                                  val wrappedTerm: hydra.core.Term = hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Name]((body: hydra.core.Term) =>
                                    (varName: hydra.core.Name) =>
                                    hydra.core.Term.function(hydra.core.Function.lambda(hydra.core.Lambda(varName,
                                       hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Type](varName)(typeMap),
                                       body))))(processedTerm)(hydra.lib.lists.reverse[hydra.core.Name](capturedVars))
                                  {
                                    val reference: hydra.core.Term = hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Name]((fn: hydra.core.Term) =>
                                      (varName: hydra.core.Name) =>
                                      hydra.core.Term.application(hydra.core.Application(fn, hydra.core.Term.variable(varName))))(hydra.core.Term.variable(bindingName))(capturedVars)
                                    {
                                      val newBinding: hydra.core.Binding = hydra.core.Binding(bindingName, wrappedTerm, None)
                                      Tuple2(Tuple2(hydra.lib.math.add(newCounter)(1), hydra.lib.lists.cons[hydra.core.Binding](newBinding)(newBindings)),
                                         reference)
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    })(Tuple2(newAcc, processedTerm))
                  }
                }
              }
            }
          }
        }
    }
    val result: Tuple2[Tuple2[Int, Seq[hydra.core.Binding]], hydra.core.Term] = hydra.rewriting.rewriteAndFoldTermWithGraphAndPath(collectAndReplace)(cx)(Tuple2(counter,
       Seq()))(subterm)
    val finalAcc: Tuple2[Int, Seq[hydra.core.Binding]] = hydra.lib.pairs.first[Tuple2[Int, Seq[hydra.core.Binding]], hydra.core.Term](result)
    val transformedSubterm: hydra.core.Term = hydra.lib.pairs.second[Tuple2[Int, Seq[hydra.core.Binding]], hydra.core.Term](result)
    val finalCounter: Int = hydra.lib.pairs.first[Int, Seq[hydra.core.Binding]](finalAcc)
    val bindings: Seq[hydra.core.Binding] = hydra.lib.pairs.second[Int, Seq[hydra.core.Binding]](finalAcc)
    hydra.lib.logic.ifElse[Tuple2[Int, hydra.core.Term]](hydra.lib.lists.`null`[hydra.core.Binding](bindings))(Tuple2(finalCounter, transformedSubterm))({
      val localLet: hydra.core.Term = hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.reverse[hydra.core.Binding](bindings), transformedSubterm))
      Tuple2(finalCounter, localLet)
    })
  }
  def processLetTerm[T0](cx: hydra.graph.Graph)(counter: T0)(path: Seq[hydra.accessors.TermAccessor])(lt: hydra.core.Let): Tuple2[T0, hydra.core.Term] =
    {
    val bindings: Seq[hydra.core.Binding] = (lt.bindings)
    val body: hydra.core.Term = (lt.body)
    def processBinding(acc: Seq[hydra.core.Binding])(binding: hydra.core.Binding): Seq[hydra.core.Binding] =
      {
      val namePrefix: scala.Predef.String = hydra.lib.strings.intercalate("_")(hydra.lib.strings.splitOn(".")(binding.name))
      val bindingPathPrefix: Seq[hydra.accessors.TermAccessor] = hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(hydra.accessors.TermAccessor.letBinding(binding.name)))
      val result: Tuple2[Int, hydra.core.Term] = processImmediateSubterm(cx)(1)(namePrefix)(bindingPathPrefix)(binding.term)
      val newValue: hydra.core.Term = hydra.lib.pairs.second[Int, hydra.core.Term](result)
      val newBinding: hydra.core.Binding = hydra.core.Binding(binding.name, newValue, (binding.`type`))
      hydra.lib.lists.cons[hydra.core.Binding](newBinding)(acc)
    }
    val newBindingsReversed: Seq[hydra.core.Binding] = hydra.lib.lists.foldl[Seq[hydra.core.Binding], hydra.core.Binding](processBinding)(Seq())(bindings)
    val newBindings: Seq[hydra.core.Binding] = hydra.lib.lists.reverse[hydra.core.Binding](newBindingsReversed)
    val bodyPathPrefix: Seq[hydra.accessors.TermAccessor] = hydra.lib.lists.concat2[hydra.accessors.TermAccessor](path)(Seq(hydra.accessors.TermAccessor.letBody))
    val bodyResult: Tuple2[Int, hydra.core.Term] = processImmediateSubterm(cx)(1)("_body")(bodyPathPrefix)(body)
    val newBody: hydra.core.Term = hydra.lib.pairs.second[Int, hydra.core.Term](bodyResult)
    Tuple2(counter, hydra.core.Term.let(hydra.core.Let(newBindings, newBody)))
  }
  def rewrite[T0, T1](recurse: (T0 => hydra.core.Term => Tuple2[T1, hydra.core.Term]))(path: Seq[hydra.accessors.TermAccessor])(cx: hydra.graph.Graph)(counter: T0)(term: hydra.core.Term): Tuple2[T1,
     hydra.core.Term] =
    term match
    case hydra.core.Term.let(v_Term_let_lt) => {
      val recursed: Tuple2[T1, hydra.core.Term] = recurse(counter)(term)
      {
        val newCounter: T1 = hydra.lib.pairs.first[T1, hydra.core.Term](recursed)
        {
          val recursedTerm: hydra.core.Term = hydra.lib.pairs.second[T1, hydra.core.Term](recursed)
          recursedTerm match
            case hydra.core.Term.let(v_Term_let_lt2) => processLetTerm(cx)(newCounter)(path)(v_Term_let_lt2)
            case _ => Tuple2(newCounter, recursedTerm)
        }
      }
    }
    case _ => recurse(counter)(term)
  hydra.lib.pairs.second[Int, hydra.core.Term](hydra.rewriting.rewriteAndFoldTermWithGraphAndPath(rewrite)(cx0)(1)(term0))
}

def isApplicationFunction(acc: hydra.accessors.TermAccessor): Boolean =
  acc match
  case hydra.accessors.TermAccessor.applicationFunction => true
  case _ => false

def isEliminationUnion(f: hydra.core.Function): Boolean =
  f match
  case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
    case hydra.core.Elimination.union(v_Elimination_union__) => true
    case _ => false
  case _ => false

def isLambdaBody(acc: hydra.accessors.TermAccessor): Boolean =
  acc match
  case hydra.accessors.TermAccessor.lambdaBody => true
  case _ => false

def isUnionElimination(term: hydra.core.Term): Boolean =
  term match
  case hydra.core.Term.function(v_Term_function_f) => hydra.hoisting.isEliminationUnion(v_Term_function_f)
  case _ => false

def isUnionEliminationApplication(term: hydra.core.Term): Boolean =
  term match
  case hydra.core.Term.application(v_Term_application_app) => hydra.hoisting.isUnionElimination(hydra.rewriting.deannotateAndDetypeTerm(v_Term_application_app.function))
  case _ => false

def normalizePathForHoisting(path: Seq[hydra.accessors.TermAccessor]): Seq[hydra.accessors.TermAccessor] =
  {
  def go(remaining: Seq[hydra.accessors.TermAccessor]): Seq[hydra.accessors.TermAccessor] =
    hydra.lib.logic.ifElse[Seq[hydra.accessors.TermAccessor]](hydra.lib.logic.or(hydra.lib.lists.`null`[hydra.accessors.TermAccessor](remaining))(hydra.lib.lists.`null`[hydra.accessors.TermAccessor](hydra.lib.lists.tail[hydra.accessors.TermAccessor](remaining))))(remaining)({
    val first: hydra.accessors.TermAccessor = hydra.lib.lists.head[hydra.accessors.TermAccessor](remaining)
    {
      val second: hydra.accessors.TermAccessor = hydra.lib.lists.head[hydra.accessors.TermAccessor](hydra.lib.lists.tail[hydra.accessors.TermAccessor](remaining))
      {
        val rest: Seq[hydra.accessors.TermAccessor] = hydra.lib.lists.tail[hydra.accessors.TermAccessor](hydra.lib.lists.tail[hydra.accessors.TermAccessor](remaining))
        hydra.lib.logic.ifElse[Seq[hydra.accessors.TermAccessor]](hydra.lib.logic.and(hydra.hoisting.isApplicationFunction(first))(hydra.hoisting.isLambdaBody(second)))(hydra.lib.lists.cons[hydra.accessors.TermAccessor](hydra.accessors.TermAccessor.letBody)(go(rest)))(hydra.lib.lists.cons[hydra.accessors.TermAccessor](first)(go(hydra.lib.lists.tail[hydra.accessors.TermAccessor](remaining))))
      }
    }
  })
  go(path)
}

def shouldHoistAll[T0, T1](_x: T0)(_2: T1): Boolean = true

def shouldHoistCaseStatement(pathAndTerm: Tuple2[Seq[hydra.accessors.TermAccessor], hydra.core.Term]): Boolean =
  {
  val path: Seq[hydra.accessors.TermAccessor] = hydra.lib.pairs.first[Seq[hydra.accessors.TermAccessor], hydra.core.Term](pathAndTerm)
  val term: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.accessors.TermAccessor], hydra.core.Term](pathAndTerm)
  hydra.lib.logic.ifElse[Boolean](hydra.lib.logic.not(hydra.lib.logic.or(hydra.hoisting.isUnionElimination(term))(hydra.hoisting.isUnionEliminationApplication(term))))(false)({
    val finalState: Tuple2[Boolean, Boolean] = hydra.lib.lists.foldl[Tuple2[Boolean, Boolean], hydra.accessors.TermAccessor]((st: Tuple2[Boolean, Boolean]) =>
      (acc: hydra.accessors.TermAccessor) => hydra.hoisting.updateHoistState(acc)(st))(Tuple2(true, false))(path)
    hydra.lib.logic.not(hydra.lib.pairs.first[Boolean, Boolean](finalState))
  })
}

def shouldHoistPolymorphic(cx: hydra.graph.Graph)(binding: hydra.core.Binding): Boolean =
  hydra.lib.logic.or(hydra.hoisting.bindingIsPolymorphic(binding))(hydra.hoisting.bindingUsesContextTypeVars(cx)(binding))

def updateHoistState(accessor: hydra.accessors.TermAccessor)(state: Tuple2[Boolean, Boolean]): Tuple2[Boolean, Boolean] =
  {
  val atTop: Boolean = hydra.lib.pairs.first[Boolean, Boolean](state)
  val usedApp: Boolean = hydra.lib.pairs.second[Boolean, Boolean](state)
  hydra.lib.logic.ifElse[Tuple2[Boolean, Boolean]](hydra.lib.logic.not(atTop))(Tuple2(false, usedApp))(accessor match
    case hydra.accessors.TermAccessor.annotatedBody => Tuple2(true, usedApp)
    case hydra.accessors.TermAccessor.letBody => Tuple2(true, usedApp)
    case hydra.accessors.TermAccessor.letBinding(v_TermAccessor_letBinding__) => Tuple2(true, usedApp)
    case hydra.accessors.TermAccessor.lambdaBody => hydra.lib.logic.ifElse[Tuple2[Boolean, Boolean]](usedApp)(Tuple2(false, true))(Tuple2(true, false))
    case hydra.accessors.TermAccessor.unionCasesBranch(v_TermAccessor_unionCasesBranch__) => hydra.lib.logic.ifElse[Tuple2[Boolean,
       Boolean]](usedApp)(Tuple2(false, true))(Tuple2(true, false))
    case hydra.accessors.TermAccessor.unionCasesDefault => hydra.lib.logic.ifElse[Tuple2[Boolean, Boolean]](usedApp)(Tuple2(false, true))(Tuple2(true, false))
    case hydra.accessors.TermAccessor.applicationFunction => hydra.lib.logic.ifElse[Tuple2[Boolean, Boolean]](usedApp)(Tuple2(false, true))(Tuple2(true, true))
    case hydra.accessors.TermAccessor.applicationArgument => Tuple2(false, usedApp)
    case _ => Tuple2(false, usedApp))
}
