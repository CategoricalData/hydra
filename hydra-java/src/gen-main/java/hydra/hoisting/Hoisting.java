// Note: this is an automatically generated file. Do not edit.

package hydra.hoisting;

/**
 * Functions for deep term rewriting operations involving hoisting subterms or bindings into enclosing let terms.
 */
public interface Hoisting {
  static hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.typing.TermSubst> augmentBindingsWithNewFreeVars(hydra.typing.TypeContext cx, java.util.Set<hydra.core.Name> boundVars, java.util.List<hydra.core.Binding> bindings) {
    java.util.Map<hydra.core.Name, hydra.core.Type> types = ((cx)).types;
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> wrapAfterTypeLambdas = new java.util.concurrent.atomic.AtomicReference<>();
    wrapAfterTypeLambdas.set((java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (vars -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>, hydra.core.Term>>) (t -> (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>, hydra.core.Term>) (p -> new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(hydra.lib.pairs.First.apply((p)), hydra.lib.pairs.Second.apply((p)), (t)))))),
          (term),
          hydra.lib.lists.Reverse.apply((vars)));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda tl) {
        return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda((((tl)).value).parameter, ((wrapAfterTypeLambdas.get()).apply((vars))).apply((((tl)).value).body)));
      }
    }))));
    java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>> augment = (java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>>) (b -> {
      java.util.List<hydra.core.Name> freeVars = hydra.lib.sets.ToList.apply(hydra.lib.sets.Intersection.apply(
        (boundVars),
        hydra.rewriting.Rewriting.freeVariablesInTerm(((b)).term)));
      java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>> varTypePairs = hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>>) (v -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>((v), hydra.lib.maps.Lookup.apply(
          (v),
          (types)))))),
        (freeVars));
      java.util.List<hydra.core.Type> varTypes = hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>, hydra.util.Maybe<hydra.core.Type>>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>, hydra.util.Maybe<hydra.core.Type>>) ((hydra.lib.pairs.Second::apply))),
        (varTypePairs)));
      return hydra.lib.logic.IfElse.apply(
        hydra.lib.logic.Or.apply(
          hydra.lib.lists.Null.apply((freeVars)),
          hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
            hydra.lib.lists.Length.apply((varTypes)),
            hydra.lib.lists.Length.apply((varTypePairs))))),
        (hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>((b), (hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>) (hydra.util.Maybe.<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>nothing())))),
        (hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>(new hydra.core.Binding(((b)).name, ((wrapAfterTypeLambdas.get()).apply((varTypePairs))).apply(((b)).term), hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (ts -> new hydra.core.TypeScheme(((ts)).variables, hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (acc -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> new hydra.core.Type.Function(new hydra.core.FunctionType((t), (acc))))),
            ((ts)).type,
            hydra.lib.lists.Reverse.apply((varTypes))), ((ts)).constraints)),
          ((b)).type)), hydra.util.Maybe.just((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>(((b)).name, hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, hydra.core.Term>>) (t -> (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (v -> new hydra.core.Term.Application(new hydra.core.Application((t), new hydra.core.Term.Variable((v)))))),
          new hydra.core.Term.Variable(((b)).name),
          (freeVars))))))))));
    });
    java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>> results = hydra.lib.lists.Map.apply(
      (augment),
      (bindings));
    return (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.typing.TermSubst>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.typing.TermSubst>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.typing.TermSubst>(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>, hydra.core.Binding>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>, hydra.core.Binding>) ((hydra.lib.pairs.First::apply))),
      (results)), new hydra.typing.TermSubst(hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>>) ((hydra.lib.pairs.Second::apply))),
      (results))))))));
  }
  
  static Boolean bindingIsPolymorphic(hydra.core.Binding binding) {
    return hydra.lib.maybes.Maybe.apply(
      false,
      (java.util.function.Function<hydra.core.TypeScheme, Boolean>) (ts -> hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply(((ts)).variables))),
      ((binding)).type);
  }
  
  static Boolean bindingUsesContextTypeVars(hydra.typing.TypeContext cx, hydra.core.Binding binding) {
    return hydra.lib.maybes.Maybe.apply(
      false,
      (java.util.function.Function<hydra.core.TypeScheme, Boolean>) (ts -> {
        java.util.Set<hydra.core.Name> contextTypeVars = ((cx)).typeVariables;
        java.util.Set<hydra.core.Name> freeInType = hydra.rewriting.Rewriting.freeVariablesInType(((ts)).type);
        return hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.lib.sets.Intersection.apply(
          (freeInType),
          (contextTypeVars))));
      }),
      ((binding)).type);
  }
  
  static hydra.core.Let hoistAllLetBindings(hydra.core.Let let0) {
    hydra.typing.InferenceContext emptyIx = new hydra.typing.InferenceContext((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), false);
    hydra.typing.TypeContext emptyCx = new hydra.typing.TypeContext((java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (emptyIx));
    return hydra.hoisting.Hoisting.hoistLetBindingsWithPredicate(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (ignored -> true),
      p0 -> p1 -> hydra.hoisting.Hoisting.<hydra.typing.TypeContext, hydra.core.Binding>shouldHoistAll(
        (p0),
        (p1)),
      (emptyCx),
      (let0));
  }
  
  static hydra.core.Term hoistCaseStatements(hydra.typing.TypeContext v1, hydra.core.Term v2) {
    return hydra.hoisting.Hoisting.hoistSubterms(
      (hydra.hoisting.Hoisting::shouldHoistCaseStatement),
      (v1),
      (v2));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.graph.Graph> hoistCaseStatementsInGraph(hydra.graph.Graph graph) {
    hydra.typing.InferenceContext emptyIx = new hydra.typing.InferenceContext((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), false);
    hydra.typing.TypeContext emptyTx = new hydra.typing.TypeContext((java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (emptyIx));
    hydra.core.Term gterm0 = hydra.schemas.Schemas.graphAsTerm((graph));
    hydra.core.Term gterm1 = hydra.hoisting.Hoisting.hoistCaseStatements(
      (emptyTx),
      (gterm0));
    java.util.List<hydra.core.Binding> newElements = hydra.schemas.Schemas.termAsGraph((gterm1));
    return hydra.lib.flows.Pure.apply(new hydra.graph.Graph((newElements), ((graph)).environment, ((graph)).types, ((graph)).body, ((graph)).primitives, ((graph)).schema));
  }
  
  static hydra.core.Let hoistLetBindingsWithContext(java.util.function.Function<hydra.core.Binding, Boolean> isParentBinding, hydra.typing.TypeContext cx, hydra.core.Let let0) {
    return hydra.hoisting.Hoisting.hoistLetBindingsWithPredicate(
      (isParentBinding),
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, Boolean>>) (p0 -> p1 -> hydra.hoisting.Hoisting.shouldHoistPolymorphic(
        (p0),
        (p1))),
      (cx),
      (let0));
  }
  
  static hydra.core.Let hoistLetBindingsWithPredicate(java.util.function.Function<hydra.core.Binding, Boolean> isParentBinding, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, Boolean>> shouldHoistBinding, hydra.typing.TypeContext cx0, hydra.core.Let let0) {
    hydra.typing.TypeContext cx1 = hydra.schemas.Schemas.extendTypeContextForLet(
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (c -> (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>) (b -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))),
      (cx0),
      (let0));
    java.util.function.Function<String, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Name>>, hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>>>>> hoistOne = (java.util.function.Function<String, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Name>>, hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>>>>>) (prefix -> (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Name>>, hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>>>>) (cx -> (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Name>>, hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>>>) (pair -> (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Name>>, hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>>) (bindingWithCapturedVars -> {
      java.util.Set<hydra.core.Name> alreadyUsedNames = hydra.lib.pairs.Second.apply((pair));
      hydra.core.Binding b = hydra.lib.pairs.First.apply((bindingWithCapturedVars));
      java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>> bindingAndReplacementPairs = hydra.lib.pairs.First.apply((pair));
      java.util.List<hydra.core.Name> capturedTermVars = hydra.lib.pairs.Second.apply((bindingWithCapturedVars));
      java.util.Map<hydra.core.Name, hydra.core.Type> types = ((cx)).types;
      java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>> capturedTermVarTypePairs = hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>>) (v -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>((v), hydra.lib.maps.Lookup.apply(
          (v),
          (types)))))),
        (capturedTermVars));
      java.util.List<hydra.core.Type> capturedTermVarTypes = hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> hydra.rewriting.Rewriting.deannotateTypeParameters((typ))),
        hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>, hydra.util.Maybe<hydra.core.Type>>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>, hydra.util.Maybe<hydra.core.Type>>) ((hydra.lib.pairs.Second::apply))),
          (capturedTermVarTypePairs))));
      java.util.List<hydra.core.Name> capturedTypeVars = hydra.lib.sets.ToList.apply(hydra.lib.sets.Intersection.apply(
        ((cx)).typeVariables,
        hydra.lib.maybes.Maybe.apply(
          (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
          (java.util.function.Function<hydra.core.TypeScheme, java.util.Set<hydra.core.Name>>) (ts -> hydra.rewriting.Rewriting.freeVariablesInType(((ts)).type)),
          ((b)).type)));
      hydra.core.Name globalBindingName = hydra.lexical.Lexical.chooseUniqueName(
        (alreadyUsedNames),
        new hydra.core.Name(hydra.lib.strings.Cat2.apply(
          (prefix),
          (((b)).name).value)));
      hydra.util.Maybe<hydra.core.TypeScheme> newTypeScheme = hydra.lib.logic.IfElse.apply(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply((capturedTermVarTypes)),
          hydra.lib.lists.Length.apply((capturedTermVarTypePairs))),
        hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (ts -> new hydra.core.TypeScheme(hydra.lib.lists.Nub.apply(hydra.lib.lists.Concat2.apply(
            (capturedTypeVars),
            ((ts)).variables)), hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (t -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (a -> new hydra.core.Type.Function(new hydra.core.FunctionType((a), (t))))),
            ((ts)).type,
            hydra.lib.lists.Reverse.apply((capturedTermVarTypes))), ((ts)).constraints)),
          ((b)).type),
        (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()));
      hydra.core.Term replacement = hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, hydra.core.Term>>) (t -> (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (v -> new hydra.core.Term.Application(new hydra.core.Application((t), new hydra.core.Term.Variable((v)))))),
        new hydra.core.Term.Variable((globalBindingName)),
        (capturedTermVars));
      hydra.core.Term strippedTerm = hydra.rewriting.Rewriting.detypeTerm(((b)).term);
      hydra.core.Term termWithLambdas = hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>, hydra.core.Term>>) (t -> (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>, hydra.core.Term>) (p -> new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(hydra.lib.pairs.First.apply((p)), hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (dom -> hydra.rewriting.Rewriting.deannotateTypeParameters((dom))),
          hydra.lib.pairs.Second.apply((p))), (t)))))),
        (strippedTerm),
        hydra.lib.lists.Reverse.apply((capturedTermVarTypePairs)));
      hydra.core.Term termWithTypeLambdas = hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, hydra.core.Term>>) (t -> (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (v -> new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda((v), (t))))),
        (termWithLambdas),
        hydra.lib.lists.Reverse.apply(hydra.lib.maybes.Maybe.apply(
          (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()),
          projected -> projected.variables,
          (newTypeScheme))));
      hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term> newBindingAndReplacement = (hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>(new hydra.core.Binding((globalBindingName), (termWithTypeLambdas), (newTypeScheme)), (replacement))));
      java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>> newPairs = hydra.lib.lists.Cons.apply(
        (newBindingAndReplacement),
        (bindingAndReplacementPairs));
      java.util.Set<hydra.core.Name> newUsedNames = hydra.lib.sets.Insert.apply(
        (globalBindingName),
        (alreadyUsedNames));
      return (hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>((newPairs), (newUsedNames))));
    }))));
    java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>> forActiveBinding = (java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>>) (b -> {
      String prefix = hydra.lib.strings.Cat2.apply(
        (((b)).name).value,
        "_");
      hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term> resultPair = hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContext(
        ((java.util.function.Function<String, java.util.function.Function<java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>>>>>>) (v1 -> (java.util.function.Function<java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>>>>>) (v2 -> (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>>>>) (v3 -> (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>>>) (v4 -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>>) (v5 -> hydra.hoisting.Hoisting.hoistLetBindingsWithPredicate_rewrite(
          (hoistOne),
          (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.typing.TermSubst>>>>) (p0 -> p1 -> p2 -> hydra.hoisting.Hoisting.augmentBindingsWithNewFreeVars(
            (p0),
            (p1),
            (p2))),
          (hydra.rewriting.Rewriting::freeVariablesInTerm),
          (hydra.schemas.Schemas::fTypeIsPolymorphic),
          (java.util.function.Function<hydra.typing.TermSubst, java.util.function.Function<hydra.core.Binding, hydra.core.Binding>>) (p0 -> p1 -> hydra.substitution.Substitution.substituteInBinding(
            (p0),
            (p1))),
          (java.util.function.Function<hydra.typing.TermSubst, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (p0 -> p1 -> hydra.substitution.Substitution.substituteInTerm(
            (p0),
            (p1))),
          (shouldHoistBinding),
          (v1),
          (v2),
          (v3),
          (v4),
          (v5)))))))).apply((prefix)),
        (cx1),
        hydra.hoisting.Hoisting.hoistLetBindingsWithPredicate_init((b)),
        ((b)).term);
      java.util.List<hydra.core.Binding> resultBindings = hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply((resultPair)));
      hydra.core.Term resultTerm = hydra.lib.pairs.Second.apply((resultPair));
      return hydra.lib.lists.Cons.apply(
        new hydra.core.Binding(((b)).name, (resultTerm), ((b)).type),
        (resultBindings));
    });
    java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>> forBinding = (java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>>) (b -> hydra.lib.logic.IfElse.apply(
      ((isParentBinding)).apply((b)),
      ((forActiveBinding)).apply((b)),
      java.util.List.of((b))));
    return new hydra.core.Let(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (forBinding),
      ((let0)).bindings)), ((let0)).body);
  }
  
  static <T0, T1, T2, T3, T4> hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1>, hydra.core.Term> hoistLetBindingsWithPredicate_rewrite(java.util.function.Function<T0, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, T1>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Name>>, hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, T1>>>>> hoistOne, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.typing.TermSubst>>>> hydra_hoisting_augmentBindingsWithNewFreeVars2, java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>> hydra_rewriting_freeVariablesInTerm2, java.util.function.Function<hydra.core.Type, Boolean> hydra_schemas_fTypeIsPolymorphic2, java.util.function.Function<hydra.typing.TermSubst, java.util.function.Function<hydra.core.Binding, hydra.core.Binding>> hydra_substitution_substituteInBinding2, java.util.function.Function<hydra.typing.TermSubst, java.util.function.Function<hydra.core.Term, hydra.core.Term>> hydra_substitution_substituteInTerm2, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, Boolean>> shouldHoistBinding, T0 prefix, java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<T2>, T3>, java.util.function.Function<T4, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1>, hydra.core.Term>>> recurse, hydra.typing.TypeContext cx, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T3> bindingsAndNames, T4 term) {
    hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1>, hydra.core.Term> result = (((recurse)).apply(hydra.hoisting.Hoisting.hoistLetBindingsWithPredicate_emptyBindingsAndNames((bindingsAndNames)))).apply((term));
    hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1> newBindingsAndNames = hydra.lib.pairs.First.apply((result));
    T1 alreadyUsedNames = hydra.lib.pairs.Second.apply((newBindingsAndNames));
    java.util.List<hydra.core.Binding> bindingsSoFar = hydra.lib.pairs.First.apply((newBindingsAndNames));
    hydra.core.Term newTerm = hydra.lib.pairs.Second.apply((result));
    java.util.List<hydra.core.Binding> previouslyFinishedBindings = hydra.lib.pairs.First.apply((bindingsAndNames));
    return ((newTerm)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1>, hydra.core.Term>((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1>(hydra.lib.lists.Concat2.apply(
          (previouslyFinishedBindings),
          (bindingsSoFar)), (alreadyUsedNames)))), (newTerm))));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1>, hydra.core.Term> visit(hydra.core.Term.Let l) {
        hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>> partitionPair = hydra.lib.lists.Partition.apply(
          (java.util.function.Function<hydra.core.Binding, Boolean>) (v1 -> (((shouldHoistBinding)).apply((cx))).apply((v1))),
          (((l)).value).bindings);
        java.util.List<hydra.core.Binding> hoistUs = hydra.lib.pairs.First.apply((partitionPair));
        java.util.Set<hydra.core.Name> boundTermVariables = hydra.lib.sets.Union.apply(
          ((cx)).lambdaVariables,
          ((cx)).letVariables);
        java.util.List<java.util.List<hydra.core.Name>> freeVariablesInEachBinding = hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Name>>) (b -> hydra.lib.sets.ToList.apply(hydra.lib.sets.Intersection.apply(
            (boundTermVariables),
            ((hydra_rewriting_freeVariablesInTerm2)).apply(((b)).term)))),
          (hoistUs));
        java.util.List<hydra.core.Name> hoistedBindingNames = hydra.lib.lists.Map.apply(
          projected -> projected.name,
          (hoistUs));
        java.util.List<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Name>>> bindingDependencies = hydra.lib.lists.Map.apply(
          (java.util.function.Function<java.util.List<hydra.core.Name>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Name>>>) (vars -> hydra.lib.lists.Partition.apply(
            (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.sets.Member.apply(
              (v),
              hydra.lib.sets.FromList.apply((hoistedBindingNames)))),
            (vars))),
          (freeVariablesInEachBinding));
        java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>> bindingEdges = hydra.lib.lists.Zip.apply(
          (hoistedBindingNames),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Name>>, java.util.List<hydra.core.Name>>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Name>>, java.util.List<hydra.core.Name>>) ((hydra.lib.pairs.First::apply))),
            (bindingDependencies)));
        java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>> bindingImmediateCapturedVars = hydra.lib.lists.Zip.apply(
          (hoistedBindingNames),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Name>>, java.util.List<hydra.core.Name>>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Name>>, java.util.List<hydra.core.Name>>) ((hydra.lib.pairs.Second::apply))),
            (bindingDependencies)));
        java.util.Map<hydra.core.Name, java.util.Set<hydra.core.Name>> capturedVarsMap = hydra.lib.maps.FromList.apply(hydra.sorting.Sorting.propagateTags(
          (bindingEdges),
          (bindingImmediateCapturedVars)));
        java.util.Set<hydra.core.Name> polyLetVariables = hydra.lib.sets.FromList.apply(hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.maybes.Maybe.apply(
            false,
            (hydra_schemas_fTypeIsPolymorphic2),
            hydra.lib.maps.Lookup.apply(
              (v),
              ((cx)).types))),
          hydra.lib.sets.ToList.apply(((cx)).letVariables)));
        java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Name>>> bindingsWithCapturedVars = hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Name>>>) (b -> (hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Name>>((b), hydra.lib.maybes.Maybe.apply(
            (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()),
            (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.List<hydra.core.Name>>) (vars -> hydra.lib.sets.ToList.apply(hydra.lib.sets.Difference.apply(
              (vars),
              (polyLetVariables)))),
            hydra.lib.maps.Lookup.apply(
              ((b)).name,
              (capturedVarsMap))))))),
          (hoistUs));
        hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, T1> hoistPairsAndNames = hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, T1>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Name>>, hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, T1>>>) (v1 -> (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, java.util.List<hydra.core.Name>>, hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, T1>>) (v2 -> (((((hoistOne)).apply((prefix))).apply((cx))).apply((v1))).apply((v2)))),
          (hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, T1>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, T1>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>, T1>((java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>) (java.util.List.<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>>of()), (alreadyUsedNames)))),
          (bindingsWithCapturedVars));
        java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>> hoistPairs = hydra.lib.lists.Reverse.apply(hydra.lib.pairs.First.apply((hoistPairsAndNames)));
        java.util.List<hydra.core.Term> replacements = hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>, hydra.core.Term>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>, hydra.core.Term>) ((hydra.lib.pairs.Second::apply))),
          (hoistPairs));
        hydra.typing.TermSubst subst = new hydra.typing.TermSubst(hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
          hydra.lib.lists.Map.apply(
            projected -> projected.name,
            (hoistUs)),
          (replacements))));
        java.util.List<hydra.core.Binding> bindingsSoFarSubst = hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (v1 -> (((hydra_substitution_substituteInBinding2)).apply((subst))).apply((v1))),
          (bindingsSoFar));
        hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.typing.TermSubst> augmentResult = ((((hydra_hoisting_augmentBindingsWithNewFreeVars2)).apply((cx))).apply(hydra.lib.sets.Difference.apply(
          (boundTermVariables),
          (polyLetVariables)))).apply((bindingsSoFarSubst));
        hydra.typing.TermSubst augmentSubst = hydra.lib.pairs.Second.apply((augmentResult));
        java.util.List<hydra.core.Binding> bindingsSoFarAugmented = hydra.lib.pairs.First.apply((augmentResult));
        java.util.List<hydra.core.Binding> bindingsSoFarFinal = hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (v1 -> (((hydra_substitution_substituteInBinding2)).apply((augmentSubst))).apply((v1))),
          (bindingsSoFarAugmented));
        hydra.core.Term body = (((l)).value).body;
        hydra.core.Term bodySubst = (((hydra_substitution_substituteInTerm2)).apply((subst))).apply((body));
        hydra.core.Term bodyFinal = (((hydra_substitution_substituteInTerm2)).apply((augmentSubst))).apply((bodySubst));
        java.util.List<hydra.core.Binding> keepUs = hydra.lib.pairs.Second.apply((partitionPair));
        java.util.List<hydra.core.Binding> keepUsSubst = hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (v1 -> (((hydra_substitution_substituteInBinding2)).apply((subst))).apply((v1))),
          (keepUs));
        java.util.List<hydra.core.Binding> keepUsFinal = hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (v1 -> (((hydra_substitution_substituteInBinding2)).apply((augmentSubst))).apply((v1))),
          (keepUsSubst));
        hydra.core.Term finalTerm = hydra.lib.logic.IfElse.apply(
          hydra.lib.lists.Null.apply((keepUsFinal)),
          (bodyFinal),
          new hydra.core.Term.Let(new hydra.core.Let((keepUsFinal), (bodyFinal))));
        T1 finalUsedNames = hydra.lib.pairs.Second.apply((hoistPairsAndNames));
        java.util.List<hydra.core.Binding> hoistedBindings = hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>, hydra.core.Binding>) ((java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Binding, hydra.core.Term>, hydra.core.Binding>) ((hydra.lib.pairs.First::apply))),
          (hoistPairs));
        java.util.List<hydra.core.Binding> hoistedBindingsSubst = hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (v1 -> (((hydra_substitution_substituteInBinding2)).apply((subst))).apply((v1))),
          (hoistedBindings));
        java.util.List<hydra.core.Binding> hoistedBindingsFinal = hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (v1 -> (((hydra_substitution_substituteInBinding2)).apply((augmentSubst))).apply((v1))),
          (hoistedBindingsSubst));
        return (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1>, hydra.core.Term>((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, T1>(hydra.lib.lists.Concat.apply(java.util.List.of(
          (previouslyFinishedBindings),
          (hoistedBindingsFinal),
          (bindingsSoFarFinal))), (finalUsedNames)))), (finalTerm))));
      }
    });
  }
  
  static <T0> hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.Set<hydra.core.Name>> hoistLetBindingsWithPredicate_init(hydra.core.Binding b) {
    return (hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.Set<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.Set<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.Set<hydra.core.Name>>((java.util.List<T0>) (java.util.List.<T0>of()), hydra.lib.sets.Singleton.apply(((b)).name))));
  }
  
  static <T0, T1, T2> hydra.util.Tuple.Tuple2<java.util.List<T2>, T1> hoistLetBindingsWithPredicate_emptyBindingsAndNames(hydra.util.Tuple.Tuple2<T0, T1> bindingsAndNames) {
    return (hydra.util.Tuple.Tuple2<java.util.List<T2>, T1>) ((hydra.util.Tuple.Tuple2<java.util.List<T2>, T1>) (new hydra.util.Tuple.Tuple2<java.util.List<T2>, T1>((java.util.List<T2>) (java.util.List.<T2>of()), hydra.lib.pairs.Second.apply((bindingsAndNames)))));
  }
  
  static hydra.core.Let hoistPolymorphicLetBindings(java.util.function.Function<hydra.core.Binding, Boolean> isParentBinding, hydra.core.Let let0) {
    hydra.typing.InferenceContext emptyIx = new hydra.typing.InferenceContext((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), false);
    hydra.typing.TypeContext emptyCx = new hydra.typing.TypeContext((java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (emptyIx));
    return hydra.hoisting.Hoisting.hoistLetBindingsWithPredicate(
      (isParentBinding),
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, Boolean>>) (p0 -> p1 -> hydra.hoisting.Hoisting.shouldHoistPolymorphic(
        (p0),
        (p1))),
      (emptyCx),
      (let0));
  }
  
  static hydra.core.Term hoistSubterms(java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.TermAccessor>, hydra.core.Term>, Boolean> shouldHoist, hydra.typing.TypeContext cx0, hydra.core.Term term0) {
    java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<Integer, java.util.function.Function<String, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<Integer, hydra.core.Term>>>>> processImmediateSubterm = (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<Integer, java.util.function.Function<String, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<Integer, hydra.core.Term>>>>>) (cx -> (java.util.function.Function<Integer, java.util.function.Function<String, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<Integer, hydra.core.Term>>>>) (counter -> (java.util.function.Function<String, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<Integer, hydra.core.Term>>>) (namePrefix -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<Integer, hydra.core.Term>>) (subterm -> {
      java.util.Set<hydra.core.Name> baselineLambdaVars = ((cx)).lambdaVariables;
      java.util.function.Function<java.util.function.Function<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>>>>> collectAndReplace = (java.util.function.Function<java.util.function.Function<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>>>>>) (recurse -> (java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>>>>) (path -> (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>>>) (cxInner -> (java.util.function.Function<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>) (term -> {
        java.util.List<hydra.core.Binding> collectedBindings = hydra.lib.pairs.Second.apply((acc));
        Integer currentCounter = hydra.lib.pairs.First.apply((acc));
        return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term> otherwise(hydra.core.Term instance) {
            hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term> result2 = (((recurse)).apply((acc))).apply((term));
            hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>> newAcc = hydra.lib.pairs.First.apply((result2));
            java.util.List<hydra.core.Binding> newBindings = hydra.lib.pairs.Second.apply((newAcc));
            Integer newCounter = hydra.lib.pairs.First.apply((newAcc));
            hydra.core.Term processedTerm = hydra.lib.pairs.Second.apply((result2));
            return hydra.lib.logic.IfElse.apply(
              ((shouldHoist)).apply((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.TermAccessor>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.TermAccessor>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.TermAccessor>, hydra.core.Term>((path), (processedTerm))))),
              ((java.util.function.Function<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>) (bindingName -> {
                java.util.Set<hydra.core.Name> allLambdaVars = ((cxInner)).lambdaVariables;
                java.util.Set<hydra.core.Name> freeVars = hydra.rewriting.Rewriting.freeVariablesInTerm((processedTerm));
                java.util.Set<hydra.core.Name> newLambdaVars = hydra.lib.sets.Difference.apply(
                  (allLambdaVars),
                  (baselineLambdaVars));
                java.util.List<hydra.core.Name> capturedVars = hydra.lib.sets.ToList.apply(hydra.lib.sets.Intersection.apply(
                  (newLambdaVars),
                  (freeVars)));
                hydra.core.Term wrappedTerm = hydra.lib.lists.Foldl.apply(
                  (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, hydra.core.Term>>) (body -> (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (varName -> new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda((varName), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), (body)))))),
                  (processedTerm),
                  hydra.lib.lists.Reverse.apply((capturedVars)));
                hydra.core.Binding newBinding = new hydra.core.Binding((bindingName), (wrappedTerm), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()));
                hydra.core.Term reference = hydra.lib.lists.Foldl.apply(
                  (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, hydra.core.Term>>) (fn -> (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (varName -> new hydra.core.Term.Application(new hydra.core.Application((fn), new hydra.core.Term.Variable((varName)))))),
                  new hydra.core.Term.Variable((bindingName)),
                  (capturedVars));
                return (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>((hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>(hydra.lib.math.Add.apply(
                  (newCounter),
                  1), hydra.lib.lists.Cons.apply(
                  (newBinding),
                  (newBindings))))), (reference))));
              })).apply(new hydra.core.Name(hydra.lib.strings.Cat.apply(java.util.List.of(
                "_hoist_",
                (namePrefix),
                "_",
                hydra.lib.literals.ShowInt32.apply((newCounter)))))),
              (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>((newAcc), (processedTerm)))));
          }
          
          @Override
          public hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term> visit(hydra.core.Term.Let ignored) {
            return (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>((acc), (term))));
          }
          
          @Override
          public hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term> visit(hydra.core.Term.TypeLambda ignored) {
            return (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>((acc), (term))));
          }
        });
      })))));
      hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term> result = hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContextAndPath(
        (collectAndReplace),
        (cx),
        (hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>) ((hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>) (new hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>>((counter), (java.util.List<hydra.core.Binding>) (java.util.List.<hydra.core.Binding>of())))),
        (subterm));
      hydra.util.Tuple.Tuple2<Integer, java.util.List<hydra.core.Binding>> finalAcc = hydra.lib.pairs.First.apply((result));
      java.util.List<hydra.core.Binding> bindings = hydra.lib.pairs.Second.apply((finalAcc));
      Integer finalCounter = hydra.lib.pairs.First.apply((finalAcc));
      hydra.core.Term transformedSubterm = hydra.lib.pairs.Second.apply((result));
      return hydra.lib.logic.IfElse.apply(
        hydra.lib.lists.Null.apply((bindings)),
        (hydra.util.Tuple.Tuple2<Integer, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<Integer, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<Integer, hydra.core.Term>((finalCounter), (transformedSubterm)))),
        ((java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<Integer, hydra.core.Term>>) (localLet -> (hydra.util.Tuple.Tuple2<Integer, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<Integer, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<Integer, hydra.core.Term>((finalCounter), (localLet)))))).apply(new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Reverse.apply((bindings)), (transformedSubterm)))));
    }))));
    return hydra.lib.pairs.Second.apply(hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContext(
      (java.util.function.Function<java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<Integer, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<Integer, hydra.core.Term>>>>>) (v1 -> (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<Integer, hydra.core.Term>>>>) (v2 -> (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<Integer, hydra.core.Term>>>) (v3 -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<Integer, hydra.core.Term>>) (v4 -> hydra.hoisting.Hoisting.hoistSubterms_rewrite(
        (processImmediateSubterm),
        (v1),
        (v2),
        (v3),
        (v4)))))),
      (cx0),
      1,
      (term0)));
  }
  
  static <T0, T1, T2> hydra.util.Tuple.Tuple2<T2, hydra.core.Term> hoistSubterms_processLetTerm(java.util.function.Function<T0, java.util.function.Function<Integer, java.util.function.Function<String, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T1, hydra.core.Term>>>>> processImmediateSubterm, T0 cx, T2 counter, hydra.core.Let lt) {
    java.util.List<hydra.core.Binding> bindings = ((lt)).bindings;
    hydra.core.Term body = ((lt)).body;
    hydra.util.Tuple.Tuple2<T1, hydra.core.Term> bodyResult = (((((processImmediateSubterm)).apply((cx))).apply(1)).apply("_body")).apply((body));
    java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>>> processBinding = (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>>>) (acc -> (java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>>) (binding -> {
      String namePrefix = hydra.lib.strings.Intercalate.apply(
        "_",
        hydra.lib.strings.SplitOn.apply(
          ".",
          (((binding)).name).value));
      hydra.util.Tuple.Tuple2<T1, hydra.core.Term> result = (((((processImmediateSubterm)).apply((cx))).apply(1)).apply((namePrefix))).apply(((binding)).term);
      hydra.core.Term newValue = hydra.lib.pairs.Second.apply((result));
      hydra.core.Binding newBinding = new hydra.core.Binding(((binding)).name, (newValue), ((binding)).type);
      return hydra.lib.lists.Cons.apply(
        (newBinding),
        (acc));
    }));
    java.util.List<hydra.core.Binding> newBindingsReversed = hydra.lib.lists.Foldl.apply(
      (processBinding),
      (java.util.List<hydra.core.Binding>) (java.util.List.<hydra.core.Binding>of()),
      (bindings));
    java.util.List<hydra.core.Binding> newBindings = hydra.lib.lists.Reverse.apply((newBindingsReversed));
    hydra.core.Term newBody = hydra.lib.pairs.Second.apply((bodyResult));
    return (hydra.util.Tuple.Tuple2<T2, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T2, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T2, hydra.core.Term>((counter), new hydra.core.Term.Let(new hydra.core.Let((newBindings), (newBody))))));
  }
  
  static <T0, T1, T2, T3> hydra.util.Tuple.Tuple2<T3, hydra.core.Term> hoistSubterms_rewrite(java.util.function.Function<T0, java.util.function.Function<Integer, java.util.function.Function<String, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T1, hydra.core.Term>>>>> processImmediateSubterm, java.util.function.Function<T2, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T3, hydra.core.Term>>> recurse, T0 cx, T2 counter, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Tuple.Tuple2<T3, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (((recurse)).apply((counter))).apply((term));
      }
      
      @Override
      public hydra.util.Tuple.Tuple2<T3, hydra.core.Term> visit(hydra.core.Term.Let lt) {
        hydra.core.Term recursedTerm = hydra.lib.pairs.Second.apply(hydra.hoisting.Hoisting.hoistSubterms_recursed(
          (counter),
          (recurse),
          (term)));
        return ((recursedTerm)).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Tuple.Tuple2<T3, hydra.core.Term> otherwise(hydra.core.Term instance) {
            return (hydra.util.Tuple.Tuple2<T3, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T3, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T3, hydra.core.Term>(hydra.hoisting.Hoisting.hoistSubterms_newCounter(hydra.hoisting.Hoisting.hoistSubterms_recursed(
              (counter),
              (recurse),
              (term))), (recursedTerm))));
          }
          
          @Override
          public hydra.util.Tuple.Tuple2<T3, hydra.core.Term> visit(hydra.core.Term.Let lt2) {
            return ((((java.util.function.Function<T0, java.util.function.Function<T3, java.util.function.Function<hydra.core.Let, hydra.util.Tuple.Tuple2<T3, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<T3, java.util.function.Function<hydra.core.Let, hydra.util.Tuple.Tuple2<T3, hydra.core.Term>>>) (v2 -> (java.util.function.Function<hydra.core.Let, hydra.util.Tuple.Tuple2<T3, hydra.core.Term>>) (v3 -> hydra.hoisting.Hoisting.<T0, T1, T3>hoistSubterms_processLetTerm(
              (processImmediateSubterm),
              (v1),
              (v2),
              (v3)))))).apply((cx))).apply(hydra.hoisting.Hoisting.hoistSubterms_newCounter(hydra.hoisting.Hoisting.hoistSubterms_recursed(
              (counter),
              (recurse),
              (term))))).apply(((lt2)).value);
          }
        });
      }
    });
  }
  
  static <T0, T1, T2> T2 hoistSubterms_recursed(T0 counter, java.util.function.Function<T0, java.util.function.Function<T1, T2>> recurse, T1 term) {
    return (((recurse)).apply((counter))).apply((term));
  }
  
  static <T0, T1> T0 hoistSubterms_newCounter(hydra.util.Tuple.Tuple2<T0, T1> recursed) {
    return hydra.lib.pairs.First.apply((recursed));
  }
  
  static Boolean isApplicationFunction(hydra.accessors.TermAccessor acc) {
    return ((acc)).accept(new hydra.accessors.TermAccessor.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.accessors.TermAccessor instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.accessors.TermAccessor.ApplicationFunction ignored) {
        return true;
      }
    });
  }
  
  static Boolean isEliminationUnion(hydra.core.Function f) {
    return ((f)).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Function instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Function.Elimination e) {
        return (((e)).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.core.Elimination instance) {
            return false;
          }
          
          @Override
          public Boolean visit(hydra.core.Elimination.Union ignored) {
            return true;
          }
        });
      }
    });
  }
  
  static Boolean isLambdaBody(hydra.accessors.TermAccessor acc) {
    return ((acc)).accept(new hydra.accessors.TermAccessor.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.accessors.TermAccessor instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.accessors.TermAccessor.LambdaBody ignored) {
        return true;
      }
    });
  }
  
  static Boolean isUnionElimination(hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Function f) {
        return hydra.hoisting.Hoisting.isEliminationUnion(((f)).value);
      }
    });
  }
  
  static java.util.List<hydra.accessors.TermAccessor> normalizePathForHoisting(java.util.List<hydra.accessors.TermAccessor> path) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.List<hydra.accessors.TermAccessor>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.List<hydra.accessors.TermAccessor>>) (remaining -> hydra.lib.logic.IfElse.apply(
      hydra.lib.logic.Or.apply(
        hydra.lib.lists.Null.apply((remaining)),
        hydra.lib.lists.Null.apply(hydra.lib.lists.Tail.apply((remaining)))),
      (remaining),
      ((java.util.function.Function<hydra.accessors.TermAccessor, java.util.List<hydra.accessors.TermAccessor>>) (first -> {
        java.util.List<hydra.accessors.TermAccessor> rest = hydra.lib.lists.Tail.apply(hydra.lib.lists.Tail.apply((remaining)));
        hydra.accessors.TermAccessor second = hydra.lib.lists.Head.apply(hydra.lib.lists.Tail.apply((remaining)));
        return hydra.lib.logic.IfElse.apply(
          hydra.lib.logic.And.apply(
            hydra.hoisting.Hoisting.isApplicationFunction((first)),
            hydra.hoisting.Hoisting.isLambdaBody((second))),
          hydra.lib.lists.Cons.apply(
            new hydra.accessors.TermAccessor.LetBody(true),
            (go.get()).apply((rest))),
          hydra.lib.lists.Cons.apply(
            (first),
            (go.get()).apply(hydra.lib.lists.Tail.apply((remaining)))));
      })).apply(hydra.lib.lists.Head.apply((remaining))))));
    return (go.get()).apply((path));
  }
  
  static <T0> hydra.util.Tuple.Tuple2<T0, hydra.core.Term> rewriteAndFoldTermWithTypeContext(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>> f, hydra.typing.TypeContext cx0, T0 val0, hydra.core.Term term0) {
    return (hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply(hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContext_result(
      (f),
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Lambda, hydra.typing.TypeContext>>) (p0 -> p1 -> hydra.schemas.Schemas.extendTypeContextForLambda(
        (p0),
        (p1))),
      (java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Let, hydra.typing.TypeContext>>>) (p0 -> p1 -> p2 -> hydra.schemas.Schemas.extendTypeContextForLet(
        (p0),
        (p1),
        (p2))),
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.TypeLambda, hydra.typing.TypeContext>>) (p0 -> p1 -> hydra.schemas.Schemas.extendTypeContextForTypeLambda(
        (p0),
        (p1))),
      (cx0),
      (term0),
      (val0)))), hydra.lib.pairs.Second.apply(hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContext_result(
      (f),
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Lambda, hydra.typing.TypeContext>>) (p0 -> p1 -> hydra.schemas.Schemas.extendTypeContextForLambda(
        (p0),
        (p1))),
      (java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Let, hydra.typing.TypeContext>>>) (p0 -> p1 -> p2 -> hydra.schemas.Schemas.extendTypeContextForLet(
        (p0),
        (p1),
        (p2))),
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.TypeLambda, hydra.typing.TypeContext>>) (p0 -> p1 -> hydra.schemas.Schemas.extendTypeContextForTypeLambda(
        (p0),
        (p1))),
      (cx0),
      (term0),
      (val0))))));
  }
  
  static <T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11> hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T6, T4>, T7> rewriteAndFoldTermWithTypeContext_wrapper(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T2, T3>>>, java.util.function.Function<T4, java.util.function.Function<T5, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T6, T7>>>>> f, java.util.function.Function<T4, java.util.function.Function<hydra.core.Lambda, T4>> hydra_schemas_extendTypeContextForLambda2, java.util.function.Function<java.util.function.Function<T8, java.util.function.Function<T9, hydra.util.Maybe<T10>>>, java.util.function.Function<T4, java.util.function.Function<hydra.core.Let, T4>>> hydra_schemas_extendTypeContextForLet2, java.util.function.Function<T4, java.util.function.Function<hydra.core.TypeLambda, T4>> hydra_schemas_extendTypeContextForTypeLambda2, java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T4>, java.util.function.Function<T1, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T2, T11>, T3>>> lowLevelRecurse, hydra.util.Tuple.Tuple2<T5, T4> valAndCx, hydra.core.Term term) {
    T4 cx = hydra.lib.pairs.Second.apply((valAndCx));
    T4 cx1 = ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public T4 otherwise(hydra.core.Term instance) {
        return (cx);
      }
      
      @Override
      public T4 visit(hydra.core.Term.Function fun) {
        return (((fun)).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public T4 otherwise(hydra.core.Function instance) {
            return (cx);
          }
          
          @Override
          public T4 visit(hydra.core.Function.Lambda l) {
            return (((hydra_schemas_extendTypeContextForLambda2)).apply((cx))).apply(((l)).value);
          }
        });
      }
      
      @Override
      public T4 visit(hydra.core.Term.Let l) {
        return ((((hydra_schemas_extendTypeContextForLet2)).apply((java.util.function.Function<T8, java.util.function.Function<T9, hydra.util.Maybe<T10>>>) (ignored -> (java.util.function.Function<T9, hydra.util.Maybe<T10>>) (_2 -> (hydra.util.Maybe<T10>) (hydra.util.Maybe.<T10>nothing()))))).apply((cx))).apply(((l)).value);
      }
      
      @Override
      public T4 visit(hydra.core.Term.TypeLambda tl) {
        return (((hydra_schemas_extendTypeContextForTypeLambda2)).apply((cx))).apply(((tl)).value);
      }
    });
    return (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T6, T4>, T7>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T6, T4>, T7>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T6, T4>, T7>((hydra.util.Tuple.Tuple2<T6, T4>) ((hydra.util.Tuple.Tuple2<T6, T4>) (new hydra.util.Tuple.Tuple2<T6, T4>(hydra.lib.pairs.First.apply(hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContext_fResult(
      (cx1),
      (f),
      (java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T2, T3>>>) (v1 -> (java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T2, T3>>) (v2 -> hydra.hoisting.Hoisting.<T4, T0, T1, T2, T11, T3>rewriteAndFoldTermWithTypeContext_recurseForUser(
        (cx1),
        (lowLevelRecurse),
        (v1),
        (v2)))),
      (term),
      hydra.hoisting.Hoisting.<T5, T4>rewriteAndFoldTermWithTypeContext_val((valAndCx)))), (cx)))), hydra.lib.pairs.Second.apply(hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContext_fResult(
      (cx1),
      (f),
      (java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T2, T3>>>) (v1 -> (java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T2, T3>>) (v2 -> hydra.hoisting.Hoisting.<T4, T0, T1, T2, T11, T3>rewriteAndFoldTermWithTypeContext_recurseForUser(
        (cx1),
        (lowLevelRecurse),
        (v1),
        (v2)))),
      (term),
      hydra.hoisting.Hoisting.<T5, T4>rewriteAndFoldTermWithTypeContext_val((valAndCx)))))));
  }
  
  static <T0, T1, T2, T3, T4> hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T0, T1>, hydra.core.Term> rewriteAndFoldTermWithTypeContext_result(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>, java.util.function.Function<T1, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>> f, java.util.function.Function<T1, java.util.function.Function<hydra.core.Lambda, T1>> hydra_schemas_extendTypeContextForLambda2, java.util.function.Function<java.util.function.Function<T2, java.util.function.Function<T3, hydra.util.Maybe<T4>>>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Let, T1>>> hydra_schemas_extendTypeContextForLet2, java.util.function.Function<T1, java.util.function.Function<hydra.core.TypeLambda, T1>> hydra_schemas_extendTypeContextForTypeLambda2, T1 cx0, hydra.core.Term term0, T0 val0) {
    return hydra.rewriting.Rewriting.<hydra.util.Tuple.Tuple2<T0, T1>>rewriteAndFoldTerm(
      (java.util.function.Function<java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T0, T1>, hydra.core.Term>>>, java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T0, T1>, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T1>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T0, T1>, hydra.core.Term>>>) (v2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T0, T1>, hydra.core.Term>>) (v3 -> hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContext_wrapper(
        (f),
        (hydra_schemas_extendTypeContextForLambda2),
        (hydra_schemas_extendTypeContextForLet2),
        (hydra_schemas_extendTypeContextForTypeLambda2),
        (v1),
        (v2),
        (v3))))),
      (hydra.util.Tuple.Tuple2<T0, T1>) ((hydra.util.Tuple.Tuple2<T0, T1>) (new hydra.util.Tuple.Tuple2<T0, T1>((val0), (cx0)))),
      (term0));
  }
  
  static <T0, T1> T0 rewriteAndFoldTermWithTypeContext_val(hydra.util.Tuple.Tuple2<T0, T1> valAndCx) {
    return hydra.lib.pairs.First.apply((valAndCx));
  }
  
  static <T0, T1, T2, T3, T4, T5> hydra.util.Tuple.Tuple2<T3, T5> rewriteAndFoldTermWithTypeContext_recurseForUser(T0 cx1, java.util.function.Function<hydra.util.Tuple.Tuple2<T1, T0>, java.util.function.Function<T2, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T3, T4>, T5>>> lowLevelRecurse, T1 newVal, T2 subterm) {
    return (hydra.util.Tuple.Tuple2<T3, T5>) ((hydra.util.Tuple.Tuple2<T3, T5>) (new hydra.util.Tuple.Tuple2<T3, T5>(hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply(hydra.hoisting.Hoisting.<T0, T1, T2, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T3, T4>, T5>>rewriteAndFoldTermWithTypeContext_result2(
      (cx1),
      (lowLevelRecurse),
      (newVal),
      (subterm)))), hydra.lib.pairs.Second.apply(hydra.hoisting.Hoisting.<T0, T1, T2, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T3, T4>, T5>>rewriteAndFoldTermWithTypeContext_result2(
      (cx1),
      (lowLevelRecurse),
      (newVal),
      (subterm))))));
  }
  
  static <T0, T1, T2, T3, T4> T4 rewriteAndFoldTermWithTypeContext_fResult(T0 cx1, java.util.function.Function<T1, java.util.function.Function<T0, java.util.function.Function<T2, java.util.function.Function<T3, T4>>>> f, T1 recurseForUser, T3 term, T2 val) {
    return (((((f)).apply((recurseForUser))).apply((cx1))).apply((val))).apply((term));
  }
  
  static <T0, T1, T2, T3> T3 rewriteAndFoldTermWithTypeContext_result2(T0 cx1, java.util.function.Function<hydra.util.Tuple.Tuple2<T1, T0>, java.util.function.Function<T2, T3>> lowLevelRecurse, T1 newVal, T2 subterm) {
    return (((lowLevelRecurse)).apply((hydra.util.Tuple.Tuple2<T1, T0>) ((hydra.util.Tuple.Tuple2<T1, T0>) (new hydra.util.Tuple.Tuple2<T1, T0>((newVal), (cx1)))))).apply((subterm));
  }
  
  static <T0> hydra.util.Tuple.Tuple2<T0, hydra.core.Term> rewriteAndFoldTermWithTypeContextAndPath(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>>> f, hydra.typing.TypeContext cx0, T0 val0, hydra.core.Term term0) {
    return (hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<T0, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<T0, hydra.core.Term>(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContextAndPath_result(
      (f),
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Lambda, hydra.typing.TypeContext>>) (p0 -> p1 -> hydra.schemas.Schemas.extendTypeContextForLambda(
        (p0),
        (p1))),
      (java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Let, hydra.typing.TypeContext>>>) (p0 -> p1 -> p2 -> hydra.schemas.Schemas.extendTypeContextForLet(
        (p0),
        (p1),
        (p2))),
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.TypeLambda, hydra.typing.TypeContext>>) (p0 -> p1 -> hydra.schemas.Schemas.extendTypeContextForTypeLambda(
        (p0),
        (p1))),
      (cx0),
      (term0),
      (val0)))), hydra.lib.pairs.Second.apply(hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContextAndPath_result(
      (f),
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Lambda, hydra.typing.TypeContext>>) (p0 -> p1 -> hydra.schemas.Schemas.extendTypeContextForLambda(
        (p0),
        (p1))),
      (java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Let, hydra.typing.TypeContext>>>) (p0 -> p1 -> p2 -> hydra.schemas.Schemas.extendTypeContextForLet(
        (p0),
        (p1),
        (p2))),
      (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.TypeLambda, hydra.typing.TypeContext>>) (p0 -> p1 -> hydra.schemas.Schemas.extendTypeContextForTypeLambda(
        (p0),
        (p1))),
      (cx0),
      (term0),
      (val0))))));
  }
  
  static <T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12> hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T5, T7>, T8> rewriteAndFoldTermWithTypeContextAndPath_wrapper(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T2, T3>>>, java.util.function.Function<T4, java.util.function.Function<T5, java.util.function.Function<T6, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T7, T8>>>>>> f, java.util.function.Function<T5, java.util.function.Function<hydra.core.Lambda, T5>> hydra_schemas_extendTypeContextForLambda2, java.util.function.Function<java.util.function.Function<T9, java.util.function.Function<T10, hydra.util.Maybe<T11>>>, java.util.function.Function<T5, java.util.function.Function<hydra.core.Let, T5>>> hydra_schemas_extendTypeContextForLet2, java.util.function.Function<T5, java.util.function.Function<hydra.core.TypeLambda, T5>> hydra_schemas_extendTypeContextForTypeLambda2, java.util.function.Function<T4, java.util.function.Function<hydra.util.Tuple.Tuple2<T5, T0>, java.util.function.Function<T1, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T12, T2>, T3>>>> recurse, T4 path, hydra.util.Tuple.Tuple2<T5, T6> cxAndVal, hydra.core.Term term) {
    T5 cx = hydra.lib.pairs.First.apply((cxAndVal));
    T5 cx1 = ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public T5 otherwise(hydra.core.Term instance) {
        return (cx);
      }
      
      @Override
      public T5 visit(hydra.core.Term.Function fun) {
        return (((fun)).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public T5 otherwise(hydra.core.Function instance) {
            return (cx);
          }
          
          @Override
          public T5 visit(hydra.core.Function.Lambda l) {
            return (((hydra_schemas_extendTypeContextForLambda2)).apply((cx))).apply(((l)).value);
          }
        });
      }
      
      @Override
      public T5 visit(hydra.core.Term.Let l) {
        return ((((hydra_schemas_extendTypeContextForLet2)).apply((java.util.function.Function<T9, java.util.function.Function<T10, hydra.util.Maybe<T11>>>) (ignored -> (java.util.function.Function<T10, hydra.util.Maybe<T11>>) (_2 -> (hydra.util.Maybe<T11>) (hydra.util.Maybe.<T11>nothing()))))).apply((cx))).apply(((l)).value);
      }
      
      @Override
      public T5 visit(hydra.core.Term.TypeLambda tl) {
        return (((hydra_schemas_extendTypeContextForTypeLambda2)).apply((cx))).apply(((tl)).value);
      }
    });
    return (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T5, T7>, T8>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T5, T7>, T8>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T5, T7>, T8>((hydra.util.Tuple.Tuple2<T5, T7>) ((hydra.util.Tuple.Tuple2<T5, T7>) (new hydra.util.Tuple.Tuple2<T5, T7>((cx), hydra.lib.pairs.First.apply(hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContextAndPath_fResult(
      (cx1),
      (f),
      (path),
      (java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T2, T3>>>) (v1 -> (java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T2, T3>>) (v2 -> hydra.hoisting.Hoisting.<T5, T4, T0, T1, T12, T2, T3>rewriteAndFoldTermWithTypeContextAndPath_recurseForUser(
        (cx1),
        (path),
        (recurse),
        (v1),
        (v2)))),
      (term),
      hydra.hoisting.Hoisting.<T5, T6>rewriteAndFoldTermWithTypeContextAndPath_val((cxAndVal))))))), hydra.lib.pairs.Second.apply(hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContextAndPath_fResult(
      (cx1),
      (f),
      (path),
      (java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T2, T3>>>) (v1 -> (java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T2, T3>>) (v2 -> hydra.hoisting.Hoisting.<T5, T4, T0, T1, T12, T2, T3>rewriteAndFoldTermWithTypeContextAndPath_recurseForUser(
        (cx1),
        (path),
        (recurse),
        (v1),
        (v2)))),
      (term),
      hydra.hoisting.Hoisting.<T5, T6>rewriteAndFoldTermWithTypeContextAndPath_val((cxAndVal)))))));
  }
  
  static <T0, T1, T2, T3, T4> hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T1, T0>, hydra.core.Term> rewriteAndFoldTermWithTypeContextAndPath_result(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<T1, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<T0, hydra.core.Term>>>>>> f, java.util.function.Function<T1, java.util.function.Function<hydra.core.Lambda, T1>> hydra_schemas_extendTypeContextForLambda2, java.util.function.Function<java.util.function.Function<T2, java.util.function.Function<T3, hydra.util.Maybe<T4>>>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Let, T1>>> hydra_schemas_extendTypeContextForLet2, java.util.function.Function<T1, java.util.function.Function<hydra.core.TypeLambda, T1>> hydra_schemas_extendTypeContextForTypeLambda2, T1 cx0, hydra.core.Term term0, T0 val0) {
    return hydra.rewriting.Rewriting.<hydra.util.Tuple.Tuple2<T1, T0>>rewriteAndFoldTermWithPath(
      (java.util.function.Function<java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Tuple.Tuple2<T1, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T1, T0>, hydra.core.Term>>>>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Tuple.Tuple2<T1, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T1, T0>, hydra.core.Term>>>>>) (v1 -> (java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Tuple.Tuple2<T1, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T1, T0>, hydra.core.Term>>>>) (v2 -> (java.util.function.Function<hydra.util.Tuple.Tuple2<T1, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T1, T0>, hydra.core.Term>>>) (v3 -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T1, T0>, hydra.core.Term>>) (v4 -> hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContextAndPath_wrapper(
        (f),
        (hydra_schemas_extendTypeContextForLambda2),
        (hydra_schemas_extendTypeContextForLet2),
        (hydra_schemas_extendTypeContextForTypeLambda2),
        (v1),
        (v2),
        (v3),
        (v4)))))),
      (hydra.util.Tuple.Tuple2<T1, T0>) ((hydra.util.Tuple.Tuple2<T1, T0>) (new hydra.util.Tuple.Tuple2<T1, T0>((cx0), (val0)))),
      (term0));
  }
  
  static <T0, T1> T1 rewriteAndFoldTermWithTypeContextAndPath_val(hydra.util.Tuple.Tuple2<T0, T1> cxAndVal) {
    return hydra.lib.pairs.Second.apply((cxAndVal));
  }
  
  static <T0, T1, T2, T3, T4, T5, T6> hydra.util.Tuple.Tuple2<T5, T6> rewriteAndFoldTermWithTypeContextAndPath_recurseForUser(T0 cx1, T1 path, java.util.function.Function<T1, java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T2>, java.util.function.Function<T3, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T4, T5>, T6>>>> recurse, T2 valIn, T3 termIn) {
    return (hydra.util.Tuple.Tuple2<T5, T6>) ((hydra.util.Tuple.Tuple2<T5, T6>) (new hydra.util.Tuple.Tuple2<T5, T6>(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(hydra.hoisting.Hoisting.<T0, T1, T2, T3, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T4, T5>, T6>>rewriteAndFoldTermWithTypeContextAndPath_result2(
      (cx1),
      (path),
      (recurse),
      (termIn),
      (valIn)))), hydra.lib.pairs.Second.apply(hydra.hoisting.Hoisting.<T0, T1, T2, T3, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<T4, T5>, T6>>rewriteAndFoldTermWithTypeContextAndPath_result2(
      (cx1),
      (path),
      (recurse),
      (termIn),
      (valIn))))));
  }
  
  static <T0, T1, T2, T3, T4, T5> T5 rewriteAndFoldTermWithTypeContextAndPath_fResult(T0 cx1, java.util.function.Function<T1, java.util.function.Function<T2, java.util.function.Function<T0, java.util.function.Function<T3, java.util.function.Function<T4, T5>>>>> f, T2 path, T1 recurseForUser, T4 term, T3 val) {
    return ((((((f)).apply((recurseForUser))).apply((path))).apply((cx1))).apply((val))).apply((term));
  }
  
  static <T0, T1, T2, T3, T4> T4 rewriteAndFoldTermWithTypeContextAndPath_result2(T0 cx1, T1 path, java.util.function.Function<T1, java.util.function.Function<hydra.util.Tuple.Tuple2<T0, T2>, java.util.function.Function<T3, T4>>> recurse, T3 termIn, T2 valIn) {
    return ((((recurse)).apply((path))).apply((hydra.util.Tuple.Tuple2<T0, T2>) ((hydra.util.Tuple.Tuple2<T0, T2>) (new hydra.util.Tuple.Tuple2<T0, T2>((cx1), (valIn)))))).apply((termIn));
  }
  
  static <T0> T0 rewriteTermWithTypeContext(java.util.function.Function<java.util.function.Function<hydra.core.Term, T0>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, T0>>> f, hydra.typing.TypeContext cx0, hydra.core.Term term0) {
    return hydra.hoisting.Hoisting.rewriteTermWithTypeContext_rewrite(
      (java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, T0>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, T0>>>) (v1 -> (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Term, T0>>) (v2 -> (java.util.function.Function<hydra.core.Term, T0>) (v3 -> hydra.hoisting.Hoisting.rewriteTermWithTypeContext_f2(
        (f),
        (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Lambda, hydra.typing.TypeContext>>) (p0 -> p1 -> hydra.schemas.Schemas.extendTypeContextForLambda(
          (p0),
          (p1))),
        (java.util.function.Function<java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.Let, hydra.typing.TypeContext>>>) (p0 -> p1 -> p2 -> hydra.schemas.Schemas.extendTypeContextForLet(
          (p0),
          (p1),
          (p2))),
        (java.util.function.Function<hydra.typing.TypeContext, java.util.function.Function<hydra.core.TypeLambda, hydra.typing.TypeContext>>) (p0 -> p1 -> hydra.schemas.Schemas.extendTypeContextForTypeLambda(
          (p0),
          (p1))),
        (v1),
        (v2),
        (v3))))),
      (cx0),
      (term0));
  }
  
  static <T0, T1, T2, T3, T5, T6, T7> T3 rewriteTermWithTypeContext_f2(java.util.function.Function<java.util.function.Function<T0, T1>, java.util.function.Function<T2, java.util.function.Function<hydra.core.Term, T3>>> f, java.util.function.Function<T2, java.util.function.Function<hydra.core.Lambda, T2>> hydra_schemas_extendTypeContextForLambda2, java.util.function.Function<java.util.function.Function<T5, java.util.function.Function<T6, hydra.util.Maybe<T7>>>, java.util.function.Function<T2, java.util.function.Function<hydra.core.Let, T2>>> hydra_schemas_extendTypeContextForLet2, java.util.function.Function<T2, java.util.function.Function<hydra.core.TypeLambda, T2>> hydra_schemas_extendTypeContextForTypeLambda2, java.util.function.Function<T2, java.util.function.Function<T0, T1>> recurse, T2 cx, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public T3 otherwise(hydra.core.Term instance) {
        return ((((f)).apply((java.util.function.Function<T0, T1>) (v1 -> hydra.hoisting.Hoisting.<T2, T0, T1>rewriteTermWithTypeContext_recurse1(
          (cx),
          (recurse),
          (v1))))).apply((cx))).apply((term));
      }
      
      @Override
      public T3 visit(hydra.core.Term.Function fun) {
        return (((fun)).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public T3 otherwise(hydra.core.Function instance) {
            return ((((f)).apply(v1 -> hydra.hoisting.Hoisting.rewriteTermWithTypeContext_recurse1(
              (cx),
              (recurse),
              (v1)))).apply((cx))).apply((term));
          }
          
          @Override
          public T3 visit(hydra.core.Function.Lambda l) {
            T2 cx1 = (((hydra_schemas_extendTypeContextForLambda2)).apply((cx))).apply(((l)).value);
            return ((((f)).apply(v1 -> hydra.hoisting.Hoisting.rewriteTermWithTypeContext_recurse2(
              (cx1),
              (recurse),
              (v1)))).apply((cx1))).apply((term));
          }
        });
      }
      
      @Override
      public T3 visit(hydra.core.Term.Let l) {
        T2 cx1 = ((((hydra_schemas_extendTypeContextForLet2)).apply((java.util.function.Function<T5, java.util.function.Function<T6, hydra.util.Maybe<T7>>>) (ignored -> (java.util.function.Function<T6, hydra.util.Maybe<T7>>) (_2 -> (hydra.util.Maybe<T7>) (hydra.util.Maybe.<T7>nothing()))))).apply((cx))).apply(((l)).value);
        return ((((f)).apply(v1 -> hydra.hoisting.Hoisting.rewriteTermWithTypeContext_recurse22(
          (cx1),
          (recurse),
          (v1)))).apply((cx1))).apply((term));
      }
      
      @Override
      public T3 visit(hydra.core.Term.TypeLambda tl) {
        T2 cx1 = (((hydra_schemas_extendTypeContextForTypeLambda2)).apply((cx))).apply(((tl)).value);
        return ((((f)).apply(v1 -> hydra.hoisting.Hoisting.rewriteTermWithTypeContext_recurse23(
          (cx1),
          (recurse),
          (v1)))).apply((cx1))).apply((term));
      }
    });
  }
  
  static <T0, T1, T2> T2 rewriteTermWithTypeContext_rewrite(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<T1, T2>>, java.util.function.Function<T0, java.util.function.Function<T1, T2>>> f2, T0 cx, T1 term) {
    return ((((f2)).apply((java.util.function.Function<T0, java.util.function.Function<T1, T2>>) (v1 -> (java.util.function.Function<T1, T2>) (v2 -> hydra.hoisting.Hoisting.<T0, T1, T2>rewriteTermWithTypeContext_rewrite(
      (f2),
      (v1),
      (v2)))))).apply((cx))).apply((term));
  }
  
  static <T0, T1, T2> T2 rewriteTermWithTypeContext_recurse1(T0 cx, java.util.function.Function<T0, java.util.function.Function<T1, T2>> recurse, T1 term) {
    return (((recurse)).apply((cx))).apply((term));
  }
  
  static <T0, T1, T2> T2 rewriteTermWithTypeContext_recurse2(T0 cx1, java.util.function.Function<T0, java.util.function.Function<T1, T2>> recurse, T1 term) {
    return (((recurse)).apply((cx1))).apply((term));
  }
  
  static <T0, T1, T2> T2 rewriteTermWithTypeContext_recurse22(T0 cx1, java.util.function.Function<T0, java.util.function.Function<T1, T2>> recurse, T1 term) {
    return (((recurse)).apply((cx1))).apply((term));
  }
  
  static <T0, T1, T2> T2 rewriteTermWithTypeContext_recurse23(T0 cx1, java.util.function.Function<T0, java.util.function.Function<T1, T2>> recurse, T1 term) {
    return (((recurse)).apply((cx1))).apply((term));
  }
  
  static <T0, T1> Boolean shouldHoistAll(T0 ignored, T1 _2) {
    return true;
  }
  
  static Boolean shouldHoistCaseStatement(hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.TermAccessor>, hydra.core.Term> pathAndTerm) {
    java.util.List<hydra.accessors.TermAccessor> path = hydra.lib.pairs.First.apply((pathAndTerm));
    hydra.core.Term term = hydra.lib.pairs.Second.apply((pathAndTerm));
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.logic.Not.apply(hydra.hoisting.Hoisting.isUnionElimination((term))),
      false,
      ((java.util.function.Function<hydra.util.Tuple.Tuple2<Boolean, Boolean>, Boolean>) (finalState -> hydra.lib.logic.Not.apply(hydra.lib.pairs.First.apply((finalState))))).apply(hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.util.Tuple.Tuple2<Boolean, Boolean>, java.util.function.Function<hydra.accessors.TermAccessor, hydra.util.Tuple.Tuple2<Boolean, Boolean>>>) (st -> (java.util.function.Function<hydra.accessors.TermAccessor, hydra.util.Tuple.Tuple2<Boolean, Boolean>>) (acc -> hydra.hoisting.Hoisting.updateHoistState(
          (acc),
          (st)))),
        (hydra.util.Tuple.Tuple2<Boolean, Boolean>) ((hydra.util.Tuple.Tuple2<Boolean, Boolean>) (new hydra.util.Tuple.Tuple2<Boolean, Boolean>(true, false))),
        (path))));
  }
  
  static Boolean shouldHoistPolymorphic(hydra.typing.TypeContext cx, hydra.core.Binding binding) {
    return hydra.lib.logic.Or.apply(
      hydra.hoisting.Hoisting.bindingIsPolymorphic((binding)),
      hydra.hoisting.Hoisting.bindingUsesContextTypeVars(
        (cx),
        (binding)));
  }
  
  static hydra.util.Tuple.Tuple2<Boolean, Boolean> updateHoistState(hydra.accessors.TermAccessor accessor, hydra.util.Tuple.Tuple2<Boolean, Boolean> state) {
    Boolean atTop = hydra.lib.pairs.First.apply((state));
    Boolean usedApp = hydra.lib.pairs.Second.apply((state));
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.logic.Not.apply((atTop)),
      (hydra.util.Tuple.Tuple2<Boolean, Boolean>) ((hydra.util.Tuple.Tuple2<Boolean, Boolean>) (new hydra.util.Tuple.Tuple2<Boolean, Boolean>(false, (usedApp)))),
      ((accessor)).accept(new hydra.accessors.TermAccessor.PartialVisitor<>() {
        @Override
        public hydra.util.Tuple.Tuple2<Boolean, Boolean> otherwise(hydra.accessors.TermAccessor instance) {
          return (hydra.util.Tuple.Tuple2<Boolean, Boolean>) ((hydra.util.Tuple.Tuple2<Boolean, Boolean>) (new hydra.util.Tuple.Tuple2<Boolean, Boolean>(false, (usedApp))));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<Boolean, Boolean> visit(hydra.accessors.TermAccessor.AnnotatedBody ignored) {
          return (hydra.util.Tuple.Tuple2<Boolean, Boolean>) ((hydra.util.Tuple.Tuple2<Boolean, Boolean>) (new hydra.util.Tuple.Tuple2<Boolean, Boolean>(true, (usedApp))));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<Boolean, Boolean> visit(hydra.accessors.TermAccessor.LetBody ignored) {
          return (hydra.util.Tuple.Tuple2<Boolean, Boolean>) ((hydra.util.Tuple.Tuple2<Boolean, Boolean>) (new hydra.util.Tuple.Tuple2<Boolean, Boolean>(true, (usedApp))));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<Boolean, Boolean> visit(hydra.accessors.TermAccessor.LetBinding ignored) {
          return (hydra.util.Tuple.Tuple2<Boolean, Boolean>) ((hydra.util.Tuple.Tuple2<Boolean, Boolean>) (new hydra.util.Tuple.Tuple2<Boolean, Boolean>(true, (usedApp))));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<Boolean, Boolean> visit(hydra.accessors.TermAccessor.LambdaBody ignored) {
          return hydra.lib.logic.IfElse.apply(
            (usedApp),
            (hydra.util.Tuple.Tuple2<Boolean, Boolean>) ((hydra.util.Tuple.Tuple2<Boolean, Boolean>) (new hydra.util.Tuple.Tuple2<Boolean, Boolean>(false, true))),
            (hydra.util.Tuple.Tuple2<Boolean, Boolean>) ((hydra.util.Tuple.Tuple2<Boolean, Boolean>) (new hydra.util.Tuple.Tuple2<Boolean, Boolean>(true, false))));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<Boolean, Boolean> visit(hydra.accessors.TermAccessor.UnionCasesBranch ignored) {
          return hydra.lib.logic.IfElse.apply(
            (usedApp),
            (hydra.util.Tuple.Tuple2<Boolean, Boolean>) ((hydra.util.Tuple.Tuple2<Boolean, Boolean>) (new hydra.util.Tuple.Tuple2<Boolean, Boolean>(false, true))),
            (hydra.util.Tuple.Tuple2<Boolean, Boolean>) ((hydra.util.Tuple.Tuple2<Boolean, Boolean>) (new hydra.util.Tuple.Tuple2<Boolean, Boolean>(true, false))));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<Boolean, Boolean> visit(hydra.accessors.TermAccessor.UnionCasesDefault ignored) {
          return hydra.lib.logic.IfElse.apply(
            (usedApp),
            (hydra.util.Tuple.Tuple2<Boolean, Boolean>) ((hydra.util.Tuple.Tuple2<Boolean, Boolean>) (new hydra.util.Tuple.Tuple2<Boolean, Boolean>(false, true))),
            (hydra.util.Tuple.Tuple2<Boolean, Boolean>) ((hydra.util.Tuple.Tuple2<Boolean, Boolean>) (new hydra.util.Tuple.Tuple2<Boolean, Boolean>(true, false))));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<Boolean, Boolean> visit(hydra.accessors.TermAccessor.ApplicationFunction ignored) {
          return hydra.lib.logic.IfElse.apply(
            (usedApp),
            (hydra.util.Tuple.Tuple2<Boolean, Boolean>) ((hydra.util.Tuple.Tuple2<Boolean, Boolean>) (new hydra.util.Tuple.Tuple2<Boolean, Boolean>(false, true))),
            (hydra.util.Tuple.Tuple2<Boolean, Boolean>) ((hydra.util.Tuple.Tuple2<Boolean, Boolean>) (new hydra.util.Tuple.Tuple2<Boolean, Boolean>(true, true))));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<Boolean, Boolean> visit(hydra.accessors.TermAccessor.ApplicationArgument ignored) {
          return (hydra.util.Tuple.Tuple2<Boolean, Boolean>) ((hydra.util.Tuple.Tuple2<Boolean, Boolean>) (new hydra.util.Tuple.Tuple2<Boolean, Boolean>(false, (usedApp))));
        }
      }));
  }
}
