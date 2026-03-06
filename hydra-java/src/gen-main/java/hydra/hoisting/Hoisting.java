// Note: this is an automatically generated file. Do not edit.

package hydra.hoisting;

/**
 * Functions for deep term rewriting operations involving hoisting subterms or bindings into enclosing let terms.
 */
public interface Hoisting {
  static hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.typing.TermSubst> augmentBindingsWithNewFreeVars(hydra.graph.Graph cx, java.util.Set<hydra.core.Name> boundVars, java.util.List<hydra.core.Binding> bindings) {
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Type>> types = new hydra.util.Lazy<>(() -> hydra.lib.maps.Map.apply(
      hydra.rewriting.Rewriting::typeSchemeToFType,
      (cx).boundTypes));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> wrapAfterTypeLambdas = new java.util.concurrent.atomic.AtomicReference<>();
    wrapAfterTypeLambdas.set((java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (vars -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>, hydra.core.Term>>) (t -> (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>, hydra.core.Term>) (p -> new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(hydra.lib.pairs.First.apply(p), hydra.lib.pairs.Second.apply(p), t))))),
          term,
          hydra.lib.lists.Reverse.apply(vars));
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda tl) {
        return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(((tl).value).parameter, ((wrapAfterTypeLambdas.get()).apply(vars)).apply(((tl).value).body)));
      }
    }))));
    java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>> augment = (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>>) (b -> {
      hydra.util.Lazy<java.util.List<hydra.core.Name>> freeVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply(hydra.lib.sets.Intersection.apply(
        boundVars,
        hydra.rewriting.Rewriting.freeVariablesInTerm((b).term))));
      hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>>> varTypePairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>>) (v -> (hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>(v, hydra.lib.maps.Lookup.apply(
          v,
          types.get()))))),
        freeVars.get()));
      hydra.util.Lazy<java.util.List<hydra.core.Type>> varTypes = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>, hydra.util.Maybe<hydra.core.Type>>) ((java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>, hydra.util.Maybe<hydra.core.Type>>) (hydra.lib.pairs.Second::apply)),
        varTypePairs.get())));
      return hydra.lib.logic.IfElse.lazy(
        hydra.lib.logic.Or.apply(
          hydra.lib.lists.Null.apply(freeVars.get()),
          hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
            hydra.lib.lists.Length.apply(varTypes.get()),
            hydra.lib.lists.Length.apply(varTypePairs.get())))),
        () -> (hydra.util.Pair<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>(b, (hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>nothing())))),
        () -> (hydra.util.Pair<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>(new hydra.core.Binding((b).name, ((wrapAfterTypeLambdas.get()).apply(varTypePairs.get())).apply((b).term), hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (ts -> new hydra.core.TypeScheme((ts).variables, hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (acc -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> new hydra.core.Type.Function(new hydra.core.FunctionType(t, acc)))),
            (ts).type,
            hydra.lib.lists.Reverse.apply(varTypes.get())), (ts).constraints)),
          (b).type)), hydra.util.Maybe.just((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>((b).name, hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, hydra.core.Term>>) (t -> (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (v -> new hydra.core.Term.Application(new hydra.core.Application(t, new hydra.core.Term.Variable(v))))),
          new hydra.core.Term.Variable((b).name),
          freeVars.get())))))))));
    });
    hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>>> results = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      augment,
      bindings));
    return (hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.typing.TermSubst>) ((hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.typing.TermSubst>) (new hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.typing.TermSubst>(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>, hydra.core.Binding>) ((java.util.function.Function<hydra.util.Pair<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>, hydra.core.Binding>) (hydra.lib.pairs.First::apply)),
      results.get()), new hydra.typing.TermSubst(hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>) ((java.util.function.Function<hydra.util.Pair<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>>) (hydra.lib.pairs.Second::apply)),
      results.get())))))));
  }
  
  static Boolean bindingIsPolymorphic(hydra.core.Binding binding) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> false,
      (java.util.function.Function<hydra.core.TypeScheme, Boolean>) (ts -> hydra.lib.logic.Not.apply(hydra.lib.lists.Null.apply((ts).variables))),
      (binding).type);
  }
  
  static Boolean bindingUsesContextTypeVars(hydra.graph.Graph cx, hydra.core.Binding binding) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> false,
      (java.util.function.Function<hydra.core.TypeScheme, Boolean>) (ts -> {
        java.util.Set<hydra.core.Name> contextTypeVars = (cx).typeVariables;
        java.util.Set<hydra.core.Name> freeInType = hydra.rewriting.Rewriting.freeVariablesInType((ts).type);
        return hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply(hydra.lib.sets.Intersection.apply(
          freeInType,
          contextTypeVars)));
      }),
      (binding).type);
  }
  
  static Integer countVarOccurrences(hydra.core.Name name, hydra.core.Term term) {
    hydra.util.Lazy<Integer> childCount = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, Integer>>) (acc -> (java.util.function.Function<hydra.core.Term, Integer>) (t -> hydra.lib.math.Add.apply(
        acc,
        hydra.hoisting.Hoisting.countVarOccurrences(
          name,
          t)))),
      0,
      hydra.rewriting.Rewriting.subterms(term)));
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Term instance) {
        return childCount.get();
      }
      
      @Override
      public Integer visit(hydra.core.Term.Variable v) {
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (v).value,
            name),
          () -> hydra.lib.math.Add.apply(
            1,
            childCount.get()),
          () -> childCount.get());
      }
    });
  }
  
  static hydra.core.Let hoistAllLetBindings(hydra.core.Let let0) {
    hydra.util.Lazy<hydra.graph.Graph> emptyCx = new hydra.util.Lazy<>(() -> new hydra.graph.Graph((java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (java.util.Map<hydra.core.Name, hydra.graph.Primitive>) ((java.util.Map<hydra.core.Name, hydra.graph.Primitive>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.graph.Primitive>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())));
    return hydra.hoisting.Hoisting.hoistLetBindingsWithPredicate(
      (java.util.function.Function<hydra.core.Binding, Boolean>) (ignored -> true),
      p0 -> p1 -> hydra.hoisting.Hoisting.<hydra.graph.Graph, hydra.core.Binding>shouldHoistAll(
        p0,
        p1),
      emptyCx.get(),
      let0);
  }
  
  static hydra.core.Term hoistCaseStatements(hydra.graph.Graph v1, hydra.core.Term v2) {
    return hydra.hoisting.Hoisting.hoistSubterms(
      hydra.hoisting.Hoisting::shouldHoistCaseStatement,
      v1,
      v2);
  }
  
  static java.util.List<hydra.core.Binding> hoistCaseStatementsInGraph(java.util.List<hydra.core.Binding> bindings) {
    hydra.util.Lazy<hydra.graph.Graph> emptyTx = new hydra.util.Lazy<>(() -> new hydra.graph.Graph((java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (java.util.Map<hydra.core.Name, hydra.graph.Primitive>) ((java.util.Map<hydra.core.Name, hydra.graph.Primitive>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.graph.Primitive>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())));
    hydra.core.Term term0 = new hydra.core.Term.Let(new hydra.core.Let(bindings, new hydra.core.Term.Unit()));
    hydra.core.Term term1 = hydra.hoisting.Hoisting.hoistCaseStatements(
      emptyTx.get(),
      term0);
    return hydra.schemas.Schemas.termAsBindings(term1);
  }
  
  static hydra.core.Let hoistLetBindingsWithContext(java.util.function.Function<hydra.core.Binding, Boolean> isParentBinding, hydra.graph.Graph cx, hydra.core.Let let0) {
    return hydra.hoisting.Hoisting.hoistLetBindingsWithPredicate(
      isParentBinding,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, Boolean>>) (p0 -> p1 -> hydra.hoisting.Hoisting.shouldHoistPolymorphic(
        p0,
        p1)),
      cx,
      let0);
  }
  
  static hydra.core.Let hoistLetBindingsWithPredicate(java.util.function.Function<hydra.core.Binding, Boolean> isParentBinding, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, Boolean>> shouldHoistBinding, hydra.graph.Graph cx0, hydra.core.Let let0) {
    hydra.util.Lazy<hydra.graph.Graph> cx1 = new hydra.util.Lazy<>(() -> hydra.schemas.Schemas.extendGraphForLet(
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (c -> (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>) (b -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))),
      cx0,
      let0));
    java.util.function.Function<String, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Name>>, hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>>>>> hoistOne = (java.util.function.Function<String, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Name>>, hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>>>>>) (prefix -> (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Name>>, hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>>>>) (cx -> (java.util.function.Function<hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Name>>, hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>>>) (pair -> (java.util.function.Function<hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Name>>, hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>>) (bindingWithCapturedVars -> {
      hydra.util.Lazy<java.util.Set<hydra.core.Name>> alreadyUsedNames = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
      hydra.util.Lazy<hydra.core.Binding> b = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindingWithCapturedVars));
      hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>> bindingAndReplacementPairs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
      hydra.util.Lazy<java.util.List<hydra.core.Name>> capturedTermVars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bindingWithCapturedVars));
      hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Type>> types = new hydra.util.Lazy<>(() -> hydra.lib.maps.Map.apply(
        hydra.rewriting.Rewriting::typeSchemeToFType,
        (cx).boundTypes));
      hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>>> capturedTermVarTypePairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>>) (v -> (hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>) ((hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>) (new hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>(v, hydra.lib.maps.Lookup.apply(
          v,
          types.get()))))),
        capturedTermVars.get()));
      hydra.util.Lazy<java.util.List<hydra.core.Type>> capturedTermVarTypes = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> hydra.rewriting.Rewriting.deannotateTypeParameters(typ)),
        hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>, hydra.util.Maybe<hydra.core.Type>>) ((java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>, hydra.util.Maybe<hydra.core.Type>>) (hydra.lib.pairs.Second::apply)),
          capturedTermVarTypePairs.get()))));
      hydra.util.Lazy<java.util.Set<hydra.core.Name>> freeInBindingType = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
        () -> (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
        (java.util.function.Function<hydra.core.TypeScheme, java.util.Set<hydra.core.Name>>) (ts -> hydra.rewriting.Rewriting.freeVariablesInType((ts).type)),
        (b.get()).type));
      hydra.util.Lazy<java.util.Set<hydra.core.Name>> freeInCapturedVarTypes = new hydra.util.Lazy<>(() -> hydra.lib.sets.Unions.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Type, java.util.Set<hydra.core.Name>>) (t -> hydra.rewriting.Rewriting.freeVariablesInType(t)),
        capturedTermVarTypes.get())));
      hydra.util.Lazy<java.util.List<hydra.core.Name>> capturedTypeVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply(hydra.lib.sets.Intersection.apply(
        (cx).typeVariables,
        hydra.lib.sets.Union.apply(
          freeInBindingType.get(),
          freeInCapturedVarTypes.get()))));
      hydra.core.Name globalBindingName = hydra.lexical.Lexical.chooseUniqueName(
        alreadyUsedNames.get(),
        new hydra.core.Name(hydra.lib.strings.Cat2.apply(
          prefix,
          ((b.get()).name).value)));
      hydra.util.Lazy<hydra.util.Maybe<hydra.core.TypeScheme>> newTypeScheme = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(capturedTermVarTypes.get()),
          hydra.lib.lists.Length.apply(capturedTermVarTypePairs.get())),
        () -> hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (ts -> new hydra.core.TypeScheme(hydra.lib.lists.Nub.apply(hydra.lib.lists.Concat2.apply(
            capturedTypeVars.get(),
            (ts).variables)), hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (t -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (a -> new hydra.core.Type.Function(new hydra.core.FunctionType(a, t)))),
            (ts).type,
            hydra.lib.lists.Reverse.apply(capturedTermVarTypes.get())), (ts).constraints)),
          (b.get()).type),
        () -> (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())));
      hydra.util.Lazy<hydra.core.Term> withTypeApps = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, hydra.core.Term>>) (t -> (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (v -> new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(t, new hydra.core.Type.Variable(v))))),
        new hydra.core.Term.Variable(globalBindingName),
        capturedTypeVars.get()));
      hydra.util.Lazy<hydra.core.Term> replacement = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, hydra.core.Term>>) (t -> (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (v -> new hydra.core.Term.Application(new hydra.core.Application(t, new hydra.core.Term.Variable(v))))),
        withTypeApps.get(),
        capturedTermVars.get()));
      hydra.core.Term strippedTerm = hydra.rewriting.Rewriting.stripTypeLambdas((b.get()).term);
      hydra.util.Lazy<hydra.core.Term> termWithLambdas = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>, hydra.core.Term>>) (t -> (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Maybe<hydra.core.Type>>, hydra.core.Term>) (p -> new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(hydra.lib.pairs.First.apply(p), hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (dom -> hydra.rewriting.Rewriting.deannotateTypeParameters(dom)),
          hydra.lib.pairs.Second.apply(p)), t))))),
        strippedTerm,
        hydra.lib.lists.Reverse.apply(capturedTermVarTypePairs.get())));
      hydra.util.Lazy<hydra.core.Term> termWithTypeLambdas = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, hydra.core.Term>>) (t -> (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (v -> new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(v, t)))),
        termWithLambdas.get(),
        hydra.lib.lists.Reverse.apply(hydra.lib.maybes.Maybe.applyLazy(
          () -> (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()),
          projected -> projected.variables,
          newTypeScheme.get()))));
      hydra.util.Lazy<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>> newBindingAndReplacement = new hydra.util.Lazy<>(() -> (hydra.util.Pair<hydra.core.Binding, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Binding, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Binding, hydra.core.Term>(new hydra.core.Binding(globalBindingName, termWithTypeLambdas.get(), newTypeScheme.get()), replacement.get()))));
      hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>> newPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Cons.apply(
        newBindingAndReplacement.get(),
        bindingAndReplacementPairs.get()));
      hydra.util.Lazy<java.util.Set<hydra.core.Name>> newUsedNames = new hydra.util.Lazy<>(() -> hydra.lib.sets.Insert.apply(
        globalBindingName,
        alreadyUsedNames.get()));
      return (hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>) ((hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>) (new hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>(newPairs.get(), newUsedNames.get())));
    }))));
    java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>> forActiveBinding = (java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>>) (b -> {
      String prefix = hydra.lib.strings.Cat2.apply(
        ((b).name).value,
        "_");
      hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>> resultPair = new hydra.util.Lazy<>(() -> hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContext(
        (java.util.function.Function<java.util.function.Function<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>>>>>) (v1 -> (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>>>>) (v2 -> (java.util.function.Function<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>>>) (v3 -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>>) (v4 -> hydra.hoisting.Hoisting.hoistLetBindingsWithPredicate_rewrite(
          hoistOne,
          (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.typing.TermSubst>>>>) (p0 -> p1 -> p2 -> hydra.hoisting.Hoisting.augmentBindingsWithNewFreeVars(
            p0,
            p1,
            p2)),
          hydra.hoisting.Hoisting::bindingIsPolymorphic,
          (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, Integer>>) (p0 -> p1 -> hydra.hoisting.Hoisting.countVarOccurrences(
            p0,
            p1)),
          hydra.rewriting.Rewriting::freeVariablesInTerm,
          hydra.rewriting.Rewriting::typeSchemeToFType,
          hydra.schemas.Schemas::fTypeIsPolymorphic,
          (java.util.function.Function<hydra.typing.TermSubst, java.util.function.Function<hydra.core.Binding, hydra.core.Binding>>) (p0 -> p1 -> hydra.substitution.Substitution.substituteInBinding(
            p0,
            p1)),
          (java.util.function.Function<hydra.typing.TermSubst, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (p0 -> p1 -> hydra.substitution.Substitution.substituteInTerm(
            p0,
            p1)),
          shouldHoistBinding,
          prefix,
          v1,
          v2,
          v3,
          v4))))),
        cx1.get(),
        hydra.hoisting.Hoisting.hoistLetBindingsWithPredicate_init(b),
        (b).term));
      hydra.util.Lazy<java.util.List<hydra.core.Binding>> resultBindings = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply(resultPair.get())));
      hydra.util.Lazy<hydra.core.Term> resultTerm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(resultPair.get()));
      return hydra.lib.lists.Cons.apply(
        new hydra.core.Binding((b).name, resultTerm.get(), (b).type),
        resultBindings.get());
    });
    java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>> forBinding = (java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>>) (b -> hydra.lib.logic.IfElse.lazy(
      (isParentBinding).apply(b),
      () -> (forActiveBinding).apply(b),
      () -> java.util.List.of(b)));
    return new hydra.core.Let(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      forBinding,
      (let0).bindings)), (let0).body);
  }
  
  static <T0, T1, T2> hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term> hoistLetBindingsWithPredicate_rewrite(java.util.function.Function<String, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Name>>, hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>>>>> hoistOne, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.typing.TermSubst>>>> hydra_hoisting_augmentBindingsWithNewFreeVars2, java.util.function.Function<hydra.core.Binding, Boolean> hydra_hoisting_bindingIsPolymorphic2, java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, Integer>> hydra_hoisting_countVarOccurrences2, java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>> hydra_rewriting_freeVariablesInTerm2, java.util.function.Function<hydra.core.TypeScheme, hydra.core.Type> hydra_rewriting_typeSchemeToFType2, java.util.function.Function<hydra.core.Type, Boolean> hydra_schemas_fTypeIsPolymorphic2, java.util.function.Function<hydra.typing.TermSubst, java.util.function.Function<hydra.core.Binding, hydra.core.Binding>> hydra_substitution_substituteInBinding2, java.util.function.Function<hydra.typing.TermSubst, java.util.function.Function<hydra.core.Term, hydra.core.Term>> hydra_substitution_substituteInTerm2, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, Boolean>> shouldHoistBinding, String prefix, java.util.function.Function<hydra.util.Pair<java.util.List<T0>, T1>, java.util.function.Function<T2, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>>> recurse, hydra.graph.Graph cx, hydra.util.Pair<java.util.List<hydra.core.Binding>, T1> bindingsAndNames, T2 term) {
    hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>> result = new hydra.util.Lazy<>(() -> ((recurse).apply(hydra.hoisting.Hoisting.<T1, T0>hoistLetBindingsWithPredicate_emptyBindingsAndNames(bindingsAndNames))).apply(term));
    hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>> newBindingsAndNames = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result.get()));
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> alreadyUsedNames = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(newBindingsAndNames.get()));
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindingsSoFar = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(newBindingsAndNames.get()));
    hydra.util.Lazy<hydra.core.Term> newTerm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result.get()));
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> previouslyFinishedBindings = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindingsAndNames));
    return (newTerm.get()).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>((hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>) ((hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>) (new hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>(hydra.lib.lists.Concat2.apply(
          previouslyFinishedBindings.get(),
          bindingsSoFar.get()), alreadyUsedNames.get()))), newTerm.get())));
      }
      
      @Override
      public hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term> visit(hydra.core.Term.Let l) {
        hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.List<hydra.core.Binding>>> partitionPair = new hydra.util.Lazy<>(() -> hydra.lib.lists.Partition.apply(
          (java.util.function.Function<hydra.core.Binding, Boolean>) (v1 -> ((shouldHoistBinding).apply(cx)).apply(v1)),
          ((l).value).bindings));
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> hoistUs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(partitionPair.get()));
        hydra.util.Lazy<java.util.Set<hydra.core.Name>> boundTermVariables = new hydra.util.Lazy<>(() -> hydra.lib.sets.Union.apply(
          (cx).lambdaVariables,
          hydra.lib.sets.Difference.apply(
            hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply((cx).boundTerms)),
            (cx).lambdaVariables)));
        hydra.util.Lazy<java.util.List<java.util.List<hydra.core.Name>>> freeVariablesInEachBinding = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Name>>) (b -> hydra.lib.sets.ToList.apply(hydra.lib.sets.Intersection.apply(
            boundTermVariables.get(),
            (hydra_rewriting_freeVariablesInTerm2).apply((b).term)))),
          hoistUs.get()));
        hydra.util.Lazy<java.util.List<hydra.core.Name>> hoistedBindingNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          projected -> projected.name,
          hoistUs.get()));
        hydra.util.Lazy<java.util.List<hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Name>>>> bindingDependencies = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<java.util.List<hydra.core.Name>, hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Name>>>) (vars -> hydra.lib.lists.Partition.apply(
            (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.sets.Member.apply(
              v,
              hydra.lib.sets.FromList.apply(hoistedBindingNames.get()))),
            vars)),
          freeVariablesInEachBinding.get()));
        hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>>> bindingEdges = new hydra.util.Lazy<>(() -> hydra.lib.lists.Zip.apply(
          hoistedBindingNames.get(),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Name>>, java.util.List<hydra.core.Name>>) ((java.util.function.Function<hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Name>>, java.util.List<hydra.core.Name>>) (hydra.lib.pairs.First::apply)),
            bindingDependencies.get())));
        hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>>> bindingImmediateCapturedVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Zip.apply(
          hoistedBindingNames.get(),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Name>>, java.util.List<hydra.core.Name>>) ((java.util.function.Function<hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.List<hydra.core.Name>>, java.util.List<hydra.core.Name>>) (hydra.lib.pairs.Second::apply)),
            bindingDependencies.get())));
        hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.Set<hydra.core.Name>>> capturedVarsMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.sorting.Sorting.propagateTags(
          bindingEdges.get(),
          bindingImmediateCapturedVars.get())));
        hydra.util.Lazy<java.util.Set<hydra.core.Name>> polyLetVariables = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.maybes.Maybe.applyLazy(
            () -> false,
            hydra_schemas_fTypeIsPolymorphic2,
            hydra.lib.maybes.Map.apply(
              hydra_rewriting_typeSchemeToFType2,
              hydra.lib.maps.Lookup.apply(
                v,
                (cx).boundTypes)))),
          hydra.lib.sets.ToList.apply(hydra.lib.sets.Difference.apply(
            hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply((cx).boundTerms)),
            (cx).lambdaVariables)))));
        hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Name>>>> bindingsWithCapturedVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Name>>>) (b -> (hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Name>>(b, hydra.lib.maybes.Maybe.applyLazy(
            () -> (java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()),
            (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.List<hydra.core.Name>>) (vars -> hydra.lib.sets.ToList.apply(hydra.lib.sets.Difference.apply(
              vars,
              polyLetVariables.get()))),
            hydra.lib.maps.Lookup.apply(
              (b).name,
              capturedVarsMap.get())))))),
          hoistUs.get()));
        hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>> hoistPairsAndNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>, java.util.function.Function<hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Name>>, hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>>>) (v1 -> (java.util.function.Function<hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Name>>, hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>>) (v2 -> ((((hoistOne).apply(prefix)).apply(cx)).apply(v1)).apply(v2))),
          (hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>) ((hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>) (new hydra.util.Pair<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>, java.util.Set<hydra.core.Name>>((java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>) (java.util.List.<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>of()), alreadyUsedNames.get()))),
          bindingsWithCapturedVars.get()));
        hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>>> hoistPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Reverse.apply(hydra.lib.pairs.First.apply(hoistPairsAndNames.get())));
        hydra.util.Lazy<java.util.List<hydra.core.Term>> replacements = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>, hydra.core.Term>) ((java.util.function.Function<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>, hydra.core.Term>) (hydra.lib.pairs.Second::apply)),
          hoistPairs.get()));
        hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>> hoistNameReplacementPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Zip.apply(
          hydra.lib.lists.Map.apply(
            projected -> projected.name,
            hoistUs.get()),
          replacements.get()));
        hydra.util.Lazy<hydra.typing.TermSubst> fullSubst = new hydra.util.Lazy<>(() -> new hydra.typing.TermSubst(hydra.lib.maps.FromList.apply(hoistNameReplacementPairs.get())));
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindingsSoFarSubst = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (v1 -> ((hydra_substitution_substituteInBinding2).apply(fullSubst.get())).apply(v1)),
          bindingsSoFar.get()));
        hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.typing.TermSubst>> augmentResult = new hydra.util.Lazy<>(() -> (((hydra_hoisting_augmentBindingsWithNewFreeVars2).apply(cx)).apply(hydra.lib.sets.Difference.apply(
          boundTermVariables.get(),
          polyLetVariables.get()))).apply(bindingsSoFarSubst.get()));
        hydra.util.Lazy<hydra.typing.TermSubst> augmentSubst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(augmentResult.get()));
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindingsSoFarAugmented = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(augmentResult.get()));
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindingsSoFarFinal = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (v1 -> ((hydra_substitution_substituteInBinding2).apply(augmentSubst.get())).apply(v1)),
          bindingsSoFarAugmented.get()));
        hydra.core.Term body = ((l).value).body;
        hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Binding>> hoistBindingMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Name, hydra.core.Binding>>) (b -> (hydra.util.Pair<hydra.core.Name, hydra.core.Binding>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Binding>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Binding>((b).name, b)))),
          hoistUs.get())));
        java.util.function.Function<hydra.core.Name, Boolean> isCacheable = (java.util.function.Function<hydra.core.Name, Boolean>) (name -> {
          hydra.util.Lazy<Boolean> isPoly = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
            () -> false,
            (java.util.function.Function<hydra.core.Binding, Boolean>) (b -> (hydra_hoisting_bindingIsPolymorphic2).apply(b)),
            hydra.lib.maps.Lookup.apply(
              name,
              hoistBindingMap.get())));
          hydra.util.Lazy<Boolean> multiRef = new hydra.util.Lazy<>(() -> hydra.lib.equality.Gte.apply(
            ((hydra_hoisting_countVarOccurrences2).apply(name)).apply(body),
            2));
          return hydra.lib.logic.And.apply(
            multiRef.get(),
            hydra.lib.logic.Not.apply(isPoly.get()));
        });
        hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>> singleRefPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Term>, Boolean>) (p -> hydra.lib.logic.Not.apply((isCacheable).apply(hydra.lib.pairs.First.apply(p)))),
          hoistNameReplacementPairs.get()));
        hydra.util.Lazy<hydra.typing.TermSubst> bodyOnlySubst = new hydra.util.Lazy<>(() -> new hydra.typing.TermSubst(hydra.lib.maps.FromList.apply(singleRefPairs.get())));
        hydra.core.Term bodySubst = ((hydra_substitution_substituteInTerm2).apply(bodyOnlySubst.get())).apply(body);
        hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>> multiRefPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Term>, Boolean>) (p -> (isCacheable).apply(hydra.lib.pairs.First.apply(p))),
          hoistNameReplacementPairs.get()));
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> cacheBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Term>, hydra.core.Binding>) (p -> {
            hydra.util.Lazy<hydra.util.Maybe<hydra.core.TypeScheme>> origType = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
              () -> (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()),
              (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.TypeScheme>>) (b -> (b).type),
              hydra.lib.maps.Lookup.apply(
                hydra.lib.pairs.First.apply(p),
                hoistBindingMap.get())));
            return new hydra.core.Binding(hydra.lib.pairs.First.apply(p), hydra.lib.pairs.Second.apply(p), origType.get());
          }),
          multiRefPairs.get()));
        hydra.util.Lazy<hydra.core.Term> bodyWithCache = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(cacheBindings.get()),
          () -> bodySubst,
          () -> new hydra.core.Term.Let(new hydra.core.Let(cacheBindings.get(), bodySubst))));
        hydra.core.Term bodyFinal = ((hydra_substitution_substituteInTerm2).apply(augmentSubst.get())).apply(bodyWithCache.get());
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> keepUs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(partitionPair.get()));
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> keepUsSubst = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (v1 -> ((hydra_substitution_substituteInBinding2).apply(fullSubst.get())).apply(v1)),
          keepUs.get()));
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> keepUsFinal = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (v1 -> ((hydra_substitution_substituteInBinding2).apply(augmentSubst.get())).apply(v1)),
          keepUsSubst.get()));
        hydra.util.Lazy<hydra.core.Term> finalTerm = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(keepUsFinal.get()),
          () -> bodyFinal,
          () -> new hydra.core.Term.Let(new hydra.core.Let(keepUsFinal.get(), bodyFinal))));
        hydra.util.Lazy<java.util.Set<hydra.core.Name>> finalUsedNames = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hoistPairsAndNames.get()));
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> hoistedBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>, hydra.core.Binding>) ((java.util.function.Function<hydra.util.Pair<hydra.core.Binding, hydra.core.Term>, hydra.core.Binding>) (hydra.lib.pairs.First::apply)),
          hoistPairs.get()));
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> hoistedBindingsSubst = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (v1 -> ((hydra_substitution_substituteInBinding2).apply(fullSubst.get())).apply(v1)),
          hoistedBindings.get()));
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> hoistedBindingsFinal = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (v1 -> ((hydra_substitution_substituteInBinding2).apply(augmentSubst.get())).apply(v1)),
          hoistedBindingsSubst.get()));
        return (hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>, hydra.core.Term>((hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>) ((hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>) (new hydra.util.Pair<java.util.List<hydra.core.Binding>, java.util.Set<hydra.core.Name>>(hydra.lib.lists.Concat.apply(java.util.List.of(
          previouslyFinishedBindings.get(),
          hoistedBindingsFinal.get(),
          bindingsSoFarFinal.get())), finalUsedNames.get()))), finalTerm.get())));
      }
    });
  }
  
  static <T0> hydra.util.Pair<java.util.List<T0>, java.util.Set<hydra.core.Name>> hoistLetBindingsWithPredicate_init(hydra.core.Binding b) {
    return (hydra.util.Pair<java.util.List<T0>, java.util.Set<hydra.core.Name>>) ((hydra.util.Pair<java.util.List<T0>, java.util.Set<hydra.core.Name>>) (new hydra.util.Pair<java.util.List<T0>, java.util.Set<hydra.core.Name>>((java.util.List<T0>) (java.util.List.<T0>of()), hydra.lib.sets.Singleton.apply((b).name))));
  }
  
  static <T1, T3> hydra.util.Pair<java.util.List<T3>, T1> hoistLetBindingsWithPredicate_emptyBindingsAndNames(hydra.util.Pair<java.util.List<hydra.core.Binding>, T1> bindingsAndNames) {
    return (hydra.util.Pair<java.util.List<T3>, T1>) ((hydra.util.Pair<java.util.List<T3>, T1>) (new hydra.util.Pair<java.util.List<T3>, T1>((java.util.List<T3>) (java.util.List.<T3>of()), hydra.lib.pairs.Second.apply(bindingsAndNames))));
  }
  
  static hydra.core.Let hoistPolymorphicLetBindings(java.util.function.Function<hydra.core.Binding, Boolean> isParentBinding, hydra.core.Let let0) {
    hydra.util.Lazy<hydra.graph.Graph> emptyCx = new hydra.util.Lazy<>(() -> new hydra.graph.Graph((java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (java.util.Map<hydra.core.Name, hydra.graph.Primitive>) ((java.util.Map<hydra.core.Name, hydra.graph.Primitive>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.graph.Primitive>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())));
    return hydra.hoisting.Hoisting.hoistLetBindingsWithPredicate(
      isParentBinding,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, Boolean>>) (p0 -> p1 -> hydra.hoisting.Hoisting.shouldHoistPolymorphic(
        p0,
        p1)),
      emptyCx.get(),
      let0);
  }
  
  static hydra.core.Term hoistSubterms(java.util.function.Function<hydra.util.Pair<java.util.List<hydra.accessors.TermAccessor>, hydra.core.Term>, Boolean> shouldHoist, hydra.graph.Graph cx0, hydra.core.Term term0) {
    java.util.function.Function<hydra.graph.Graph, java.util.function.Function<Integer, java.util.function.Function<String, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.core.Term>>>>>> processImmediateSubterm = (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<Integer, java.util.function.Function<String, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.core.Term>>>>>>) (cx -> (java.util.function.Function<Integer, java.util.function.Function<String, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.core.Term>>>>>) (counter -> (java.util.function.Function<String, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.core.Term>>>>) (namePrefix -> (java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.core.Term>>>) (pathPrefix -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.core.Term>>) (subterm -> {
      java.util.Set<hydra.core.Name> baselineLambdaVars = (cx).lambdaVariables;
      java.util.function.Function<java.util.function.Function<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>>>>> collectAndReplace = (java.util.function.Function<java.util.function.Function<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>>>>>) (recurse -> (java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>>>>) (path -> (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>>>) (cxInner -> (java.util.function.Function<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>) (term -> {
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> collectedBindings = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(acc));
        hydra.util.Lazy<Integer> currentCounter = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(acc));
        return (term).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term> otherwise(hydra.core.Term instance) {
            hydra.util.Lazy<java.util.List<hydra.accessors.TermAccessor>> fullPath = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
              pathPrefix,
              path));
            hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term> result2 = ((recurse).apply(acc)).apply(term);
            hydra.util.Lazy<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>> newAcc = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result2));
            hydra.util.Lazy<java.util.List<hydra.core.Binding>> newBindings = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(newAcc.get()));
            hydra.util.Lazy<Integer> newCounter = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(newAcc.get()));
            hydra.util.Lazy<hydra.core.Term> processedTerm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result2));
            return hydra.lib.logic.IfElse.lazy(
              (shouldHoist).apply((hydra.util.Pair<java.util.List<hydra.accessors.TermAccessor>, hydra.core.Term>) ((hydra.util.Pair<java.util.List<hydra.accessors.TermAccessor>, hydra.core.Term>) (new hydra.util.Pair<java.util.List<hydra.accessors.TermAccessor>, hydra.core.Term>(fullPath.get(), processedTerm.get())))),
              () -> ((java.util.function.Supplier<hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>) (() -> {
                hydra.core.Name bindingName = new hydra.core.Name(hydra.lib.strings.Cat.apply(java.util.List.of(
                  "_hoist_",
                  namePrefix,
                  "_",
                  hydra.lib.literals.ShowInt32.apply(newCounter.get()))));
                return ((java.util.function.Supplier<hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>) (() -> {
                  java.util.Set<hydra.core.Name> allLambdaVars = (cxInner).lambdaVariables;
                  return ((java.util.function.Supplier<hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>) (() -> {
                    hydra.util.Lazy<java.util.Set<hydra.core.Name>> newLambdaVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.Difference.apply(
                      allLambdaVars,
                      baselineLambdaVars));
                    return ((java.util.function.Supplier<hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>) (() -> {
                      java.util.Set<hydra.core.Name> freeVars = hydra.rewriting.Rewriting.freeVariablesInTerm(processedTerm.get());
                      return ((java.util.function.Supplier<hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>) (() -> {
                        hydra.util.Lazy<java.util.List<hydra.core.Name>> capturedVars = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply(hydra.lib.sets.Intersection.apply(
                          newLambdaVars.get(),
                          freeVars)));
                        return ((java.util.function.Supplier<hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>) (() -> {
                          hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Type>> typeMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.Map.apply(
                            hydra.rewriting.Rewriting::typeSchemeToFType,
                            (cxInner).boundTypes));
                          return ((java.util.function.Supplier<hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>) (() -> {
                            hydra.util.Lazy<hydra.core.Term> wrappedTerm = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                              (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, hydra.core.Term>>) (body -> (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (varName -> new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(varName, hydra.lib.maps.Lookup.apply(
                                varName,
                                typeMap.get()), body))))),
                              processedTerm.get(),
                              hydra.lib.lists.Reverse.apply(capturedVars.get())));
                            return ((java.util.function.Supplier<hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>) (() -> {
                              hydra.util.Lazy<hydra.core.Term> reference = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                                (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, hydra.core.Term>>) (fn -> (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (varName -> new hydra.core.Term.Application(new hydra.core.Application(fn, new hydra.core.Term.Variable(varName))))),
                                new hydra.core.Term.Variable(bindingName),
                                capturedVars.get()));
                              return ((java.util.function.Supplier<hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>>) (() -> {
                                hydra.util.Lazy<hydra.core.Binding> newBinding = new hydra.util.Lazy<>(() -> new hydra.core.Binding(bindingName, wrappedTerm.get(), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())));
                                return (hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>((hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>) ((hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>) (new hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>(hydra.lib.math.Add.apply(
                                  newCounter.get(),
                                  1), hydra.lib.lists.Cons.apply(
                                  newBinding.get(),
                                  newBindings.get())))), reference.get())));
                              })).get();
                            })).get();
                          })).get();
                        })).get();
                      })).get();
                    })).get();
                  })).get();
                })).get();
              })).get(),
              () -> (hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>(newAcc.get(), processedTerm.get()))));
          }
          
          @Override
          public hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term> visit(hydra.core.Term.Let ignored) {
            return (hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>(acc, term)));
          }
          
          @Override
          public hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term> visit(hydra.core.Term.TypeLambda ignored) {
            return (hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>(acc, term)));
          }
        });
      })))));
      hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>, hydra.core.Term>> result = new hydra.util.Lazy<>(() -> hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContextAndPath(
        collectAndReplace,
        cx,
        (hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>) ((hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>) (new hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>(counter, (java.util.List<hydra.core.Binding>) (java.util.List.<hydra.core.Binding>of())))),
        subterm));
      hydra.util.Lazy<hydra.util.Pair<Integer, java.util.List<hydra.core.Binding>>> finalAcc = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result.get()));
      hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindings = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(finalAcc.get()));
      hydra.util.Lazy<Integer> finalCounter = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(finalAcc.get()));
      hydra.util.Lazy<hydra.core.Term> transformedSubterm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result.get()));
      return hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(bindings.get()),
        () -> (hydra.util.Pair<Integer, hydra.core.Term>) ((hydra.util.Pair<Integer, hydra.core.Term>) (new hydra.util.Pair<Integer, hydra.core.Term>(finalCounter.get(), transformedSubterm.get()))),
        () -> ((java.util.function.Supplier<hydra.util.Pair<Integer, hydra.core.Term>>) (() -> {
          hydra.util.Lazy<hydra.core.Term> localLet = new hydra.util.Lazy<>(() -> new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Reverse.apply(bindings.get()), transformedSubterm.get())));
          return (hydra.util.Pair<Integer, hydra.core.Term>) ((hydra.util.Pair<Integer, hydra.core.Term>) (new hydra.util.Pair<Integer, hydra.core.Term>(finalCounter.get(), localLet.get())));
        })).get());
    })))));
    return hydra.lib.pairs.Second.apply(hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContextAndPath(
      (java.util.function.Function<java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.core.Term>>>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.core.Term>>>>>>) (v1 -> (java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.core.Term>>>>>) (v2 -> (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.core.Term>>>>) (v3 -> (java.util.function.Function<Integer, java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.core.Term>>>) (v4 -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.core.Term>>) (v5 -> hydra.hoisting.Hoisting.hoistSubterms_rewrite(
        processImmediateSubterm,
        v1,
        v2,
        v3,
        v4,
        v5)))))),
      cx0,
      1,
      term0));
  }
  
  static <T0> hydra.util.Pair<T0, hydra.core.Term> hoistSubterms_processLetTerm(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<Integer, java.util.function.Function<String, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.core.Term>>>>>> processImmediateSubterm, hydra.graph.Graph cx, T0 counter, java.util.List<hydra.accessors.TermAccessor> path, hydra.core.Let lt) {
    java.util.List<hydra.core.Binding> bindings = (lt).bindings;
    hydra.core.Term body = (lt).body;
    hydra.util.Lazy<java.util.List<hydra.accessors.TermAccessor>> bodyPathPrefix = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
      path,
      java.util.List.of(new hydra.accessors.TermAccessor.LetBody())));
    hydra.util.Pair<Integer, hydra.core.Term> bodyResult = (((((processImmediateSubterm).apply(cx)).apply(1)).apply("_body")).apply(bodyPathPrefix.get())).apply(body);
    java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>>> processBinding = (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>>>) (acc -> (java.util.function.Function<hydra.core.Binding, java.util.List<hydra.core.Binding>>) (binding -> {
      hydra.util.Lazy<java.util.List<hydra.accessors.TermAccessor>> bindingPathPrefix = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
        path,
        java.util.List.of(new hydra.accessors.TermAccessor.LetBinding((binding).name))));
      String namePrefix = hydra.lib.strings.Intercalate.apply(
        "_",
        hydra.lib.strings.SplitOn.apply(
          ".",
          ((binding).name).value));
      hydra.util.Pair<Integer, hydra.core.Term> result = (((((processImmediateSubterm).apply(cx)).apply(1)).apply(namePrefix)).apply(bindingPathPrefix.get())).apply((binding).term);
      hydra.util.Lazy<hydra.core.Term> newValue = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result));
      hydra.core.Binding newBinding = new hydra.core.Binding((binding).name, newValue.get(), (binding).type);
      return hydra.lib.lists.Cons.apply(
        newBinding,
        acc);
    }));
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> newBindingsReversed = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      processBinding,
      (java.util.List<hydra.core.Binding>) (java.util.List.<hydra.core.Binding>of()),
      bindings));
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> newBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Reverse.apply(newBindingsReversed.get()));
    hydra.util.Lazy<hydra.core.Term> newBody = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bodyResult));
    return (hydra.util.Pair<T0, hydra.core.Term>) ((hydra.util.Pair<T0, hydra.core.Term>) (new hydra.util.Pair<T0, hydra.core.Term>(counter, new hydra.core.Term.Let(new hydra.core.Let(newBindings.get(), newBody.get())))));
  }
  
  static <T0, T1> hydra.util.Pair<T1, hydra.core.Term> hoistSubterms_rewrite(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<Integer, java.util.function.Function<String, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.core.Term>>>>>> processImmediateSubterm, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, java.util.List<hydra.accessors.TermAccessor> path, hydra.graph.Graph cx, T0 counter, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<T1, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return ((recurse).apply(counter)).apply(term);
      }
      
      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Let lt) {
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> recursed = new hydra.util.Lazy<>(() -> hydra.hoisting.Hoisting.<T0, T1>hoistSubterms_recursed(
          counter,
          recurse,
          term));
        hydra.util.Lazy<T1> newCounter = new hydra.util.Lazy<>(() -> hydra.hoisting.Hoisting.<T1>hoistSubterms_newCounter(recursed.get()));
        hydra.util.Lazy<hydra.core.Term> recursedTerm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(recursed.get()));
        return (recursedTerm.get()).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Pair<T1, hydra.core.Term> otherwise(hydra.core.Term instance) {
            return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(newCounter.get(), recursedTerm.get())));
          }
          
          @Override
          public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Let lt2) {
            return hydra.hoisting.Hoisting.<T1>hoistSubterms_processLetTerm(
              processImmediateSubterm,
              cx,
              newCounter.get(),
              path,
              (lt2).value);
          }
        });
      }
    });
  }
  
  static <T0, T1> hydra.util.Pair<T1, hydra.core.Term> hoistSubterms_recursed(T0 counter, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, hydra.core.Term term) {
    return ((recurse).apply(counter)).apply(term);
  }
  
  static <T1> T1 hoistSubterms_newCounter(hydra.util.Pair<T1, hydra.core.Term> recursed) {
    return hydra.lib.pairs.First.apply(recursed);
  }
  
  static Boolean isApplicationFunction(hydra.accessors.TermAccessor acc) {
    return (acc).accept(new hydra.accessors.TermAccessor.PartialVisitor<>() {
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
    return (f).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Function instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Function.Elimination e) {
        return ((e).value).accept(new hydra.core.Elimination.PartialVisitor<>() {
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
    return (acc).accept(new hydra.accessors.TermAccessor.PartialVisitor<>() {
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
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Function f) {
        return hydra.hoisting.Hoisting.isEliminationUnion((f).value);
      }
    });
  }
  
  static Boolean isUnionEliminationApplication(hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Term instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Term.Application app) {
        return hydra.hoisting.Hoisting.isUnionElimination(hydra.rewriting.Rewriting.deannotateAndDetypeTerm(((app).value).function));
      }
    });
  }
  
  static java.util.List<hydra.accessors.TermAccessor> normalizePathForHoisting(java.util.List<hydra.accessors.TermAccessor> path) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.List<hydra.accessors.TermAccessor>>> go = new java.util.concurrent.atomic.AtomicReference<>();
    go.set((java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.List<hydra.accessors.TermAccessor>>) (remaining -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Or.apply(
        hydra.lib.lists.Null.apply(remaining),
        hydra.lib.lists.Null.apply(hydra.lib.lists.Tail.apply(remaining))),
      () -> remaining,
      () -> ((java.util.function.Supplier<java.util.List<hydra.accessors.TermAccessor>>) (() -> {
        hydra.util.Lazy<hydra.accessors.TermAccessor> first = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(remaining));
        return ((java.util.function.Supplier<java.util.List<hydra.accessors.TermAccessor>>) (() -> {
          hydra.util.Lazy<hydra.accessors.TermAccessor> second = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(hydra.lib.lists.Tail.apply(remaining)));
          return ((java.util.function.Supplier<java.util.List<hydra.accessors.TermAccessor>>) (() -> {
            hydra.util.Lazy<java.util.List<hydra.accessors.TermAccessor>> rest = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(hydra.lib.lists.Tail.apply(remaining)));
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.And.apply(
                hydra.hoisting.Hoisting.isApplicationFunction(first.get()),
                hydra.hoisting.Hoisting.isLambdaBody(second.get())),
              () -> hydra.lib.lists.Cons.apply(
                new hydra.accessors.TermAccessor.LetBody(),
                (go.get()).apply(rest.get())),
              () -> hydra.lib.lists.Cons.apply(
                first.get(),
                (go.get()).apply(hydra.lib.lists.Tail.apply(remaining))));
          })).get();
        })).get();
      })).get())));
    return (go.get()).apply(path);
  }
  
  static <T0> hydra.util.Pair<T0, hydra.core.Term> rewriteAndFoldTermWithTypeContext(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>> f, hydra.graph.Graph cx0, T0 val0, hydra.core.Term term0) {
    hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term>> result = new hydra.util.Lazy<>(() -> hydra.hoisting.Hoisting.<T0>rewriteAndFoldTermWithTypeContext_result(
      f,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>>) (p0 -> p1 -> hydra.schemas.Schemas.extendGraphForLambda(
        p0,
        p1)),
      (java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>>) (p0 -> p1 -> p2 -> hydra.schemas.Schemas.extendGraphForLet(
        p0,
        p1,
        p2)),
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>>) (p0 -> p1 -> hydra.schemas.Schemas.extendGraphForTypeLambda(
        p0,
        p1)),
      cx0,
      term0,
      val0));
    return (hydra.util.Pair<T0, hydra.core.Term>) ((hydra.util.Pair<T0, hydra.core.Term>) (new hydra.util.Pair<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply(result.get())), hydra.lib.pairs.Second.apply(result.get()))));
  }
  
  static <T0, T1> hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term> rewriteAndFoldTermWithTypeContext_wrapper(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>> f, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>> hydra_schemas_extendGraphForLambda2, java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>> hydra_schemas_extendGraphForLet2, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>> hydra_schemas_extendGraphForTypeLambda2, java.util.function.Function<hydra.util.Pair<T0, hydra.graph.Graph>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T0, T1>, hydra.core.Term>>> lowLevelRecurse, hydra.util.Pair<T0, hydra.graph.Graph> valAndCx, hydra.core.Term term) {
    hydra.util.Lazy<hydra.graph.Graph> cx = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(valAndCx));
    hydra.util.Lazy<hydra.graph.Graph> cx1 = new hydra.util.Lazy<>(() -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.graph.Graph otherwise(hydra.core.Term instance) {
        return cx.get();
      }
      
      @Override
      public hydra.graph.Graph visit(hydra.core.Term.Function fun) {
        return ((fun).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.graph.Graph otherwise(hydra.core.Function instance) {
            return cx.get();
          }
          
          @Override
          public hydra.graph.Graph visit(hydra.core.Function.Lambda l) {
            return ((hydra_schemas_extendGraphForLambda2).apply(cx.get())).apply((l).value);
          }
        });
      }
      
      @Override
      public hydra.graph.Graph visit(hydra.core.Term.Let l) {
        return (((hydra_schemas_extendGraphForLet2).apply((java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (ignored -> (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>) (_2 -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))).apply(cx.get())).apply((l).value);
      }
      
      @Override
      public hydra.graph.Graph visit(hydra.core.Term.TypeLambda tl) {
        return ((hydra_schemas_extendGraphForTypeLambda2).apply(cx.get())).apply((tl).value);
      }
    }));
    hydra.util.Lazy<hydra.util.Pair<T0, hydra.core.Term>> fResult = new hydra.util.Lazy<>(() -> hydra.hoisting.Hoisting.<T0>rewriteAndFoldTermWithTypeContext_fResult(
      cx1.get(),
      f,
      (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>) (v2 -> hydra.hoisting.Hoisting.<T0, T1>rewriteAndFoldTermWithTypeContext_recurseForUser(
        cx1.get(),
        lowLevelRecurse,
        v1,
        v2))),
      term,
      hydra.hoisting.Hoisting.<T0>rewriteAndFoldTermWithTypeContext_val(valAndCx)));
    return (hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term>((hydra.util.Pair<T0, hydra.graph.Graph>) ((hydra.util.Pair<T0, hydra.graph.Graph>) (new hydra.util.Pair<T0, hydra.graph.Graph>(hydra.lib.pairs.First.apply(fResult.get()), cx.get()))), hydra.lib.pairs.Second.apply(fResult.get()))));
  }
  
  static <T0> hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term> rewriteAndFoldTermWithTypeContext_result(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>> f, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>> hydra_schemas_extendGraphForLambda2, java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>> hydra_schemas_extendGraphForLet2, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>> hydra_schemas_extendGraphForTypeLambda2, hydra.graph.Graph cx0, hydra.core.Term term0, T0 val0) {
    return hydra.rewriting.Rewriting.rewriteAndFoldTerm(
      (java.util.function.Function<java.util.function.Function<hydra.util.Pair<T0, hydra.graph.Graph>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term>>>, java.util.function.Function<hydra.util.Pair<T0, hydra.graph.Graph>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.util.Pair<T0, hydra.graph.Graph>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term>>>) (v2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term>>) (v3 -> hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContext_wrapper(
        f,
        hydra_schemas_extendGraphForLambda2,
        hydra_schemas_extendGraphForLet2,
        hydra_schemas_extendGraphForTypeLambda2,
        v1,
        v2,
        v3)))),
      (hydra.util.Pair<T0, hydra.graph.Graph>) ((hydra.util.Pair<T0, hydra.graph.Graph>) (new hydra.util.Pair<T0, hydra.graph.Graph>(val0, cx0))),
      term0);
  }
  
  static <T0> T0 rewriteAndFoldTermWithTypeContext_val(hydra.util.Pair<T0, hydra.graph.Graph> valAndCx) {
    return hydra.lib.pairs.First.apply(valAndCx);
  }
  
  static <T0, T1> hydra.util.Pair<T0, hydra.core.Term> rewriteAndFoldTermWithTypeContext_recurseForUser(hydra.graph.Graph cx1, java.util.function.Function<hydra.util.Pair<T0, hydra.graph.Graph>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T0, T1>, hydra.core.Term>>> lowLevelRecurse, T0 newVal, hydra.core.Term subterm) {
    hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<T0, T1>, hydra.core.Term>> result = new hydra.util.Lazy<>(() -> hydra.hoisting.Hoisting.<T0, T1>rewriteAndFoldTermWithTypeContext_result2(
      cx1,
      lowLevelRecurse,
      newVal,
      subterm));
    return (hydra.util.Pair<T0, hydra.core.Term>) ((hydra.util.Pair<T0, hydra.core.Term>) (new hydra.util.Pair<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply(result.get())), hydra.lib.pairs.Second.apply(result.get()))));
  }
  
  static <T0> hydra.util.Pair<T0, hydra.core.Term> rewriteAndFoldTermWithTypeContext_fResult(hydra.graph.Graph cx1, java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>> f, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>> recurseForUser, hydra.core.Term term, T0 val) {
    return ((((f).apply(recurseForUser)).apply(cx1)).apply(val)).apply(term);
  }
  
  static <T0, T1> hydra.util.Pair<hydra.util.Pair<T0, T1>, hydra.core.Term> rewriteAndFoldTermWithTypeContext_result2(hydra.graph.Graph cx1, java.util.function.Function<hydra.util.Pair<T0, hydra.graph.Graph>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T0, T1>, hydra.core.Term>>> lowLevelRecurse, T0 newVal, hydra.core.Term subterm) {
    return ((lowLevelRecurse).apply((hydra.util.Pair<T0, hydra.graph.Graph>) ((hydra.util.Pair<T0, hydra.graph.Graph>) (new hydra.util.Pair<T0, hydra.graph.Graph>(newVal, cx1))))).apply(subterm);
  }
  
  static <T0> hydra.util.Pair<T0, hydra.core.Term> rewriteAndFoldTermWithTypeContextAndPath(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>>> f, hydra.graph.Graph cx0, T0 val0, hydra.core.Term term0) {
    hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>> result = new hydra.util.Lazy<>(() -> hydra.hoisting.Hoisting.<T0>rewriteAndFoldTermWithTypeContextAndPath_result(
      f,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>>) (p0 -> p1 -> hydra.schemas.Schemas.extendGraphForLambda(
        p0,
        p1)),
      (java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>>) (p0 -> p1 -> p2 -> hydra.schemas.Schemas.extendGraphForLet(
        p0,
        p1,
        p2)),
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>>) (p0 -> p1 -> hydra.schemas.Schemas.extendGraphForTypeLambda(
        p0,
        p1)),
      cx0,
      term0,
      val0));
    return (hydra.util.Pair<T0, hydra.core.Term>) ((hydra.util.Pair<T0, hydra.core.Term>) (new hydra.util.Pair<T0, hydra.core.Term>(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result.get())), hydra.lib.pairs.Second.apply(result.get()))));
  }
  
  static <T0, T1> hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term> rewriteAndFoldTermWithTypeContextAndPath_wrapper(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>>> f, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>> hydra_schemas_extendGraphForLambda2, java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>> hydra_schemas_extendGraphForLet2, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>> hydra_schemas_extendGraphForTypeLambda2, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T1, T0>, hydra.core.Term>>>> recurse, java.util.List<hydra.accessors.TermAccessor> path, hydra.util.Pair<hydra.graph.Graph, T0> cxAndVal, hydra.core.Term term) {
    hydra.util.Lazy<hydra.graph.Graph> cx = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(cxAndVal));
    hydra.util.Lazy<hydra.graph.Graph> cx1 = new hydra.util.Lazy<>(() -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.graph.Graph otherwise(hydra.core.Term instance) {
        return cx.get();
      }
      
      @Override
      public hydra.graph.Graph visit(hydra.core.Term.Function fun) {
        return ((fun).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public hydra.graph.Graph otherwise(hydra.core.Function instance) {
            return cx.get();
          }
          
          @Override
          public hydra.graph.Graph visit(hydra.core.Function.Lambda l) {
            return ((hydra_schemas_extendGraphForLambda2).apply(cx.get())).apply((l).value);
          }
        });
      }
      
      @Override
      public hydra.graph.Graph visit(hydra.core.Term.Let l) {
        return (((hydra_schemas_extendGraphForLet2).apply((java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (ignored -> (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>) (_2 -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))).apply(cx.get())).apply((l).value);
      }
      
      @Override
      public hydra.graph.Graph visit(hydra.core.Term.TypeLambda tl) {
        return ((hydra_schemas_extendGraphForTypeLambda2).apply(cx.get())).apply((tl).value);
      }
    }));
    hydra.util.Lazy<hydra.util.Pair<T0, hydra.core.Term>> fResult = new hydra.util.Lazy<>(() -> hydra.hoisting.Hoisting.<T0>rewriteAndFoldTermWithTypeContextAndPath_fResult(
      cx1.get(),
      f,
      path,
      (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>) (v2 -> hydra.hoisting.Hoisting.<T0, T1>rewriteAndFoldTermWithTypeContextAndPath_recurseForUser(
        cx1.get(),
        path,
        recurse,
        v1,
        v2))),
      term,
      hydra.hoisting.Hoisting.<T0>rewriteAndFoldTermWithTypeContextAndPath_val(cxAndVal)));
    return (hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>((hydra.util.Pair<hydra.graph.Graph, T0>) ((hydra.util.Pair<hydra.graph.Graph, T0>) (new hydra.util.Pair<hydra.graph.Graph, T0>(cx.get(), hydra.lib.pairs.First.apply(fResult.get())))), hydra.lib.pairs.Second.apply(fResult.get()))));
  }
  
  static <T0> hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term> rewriteAndFoldTermWithTypeContextAndPath_result(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>>> f, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>> hydra_schemas_extendGraphForLambda2, java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>> hydra_schemas_extendGraphForLet2, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>> hydra_schemas_extendGraphForTypeLambda2, hydra.graph.Graph cx0, hydra.core.Term term0, T0 val0) {
    return hydra.rewriting.Rewriting.rewriteAndFoldTermWithPath(
      (java.util.function.Function<java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>>>>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>>>>>) (v1 -> (java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>>>>) (v2 -> (java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>>>) (v3 -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>>) (v4 -> hydra.hoisting.Hoisting.rewriteAndFoldTermWithTypeContextAndPath_wrapper(
        f,
        hydra_schemas_extendGraphForLambda2,
        hydra_schemas_extendGraphForLet2,
        hydra_schemas_extendGraphForTypeLambda2,
        v1,
        v2,
        v3,
        v4))))),
      (hydra.util.Pair<hydra.graph.Graph, T0>) ((hydra.util.Pair<hydra.graph.Graph, T0>) (new hydra.util.Pair<hydra.graph.Graph, T0>(cx0, val0))),
      term0);
  }
  
  static <T0> T0 rewriteAndFoldTermWithTypeContextAndPath_val(hydra.util.Pair<hydra.graph.Graph, T0> cxAndVal) {
    return hydra.lib.pairs.Second.apply(cxAndVal);
  }
  
  static <T0, T1> hydra.util.Pair<T0, hydra.core.Term> rewriteAndFoldTermWithTypeContextAndPath_recurseForUser(hydra.graph.Graph cx1, java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T1, T0>, hydra.core.Term>>>> recurse, T0 valIn, hydra.core.Term termIn) {
    hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<T1, T0>, hydra.core.Term>> result = new hydra.util.Lazy<>(() -> hydra.hoisting.Hoisting.<T0, T1>rewriteAndFoldTermWithTypeContextAndPath_result2(
      cx1,
      path,
      recurse,
      termIn,
      valIn));
    return (hydra.util.Pair<T0, hydra.core.Term>) ((hydra.util.Pair<T0, hydra.core.Term>) (new hydra.util.Pair<T0, hydra.core.Term>(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result.get())), hydra.lib.pairs.Second.apply(result.get()))));
  }
  
  static <T0> hydra.util.Pair<T0, hydra.core.Term> rewriteAndFoldTermWithTypeContextAndPath_fResult(hydra.graph.Graph cx1, java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>>> f, java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>> recurseForUser, hydra.core.Term term, T0 val) {
    return (((((f).apply(recurseForUser)).apply(path)).apply(cx1)).apply(val)).apply(term);
  }
  
  static <T0, T1> hydra.util.Pair<hydra.util.Pair<T1, T0>, hydra.core.Term> rewriteAndFoldTermWithTypeContextAndPath_result2(hydra.graph.Graph cx1, java.util.List<hydra.accessors.TermAccessor> path, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T1, T0>, hydra.core.Term>>>> recurse, hydra.core.Term termIn, T0 valIn) {
    return (((recurse).apply(path)).apply((hydra.util.Pair<hydra.graph.Graph, T0>) ((hydra.util.Pair<hydra.graph.Graph, T0>) (new hydra.util.Pair<hydra.graph.Graph, T0>(cx1, valIn))))).apply(termIn);
  }
  
  static <T0> T0 rewriteTermWithTypeContext(java.util.function.Function<java.util.function.Function<hydra.core.Term, T0>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>>> f, hydra.graph.Graph cx0, hydra.core.Term term0) {
    return hydra.hoisting.Hoisting.<T0>rewriteTermWithTypeContext_rewrite(
      (java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>>>) (v1 -> (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>>) (v2 -> (java.util.function.Function<hydra.core.Term, T0>) (v3 -> hydra.hoisting.Hoisting.<T0>rewriteTermWithTypeContext_f2(
        f,
        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>>) (p0 -> p1 -> hydra.schemas.Schemas.extendGraphForLambda(
          p0,
          p1)),
        (java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>>) (p0 -> p1 -> p2 -> hydra.schemas.Schemas.extendGraphForLet(
          p0,
          p1,
          p2)),
        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>>) (p0 -> p1 -> hydra.schemas.Schemas.extendGraphForTypeLambda(
          p0,
          p1)),
        v1,
        v2,
        v3)))),
      cx0,
      term0);
  }
  
  static <T0> T0 rewriteTermWithTypeContext_f2(java.util.function.Function<java.util.function.Function<hydra.core.Term, T0>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>>> f, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>> hydra_schemas_extendGraphForLambda2, java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>> hydra_schemas_extendGraphForLet2, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>> hydra_schemas_extendGraphForTypeLambda2, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>> recurse, hydra.graph.Graph cx, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Term, T0> recurse1 = (java.util.function.Function<hydra.core.Term, T0>) (v1 -> hydra.hoisting.Hoisting.<T0>rewriteTermWithTypeContext_recurse1(
      cx,
      recurse,
      v1));
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public T0 otherwise(hydra.core.Term instance) {
        return (((f).apply(recurse1)).apply(cx)).apply(term);
      }
      
      @Override
      public T0 visit(hydra.core.Term.Function fun) {
        return ((fun).value).accept(new hydra.core.Function.PartialVisitor<>() {
          @Override
          public T0 otherwise(hydra.core.Function instance) {
            return (((f).apply(recurse1)).apply(cx)).apply(term);
          }
          
          @Override
          public T0 visit(hydra.core.Function.Lambda l) {
            hydra.graph.Graph cx1 = ((hydra_schemas_extendGraphForLambda2).apply(cx)).apply((l).value);
            return (((f).apply((java.util.function.Function<hydra.core.Term, T0>) (v1 -> hydra.hoisting.Hoisting.<T0>rewriteTermWithTypeContext_recurse2(
              cx1,
              recurse,
              v1)))).apply(cx1)).apply(term);
          }
        });
      }
      
      @Override
      public T0 visit(hydra.core.Term.Let l) {
        hydra.util.Lazy<hydra.graph.Graph> cx1 = new hydra.util.Lazy<>(() -> (((hydra_schemas_extendGraphForLet2).apply((java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (ignored -> (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>) (_2 -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))).apply(cx)).apply((l).value));
        return (((f).apply((java.util.function.Function<hydra.core.Term, T0>) (v1 -> hydra.hoisting.Hoisting.<T0>rewriteTermWithTypeContext_recurse22(
          cx1.get(),
          recurse,
          v1)))).apply(cx1.get())).apply(term);
      }
      
      @Override
      public T0 visit(hydra.core.Term.TypeLambda tl) {
        hydra.graph.Graph cx1 = ((hydra_schemas_extendGraphForTypeLambda2).apply(cx)).apply((tl).value);
        return (((f).apply((java.util.function.Function<hydra.core.Term, T0>) (v1 -> hydra.hoisting.Hoisting.<T0>rewriteTermWithTypeContext_recurse23(
          cx1,
          recurse,
          v1)))).apply(cx1)).apply(term);
      }
    });
  }
  
  static <T0> T0 rewriteTermWithTypeContext_rewrite(java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>>> f2, hydra.graph.Graph cx, hydra.core.Term term) {
    return (((f2).apply((java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>>) (v1 -> (java.util.function.Function<hydra.core.Term, T0>) (v2 -> hydra.hoisting.Hoisting.<T0>rewriteTermWithTypeContext_rewrite(
      f2,
      v1,
      v2))))).apply(cx)).apply(term);
  }
  
  static <T0> T0 rewriteTermWithTypeContext_recurse1(hydra.graph.Graph cx, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>> recurse, hydra.core.Term term) {
    return ((recurse).apply(cx)).apply(term);
  }
  
  static <T0> T0 rewriteTermWithTypeContext_recurse2(hydra.graph.Graph cx1, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>> recurse, hydra.core.Term term) {
    return ((recurse).apply(cx1)).apply(term);
  }
  
  static <T0> T0 rewriteTermWithTypeContext_recurse22(hydra.graph.Graph cx1, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>> recurse, hydra.core.Term term) {
    return ((recurse).apply(cx1)).apply(term);
  }
  
  static <T0> T0 rewriteTermWithTypeContext_recurse23(hydra.graph.Graph cx1, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>> recurse, hydra.core.Term term) {
    return ((recurse).apply(cx1)).apply(term);
  }
  
  static <T0, T1> Boolean shouldHoistAll(T0 ignored, T1 _2) {
    return true;
  }
  
  static Boolean shouldHoistCaseStatement(hydra.util.Pair<java.util.List<hydra.accessors.TermAccessor>, hydra.core.Term> pathAndTerm) {
    hydra.util.Lazy<java.util.List<hydra.accessors.TermAccessor>> path = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pathAndTerm));
    hydra.util.Lazy<hydra.core.Term> term = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pathAndTerm));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Not.apply(hydra.lib.logic.Or.apply(
        hydra.hoisting.Hoisting.isUnionElimination(term.get()),
        hydra.hoisting.Hoisting.isUnionEliminationApplication(term.get()))),
      () -> false,
      () -> ((java.util.function.Supplier<Boolean>) (() -> {
        hydra.util.Lazy<hydra.util.Pair<Boolean, Boolean>> finalState = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.util.Pair<Boolean, Boolean>, java.util.function.Function<hydra.accessors.TermAccessor, hydra.util.Pair<Boolean, Boolean>>>) (st -> (java.util.function.Function<hydra.accessors.TermAccessor, hydra.util.Pair<Boolean, Boolean>>) (acc -> hydra.hoisting.Hoisting.updateHoistState(
            acc,
            st))),
          (hydra.util.Pair<Boolean, Boolean>) ((hydra.util.Pair<Boolean, Boolean>) (new hydra.util.Pair<Boolean, Boolean>(true, false))),
          path.get()));
        return hydra.lib.logic.Not.apply(hydra.lib.pairs.First.apply(finalState.get()));
      })).get());
  }
  
  static Boolean shouldHoistPolymorphic(hydra.graph.Graph cx, hydra.core.Binding binding) {
    return hydra.lib.logic.Or.apply(
      hydra.hoisting.Hoisting.bindingIsPolymorphic(binding),
      hydra.hoisting.Hoisting.bindingUsesContextTypeVars(
        cx,
        binding));
  }
  
  static hydra.util.Pair<Boolean, Boolean> updateHoistState(hydra.accessors.TermAccessor accessor, hydra.util.Pair<Boolean, Boolean> state) {
    hydra.util.Lazy<Boolean> atTop = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(state));
    hydra.util.Lazy<Boolean> usedApp = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(state));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Not.apply(atTop.get()),
      () -> (hydra.util.Pair<Boolean, Boolean>) ((hydra.util.Pair<Boolean, Boolean>) (new hydra.util.Pair<Boolean, Boolean>(false, usedApp.get()))),
      () -> (accessor).accept(new hydra.accessors.TermAccessor.PartialVisitor<>() {
        @Override
        public hydra.util.Pair<Boolean, Boolean> otherwise(hydra.accessors.TermAccessor instance) {
          return (hydra.util.Pair<Boolean, Boolean>) ((hydra.util.Pair<Boolean, Boolean>) (new hydra.util.Pair<Boolean, Boolean>(false, usedApp.get())));
        }
        
        @Override
        public hydra.util.Pair<Boolean, Boolean> visit(hydra.accessors.TermAccessor.AnnotatedBody ignored) {
          return (hydra.util.Pair<Boolean, Boolean>) ((hydra.util.Pair<Boolean, Boolean>) (new hydra.util.Pair<Boolean, Boolean>(true, usedApp.get())));
        }
        
        @Override
        public hydra.util.Pair<Boolean, Boolean> visit(hydra.accessors.TermAccessor.LetBody ignored) {
          return (hydra.util.Pair<Boolean, Boolean>) ((hydra.util.Pair<Boolean, Boolean>) (new hydra.util.Pair<Boolean, Boolean>(true, usedApp.get())));
        }
        
        @Override
        public hydra.util.Pair<Boolean, Boolean> visit(hydra.accessors.TermAccessor.LetBinding ignored) {
          return (hydra.util.Pair<Boolean, Boolean>) ((hydra.util.Pair<Boolean, Boolean>) (new hydra.util.Pair<Boolean, Boolean>(true, usedApp.get())));
        }
        
        @Override
        public hydra.util.Pair<Boolean, Boolean> visit(hydra.accessors.TermAccessor.LambdaBody ignored) {
          return hydra.lib.logic.IfElse.lazy(
            usedApp.get(),
            () -> (hydra.util.Pair<Boolean, Boolean>) ((hydra.util.Pair<Boolean, Boolean>) (new hydra.util.Pair<Boolean, Boolean>(false, true))),
            () -> (hydra.util.Pair<Boolean, Boolean>) ((hydra.util.Pair<Boolean, Boolean>) (new hydra.util.Pair<Boolean, Boolean>(true, false))));
        }
        
        @Override
        public hydra.util.Pair<Boolean, Boolean> visit(hydra.accessors.TermAccessor.UnionCasesBranch ignored) {
          return hydra.lib.logic.IfElse.lazy(
            usedApp.get(),
            () -> (hydra.util.Pair<Boolean, Boolean>) ((hydra.util.Pair<Boolean, Boolean>) (new hydra.util.Pair<Boolean, Boolean>(false, true))),
            () -> (hydra.util.Pair<Boolean, Boolean>) ((hydra.util.Pair<Boolean, Boolean>) (new hydra.util.Pair<Boolean, Boolean>(true, false))));
        }
        
        @Override
        public hydra.util.Pair<Boolean, Boolean> visit(hydra.accessors.TermAccessor.UnionCasesDefault ignored) {
          return hydra.lib.logic.IfElse.lazy(
            usedApp.get(),
            () -> (hydra.util.Pair<Boolean, Boolean>) ((hydra.util.Pair<Boolean, Boolean>) (new hydra.util.Pair<Boolean, Boolean>(false, true))),
            () -> (hydra.util.Pair<Boolean, Boolean>) ((hydra.util.Pair<Boolean, Boolean>) (new hydra.util.Pair<Boolean, Boolean>(true, false))));
        }
        
        @Override
        public hydra.util.Pair<Boolean, Boolean> visit(hydra.accessors.TermAccessor.ApplicationFunction ignored) {
          return hydra.lib.logic.IfElse.lazy(
            usedApp.get(),
            () -> (hydra.util.Pair<Boolean, Boolean>) ((hydra.util.Pair<Boolean, Boolean>) (new hydra.util.Pair<Boolean, Boolean>(false, true))),
            () -> (hydra.util.Pair<Boolean, Boolean>) ((hydra.util.Pair<Boolean, Boolean>) (new hydra.util.Pair<Boolean, Boolean>(true, true))));
        }
        
        @Override
        public hydra.util.Pair<Boolean, Boolean> visit(hydra.accessors.TermAccessor.ApplicationArgument ignored) {
          return (hydra.util.Pair<Boolean, Boolean>) ((hydra.util.Pair<Boolean, Boolean>) (new hydra.util.Pair<Boolean, Boolean>(false, usedApp.get())));
        }
      }));
  }
}
