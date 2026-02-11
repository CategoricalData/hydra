// Note: this is an automatically generated file. Do not edit.

package hydra.inference;

/**
 * Type inference following Algorithm W, extended for nominal terms and types
 */
public interface Inference {
  static <T0, T1> hydra.compute.Flow<T0, T1> bindConstraints(hydra.typing.InferenceContext cx, java.util.function.Function<hydra.typing.TypeSubst, hydra.compute.Flow<T0, T1>> f, java.util.List<hydra.typing.TypeConstraint> constraints) {
    return hydra.lib.flows.Bind.apply(
      hydra.unification.Unification.unifyTypeConstraints(
        (cx).schemaTypes,
        constraints),
      (java.util.function.Function<hydra.typing.TypeSubst, hydra.compute.Flow<T0, T1>>) (s -> hydra.lib.flows.Bind.apply(
        hydra.checking.Checking.<T0>checkTypeSubst(
          cx,
          s),
        (java.util.function.Function<hydra.typing.TypeSubst, hydra.compute.Flow<T0, T1>>) (ignored -> (f).apply(s)))));
  }
  
  static hydra.core.Term bindUnboundTypeVariables(hydra.typing.InferenceContext cx, hydra.core.Term term0) {
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> svars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maps.Keys.apply((cx).schemaTypes)));
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> rewrite = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (recurse).apply(term);
      }
      
      @Override
      public hydra.core.Term visit(hydra.core.Term.Let l) {
        java.util.function.Function<hydra.core.Binding, hydra.core.Binding> forBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> {
          hydra.core.Name bname = (b).name;
          hydra.core.Term bterm = (b).term;
          return hydra.lib.maybes.Maybe.apply(
            new hydra.core.Binding(bname, hydra.inference.Inference.bindUnboundTypeVariables(
              cx,
              bterm), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
            (java.util.function.Function<hydra.core.TypeScheme, hydra.core.Binding>) (ts -> {
              hydra.util.Lazy<java.util.Set<hydra.core.Name>> bvars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply((ts).variables));
              java.util.Set<hydra.core.Name> unboundInTerm = hydra.rewriting.Rewriting.freeTypeVariablesInTerm(bterm);
              java.util.Set<hydra.core.Name> unboundInType = hydra.rewriting.Rewriting.freeVariablesInType((ts).type);
              hydra.util.Lazy<java.util.List<hydra.core.Name>> unbound = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply(hydra.lib.sets.Difference.apply(
                hydra.lib.sets.Union.apply(
                  unboundInType,
                  unboundInTerm),
                hydra.lib.sets.Union.apply(
                  svars.get(),
                  bvars.get()))));
              hydra.util.Lazy<hydra.core.Term> bterm2 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, hydra.core.Term>>) (t -> (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (v -> new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(v, t)))),
                bterm,
                unbound.get()));
              hydra.util.Lazy<hydra.core.TypeScheme> ts2 = new hydra.util.Lazy<>(() -> new hydra.core.TypeScheme(hydra.lib.lists.Concat2.apply(
                (ts).variables,
                unbound.get()), (ts).type, (ts).constraints));
              return new hydra.core.Binding(bname, bterm2.get(), hydra.util.Maybe.just(ts2.get()));
            }),
            (b).type);
        });
        return new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Map.apply(
          forBinding,
          ((l).value).bindings), hydra.inference.Inference.bindUnboundTypeVariables(
          cx,
          ((l).value).body)));
      }
    })));
    return hydra.rewriting.Rewriting.rewriteTerm(
      rewrite,
      term0);
  }
  
  static hydra.core.Term buildTypeApplicationTerm(java.util.List<hydra.core.Name> tvars, hydra.core.Term body) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, hydra.core.Term>>) (t -> (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (v -> new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(t, new hydra.core.Type.Variable(v))))),
      body,
      tvars);
  }
  
  static hydra.typing.InferenceContext emptyInferenceContext() {
    return new hydra.typing.InferenceContext((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), false);
  }
  
  static hydra.typing.InferenceContext extendContext(java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>> pairs, hydra.typing.InferenceContext cx) {
    return new hydra.typing.InferenceContext((cx).schemaTypes, (cx).primitiveTypes, hydra.lib.maps.Union.apply(
      hydra.lib.maps.FromList.apply(pairs),
      (cx).dataTypes), (cx).classConstraints, (cx).debug);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> finalizeInferredTerm(hydra.typing.InferenceContext cx, hydra.core.Term term) {
    hydra.core.Term term2 = hydra.inference.Inference.bindUnboundTypeVariables(
      cx,
      term);
    return hydra.lib.flows.Bind.apply(
      hydra.checking.Checking.<T0>checkForUnboundTypeVariables(
        cx,
        term2),
      (java.util.function.Function<java.lang.Void, hydra.compute.Flow<T0, hydra.core.Term>>) (ignored -> hydra.lib.flows.Pure.apply(hydra.rewriting.Rewriting.normalizeTypeVariablesInTerm(term2))));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, T0> forInferredTerm(hydra.typing.InferenceContext cx, hydra.core.Term term, String desc, java.util.function.Function<hydra.typing.InferenceResult, T0> f) {
    return hydra.lib.flows.Map.apply(
      f,
      hydra.inference.Inference.<T1>inferTypeOfTerm(
        cx,
        term,
        desc));
  }
  
  static java.util.Set<hydra.core.Name> freeVariablesInContext(hydra.typing.InferenceContext cx) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.Set<hydra.core.Name>>>) ((java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.Set<hydra.core.Name>>>) (p0 -> p1 -> hydra.lib.sets.Union.apply(
        p0,
        p1))),
      (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      hydra.lib.lists.Map.apply(
        hydra.rewriting.Rewriting::freeVariablesInTypeSchemeSimple,
        hydra.lib.maps.Elems.apply((cx).dataTypes)));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Type> freshVariableType() {
    return hydra.lib.flows.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
      hydra.schemas.Schemas.<T0>freshName());
  }
  
  static hydra.core.TypeScheme generalize(hydra.typing.InferenceContext cx, hydra.core.Type typ) {
    java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> allConstraints = (cx).classConstraints;
    java.util.function.Function<hydra.core.Name, Boolean> isTypeVarName = (java.util.function.Function<hydra.core.Name, Boolean>) (name -> {
      java.util.List<String> parts = hydra.lib.strings.SplitOn.apply(
        ".",
        (name).value);
      return hydra.lib.equality.Lte.apply(
        hydra.lib.lists.Length.apply(parts),
        1);
    });
    hydra.util.Lazy<java.util.List<hydra.core.Name>> vars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Nub.apply(hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.Name, Boolean>) (v -> hydra.lib.logic.And.apply(
        hydra.inference.Inference.isUnbound(
          cx,
          v),
        (isTypeVarName).apply(v))),
      hydra.rewriting.Rewriting.freeVariablesInTypeOrdered(typ))));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> relevantConstraints = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (v -> hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.core.TypeVariableMetadata, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (meta -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeVariableMetadata>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeVariableMetadata>(v, meta)))),
        hydra.lib.maps.Lookup.apply(
          v,
          allConstraints))),
      vars.get()))));
    hydra.util.Lazy<hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>> constraintsMaybe = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.maps.Null.apply(relevantConstraints.get()),
      () -> (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()),
      () -> hydra.util.Maybe.just(relevantConstraints.get())));
    return new hydra.core.TypeScheme(vars.get(), typ, constraintsMaybe.get());
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.graph.Graph> inferGraphTypes(hydra.graph.Graph g0) {
    java.util.function.Function<hydra.core.Let, hydra.graph.Graph> fromLetTerm = (java.util.function.Function<hydra.core.Let, hydra.graph.Graph>) (l -> {
      java.util.List<hydra.core.Binding> bindings = (l).bindings;
      hydra.core.Term body = (l).body;
      return new hydra.graph.Graph(bindings, (java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) ((java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), body, (g0).primitives, (g0).schema);
    });
    java.util.function.Function<hydra.graph.Graph, hydra.core.Term> toLetTerm = (java.util.function.Function<hydra.graph.Graph, hydra.core.Term>) (g -> {
      java.util.function.Function<hydra.core.Binding, hydra.core.Binding> toBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (el -> new hydra.core.Binding((el).name, (el).term, (el).type));
      return new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Map.apply(
        toBinding,
        (g).elements), (g).body));
    });
    return hydra.monads.Monads.withTrace(
      "graph inference",
      hydra.lib.flows.Bind.apply(
        hydra.schemas.Schemas.<T0>graphToInferenceContext(g0),
        (java.util.function.Function<hydra.typing.InferenceContext, hydra.compute.Flow<T0, hydra.graph.Graph>>) (cx -> hydra.lib.flows.Bind.apply(
          hydra.inference.Inference.<T0>inferTypeOfTerm(
            cx,
            (toLetTerm).apply(g0),
            "graph term"),
          (java.util.function.Function<hydra.typing.InferenceResult, hydra.compute.Flow<T0, hydra.graph.Graph>>) (result -> {
            hydra.core.Term term = (result).term;
            hydra.core.Type ts = (result).type;
            return hydra.lib.flows.Bind.apply(
              hydra.inference.Inference.<T0>finalizeInferredTerm(
                cx,
                term),
              (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.graph.Graph>>) (finalized -> ((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.graph.Graph>>) (v1 -> hydra.inference.Inference.inferGraphTypes_forFinal(
                fromLetTerm,
                v1))).apply(finalized)));
          })))));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, T0> inferGraphTypes_forFinal(java.util.function.Function<hydra.core.Let, T0> fromLetTerm, hydra.core.Term finalized) {
    return (finalized).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T1, T0> visit(hydra.core.Term.Let l) {
        return hydra.lib.flows.Pure.apply((fromLetTerm).apply((l).value));
      }
      
      @Override
      public hydra.compute.Flow<T1, T0> visit(hydra.core.Term.Variable ignored) {
        return hydra.lib.flows.Fail.apply("Expected inferred graph as let term");
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.typing.InferenceResult> inferInGraphContext(hydra.core.Term term) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.graph.Graph>getState(),
      (java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<hydra.graph.Graph, hydra.typing.InferenceResult>>) (g -> hydra.lib.flows.Bind.apply(
        hydra.schemas.Schemas.graphToInferenceContext(g),
        (java.util.function.Function<hydra.typing.InferenceContext, hydra.compute.Flow<hydra.graph.Graph, hydra.typing.InferenceResult>>) (cx -> hydra.inference.Inference.inferTypeOfTerm(
          cx,
          term,
          "single term")))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>> inferMany(hydra.typing.InferenceContext cx, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Term, String>> pairs) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(pairs),
      () -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()), (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()), hydra.substitution.Substitution.idTypeSubst()))))))),
      () -> hydra.inference.Inference.<T0>inferMany_dflt(
        cx,
        (java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>>) (p0 -> p1 -> hydra.substitution.Substitution.composeTypeSubst(
          p0,
          p1)),
        (java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.typing.InferenceContext, hydra.typing.InferenceContext>>) (p0 -> p1 -> hydra.substitution.Substitution.substInContext(
          p0,
          p1)),
        (java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (p0 -> p1 -> hydra.substitution.Substitution.substInType(
          p0,
          p1)),
        (java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (p0 -> p1 -> hydra.substitution.Substitution.substTypesInTerm(
          p0,
          p1)),
        pairs));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>> inferMany_dflt(hydra.typing.InferenceContext cx, java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>> hydra_substitution_composeTypeSubst2, java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.typing.InferenceContext, hydra.typing.InferenceContext>> hydra_substitution_substInContext2, java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.core.Type, hydra.core.Type>> hydra_substitution_substInType2, java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.core.Term, hydra.core.Term>> hydra_substitution_substTypesInTerm2, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Term, String>> pairs) {
    hydra.util.Lazy<String> desc = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.lists.Head.apply(pairs)));
    hydra.util.Lazy<hydra.core.Term> e = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.lists.Head.apply(pairs)));
    hydra.util.Lazy<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Term, String>>> tl = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(pairs));
    return hydra.lib.flows.Bind.apply(
      hydra.inference.Inference.<T0>inferTypeOfTerm(
        cx,
        e.get(),
        desc.get()),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>>>) (result1 -> {
        hydra.core.Term e1 = (result1).term;
        hydra.typing.TypeSubst s1 = (result1).subst;
        hydra.core.Type t1 = (result1).type;
        return hydra.lib.flows.Bind.apply(
          hydra.inference.Inference.<T0>inferMany(
            ((hydra_substitution_substInContext2).apply(s1)).apply(cx),
            tl.get()),
          (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>>>) (result2 -> {
            hydra.util.Lazy<java.util.List<hydra.core.Term>> e2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result2));
            hydra.util.Lazy<hydra.typing.TypeSubst> s2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(result2)));
            hydra.util.Lazy<java.util.List<hydra.core.Type>> t2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(result2)));
            return hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>(hydra.lib.lists.Cons.apply(
              ((hydra_substitution_substTypesInTerm2).apply(s2.get())).apply(e1),
              e2.get()), (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>(hydra.lib.lists.Cons.apply(
              ((hydra_substitution_substInType2).apply(s2.get())).apply(t1),
              t2.get()), ((hydra_substitution_composeTypeSubst2).apply(s1)).apply(s2.get()))))))));
          }));
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfAnnotatedTerm(hydra.typing.InferenceContext cx, hydra.core.AnnotatedTerm at) {
    java.util.Map<hydra.core.Name, hydra.core.Term> ann = (at).annotation;
    hydra.core.Term term = (at).body;
    return hydra.lib.flows.Bind.apply(
      hydra.inference.Inference.<T0>inferTypeOfTerm(
        cx,
        term,
        "annotated term"),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (result -> {
        java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> iconstraints = (result).classConstraints;
        hydra.typing.TypeSubst isubst = (result).subst;
        hydra.core.Term iterm = (result).term;
        hydra.core.Type itype = (result).type;
        return hydra.lib.flows.Pure.apply(new hydra.typing.InferenceResult(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(iterm, ann)), itype, isubst, iconstraints));
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfApplication(hydra.typing.InferenceContext cx, hydra.core.Application app) {
    hydra.core.Term e0 = (app).function;
    hydra.core.Term e1 = (app).argument;
    return hydra.lib.flows.Bind.apply(
      hydra.inference.Inference.<T0>inferTypeOfTerm(
        cx,
        e0,
        "lhs"),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (lhsResult -> {
        hydra.core.Term a = (lhsResult).term;
        java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c0 = (lhsResult).classConstraints;
        hydra.typing.TypeSubst s0 = (lhsResult).subst;
        hydra.core.Type t0 = (lhsResult).type;
        return hydra.lib.flows.Bind.apply(
          hydra.inference.Inference.<T0>inferTypeOfTerm(
            hydra.substitution.Substitution.substInContext(
              s0,
              cx),
            e1,
            "rhs"),
          (java.util.function.Function<hydra.typing.InferenceResult, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (rhsResult -> {
            hydra.core.Term b = (rhsResult).term;
            java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c1 = (rhsResult).classConstraints;
            hydra.typing.TypeSubst s1 = (rhsResult).subst;
            hydra.core.Type t1 = (rhsResult).type;
            return hydra.lib.flows.Bind.apply(
              hydra.schemas.Schemas.<T0>freshName(),
              (java.util.function.Function<hydra.core.Name, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (v -> hydra.lib.flows.Bind.apply(
                hydra.unification.Unification.unifyTypes(
                  (cx).schemaTypes,
                  hydra.substitution.Substitution.substInType(
                    s1,
                    t0),
                  new hydra.core.Type.Function(new hydra.core.FunctionType(t1, new hydra.core.Type.Variable(v))),
                  "application lhs"),
                (java.util.function.Function<hydra.typing.TypeSubst, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (s2 -> hydra.lib.flows.Bind.apply(
                  hydra.checking.Checking.<T0>checkTypeSubst(
                    cx,
                    s2),
                  (java.util.function.Function<hydra.typing.TypeSubst, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (ignored -> {
                    java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c0Subst = hydra.substitution.Substitution.substInClassConstraints(
                      s2,
                      hydra.substitution.Substitution.substInClassConstraints(
                        s1,
                        c0));
                    java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c1Subst = hydra.substitution.Substitution.substInClassConstraints(
                      s2,
                      c1);
                    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> rConstraints = new hydra.util.Lazy<>(() -> hydra.inference.Inference.mergeClassConstraints(
                      c0Subst,
                      c1Subst));
                    hydra.core.Term rExpr = new hydra.core.Term.Application(new hydra.core.Application(hydra.substitution.Substitution.substTypesInTerm(
                      hydra.substitution.Substitution.composeTypeSubst(
                        s1,
                        s2),
                      a), hydra.substitution.Substitution.substTypesInTerm(
                      s2,
                      b)));
                    hydra.typing.TypeSubst rSubst = hydra.substitution.Substitution.composeTypeSubstList(java.util.List.of(
                      s0,
                      s1,
                      s2));
                    hydra.core.Type rType = hydra.substitution.Substitution.substInType(
                      s2,
                      new hydra.core.Type.Variable(v));
                    return hydra.lib.flows.Pure.apply(new hydra.typing.InferenceResult(rExpr, rType, rSubst, rConstraints.get()));
                  }))))));
          }));
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfCaseStatement(hydra.typing.InferenceContext cx, hydra.core.CaseStatement caseStmt) {
    java.util.List<hydra.core.Field> cases = (caseStmt).cases;
    hydra.util.Maybe<hydra.core.Term> dflt = (caseStmt).default_;
    hydra.util.Lazy<java.util.List<hydra.core.Name>> fnames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      projected -> projected.name,
      cases));
    hydra.core.Name tname = (caseStmt).typeName;
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>requireSchemaType(
        cx,
        tname),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (schemaType -> {
        hydra.core.Type stype = (schemaType).type;
        java.util.List<hydra.core.Name> svars = (schemaType).variables;
        return hydra.lib.flows.Bind.apply(
          hydra.extract.core.Core.<T0>unionType(
            tname,
            stype),
          (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (sfields -> hydra.lib.flows.Bind.apply(
            hydra.lib.flows.MapMaybe.apply(
              (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (t -> hydra.inference.Inference.<T0>inferTypeOfTerm(
                cx,
                t,
                hydra.lib.strings.Cat.apply(java.util.List.of(
                  "case ",
                  (tname).value,
                  ".<default>")))),
              dflt),
            (java.util.function.Function<hydra.util.Maybe<hydra.typing.InferenceResult>, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (dfltResult -> hydra.lib.flows.Bind.apply(
              hydra.inference.Inference.<T0>inferMany(
                cx,
                hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<hydra.core.Term, String>>) (f -> (hydra.util.Tuple.Tuple2<hydra.core.Term, String>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, String>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, String>((f).term, hydra.lib.strings.Cat.apply(java.util.List.of(
                    "case ",
                    (tname).value,
                    ".",
                    ((f).name).value)))))),
                  cases)),
              (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (caseResults -> {
                hydra.util.Lazy<hydra.typing.TypeSubst> isubst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(caseResults)));
                hydra.util.Lazy<java.util.List<hydra.core.Term>> iterms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(caseResults));
                hydra.util.Lazy<java.util.List<hydra.core.Type>> itypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(caseResults)));
                return hydra.lib.flows.Bind.apply(
                  hydra.schemas.Schemas.<T0>freshName(),
                  (java.util.function.Function<hydra.core.Name, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (codv -> {
                    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Type>> caseMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
                      (java.util.function.Function<hydra.core.FieldType, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>) (ft -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>((ft).name, (ft).type)))),
                      sfields)));
                    hydra.core.Type cod = new hydra.core.Type.Variable(codv);
                    hydra.util.Lazy<java.util.List<hydra.typing.TypeConstraint>> caseConstraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.ZipWith.apply(
                      (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.typing.TypeConstraint>>>) (fname -> (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.typing.TypeConstraint>>) (itype -> hydra.lib.maybes.Map.apply(
                        (java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>) (ftype -> new hydra.typing.TypeConstraint(itype, new hydra.core.Type.Function(new hydra.core.FunctionType(ftype, cod)), "case type")),
                        hydra.lib.maps.Lookup.apply(
                          fname,
                          caseMap.get())))),
                      fnames.get(),
                      itypes.get())));
                    hydra.util.Lazy<java.util.List<hydra.typing.TypeConstraint>> dfltConstraints = new hydra.util.Lazy<>(() -> hydra.monads.Monads.maybeToList(hydra.lib.maybes.Map.apply(
                      (java.util.function.Function<hydra.typing.InferenceResult, hydra.typing.TypeConstraint>) (r -> new hydra.typing.TypeConstraint(cod, hydra.substitution.Substitution.substInType(
                        isubst.get(),
                        (r).type), "match default")),
                      dfltResult)));
                    return hydra.inference.Inference.mapConstraints(
                      cx,
                      (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst -> hydra.inference.Inference.yield(
                        hydra.inference.Inference.buildTypeApplicationTerm(
                          svars,
                          new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Union(new hydra.core.CaseStatement(tname, hydra.lib.maybes.Map.apply(
                            projected -> projected.term,
                            dfltResult), hydra.lib.lists.ZipWith.apply(
                            (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.core.Field>>) (n -> (java.util.function.Function<hydra.core.Term, hydra.core.Field>) (t -> new hydra.core.Field(n, t))),
                            fnames.get(),
                            iterms.get())))))),
                        new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.schemas.Schemas.nominalApplication(
                          tname,
                          hydra.lib.lists.Map.apply(
                            (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
                            svars)), cod)),
                        hydra.substitution.Substitution.composeTypeSubstList(hydra.lib.lists.Concat.apply(java.util.List.of(
                          hydra.monads.Monads.maybeToList(hydra.lib.maybes.Map.apply(
                            projected -> projected.subst,
                            dfltResult)),
                          java.util.List.of(
                            isubst.get(),
                            subst)))))),
                      hydra.lib.lists.Concat.apply(java.util.List.of(
                        dfltConstraints.get(),
                        caseConstraints.get())));
                  }));
              }))))));
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfCollection(hydra.typing.InferenceContext cx, java.util.function.Function<hydra.core.Type, hydra.core.Type> typCons, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term> trmCons, String desc, java.util.List<hydra.core.Term> els) {
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>freshName(),
      (java.util.function.Function<hydra.core.Name, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (var -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(els),
        () -> hydra.lib.flows.Pure.apply(hydra.inference.Inference.yield(
          hydra.inference.Inference.buildTypeApplicationTerm(
            java.util.List.of(var),
            (trmCons).apply((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))),
          (typCons).apply(new hydra.core.Type.Variable(var)),
          hydra.substitution.Substitution.idTypeSubst())),
        () -> hydra.lib.flows.Bind.apply(
          hydra.inference.Inference.<T0>inferMany(
            cx,
            hydra.lib.lists.Zip.apply(
              els,
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<Integer, String>) (i -> hydra.lib.strings.Cat.apply(java.util.List.of(
                  "#",
                  hydra.lib.literals.ShowInt32.apply(i)))),
                hydra.lib.math.Range.apply(
                  1,
                  hydra.lib.math.Add.apply(
                    hydra.lib.lists.Length.apply(els),
                    1))))),
          (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (results -> {
            hydra.util.Lazy<java.util.List<hydra.core.Type>> types = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(results)));
            hydra.util.Lazy<java.util.List<hydra.typing.TypeConstraint>> constraints = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>) (t -> new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(var), t, desc)),
              types.get()));
            hydra.util.Lazy<hydra.typing.TypeSubst> subst1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(results)));
            hydra.util.Lazy<java.util.List<hydra.core.Term>> terms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(results));
            return hydra.inference.Inference.mapConstraints(
              cx,
              (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst2 -> {
                hydra.typing.TypeSubst isubst = hydra.substitution.Substitution.composeTypeSubst(
                  subst1.get(),
                  subst2);
                hydra.core.Term iterm = (trmCons).apply(terms.get());
                hydra.core.Type itype = (typCons).apply(new hydra.core.Type.Variable(var));
                return hydra.inference.Inference.yield(
                  iterm,
                  itype,
                  isubst);
              }),
              constraints.get());
          })))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.TypeScheme>> inferTypeOf(hydra.typing.InferenceContext cx, hydra.core.Term term) {
    hydra.util.Lazy<hydra.core.Term> letTerm = new hydra.util.Lazy<>(() -> new hydra.core.Term.Let(new hydra.core.Let(java.util.List.of(new hydra.core.Binding(new hydra.core.Name("ignoredVariableName"), term, (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("ignoredBody")))));
    return hydra.lib.flows.Bind.apply(
      hydra.inference.Inference.<T0>inferTypeOfTerm(
        cx,
        letTerm.get(),
        "infer type of term"),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.TypeScheme>>>) (result -> ((java.util.function.Function<hydra.typing.InferenceResult, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.TypeScheme>>>) (v1 -> hydra.inference.Inference.<T0>inferTypeOf_unifyAndSubst(
        cx,
        hydra.extract.core.Core::let,
        v1))).apply(result)));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.TypeScheme>> inferTypeOf_forBindings(java.util.List<hydra.core.Binding> bindings) {
    hydra.util.Lazy<hydra.core.Binding> binding = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(bindings));
    hydra.util.Maybe<hydra.core.TypeScheme> mts = (binding.get()).type;
    hydra.core.Term term1 = (binding.get()).term;
    return hydra.lib.maybes.Maybe.apply(
      hydra.lib.flows.Fail.apply("Expected a type scheme"),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.TypeScheme>>>) (ts -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.TypeScheme>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.TypeScheme>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.TypeScheme>(term1, ts))))),
      mts);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.TypeScheme>> inferTypeOf_unifyAndSubst(hydra.typing.InferenceContext cx, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Let>> hydra_extract_core_let2, hydra.typing.InferenceResult result) {
    hydra.typing.TypeSubst subst = (result).subst;
    return hydra.lib.flows.Bind.apply(
      hydra.inference.Inference.<T0>finalizeInferredTerm(
        cx,
        (result).term),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.TypeScheme>>>) (finalized -> hydra.lib.flows.Bind.apply(
        hydra.lexical.Lexical.withEmptyGraph((hydra_extract_core_let2).apply(finalized)),
        (java.util.function.Function<hydra.core.Let, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.TypeScheme>>>) (letResult -> {
          java.util.List<hydra.core.Binding> bindings = (letResult).bindings;
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              1,
              hydra.lib.lists.Length.apply(bindings)),
            () -> hydra.inference.Inference.<T0>inferTypeOf_forBindings(bindings),
            () -> hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
              "Expected a single binding with a type scheme, but got: ",
              hydra.lib.literals.ShowInt32.apply(hydra.lib.lists.Length.apply(bindings)),
              " bindings"))));
        }))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfEither(hydra.typing.InferenceContext cx, hydra.util.Either<hydra.core.Term, hydra.core.Term> e) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (l -> hydra.lib.flows.Bind.apply(
        hydra.inference.Inference.<T0>inferTypeOfTerm(
          cx,
          l,
          "either left value"),
        (java.util.function.Function<hydra.typing.InferenceResult, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (r1 -> {
          hydra.core.Term iterm = (r1).term;
          hydra.core.Type leftType = (r1).type;
          hydra.typing.TypeSubst subst = (r1).subst;
          return hydra.lib.flows.Bind.apply(
            hydra.inference.Inference.<T0>freshVariableType(),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (rightType -> {
              hydra.util.Lazy<hydra.core.Term> eitherTerm = new hydra.util.Lazy<>(() -> new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(iterm)))));
              hydra.core.Type eitherType = new hydra.core.Type.Either(new hydra.core.EitherType(leftType, rightType));
              hydra.core.Term termWithLeftType = new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(eitherTerm.get(), leftType));
              hydra.core.Term termWithBothTypes = new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(termWithLeftType, rightType));
              return hydra.inference.Inference.<T0>yieldChecked(
                termWithBothTypes,
                eitherType,
                subst);
            }));
        }))),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (r -> hydra.lib.flows.Bind.apply(
        hydra.inference.Inference.<T0>inferTypeOfTerm(
          cx,
          r,
          "either right value"),
        (java.util.function.Function<hydra.typing.InferenceResult, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (r1 -> {
          hydra.core.Term iterm = (r1).term;
          hydra.core.Type rightType = (r1).type;
          hydra.typing.TypeSubst subst = (r1).subst;
          return hydra.lib.flows.Bind.apply(
            hydra.inference.Inference.<T0>freshVariableType(),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (leftType -> {
              hydra.util.Lazy<hydra.core.Term> eitherTerm = new hydra.util.Lazy<>(() -> new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(iterm)))));
              hydra.core.Type eitherType = new hydra.core.Type.Either(new hydra.core.EitherType(leftType, rightType));
              hydra.core.Term termWithLeftType = new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(eitherTerm.get(), leftType));
              hydra.core.Term termWithBothTypes = new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(termWithLeftType, rightType));
              return hydra.inference.Inference.<T0>yieldChecked(
                termWithBothTypes,
                eitherType,
                subst);
            }));
        }))),
      e);
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfElimination(hydra.typing.InferenceContext cx, hydra.core.Elimination elm) {
    return (elm).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Elimination.Record p) {
        return hydra.inference.Inference.<T0>inferTypeOfProjection(
          cx,
          (p).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Elimination.Union c) {
        return hydra.inference.Inference.<T0>inferTypeOfCaseStatement(
          cx,
          (c).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Elimination.Wrap tname) {
        return hydra.inference.Inference.<T0>inferTypeOfUnwrap(
          cx,
          (tname).value);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfFunction(hydra.typing.InferenceContext cx, hydra.core.Function f) {
    return (f).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Function.Elimination elm) {
        return hydra.inference.Inference.<T0>inferTypeOfElimination(
          cx,
          (elm).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Function.Lambda l) {
        return hydra.inference.Inference.<T0>inferTypeOfLambda(
          cx,
          (l).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Function.Primitive name) {
        return hydra.inference.Inference.<T0>inferTypeOfPrimitive(
          cx,
          (name).value);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfInjection(hydra.typing.InferenceContext cx, hydra.core.Injection injection) {
    hydra.core.Field field = (injection).field;
    hydra.core.Name fname = (field).name;
    hydra.core.Term term = (field).term;
    hydra.core.Name tname = (injection).typeName;
    return hydra.lib.flows.Bind.apply(
      hydra.inference.Inference.<T0>inferTypeOfTerm(
        cx,
        term,
        "injected term"),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (result -> hydra.lib.flows.Bind.apply(
        hydra.schemas.Schemas.<T0>requireSchemaType(
          cx,
          tname),
        (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (schemaType -> {
          hydra.typing.TypeSubst isubst = (result).subst;
          hydra.core.Term iterm = (result).term;
          hydra.core.Type ityp = (result).type;
          hydra.core.Type stype = (schemaType).type;
          java.util.List<hydra.core.Name> svars = (schemaType).variables;
          return hydra.lib.flows.Bind.apply(
            hydra.extract.core.Core.<T0>unionType(
              tname,
              stype),
            (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (sfields -> hydra.lib.flows.Bind.apply(
              hydra.schemas.Schemas.<T0>findFieldType(
                fname,
                sfields),
              (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (ftyp -> hydra.inference.Inference.mapConstraints(
                cx,
                (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst -> hydra.inference.Inference.yield(
                  hydra.inference.Inference.buildTypeApplicationTerm(
                    svars,
                    new hydra.core.Term.Union(new hydra.core.Injection(tname, new hydra.core.Field(fname, iterm)))),
                  hydra.schemas.Schemas.nominalApplication(
                    tname,
                    hydra.lib.lists.Map.apply(
                      (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
                      svars)),
                  hydra.substitution.Substitution.composeTypeSubst(
                    isubst,
                    subst))),
                java.util.List.of(new hydra.typing.TypeConstraint(ftyp, ityp, "schema type of injected field")))))));
        }))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfLambda(hydra.typing.InferenceContext cx, hydra.core.Lambda lambda) {
    hydra.core.Term body = (lambda).body;
    hydra.core.Name var = (lambda).parameter;
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>freshName(),
      (java.util.function.Function<hydra.core.Name, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (vdom -> {
        hydra.core.Type dom = new hydra.core.Type.Variable(vdom);
        hydra.util.Lazy<hydra.typing.InferenceContext> cx2 = new hydra.util.Lazy<>(() -> hydra.inference.Inference.extendContext(
          java.util.List.of((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>(var, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()), dom, (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
          cx));
        return hydra.lib.flows.Bind.apply(
          hydra.inference.Inference.<T0>inferTypeOfTerm(
            cx2.get(),
            body,
            "lambda body"),
          (java.util.function.Function<hydra.typing.InferenceResult, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (result -> {
            hydra.typing.TypeSubst isubst = (result).subst;
            hydra.typing.InferenceContext cx3 = hydra.substitution.Substitution.substInContext(
              isubst,
              cx);
            hydra.core.Type icod = (result).type;
            java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> iconstraints = hydra.substitution.Substitution.substInClassConstraints(
              isubst,
              (result).classConstraints);
            hydra.core.Term iterm = (result).term;
            hydra.core.Type rdom = hydra.substitution.Substitution.substInType(
              isubst,
              dom);
            hydra.core.Term rterm = new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(var, hydra.util.Maybe.just(rdom), iterm)));
            hydra.core.Type rtype = new hydra.core.Type.Function(new hydra.core.FunctionType(rdom, icod));
            hydra.util.Lazy<java.util.Set<hydra.core.Name>> vars = new hydra.util.Lazy<>(() -> hydra.lib.sets.Unions.apply(java.util.List.of(
              hydra.rewriting.Rewriting.freeVariablesInType(rdom),
              hydra.rewriting.Rewriting.freeVariablesInType(icod),
              hydra.inference.Inference.freeVariablesInContext(hydra.substitution.Substitution.substInContext(
                isubst,
                cx2.get())))));
            return hydra.lib.flows.Pure.apply(new hydra.typing.InferenceResult(rterm, rtype, isubst, iconstraints));
          }));
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfLetNormalized(hydra.typing.InferenceContext cx0, hydra.core.Let letTerm) {
    java.util.List<hydra.core.Binding> bins0 = (letTerm).bindings;
    hydra.util.Lazy<java.util.List<hydra.core.Name>> bnames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      projected -> projected.name,
      bins0));
    hydra.core.Term body0 = (letTerm).body;
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>freshNames(hydra.lib.lists.Length.apply(bins0)),
      (java.util.function.Function<java.util.List<hydra.core.Name>, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (bvars -> {
        hydra.util.Lazy<java.util.List<hydra.core.Type>> tbins0 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
          bvars));
        hydra.util.Lazy<hydra.typing.InferenceContext> cx1 = new hydra.util.Lazy<>(() -> hydra.inference.Inference.extendContext(
          hydra.lib.lists.Zip.apply(
            bnames.get(),
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>) (t -> new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()), t, (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))),
              tbins0.get())),
          cx0));
        return hydra.lib.flows.Bind.apply(
          hydra.inference.Inference.<T0>inferTypesOfTemporaryBindings(
            cx1.get(),
            bins0),
          (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (inferredResult -> {
            hydra.util.Lazy<java.util.List<hydra.core.Term>> bterms1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(inferredResult));
            hydra.util.Lazy<hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>> substAndConstraints = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(inferredResult)));
            hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> inferredConstraints = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(substAndConstraints.get()));
            hydra.util.Lazy<hydra.typing.TypeSubst> s1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(substAndConstraints.get()));
            hydra.util.Lazy<java.util.List<hydra.core.Type>> tbins1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(inferredResult)));
            return hydra.lib.flows.Bind.apply(
              hydra.unification.Unification.unifyTypeLists(
                (cx0).schemaTypes,
                hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (v1 -> hydra.substitution.Substitution.substInType(
                    s1.get(),
                    v1)),
                  tbins0.get()),
                tbins1.get(),
                "temporary type bindings"),
              (java.util.function.Function<hydra.typing.TypeSubst, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (s2 -> hydra.lib.flows.Bind.apply(
                hydra.checking.Checking.<T0>checkTypeSubst(
                  cx0,
                  s2),
                (java.util.function.Function<hydra.typing.TypeSubst, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (ignored -> {
                  java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> constraintsWithS2 = hydra.substitution.Substitution.substInClassConstraints(
                    s2,
                    inferredConstraints.get());
                  hydra.typing.TypeSubst composedSubst = hydra.substitution.Substitution.composeTypeSubst(
                    s1.get(),
                    s2);
                  hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> originalBindingConstraints = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                    (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, java.util.function.Function<hydra.core.Binding, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (acc -> (java.util.function.Function<hydra.core.Binding, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (b -> hydra.lib.maybes.Maybe.apply(
                      acc,
                      (java.util.function.Function<hydra.core.TypeScheme, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (ts -> hydra.lib.maybes.Maybe.apply(
                        acc,
                        (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (c -> hydra.inference.Inference.mergeClassConstraints(
                          acc,
                          c)),
                        (ts).constraints)),
                      (b).type))),
                    (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())),
                    bins0));
                  java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> originalConstraintsSubst = hydra.substitution.Substitution.substInClassConstraints(
                    composedSubst,
                    originalBindingConstraints.get());
                  hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> allInferredConstraints = new hydra.util.Lazy<>(() -> hydra.inference.Inference.mergeClassConstraints(
                    constraintsWithS2,
                    originalConstraintsSubst));
                  hydra.util.Lazy<java.util.List<hydra.core.Term>> bterms1Subst = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v1 -> hydra.substitution.Substitution.substTypesInTerm(
                      s2,
                      v1)),
                    bterms1.get()));
                  hydra.typing.InferenceContext g2base = hydra.substitution.Substitution.substInContext(
                    hydra.substitution.Substitution.composeTypeSubst(
                      s1.get(),
                      s2),
                    cx0);
                  hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> mergedConstraints = new hydra.util.Lazy<>(() -> hydra.inference.Inference.mergeClassConstraints(
                    (g2base).classConstraints,
                    allInferredConstraints.get()));
                  hydra.typing.InferenceContext g2 = new hydra.typing.InferenceContext((g2base).schemaTypes, (g2base).primitiveTypes, (g2base).dataTypes, mergedConstraints.get(), (g2base).debug);
                  hydra.util.Lazy<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>>> tsbins1 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Zip.apply(
                    bnames.get(),
                    hydra.lib.lists.Map.apply(
                      (java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>) (t -> hydra.inference.Inference.generalize(
                        g2,
                        hydra.substitution.Substitution.substInType(
                          s2,
                          t))),
                      tbins1.get())));
                  return hydra.lib.flows.Bind.apply(
                    hydra.inference.Inference.<T0>inferTypeOfTerm(
                      hydra.inference.Inference.extendContext(
                        tsbins1.get(),
                        g2),
                      body0,
                      "let body"),
                    (java.util.function.Function<hydra.typing.InferenceResult, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (bodyResult -> {
                      hydra.typing.TypeSubst sbody = (bodyResult).subst;
                      java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> bindingConstraintsSubst = hydra.substitution.Substitution.substInClassConstraints(
                        sbody,
                        constraintsWithS2);
                      java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> bodyConstraints = hydra.substitution.Substitution.substInClassConstraints(
                        sbody,
                        (bodyResult).classConstraints);
                      hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> allConstraints = new hydra.util.Lazy<>(() -> hydra.inference.Inference.mergeClassConstraints(
                        bindingConstraintsSubst,
                        bodyConstraints));
                      hydra.util.Lazy<hydra.typing.TermSubst> st1 = new hydra.util.Lazy<>(() -> new hydra.typing.TermSubst(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
                        (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>) (pair -> {
                          hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
                          hydra.util.Lazy<hydra.core.TypeScheme> ts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
                          return (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>(name.get(), hydra.inference.Inference.buildTypeApplicationTerm(
                            (ts.get()).variables,
                            new hydra.core.Term.Variable(name.get())))));
                        }),
                        tsbins1.get()))));
                      java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>, hydra.core.Term>, hydra.core.Binding> createBinding = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>, hydra.core.Term>, hydra.core.Binding>) (bindingPair -> {
                        hydra.util.Lazy<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.TypeScheme>> nameTsPair = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindingPair));
                        hydra.util.Lazy<hydra.core.TypeScheme> ts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nameTsPair.get()));
                        hydra.core.TypeScheme finalTs = hydra.substitution.Substitution.substInTypeScheme(
                          sbody,
                          ts.get());
                        hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(nameTsPair.get()));
                        hydra.util.Lazy<hydra.core.Term> term = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bindingPair));
                        hydra.util.Lazy<hydra.core.Term> typeLambdaTerm = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                          (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, hydra.core.Term>>) (b -> (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (v -> new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(v, b)))),
                          hydra.substitution.Substitution.substituteInTerm(
                            st1.get(),
                            term.get()),
                          hydra.lib.lists.Reverse.apply((finalTs).variables)));
                        return new hydra.core.Binding(name.get(), hydra.substitution.Substitution.substTypesInTerm(
                          hydra.substitution.Substitution.composeTypeSubst(
                            sbody,
                            s2),
                          typeLambdaTerm.get()), hydra.util.Maybe.just(finalTs));
                      });
                      hydra.util.Lazy<java.util.List<hydra.core.Binding>> bins1 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                        createBinding,
                        hydra.lib.lists.Zip.apply(
                          tsbins1.get(),
                          bterms1Subst.get())));
                      hydra.core.Term body1 = (bodyResult).term;
                      hydra.core.Type tbody = (bodyResult).type;
                      hydra.typing.InferenceResult ret = new hydra.typing.InferenceResult(new hydra.core.Term.Let(new hydra.core.Let(bins1.get(), body1)), tbody, hydra.substitution.Substitution.composeTypeSubstList(java.util.List.of(
                        s1.get(),
                        s2,
                        sbody)), allConstraints.get());
                      return hydra.lib.flows.Pure.apply(ret);
                    }));
                }))));
          }));
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfLet(hydra.typing.InferenceContext cx, hydra.core.Let let0) {
    java.util.List<hydra.core.Binding> bindings0 = (let0).bindings;
    hydra.util.Lazy<java.util.List<hydra.core.Name>> names = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      projected -> projected.name,
      bindings0));
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> nameSet = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(names.get()));
    java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>> toPair = (java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>>) (binding -> {
      hydra.core.Name name = (binding).name;
      hydra.core.Term term = (binding).term;
      return (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>(name, hydra.lib.lists.Filter.apply(
        (java.util.function.Function<hydra.core.Name, Boolean>) (n -> hydra.lib.sets.Member.apply(
          n,
          nameSet.get())),
        hydra.lib.sets.ToList.apply(hydra.rewriting.Rewriting.freeVariablesInTerm(term))))));
    });
    hydra.util.Lazy<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.List<hydra.core.Name>>>> adjList = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      toPair,
      bindings0));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Binding>> bindingMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
      names.get(),
      bindings0)));
    hydra.core.Term body0 = (let0).body;
    java.util.function.Function<hydra.core.Term, java.util.function.Function<java.util.List<hydra.core.Name>, hydra.core.Term>> createLet = (java.util.function.Function<hydra.core.Term, java.util.function.Function<java.util.List<hydra.core.Name>, hydra.core.Term>>) (e -> (java.util.function.Function<java.util.List<hydra.core.Name>, hydra.core.Term>) (group -> new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.core.Binding>>) (n -> hydra.lib.maps.Lookup.apply(
        n,
        bindingMap.get())),
      group)), e))));
    hydra.util.Lazy<java.util.List<java.util.List<hydra.core.Name>>> groups = new hydra.util.Lazy<>(() -> hydra.sorting.Sorting.topologicalSortComponents(adjList.get()));
    java.util.function.Function<hydra.core.Term, hydra.core.Term> restoreLet = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (iterm -> {
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Integer, java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term>>>>> helper = new java.util.concurrent.atomic.AtomicReference<>();
      helper.set((java.util.function.Function<Integer, java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term>>>>) (level -> (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term>>>) (bins -> (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term>>) (term -> {
        java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term>> nonzero = (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term>>) (term2 -> (term2).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term> visit(hydra.core.Term.Let l) {
            java.util.List<hydra.core.Binding> bs = ((l).value).bindings;
            hydra.core.Term letBody = ((l).value).body;
            return (((helper.get()).apply(hydra.lib.math.Sub.apply(
              level,
              1))).apply(hydra.lib.lists.Concat.apply(java.util.List.of(
              bs,
              bins)))).apply(letBody);
          }
        }));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            level,
            0),
          () -> (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term>(bins, term))),
          () -> (nonzero).apply(term));
      }))));
      hydra.util.Lazy<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Binding>, hydra.core.Term>> result = new hydra.util.Lazy<>(() -> (((helper.get()).apply(hydra.lib.lists.Length.apply(groups.get()))).apply((java.util.List<hydra.core.Binding>) (java.util.List.<hydra.core.Binding>of()))).apply(iterm));
      hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindingList = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result.get()));
      hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Binding>> bindingMap2 = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Binding, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Binding>>) (b -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Binding>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Binding>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Binding>((b).name, b)))),
        bindingList.get())));
      hydra.util.Lazy<hydra.core.Term> e = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result.get()));
      return new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.core.Binding>>) (n -> hydra.lib.maps.Lookup.apply(
          n,
          bindingMap2.get())),
        names.get())), e.get()));
    });
    java.util.function.Function<hydra.typing.InferenceResult, hydra.typing.InferenceResult> rewriteResult = (java.util.function.Function<hydra.typing.InferenceResult, hydra.typing.InferenceResult>) (result -> {
      java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> iconstraints = (result).classConstraints;
      hydra.typing.TypeSubst isubst = (result).subst;
      hydra.core.Term iterm = (result).term;
      hydra.core.Type itype = (result).type;
      return new hydra.typing.InferenceResult((restoreLet).apply(iterm), itype, isubst, iconstraints);
    });
    hydra.util.Lazy<hydra.core.Term> rewrittenLet = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      createLet,
      body0,
      hydra.lib.lists.Reverse.apply(groups.get())));
    return hydra.lib.flows.Map.apply(
      rewriteResult,
      hydra.inference.Inference.<T0>inferTypeOfLet_res(
        cx,
        rewrittenLet.get()));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfLet_res(hydra.typing.InferenceContext cx, hydra.core.Term rewrittenLet) {
    return (rewrittenLet).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> otherwise(hydra.core.Term instance) {
        return hydra.inference.Inference.<T0>inferTypeOfTerm(
          cx,
          rewrittenLet,
          "empty let term");
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.Let l) {
        return hydra.inference.Inference.<T0>inferTypeOfLetNormalized(
          cx,
          (l).value);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfList(hydra.typing.InferenceContext cx, java.util.List<hydra.core.Term> v1) {
    return hydra.inference.Inference.<T0>inferTypeOfCollection(
      cx,
      (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> new hydra.core.Type.List(x)),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (x -> new hydra.core.Term.List(x)),
      "list element",
      v1);
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.typing.InferenceResult> inferTypeOfLiteral(T0 ignored, hydra.core.Literal lit) {
    return hydra.lib.flows.Pure.apply(new hydra.typing.InferenceResult(new hydra.core.Term.Literal(lit), new hydra.core.Type.Literal(hydra.reflect.Reflect.literalType(lit)), hydra.substitution.Substitution.idTypeSubst(), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply()))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfMap(hydra.typing.InferenceContext cx, java.util.Map<hydra.core.Term, hydra.core.Term> m) {
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>freshName(),
      (java.util.function.Function<hydra.core.Name, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (kvar -> hydra.lib.flows.Bind.apply(
        hydra.schemas.Schemas.<T0>freshName(),
        (java.util.function.Function<hydra.core.Name, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (vvar -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.maps.Null.apply(m),
          () -> hydra.lib.flows.Pure.apply(hydra.inference.Inference.yield(
            hydra.inference.Inference.buildTypeApplicationTerm(
              java.util.List.of(
                kvar,
                vvar),
              new hydra.core.Term.Map((java.util.Map<hydra.core.Term, hydra.core.Term>) ((java.util.Map<hydra.core.Term, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Term, hydra.core.Term>apply())))),
            new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(kvar), new hydra.core.Type.Variable(vvar))),
            hydra.substitution.Substitution.idTypeSubst())),
          () -> hydra.lib.flows.Bind.apply(
            hydra.inference.Inference.<T0>inferMany(
              cx,
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.core.Term, String>>) (k -> (hydra.util.Tuple.Tuple2<hydra.core.Term, String>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, String>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, String>(k, "map key")))),
                hydra.lib.maps.Keys.apply(m))),
            (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (kresults -> {
              hydra.util.Lazy<hydra.typing.TypeSubst> ksubst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(kresults)));
              hydra.util.Lazy<java.util.List<hydra.core.Term>> kterms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(kresults));
              hydra.util.Lazy<java.util.List<hydra.core.Type>> ktypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(kresults)));
              return hydra.lib.flows.Bind.apply(
                hydra.inference.Inference.<T0>inferMany(
                  cx,
                  hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.core.Term, hydra.util.Tuple.Tuple2<hydra.core.Term, String>>) (v -> (hydra.util.Tuple.Tuple2<hydra.core.Term, String>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, String>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, String>(v, "map value")))),
                    hydra.lib.maps.Elems.apply(m))),
                (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (vresults -> {
                  hydra.util.Lazy<java.util.List<hydra.typing.TypeConstraint>> kcons = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>) (t -> new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(kvar), t, "map key")),
                    ktypes.get()));
                  hydra.util.Lazy<java.util.List<hydra.core.Type>> vtypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(vresults)));
                  hydra.util.Lazy<java.util.List<hydra.typing.TypeConstraint>> vcons = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>) (t -> new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(vvar), t, "map value")),
                    vtypes.get()));
                  hydra.util.Lazy<hydra.typing.TypeSubst> vsubst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(vresults)));
                  hydra.util.Lazy<java.util.List<hydra.core.Term>> vterms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(vresults));
                  return hydra.inference.Inference.mapConstraints(
                    cx,
                    (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst -> hydra.inference.Inference.yield(
                      new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
                        kterms.get(),
                        vterms.get()))),
                      new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(kvar), new hydra.core.Type.Variable(vvar))),
                      hydra.substitution.Substitution.composeTypeSubstList(java.util.List.of(
                        ksubst.get(),
                        vsubst.get(),
                        subst)))),
                    hydra.lib.lists.Concat.apply(java.util.List.of(
                      kcons.get(),
                      vcons.get())));
                }));
            })))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfOptional(hydra.typing.InferenceContext cx, hydra.util.Maybe<hydra.core.Term> m) {
    java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term> trmCons = (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (terms -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(terms),
      () -> new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())),
      () -> new hydra.core.Term.Maybe(hydra.util.Maybe.just(hydra.lib.lists.Head.apply(terms)))));
    return hydra.inference.Inference.<T0>inferTypeOfCollection(
      cx,
      (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> new hydra.core.Type.Maybe(x)),
      trmCons,
      "optional element",
      hydra.lib.maybes.Maybe.apply(
        (java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()),
        (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (hydra.lib.lists.Singleton::apply),
        m));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfPair(hydra.typing.InferenceContext cx, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term> p) {
    return hydra.lib.flows.Map.apply(
      (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.typing.InferenceResult>) (results -> {
        hydra.util.Lazy<java.util.List<hydra.core.Term>> iterms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(results));
        hydra.util.Lazy<hydra.core.Term> ifst = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(iterms.get()));
        hydra.util.Lazy<hydra.core.Term> isnd = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(hydra.lib.lists.Tail.apply(iterms.get())));
        hydra.util.Lazy<hydra.typing.TypeSubst> isubst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(results)));
        hydra.util.Lazy<java.util.List<hydra.core.Type>> itypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(results)));
        hydra.util.Lazy<hydra.core.Term> pairTerm = new hydra.util.Lazy<>(() -> new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(ifst.get(), isnd.get())))));
        hydra.util.Lazy<hydra.core.Type> tyFst = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(itypes.get()));
        hydra.util.Lazy<hydra.core.Type> tySnd = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(hydra.lib.lists.Tail.apply(itypes.get())));
        hydra.core.Term termWithTypes = new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(pairTerm.get(), tyFst.get())), tySnd.get()));
        return hydra.inference.Inference.yield(
          termWithTypes,
          new hydra.core.Type.Pair(new hydra.core.PairType(tyFst.get(), tySnd.get())),
          isubst.get());
      }),
      hydra.inference.Inference.<T0>inferMany(
        cx,
        java.util.List.of(
          (hydra.util.Tuple.Tuple2<hydra.core.Term, String>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, String>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, String>(hydra.lib.pairs.First.apply(p), "pair first element"))),
          (hydra.util.Tuple.Tuple2<hydra.core.Term, String>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, String>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, String>(hydra.lib.pairs.Second.apply(p), "pair second element"))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfPrimitive(hydra.typing.InferenceContext cx, hydra.core.Name name) {
    return hydra.lib.maybes.Maybe.apply(
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
        "No such primitive: ",
        (name).value)),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (scheme -> hydra.lib.flows.Bind.apply(
        hydra.schemas.Schemas.<T0>instantiateTypeScheme(scheme),
        (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (ts -> {
          hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> constraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.apply(
            (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())),
            (ts).constraints));
          return hydra.inference.Inference.<T0>yieldCheckedWithConstraints(
            hydra.inference.Inference.buildTypeApplicationTerm(
              (ts).variables,
              new hydra.core.Term.Function(new hydra.core.Function.Primitive(name))),
            (ts).type,
            hydra.substitution.Substitution.idTypeSubst(),
            constraints.get());
        }))),
      hydra.lib.maps.Lookup.apply(
        name,
        (cx).primitiveTypes));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfProjection(hydra.typing.InferenceContext cx, hydra.core.Projection proj) {
    hydra.core.Name fname = (proj).field;
    hydra.core.Name tname = (proj).typeName;
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>requireSchemaType(
        cx,
        tname),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (schemaType -> {
        hydra.core.Type stype = (schemaType).type;
        java.util.List<hydra.core.Name> svars = (schemaType).variables;
        return hydra.lib.flows.Bind.apply(
          hydra.extract.core.Core.<T0>recordType(
            tname,
            stype),
          (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (sfields -> hydra.lib.flows.Bind.apply(
            hydra.schemas.Schemas.<T0>findFieldType(
              fname,
              sfields),
            (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (ftyp -> hydra.lib.flows.Pure.apply(hydra.inference.Inference.yield(
              hydra.inference.Inference.buildTypeApplicationTerm(
                svars,
                new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(tname, fname))))),
              new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.schemas.Schemas.nominalApplication(
                tname,
                hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
                  svars)), ftyp)),
              hydra.substitution.Substitution.idTypeSubst()))))));
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfRecord(hydra.typing.InferenceContext cx, hydra.core.Record record) {
    java.util.List<hydra.core.Field> fields = (record).fields;
    hydra.util.Lazy<java.util.List<hydra.core.Name>> fnames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      projected -> projected.name,
      fields));
    hydra.core.Name tname = (record).typeName;
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>requireSchemaType(
        cx,
        tname),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (schemaType -> hydra.lib.flows.Bind.apply(
        hydra.inference.Inference.<T0>inferMany(
          cx,
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<hydra.core.Term, String>>) (f -> (hydra.util.Tuple.Tuple2<hydra.core.Term, String>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, String>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, String>((f).term, hydra.lib.strings.Cat2.apply(
              "field ",
              ((f).name).value))))),
            fields)),
        (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (results -> {
          hydra.util.Lazy<hydra.typing.TypeSubst> isubst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(results)));
          hydra.util.Lazy<java.util.List<hydra.core.Term>> iterms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(results));
          hydra.util.Lazy<java.util.List<hydra.core.Type>> itypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(results)));
          hydra.util.Lazy<hydra.core.Type> ityp = new hydra.util.Lazy<>(() -> new hydra.core.Type.Record(new hydra.core.RowType(tname, hydra.lib.lists.ZipWith.apply(
            (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.core.FieldType>>) (n -> (java.util.function.Function<hydra.core.Type, hydra.core.FieldType>) (t -> new hydra.core.FieldType(n, t))),
            fnames.get(),
            itypes.get()))));
          hydra.core.Type stype = (schemaType).type;
          java.util.List<hydra.core.Name> svars = (schemaType).variables;
          return hydra.inference.Inference.mapConstraints(
            cx,
            (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst -> hydra.inference.Inference.yield(
              hydra.inference.Inference.buildTypeApplicationTerm(
                svars,
                new hydra.core.Term.Record(new hydra.core.Record(tname, hydra.lib.lists.ZipWith.apply(
                  (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.core.Field>>) (n -> (java.util.function.Function<hydra.core.Term, hydra.core.Field>) (t -> new hydra.core.Field(n, t))),
                  fnames.get(),
                  iterms.get())))),
              hydra.schemas.Schemas.nominalApplication(
                tname,
                hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
                  svars)),
              hydra.substitution.Substitution.composeTypeSubst(
                isubst.get(),
                subst))),
            java.util.List.of(new hydra.typing.TypeConstraint(stype, ityp.get(), "schema type of record")));
        }))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfSet(hydra.typing.InferenceContext cx, java.util.Set<hydra.core.Term> s) {
    return hydra.inference.Inference.<T0>inferTypeOfCollection(
      cx,
      (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> new hydra.core.Type.Set(x)),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (terms -> new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(terms))),
      "set element",
      hydra.lib.sets.ToList.apply(s));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfTerm(hydra.typing.InferenceContext cx, hydra.core.Term term, String desc) {
    return hydra.monads.Monads.withTrace(
      desc,
      hydra.inference.Inference.<T0>inferTypeOfTerm_matchTerm(
        cx,
        hydra.inference.Inference.inferTypeOfUnit(),
        term));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfTerm_matchTerm(hydra.typing.InferenceContext cx, hydra.typing.InferenceResult hydra_inference_inferTypeOfUnit2, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.Annotated a) {
        return hydra.inference.Inference.<T0>inferTypeOfAnnotatedTerm(
          cx,
          (a).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.Application a) {
        return hydra.inference.Inference.<T0>inferTypeOfApplication(
          cx,
          (a).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.Either e) {
        return hydra.inference.Inference.<T0>inferTypeOfEither(
          cx,
          (e).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.Function f) {
        return hydra.inference.Inference.<T0>inferTypeOfFunction(
          cx,
          (f).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.Let l) {
        return hydra.inference.Inference.<T0>inferTypeOfLet(
          cx,
          (l).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.List els) {
        return hydra.inference.Inference.<T0>inferTypeOfList(
          cx,
          (els).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.Literal l) {
        return hydra.inference.Inference.inferTypeOfLiteral(
          cx,
          (l).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.Map m) {
        return hydra.inference.Inference.<T0>inferTypeOfMap(
          cx,
          (m).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.Maybe m) {
        return hydra.inference.Inference.<T0>inferTypeOfOptional(
          cx,
          (m).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.Pair p) {
        return hydra.inference.Inference.<T0>inferTypeOfPair(
          cx,
          (p).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.Record r) {
        return hydra.inference.Inference.<T0>inferTypeOfRecord(
          cx,
          (r).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.Set s) {
        return hydra.inference.Inference.<T0>inferTypeOfSet(
          cx,
          (s).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.TypeApplication tt) {
        return hydra.inference.Inference.<T0>inferTypeOfTypeApplication(
          cx,
          (tt).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.TypeLambda ta) {
        return hydra.inference.Inference.<T0>inferTypeOfTypeLambda(
          cx,
          (ta).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.Union i) {
        return hydra.inference.Inference.<T0>inferTypeOfInjection(
          cx,
          (i).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.Unit ignored) {
        return hydra.lib.flows.Pure.apply(hydra_inference_inferTypeOfUnit2);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.Variable name) {
        return hydra.inference.Inference.<T0>inferTypeOfVariable(
          cx,
          (name).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.typing.InferenceResult> visit(hydra.core.Term.Wrap w) {
        return hydra.inference.Inference.<T0>inferTypeOfWrappedTerm(
          cx,
          (w).value);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfTypeLambda(hydra.typing.InferenceContext cx, hydra.core.TypeLambda ta) {
    return hydra.inference.Inference.<T0>inferTypeOfTerm(
      cx,
      (ta).body,
      "type abstraction");
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfTypeApplication(hydra.typing.InferenceContext cx, hydra.core.TypeApplicationTerm tt) {
    return hydra.inference.Inference.<T0>inferTypeOfTerm(
      cx,
      (tt).body,
      "type application term");
  }
  
  static hydra.typing.InferenceResult inferTypeOfUnit() {
    return new hydra.typing.InferenceResult(new hydra.core.Term.Unit(), new hydra.core.Type.Unit(), hydra.substitution.Substitution.idTypeSubst(), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfUnwrap(hydra.typing.InferenceContext cx, hydra.core.Name tname) {
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>requireSchemaType(
        cx,
        tname),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (schemaType -> {
        hydra.core.Type stype = (schemaType).type;
        java.util.List<hydra.core.Name> svars = (schemaType).variables;
        return hydra.lib.flows.Bind.apply(
          hydra.extract.core.Core.<T0>wrappedType(
            tname,
            stype),
          (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (wtyp -> hydra.lib.flows.Pure.apply(hydra.inference.Inference.yield(
            hydra.inference.Inference.buildTypeApplicationTerm(
              svars,
              new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(tname)))),
            new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.schemas.Schemas.nominalApplication(
              tname,
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
                svars)), wtyp)),
            hydra.substitution.Substitution.idTypeSubst()))));
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfVariable(hydra.typing.InferenceContext cx, hydra.core.Name name) {
    return hydra.lib.maybes.Maybe.apply(
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
        "Variable not bound to type: ",
        (name).value)),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (scheme -> hydra.lib.flows.Bind.apply(
        hydra.schemas.Schemas.<T0>instantiateTypeScheme(scheme),
        (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (ts -> {
          hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> constraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.apply(
            (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())),
            (ts).constraints));
          return hydra.lib.flows.Pure.apply(new hydra.typing.InferenceResult(hydra.inference.Inference.buildTypeApplicationTerm(
            (ts).variables,
            new hydra.core.Term.Variable(name)), (ts).type, hydra.substitution.Substitution.idTypeSubst(), constraints.get()));
        }))),
      hydra.lib.maps.Lookup.apply(
        name,
        (cx).dataTypes));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> inferTypeOfWrappedTerm(hydra.typing.InferenceContext cx, hydra.core.WrappedTerm wt) {
    hydra.core.Term term = (wt).body;
    hydra.core.Name tname = (wt).typeName;
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>requireSchemaType(
        cx,
        tname),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (schemaType -> hydra.lib.flows.Bind.apply(
        hydra.inference.Inference.<T0>inferTypeOfTerm(
          cx,
          term,
          "wrapped term"),
        (java.util.function.Function<hydra.typing.InferenceResult, hydra.compute.Flow<T0, hydra.typing.InferenceResult>>) (result -> {
          hydra.typing.TypeSubst isubst = (result).subst;
          hydra.core.Term iterm = (result).term;
          hydra.core.Type itype = (result).type;
          hydra.core.Type ityp = new hydra.core.Type.Wrap(new hydra.core.WrappedType(tname, itype));
          hydra.core.Type stype = (schemaType).type;
          java.util.List<hydra.core.Name> svars = (schemaType).variables;
          return hydra.inference.Inference.mapConstraints(
            cx,
            (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst -> hydra.inference.Inference.yield(
              hydra.inference.Inference.buildTypeApplicationTerm(
                svars,
                new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(tname, iterm))),
              hydra.schemas.Schemas.nominalApplication(
                tname,
                hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
                  svars)),
              hydra.substitution.Substitution.composeTypeSubst(
                isubst,
                subst))),
            java.util.List.of(new hydra.typing.TypeConstraint(stype, ityp, "schema type of wrapper")));
        }))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>> inferTypesOfTemporaryBindings(hydra.typing.InferenceContext cx, java.util.List<hydra.core.Binding> bins) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(bins),
      () -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()), (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()), (hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) ((hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (new hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>(hydra.substitution.Substitution.idTypeSubst(), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())))))))))))),
      () -> hydra.inference.Inference.<T0>inferTypesOfTemporaryBindings_dflt(
        bins,
        cx,
        (java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>>) (p0 -> p1 -> hydra.substitution.Substitution.composeTypeSubst(
          p0,
          p1)),
        (java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (p0 -> p1 -> hydra.substitution.Substitution.substInClassConstraints(
          p0,
          p1)),
        (java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.typing.InferenceContext, hydra.typing.InferenceContext>>) (p0 -> p1 -> hydra.substitution.Substitution.substInContext(
          p0,
          p1)),
        (java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (p0 -> p1 -> hydra.substitution.Substitution.substInType(
          p0,
          p1)),
        (java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (p0 -> p1 -> hydra.substitution.Substitution.substTypesInTerm(
          p0,
          p1))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>> inferTypesOfTemporaryBindings_dflt(java.util.List<hydra.core.Binding> bins, hydra.typing.InferenceContext cx, java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>> hydra_substitution_composeTypeSubst2, java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>> hydra_substitution_substInClassConstraints2, java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.typing.InferenceContext, hydra.typing.InferenceContext>> hydra_substitution_substInContext2, java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.core.Type, hydra.core.Type>> hydra_substitution_substInType2, java.util.function.Function<hydra.typing.TypeSubst, java.util.function.Function<hydra.core.Term, hydra.core.Term>> hydra_substitution_substTypesInTerm2) {
    hydra.util.Lazy<hydra.core.Binding> binding = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(bins));
    hydra.core.Name k = (binding.get()).name;
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> tl = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(bins));
    hydra.core.Term v = (binding.get()).term;
    return hydra.lib.flows.Bind.apply(
      hydra.inference.Inference.<T0>inferTypeOfTerm(
        cx,
        v,
        hydra.lib.strings.Cat.apply(java.util.List.of(
          "temporary let binding '",
          (k).value,
          "'"))),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>>>) (result1 -> {
        java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c1Inferred = (result1).classConstraints;
        hydra.core.Term j = (result1).term;
        hydra.typing.TypeSubst u = (result1).subst;
        hydra.core.Type u_prime = (result1).type;
        return hydra.lib.flows.Bind.apply(
          hydra.lib.maybes.Maybe.apply(
            hydra.lib.flows.Pure.apply((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply()))),
            (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (ts -> hydra.lib.flows.Bind.apply(
              hydra.schemas.Schemas.<T0>instantiateTypeScheme(ts),
              (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T0, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (instantiatedTs -> {
                hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> freshConstraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.apply(
                  (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())),
                  (instantiatedTs).constraints));
                return hydra.lib.flows.Bind.apply(
                  hydra.unification.Unification.unifyTypes(
                    (cx).schemaTypes,
                    (instantiatedTs).type,
                    u_prime,
                    "original binding type"),
                  (java.util.function.Function<hydra.typing.TypeSubst, hydra.compute.Flow<T0, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (unifySubst -> hydra.lib.flows.Pure.apply(((hydra_substitution_substInClassConstraints2).apply(unifySubst)).apply(freshConstraints.get()))));
              }))),
            (binding.get()).type),
          (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>>>) (originalBindingConstraints -> {
            hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> c1 = new hydra.util.Lazy<>(() -> hydra.inference.Inference.mergeClassConstraints(
              c1Inferred,
              originalBindingConstraints));
            return hydra.lib.flows.Bind.apply(
              hydra.inference.Inference.<T0>inferTypesOfTemporaryBindings(
                ((hydra_substitution_substInContext2).apply(u)).apply(cx),
                tl.get()),
              (java.util.function.Function<hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>>>) (result2 -> {
                hydra.util.Lazy<hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>> restPair = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(result2)));
                hydra.util.Lazy<hydra.typing.TypeSubst> r = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(restPair.get()));
                java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c1Subst = ((hydra_substitution_substInClassConstraints2).apply(r.get())).apply(c1.get());
                hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> c2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(restPair.get()));
                hydra.util.Lazy<java.util.List<hydra.core.Term>> h = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result2));
                hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> mergedConstraints = new hydra.util.Lazy<>(() -> hydra.inference.Inference.mergeClassConstraints(
                  c1Subst,
                  c2.get()));
                hydra.util.Lazy<java.util.List<hydra.core.Type>> r_prime = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(result2)));
                return hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Term>, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>(hydra.lib.lists.Cons.apply(
                  ((hydra_substitution_substTypesInTerm2).apply(r.get())).apply(j),
                  h.get()), (hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Type>, hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>(hydra.lib.lists.Cons.apply(
                  ((hydra_substitution_substInType2).apply(r.get())).apply(u_prime),
                  r_prime.get()), (hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) ((hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (new hydra.util.Tuple.Tuple2<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>(((hydra_substitution_composeTypeSubst2).apply(u)).apply(r.get()), mergedConstraints.get()))))))))));
              }));
          }));
      }));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.TypeContext> initialTypeContext(hydra.graph.Graph g) {
    return hydra.lib.flows.Bind.apply(
      hydra.schemas.Schemas.<T0>graphToInferenceContext(g),
      (java.util.function.Function<hydra.typing.InferenceContext, hydra.compute.Flow<T0, hydra.typing.TypeContext>>) (ix -> hydra.lib.flows.Bind.apply(
        hydra.lib.flows.Map.apply(
          (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>, java.util.Map<hydra.core.Name, hydra.core.Type>>) ((java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>, java.util.Map<hydra.core.Name, hydra.core.Type>>) (hydra.lib.maps.FromList::apply)),
          hydra.lib.flows.MapList.apply(
            (java.util.function.Function<hydra.core.Binding, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>>) (v1 -> hydra.inference.Inference.initialTypeContext_toPair(
              hydra.schemas.Schemas::typeSchemeToFType,
              v1)),
            (g).elements)),
        (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, hydra.compute.Flow<T0, hydra.typing.TypeContext>>) (types -> hydra.lib.flows.Pure.apply(new hydra.typing.TypeContext(types, (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), ix))))));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<hydra.core.Name, T0>> initialTypeContext_toPair(java.util.function.Function<hydra.core.TypeScheme, T0> hydra_schemas_typeSchemeToFType2, hydra.core.Binding el) {
    hydra.core.Name name = (el).name;
    return hydra.lib.maybes.Maybe.apply(
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
        "untyped element: ",
        (name).value)),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<hydra.core.Name, T0>>>) (ts -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.core.Name, T0>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, T0>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, T0>(name, (hydra_schemas_typeSchemeToFType2).apply(ts)))))),
      (el).type);
  }
  
  static Boolean isUnbound(hydra.typing.InferenceContext cx, hydra.core.Name v) {
    return hydra.lib.logic.And.apply(
      hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
        v,
        hydra.inference.Inference.freeVariablesInContext(cx))),
      hydra.lib.logic.Not.apply(hydra.lib.maps.Member.apply(
        v,
        (cx).schemaTypes)));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, T0> mapConstraints(hydra.typing.InferenceContext cx, java.util.function.Function<hydra.typing.TypeSubst, T0> f, java.util.List<hydra.typing.TypeConstraint> constraints) {
    return hydra.lib.flows.Bind.apply(
      hydra.unification.Unification.unifyTypeConstraints(
        (cx).schemaTypes,
        constraints),
      (java.util.function.Function<hydra.typing.TypeSubst, hydra.compute.Flow<T1, T0>>) (s -> hydra.lib.flows.Bind.apply(
        hydra.checking.Checking.<T1>checkTypeSubst(
          cx,
          s),
        (java.util.function.Function<hydra.typing.TypeSubst, hydra.compute.Flow<T1, T0>>) (ignored -> hydra.lib.flows.Pure.apply((f).apply(s))))));
  }
  
  static <T0> java.util.Map<T0, hydra.core.TypeVariableMetadata> mergeClassConstraints(java.util.Map<T0, hydra.core.TypeVariableMetadata> m1, java.util.Map<T0, hydra.core.TypeVariableMetadata> m2) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Map<T0, hydra.core.TypeVariableMetadata>, java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.TypeVariableMetadata>, java.util.Map<T0, hydra.core.TypeVariableMetadata>>>) (acc -> (java.util.function.Function<hydra.util.Tuple.Tuple2<T0, hydra.core.TypeVariableMetadata>, java.util.Map<T0, hydra.core.TypeVariableMetadata>>) (pair -> {
        hydra.util.Lazy<T0> k = new hydra.util.Lazy<>(() -> hydra.inference.Inference.mergeClassConstraints_k(pair));
        hydra.util.Lazy<hydra.core.TypeVariableMetadata> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
        return hydra.lib.maybes.Maybe.apply(
          hydra.lib.maps.Insert.apply(
            k.get(),
            v.get(),
            acc),
          (java.util.function.Function<hydra.core.TypeVariableMetadata, java.util.Map<T0, hydra.core.TypeVariableMetadata>>) (existing -> {
            hydra.util.Lazy<hydra.core.TypeVariableMetadata> merged = new hydra.util.Lazy<>(() -> new hydra.core.TypeVariableMetadata(hydra.lib.sets.Union.apply(
              (existing).classes,
              (v.get()).classes)));
            return hydra.lib.maps.Insert.apply(
              k.get(),
              merged.get(),
              acc);
          }),
          hydra.lib.maps.Lookup.apply(
            k.get(),
            acc));
      })),
      m1,
      hydra.lib.maps.ToList.apply(m2));
  }
  
  static <T0, T1> T0 mergeClassConstraints_k(hydra.util.Tuple.Tuple2<T0, T1> pair) {
    return hydra.lib.pairs.First.apply(pair);
  }
  
  static String showInferenceResult(hydra.typing.InferenceResult result) {
    hydra.typing.TypeSubst subst = (result).subst;
    hydra.core.Term term = (result).term;
    hydra.core.Type typ = (result).type;
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      "{term=",
      hydra.show.core.Core.term(term),
      ", type=",
      hydra.show.core.Core.type(typ),
      ", subst=",
      hydra.show.typing.Typing.typeSubst(subst),
      "}"));
  }
  
  static hydra.typing.InferenceResult yield(hydra.core.Term term, hydra.core.Type typ, hydra.typing.TypeSubst subst) {
    return new hydra.typing.InferenceResult(hydra.substitution.Substitution.substTypesInTerm(
      subst,
      term), hydra.substitution.Substitution.substInType(
      subst,
      typ), subst, (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> yieldChecked(hydra.core.Term term, hydra.core.Type typ, hydra.typing.TypeSubst subst) {
    hydra.core.Term iterm = hydra.substitution.Substitution.substTypesInTerm(
      subst,
      term);
    hydra.core.Type itype = hydra.substitution.Substitution.substInType(
      subst,
      typ);
    return hydra.lib.flows.Pure.apply(new hydra.typing.InferenceResult(iterm, itype, subst, (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply()))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.typing.InferenceResult> yieldCheckedWithConstraints(hydra.core.Term term, hydra.core.Type typ, hydra.typing.TypeSubst subst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> constraints) {
    java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> iconstraints = hydra.substitution.Substitution.substInClassConstraints(
      subst,
      constraints);
    hydra.core.Term iterm = hydra.substitution.Substitution.substTypesInTerm(
      subst,
      term);
    hydra.core.Type itype = hydra.substitution.Substitution.substInType(
      subst,
      typ);
    return hydra.lib.flows.Pure.apply(new hydra.typing.InferenceResult(iterm, itype, subst, iconstraints));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T2, hydra.typing.InferenceResult> yieldDebug(T0 cx, T1 debugId, hydra.core.Term term, hydra.core.Type typ, hydra.typing.TypeSubst subst) {
    hydra.core.Term rterm = hydra.substitution.Substitution.substTypesInTerm(
      subst,
      term);
    hydra.core.Type rtyp = hydra.substitution.Substitution.substInType(
      subst,
      typ);
    return hydra.lib.flows.Bind.apply(
      hydra.annotations.Annotations.<T1, T2>debugIf(
        debugId,
        hydra.lib.strings.Cat.apply(java.util.List.of(
          "\n\tterm: ",
          hydra.show.core.Core.term(term),
          "\n\ttyp: ",
          hydra.show.core.Core.type(typ),
          "\n\tsubst: ",
          hydra.show.typing.Typing.typeSubst(subst),
          "\n\trterm: ",
          hydra.show.core.Core.term(rterm),
          "\n\trtyp: ",
          hydra.show.core.Core.type(rtyp)))),
      (java.util.function.Function<java.lang.Void, hydra.compute.Flow<T2, hydra.typing.InferenceResult>>) (result -> hydra.lib.flows.Pure.apply(new hydra.typing.InferenceResult(rterm, rtyp, subst, (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply()))))));
  }
  
  static hydra.typing.InferenceResult yieldWithConstraints(hydra.core.Term term, hydra.core.Type typ, hydra.typing.TypeSubst subst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> constraints) {
    return new hydra.typing.InferenceResult(hydra.substitution.Substitution.substTypesInTerm(
      subst,
      term), hydra.substitution.Substitution.substInType(
      subst,
      typ), subst, constraints);
  }
}
