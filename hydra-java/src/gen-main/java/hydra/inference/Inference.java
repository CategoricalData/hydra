// Note: this is an automatically generated file. Do not edit.

package hydra.inference;

/**
 * Type inference following Algorithm W, extended for nominal terms and types
 */
public interface Inference {
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.TypeSubst> bindConstraints(hydra.context.Context flowCx, hydra.graph.Graph cx, java.util.List<hydra.typing.TypeConstraint> constraints) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.error.UnificationError>, hydra.context.InContext<hydra.error.OtherError>>) (_ic -> (hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError((((java.util.function.Function<hydra.context.InContext<hydra.error.UnificationError>, hydra.error.UnificationError>) (projected -> projected.object)).apply(_ic)).message), ((java.util.function.Function<hydra.context.InContext<hydra.error.UnificationError>, hydra.context.Context>) (projected -> projected.context)).apply(_ic)))),
        (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>) (_a -> _a),
        hydra.unification.Unification.unifyTypeConstraints(
          flowCx,
          (cx).schemaTypes,
          constraints)),
      (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.TypeSubst>>) (s -> hydra.lib.eithers.Bind.apply(
        hydra.checking.Checking.checkTypeSubst(
          flowCx,
          cx,
          s),
        (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.TypeSubst>>) (ignored -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.TypeSubst>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.TypeSubst>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.TypeSubst>right(s)))))));
  }
  
  static hydra.core.Term bindUnboundTypeVariables(hydra.graph.Graph cx, hydra.core.Term term0) {
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
  
  static hydra.graph.Graph extendContext(java.util.List<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>> pairs, hydra.graph.Graph cx) {
    return new hydra.graph.Graph((cx).boundTerms, hydra.lib.maps.Union.apply(
      hydra.lib.maps.FromList.apply(pairs),
      (cx).boundTypes), (cx).classConstraints, (cx).lambdaVariables, (cx).metadata, (cx).primitives, (cx).schemaTypes, (cx).typeVariables);
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> finalizeInferredTerm(hydra.context.Context flowCx, hydra.graph.Graph cx, hydra.core.Term term) {
    hydra.core.Term term2 = hydra.inference.Inference.bindUnboundTypeVariables(
      cx,
      term);
    return hydra.lib.eithers.Bind.apply(
      hydra.checking.Checking.checkForUnboundTypeVariables(
        flowCx,
        cx,
        term2),
      (java.util.function.Function<java.lang.Void, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (ignored -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(hydra.rewriting.Rewriting.normalizeTypeVariablesInTerm(term2))))));
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<T0, hydra.context.Context>> forInferredTerm(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Term term, String desc, java.util.function.Function<hydra.typing.InferenceResult, T0> f) {
    return hydra.lib.eithers.Bind.apply(
      hydra.inference.Inference.inferTypeOfTerm(
        fcx,
        cx,
        term,
        desc),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<T0, hydra.context.Context>>>) (rp -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<T0, hydra.context.Context>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<T0, hydra.context.Context>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<T0, hydra.context.Context>>right((hydra.util.Pair<T0, hydra.context.Context>) ((hydra.util.Pair<T0, hydra.context.Context>) (new hydra.util.Pair<T0, hydra.context.Context>((f).apply(rp), (rp).context))))))));
  }
  
  static java.util.Set<hydra.core.Name> freeVariablesInContext(hydra.graph.Graph cx) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.Set<hydra.core.Name>>>) ((java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.Set<hydra.core.Name>>>) (p0 -> p1 -> hydra.lib.sets.Union.apply(
        p0,
        p1))),
      (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      hydra.lib.lists.Map.apply(
        hydra.rewriting.Rewriting::freeVariablesInTypeSchemeSimple,
        hydra.lib.maps.Elems.apply((cx).boundTypes)));
  }
  
  static hydra.util.Pair<hydra.core.Type, hydra.context.Context> freshVariableType(hydra.context.Context cx) {
    hydra.util.Pair<hydra.core.Name, hydra.context.Context> result = hydra.schemas.Schemas.freshName(cx);
    hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result));
    hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
    return (hydra.util.Pair<hydra.core.Type, hydra.context.Context>) ((hydra.util.Pair<hydra.core.Type, hydra.context.Context>) (new hydra.util.Pair<hydra.core.Type, hydra.context.Context>(new hydra.core.Type.Variable(name.get()), cx2.get())));
  }
  
  static hydra.core.TypeScheme generalize(hydra.graph.Graph cx, hydra.core.Type typ) {
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
      (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (v -> hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.core.TypeVariableMetadata, hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (meta -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeVariableMetadata>(v, meta)))),
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
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>> inferGraphTypes(hydra.context.Context fcx0, java.util.List<hydra.core.Binding> bindings0, hydra.graph.Graph g0) {
    hydra.util.Lazy<hydra.context.Context> fcx = new hydra.util.Lazy<>(() -> new hydra.context.Context(hydra.lib.lists.Cons.apply(
      "graph inference",
      (fcx0).trace), (fcx0).messages, (fcx0).other));
    java.util.function.Function<hydra.core.Let, hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>> fromLetTerm = (java.util.function.Function<hydra.core.Let, hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>>) (l -> {
      java.util.List<hydra.core.Binding> bindings = (l).bindings;
      java.util.Map<hydra.core.Name, hydra.graph.Primitive> prims = (g0).primitives;
      java.util.Map<hydra.core.Name, hydra.core.TypeScheme> schemaTypes = (g0).schemaTypes;
      hydra.util.Lazy<hydra.graph.Graph> g = new hydra.util.Lazy<>(() -> new hydra.graph.Graph((hydra.lexical.Lexical.buildGraph(
        bindings,
        (java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) ((java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>apply())),
        prims)).boundTerms, (hydra.lexical.Lexical.buildGraph(
        bindings,
        (java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) ((java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>apply())),
        prims)).boundTypes, (hydra.lexical.Lexical.buildGraph(
        bindings,
        (java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) ((java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>apply())),
        prims)).classConstraints, (hydra.lexical.Lexical.buildGraph(
        bindings,
        (java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) ((java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>apply())),
        prims)).lambdaVariables, (hydra.lexical.Lexical.buildGraph(
        bindings,
        (java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) ((java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>apply())),
        prims)).metadata, (hydra.lexical.Lexical.buildGraph(
        bindings,
        (java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) ((java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>apply())),
        prims)).primitives, schemaTypes, (hydra.lexical.Lexical.buildGraph(
        bindings,
        (java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) ((java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>apply())),
        prims)).typeVariables));
      return (hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>) ((hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>) (new hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>(g.get(), bindings)));
    });
    hydra.core.Let let0 = new hydra.core.Let(bindings0, new hydra.core.Term.Unit());
    return hydra.lib.eithers.Bind.apply(
      hydra.inference.Inference.inferTypeOfTerm(
        fcx.get(),
        g0,
        new hydra.core.Term.Let(let0),
        "graph term"),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>>>) (result -> {
        hydra.context.Context fcx2 = (result).context;
        hydra.core.Term term = (result).term;
        return hydra.lib.eithers.Bind.apply(
          hydra.inference.Inference.finalizeInferredTerm(
            fcx2,
            g0,
            term),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>>>) (finalized -> (finalized).accept(new hydra.core.Term.PartialVisitor<>() {
            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>> visit(hydra.core.Term.Let l) {
              return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>((fromLetTerm).apply((l).value), fcx2))))));
            }
            
            @Override
            public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>> visit(hydra.core.Term.Variable ignored) {
              return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError("Expected inferred graph as let term"), fcx2)))));
            }
          })));
      }));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferInGraphContext(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Term term) {
    return hydra.inference.Inference.inferTypeOfTerm(
      fcx,
      cx,
      term,
      "single term");
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>> inferMany(hydra.context.Context fcx, hydra.graph.Graph cx, java.util.List<hydra.util.Pair<hydra.core.Term, String>> pairs) {
    hydra.util.Lazy<hydra.core.Term> e = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.lists.Head.apply(pairs)));
    hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>>> dflt = new hydra.util.Lazy<>(() -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>>>) (() -> {
      hydra.util.Lazy<String> desc = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.lists.Head.apply(pairs)));
      return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>>>) (() -> {
        hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Term, String>>> tl = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(pairs));
        return hydra.lib.eithers.Bind.apply(
          hydra.inference.Inference.inferTypeOfTerm(
            fcx,
            cx,
            e.get(),
            desc.get()),
          (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>>>) (result1 -> {
            hydra.core.Term e1 = (result1).term;
            hydra.context.Context fcx2 = (result1).context;
            hydra.typing.TypeSubst s1 = (result1).subst;
            hydra.core.Type t1 = (result1).type;
            return hydra.lib.eithers.Bind.apply(
              hydra.inference.Inference.inferMany(
                fcx2,
                hydra.substitution.Substitution.substInContext(
                  s1,
                  cx),
                tl.get()),
              (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>>>) (rp2 -> {
                hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>> result2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(rp2));
                hydra.util.Lazy<java.util.List<hydra.core.Term>> e2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result2.get()));
                hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(rp2));
                hydra.util.Lazy<hydra.typing.TypeSubst> s2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(result2.get())));
                hydra.util.Lazy<java.util.List<hydra.core.Type>> t2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(result2.get())));
                return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>((hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>) ((hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>) (new hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>(hydra.lib.lists.Cons.apply(
                  hydra.substitution.Substitution.substTypesInTerm(
                    s2.get(),
                    e1),
                  e2.get()), (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>(hydra.lib.lists.Cons.apply(
                  hydra.substitution.Substitution.substInType(
                    s2.get(),
                    t1),
                  t2.get()), hydra.substitution.Substitution.composeTypeSubst(
                  s1,
                  s2.get()))))))), fcx3.get()))))));
              }));
          }));
      })).get();
    })).get());
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(pairs),
      () -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>((hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>) ((hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>) (new hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()), (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()), hydra.substitution.Substitution.idTypeSubst())))))), fcx)))))),
      () -> dflt.get());
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>> inferTypeOf(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Term term) {
    hydra.util.Lazy<hydra.core.Term> letTerm = new hydra.util.Lazy<>(() -> new hydra.core.Term.Let(new hydra.core.Let(java.util.List.of(new hydra.core.Binding(new hydra.core.Name("ignoredVariableName"), term, (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("ignoredBody")))));
    return hydra.lib.eithers.Bind.apply(
      hydra.inference.Inference.inferTypeOfTerm(
        fcx,
        cx,
        letTerm.get(),
        "infer type of term"),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>>) (result -> {
        hydra.context.Context fcx2 = (result).context;
        return hydra.lib.eithers.Bind.apply(
          hydra.inference.Inference.finalizeInferredTerm(
            fcx2,
            cx,
            (result).term),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>>) (finalized -> hydra.lib.eithers.Bind.apply(
            hydra.extract.core.Core.let(
              fcx2,
              cx,
              finalized),
            (java.util.function.Function<hydra.core.Let, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>>) (letResult -> {
              java.util.List<hydra.core.Binding> bindings = (letResult).bindings;
              return hydra.lib.logic.IfElse.lazy(
                hydra.lib.equality.Equal.apply(
                  1,
                  hydra.lib.lists.Length.apply(bindings)),
                () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>>) (() -> {
                  hydra.util.Lazy<hydra.core.Binding> binding = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(bindings));
                  return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>>) (() -> {
                    hydra.core.Term term1 = (binding.get()).term;
                    return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>>) (() -> {
                      hydra.util.Maybe<hydra.core.TypeScheme> mts = (binding.get()).type;
                      return hydra.lib.maybes.Maybe.apply(
                        (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError("Expected a type scheme"), fcx2))))),
                        (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>>) (ts -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>((hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>(term1, ts))), fcx2))))))),
                        mts);
                    })).get();
                  })).get();
                })).get(),
                () -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(java.util.List.of(
                  "Expected a single binding with a type scheme, but got: ",
                  hydra.lib.literals.ShowInt32.apply(hydra.lib.lists.Length.apply(bindings)),
                  " bindings"))), fcx2))))));
            }))));
      }));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfAnnotatedTerm(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.AnnotatedTerm at) {
    java.util.Map<hydra.core.Name, hydra.core.Term> ann = (at).annotation;
    hydra.core.Term term = (at).body;
    return hydra.lib.eithers.Bind.apply(
      hydra.inference.Inference.inferTypeOfTerm(
        fcx,
        cx,
        term,
        "annotated term"),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (result -> {
        hydra.context.Context fcx2 = (result).context;
        java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> iconstraints = (result).classConstraints;
        hydra.typing.TypeSubst isubst = (result).subst;
        hydra.core.Term iterm = (result).term;
        hydra.core.Type itype = (result).type;
        return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(new hydra.typing.InferenceResult(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(iterm, ann)), itype, isubst, iconstraints, fcx2))));
      }));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfApplication(hydra.context.Context fcx0, hydra.graph.Graph cx, hydra.core.Application app) {
    hydra.core.Term e0 = (app).function;
    hydra.core.Term e1 = (app).argument;
    hydra.util.Lazy<hydra.context.Context> fcx = new hydra.util.Lazy<>(() -> new hydra.context.Context(hydra.lib.lists.Cons.apply(
      "application",
      (fcx0).trace), (fcx0).messages, (fcx0).other));
    return hydra.lib.eithers.Bind.apply(
      hydra.inference.Inference.inferTypeOfTerm(
        fcx.get(),
        cx,
        e0,
        "lhs"),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (lhsResult -> {
        hydra.core.Term a = (lhsResult).term;
        java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c0 = (lhsResult).classConstraints;
        hydra.context.Context fcx2 = (lhsResult).context;
        hydra.typing.TypeSubst s0 = (lhsResult).subst;
        hydra.core.Type t0 = (lhsResult).type;
        return hydra.lib.eithers.Bind.apply(
          hydra.inference.Inference.inferTypeOfTerm(
            fcx2,
            hydra.substitution.Substitution.substInContext(
              s0,
              cx),
            e1,
            "rhs"),
          (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (rhsResult -> {
            hydra.core.Term b = (rhsResult).term;
            java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c1 = (rhsResult).classConstraints;
            hydra.context.Context fcx3 = (rhsResult).context;
            hydra.util.Pair<hydra.core.Name, hydra.context.Context> vResult = hydra.schemas.Schemas.freshName(fcx3);
            hydra.util.Lazy<hydra.context.Context> fcx4 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(vResult));
            hydra.typing.TypeSubst s1 = (rhsResult).subst;
            hydra.core.Type t1 = (rhsResult).type;
            hydra.util.Lazy<hydra.core.Name> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(vResult));
            return hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.Bimap.apply(
                (java.util.function.Function<hydra.context.InContext<hydra.error.UnificationError>, hydra.context.InContext<hydra.error.OtherError>>) (_ic -> (hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError((((java.util.function.Function<hydra.context.InContext<hydra.error.UnificationError>, hydra.error.UnificationError>) (projected -> projected.object)).apply(_ic)).message), ((java.util.function.Function<hydra.context.InContext<hydra.error.UnificationError>, hydra.context.Context>) (projected -> projected.context)).apply(_ic)))),
                (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>) (_a -> _a),
                hydra.unification.Unification.unifyTypes(
                  fcx4.get(),
                  (cx).schemaTypes,
                  hydra.substitution.Substitution.substInType(
                    s1,
                    t0),
                  new hydra.core.Type.Function(new hydra.core.FunctionType(t1, new hydra.core.Type.Variable(v.get()))),
                  "application lhs")),
              (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (s2 -> hydra.lib.eithers.Bind.apply(
                hydra.checking.Checking.checkTypeSubst(
                  fcx4.get(),
                  cx,
                  s2),
                (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (ignored -> {
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
                    new hydra.core.Type.Variable(v.get()));
                  return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(new hydra.typing.InferenceResult(rExpr, rType, rSubst, rConstraints.get(), fcx4.get()))));
                }))));
          }));
      }));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfCaseStatement(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.CaseStatement caseStmt) {
    java.util.List<hydra.core.Field> cases = (caseStmt).cases;
    hydra.util.Maybe<hydra.core.Term> dflt = (caseStmt).default_;
    hydra.util.Lazy<java.util.List<hydra.core.Name>> fnames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      projected -> projected.name,
      cases));
    hydra.core.Name tname = (caseStmt).typeName;
    return hydra.lib.eithers.Bind.apply(
      hydra.schemas.Schemas.requireSchemaType(
        fcx,
        (cx).schemaTypes,
        tname),
      (java.util.function.Function<hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (stRp -> {
        hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(stRp));
        hydra.util.Lazy<hydra.core.TypeScheme> schemaType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(stRp));
        hydra.core.Type stype = (schemaType.get()).type;
        java.util.List<hydra.core.Name> svars = (schemaType.get()).variables;
        return hydra.lib.eithers.Bind.apply(
          hydra.extract.core.Core.unionType(
            fcx2.get(),
            tname,
            stype),
          (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (sfields -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.MapMaybe.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (t -> hydra.inference.Inference.inferTypeOfTerm(
                fcx2.get(),
                cx,
                t,
                hydra.lib.strings.Cat.apply(java.util.List.of(
                  "case ",
                  (tname).value,
                  ".<default>")))),
              dflt),
            (java.util.function.Function<hydra.util.Maybe<hydra.typing.InferenceResult>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (dfltRp -> {
              hydra.util.Maybe<hydra.typing.InferenceResult> dfltResult = dfltRp;
              hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.apply(
                fcx2.get(),
                hydra.lib.maybes.Map.apply(
                  projected -> projected.context,
                  dfltRp)));
              return hydra.lib.eithers.Bind.apply(
                hydra.inference.Inference.inferMany(
                  fcx3.get(),
                  cx,
                  hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.core.Field, hydra.util.Pair<hydra.core.Term, String>>) (f -> (hydra.util.Pair<hydra.core.Term, String>) ((hydra.util.Pair<hydra.core.Term, String>) (new hydra.util.Pair<hydra.core.Term, String>((f).term, hydra.lib.strings.Cat.apply(java.util.List.of(
                      "case ",
                      (tname).value,
                      ".",
                      ((f).name).value)))))),
                    cases)),
                (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (caseRp -> {
                  hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Type>> caseMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>) (ft -> (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>((ft).name, (ft).type)))),
                    sfields)));
                  hydra.util.Lazy<hydra.context.Context> fcx4 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(caseRp));
                  hydra.util.Pair<hydra.core.Name, hydra.context.Context> codvResult = hydra.schemas.Schemas.freshName(fcx4.get());
                  hydra.util.Lazy<hydra.core.Name> codv = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(codvResult));
                  hydra.core.Type cod = new hydra.core.Type.Variable(codv.get());
                  hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>> caseResults = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(caseRp));
                  hydra.util.Lazy<java.util.List<hydra.core.Type>> itypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(caseResults.get())));
                  hydra.util.Lazy<java.util.List<hydra.typing.TypeConstraint>> caseConstraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.ZipWith.apply(
                    (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.typing.TypeConstraint>>>) (fname -> (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.typing.TypeConstraint>>) (itype -> hydra.lib.maybes.Map.apply(
                      (java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>) (ftype -> new hydra.typing.TypeConstraint(itype, new hydra.core.Type.Function(new hydra.core.FunctionType(ftype, cod)), "case type")),
                      hydra.lib.maps.Lookup.apply(
                        fname,
                        caseMap.get())))),
                    fnames.get(),
                    itypes.get())));
                  hydra.util.Lazy<hydra.typing.TypeSubst> isubst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(caseResults.get())));
                  hydra.util.Lazy<java.util.List<hydra.typing.TypeConstraint>> dfltConstraints = new hydra.util.Lazy<>(() -> hydra.monads.Monads.maybeToList(hydra.lib.maybes.Map.apply(
                    (java.util.function.Function<hydra.typing.InferenceResult, hydra.typing.TypeConstraint>) (r -> new hydra.typing.TypeConstraint(cod, hydra.substitution.Substitution.substInType(
                      isubst.get(),
                      (r).type), "match default")),
                    dfltResult)));
                  hydra.util.Lazy<hydra.context.Context> fcx5 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(codvResult));
                  hydra.util.Lazy<java.util.List<hydra.core.Term>> iterms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(caseResults.get()));
                  return hydra.lib.eithers.Bind.apply(
                    hydra.inference.Inference.mapConstraints(
                      fcx5.get(),
                      cx,
                      (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst -> hydra.inference.Inference.yield(
                        fcx5.get(),
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
                        caseConstraints.get()))),
                    (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (mcResult -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(mcResult)))));
                }));
            }))));
      }));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfCollection(hydra.context.Context fcx, hydra.graph.Graph cx, java.util.function.Function<hydra.core.Type, hydra.core.Type> typCons, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term> trmCons, String desc, java.util.List<hydra.core.Term> els) {
    hydra.util.Pair<hydra.core.Name, hydra.context.Context> varResult = hydra.schemas.Schemas.freshName(fcx);
    hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(varResult));
    hydra.util.Lazy<hydra.core.Name> var = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(varResult));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(els),
      () -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(hydra.inference.Inference.yield(
        fcx2.get(),
        hydra.inference.Inference.buildTypeApplicationTerm(
          java.util.List.of(var.get()),
          (trmCons).apply((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))),
        (typCons).apply(new hydra.core.Type.Variable(var.get())),
        hydra.substitution.Substitution.idTypeSubst())))),
      () -> hydra.lib.eithers.Bind.apply(
        hydra.inference.Inference.inferMany(
          fcx2.get(),
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
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (resultsRp -> {
          hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>> results = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(resultsRp));
          hydra.util.Lazy<java.util.List<hydra.core.Type>> types = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(results.get())));
          hydra.util.Lazy<java.util.List<hydra.typing.TypeConstraint>> constraints = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>) (t -> new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(var.get()), t, desc)),
            types.get()));
          hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(resultsRp));
          hydra.util.Lazy<hydra.typing.TypeSubst> subst1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(results.get())));
          hydra.util.Lazy<java.util.List<hydra.core.Term>> terms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(results.get()));
          return hydra.lib.eithers.Bind.apply(
            hydra.inference.Inference.mapConstraints(
              fcx3.get(),
              cx,
              (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst2 -> {
                hydra.typing.TypeSubst isubst = hydra.substitution.Substitution.composeTypeSubst(
                  subst1.get(),
                  subst2);
                hydra.core.Term iterm = (trmCons).apply(terms.get());
                hydra.core.Type itype = (typCons).apply(new hydra.core.Type.Variable(var.get()));
                return hydra.inference.Inference.yield(
                  fcx3.get(),
                  iterm,
                  itype,
                  isubst);
              }),
              constraints.get()),
            (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (mcResult -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(mcResult)))));
        })));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfEither(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.util.Either<hydra.core.Term, hydra.core.Term> e) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.inference.Inference.inferTypeOfTerm(
          fcx,
          cx,
          l,
          "either left value"),
        (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (r1 -> {
          hydra.core.Term iterm = (r1).term;
          hydra.util.Lazy<hydra.core.Term> eitherTerm = new hydra.util.Lazy<>(() -> new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(iterm)))));
          hydra.core.Type leftType = (r1).type;
          hydra.context.Context fcx2 = (r1).context;
          hydra.util.Pair<hydra.core.Type, hydra.context.Context> fvResult = hydra.inference.Inference.freshVariableType(fcx2);
          hydra.util.Lazy<hydra.core.Type> rightType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(fvResult));
          hydra.core.Type eitherType = new hydra.core.Type.Either(new hydra.core.EitherType(leftType, rightType.get()));
          hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(fvResult));
          hydra.typing.TypeSubst subst = (r1).subst;
          hydra.core.Term termWithLeftType = new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(eitherTerm.get(), leftType));
          hydra.core.Term termWithBothTypes = new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(termWithLeftType, rightType.get()));
          return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(hydra.inference.Inference.yieldChecked(
            fcx3.get(),
            termWithBothTypes,
            eitherType,
            subst))));
        }))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (r -> hydra.lib.eithers.Bind.apply(
        hydra.inference.Inference.inferTypeOfTerm(
          fcx,
          cx,
          r,
          "either right value"),
        (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (r1 -> {
          hydra.core.Term iterm = (r1).term;
          hydra.util.Lazy<hydra.core.Term> eitherTerm = new hydra.util.Lazy<>(() -> new hydra.core.Term.Either((hydra.util.Either<hydra.core.Term, hydra.core.Term>) ((hydra.util.Either<hydra.core.Term, hydra.core.Term>) (hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(iterm)))));
          hydra.context.Context fcx2 = (r1).context;
          hydra.util.Pair<hydra.core.Type, hydra.context.Context> fvResult = hydra.inference.Inference.freshVariableType(fcx2);
          hydra.util.Lazy<hydra.core.Type> leftType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(fvResult));
          hydra.core.Type rightType = (r1).type;
          hydra.core.Type eitherType = new hydra.core.Type.Either(new hydra.core.EitherType(leftType.get(), rightType));
          hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(fvResult));
          hydra.typing.TypeSubst subst = (r1).subst;
          hydra.core.Term termWithLeftType = new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(eitherTerm.get(), leftType.get()));
          hydra.core.Term termWithBothTypes = new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(termWithLeftType, rightType));
          return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(hydra.inference.Inference.yieldChecked(
            fcx3.get(),
            termWithBothTypes,
            eitherType,
            subst))));
        }))),
      e);
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfElimination(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Elimination elm) {
    return (elm).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Elimination.Record p) {
        return hydra.inference.Inference.inferTypeOfProjection(
          fcx,
          cx,
          (p).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Elimination.Union c) {
        return hydra.inference.Inference.inferTypeOfCaseStatement(
          fcx,
          cx,
          (c).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Elimination.Wrap tname) {
        return hydra.inference.Inference.inferTypeOfUnwrap(
          fcx,
          cx,
          (tname).value);
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfFunction(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Function f) {
    return (f).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Function.Elimination elm) {
        return hydra.inference.Inference.inferTypeOfElimination(
          fcx,
          cx,
          (elm).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Function.Lambda l) {
        return hydra.inference.Inference.inferTypeOfLambda(
          fcx,
          cx,
          (l).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Function.Primitive name) {
        return hydra.inference.Inference.inferTypeOfPrimitive(
          fcx,
          cx,
          (name).value);
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfInjection(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Injection injection) {
    hydra.core.Field field = (injection).field;
    hydra.core.Name fname = (field).name;
    hydra.core.Term term = (field).term;
    hydra.core.Name tname = (injection).typeName;
    return hydra.lib.eithers.Bind.apply(
      hydra.inference.Inference.inferTypeOfTerm(
        fcx,
        cx,
        term,
        "injected term"),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (result -> {
        hydra.context.Context fcx2 = (result).context;
        return hydra.lib.eithers.Bind.apply(
          hydra.schemas.Schemas.requireSchemaType(
            fcx2,
            (cx).schemaTypes,
            tname),
          (java.util.function.Function<hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (stRp -> {
            hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(stRp));
            hydra.typing.TypeSubst isubst = (result).subst;
            hydra.core.Term iterm = (result).term;
            hydra.core.Type ityp = (result).type;
            hydra.util.Lazy<hydra.core.TypeScheme> schemaType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(stRp));
            hydra.core.Type stype = (schemaType.get()).type;
            java.util.List<hydra.core.Name> svars = (schemaType.get()).variables;
            return hydra.lib.eithers.Bind.apply(
              hydra.extract.core.Core.unionType(
                fcx3.get(),
                tname,
                stype),
              (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (sfields -> hydra.lib.eithers.Bind.apply(
                hydra.schemas.Schemas.findFieldType(
                  fcx3.get(),
                  fname,
                  sfields),
                (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (ftyp -> hydra.lib.eithers.Bind.apply(
                  hydra.inference.Inference.mapConstraints(
                    fcx3.get(),
                    cx,
                    (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst -> hydra.inference.Inference.yield(
                      fcx3.get(),
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
                    java.util.List.of(new hydra.typing.TypeConstraint(ftyp, ityp, "schema type of injected field"))),
                  (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (mcResult -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(mcResult)))))))));
          }));
      }));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfLambda(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Lambda lambda) {
    hydra.core.Term body = (lambda).body;
    hydra.util.Pair<hydra.core.Name, hydra.context.Context> vdomResult = hydra.schemas.Schemas.freshName(fcx);
    hydra.util.Lazy<hydra.core.Name> vdom = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(vdomResult));
    hydra.core.Type dom = new hydra.core.Type.Variable(vdom.get());
    hydra.core.Name var = (lambda).parameter;
    hydra.util.Lazy<hydra.graph.Graph> cx2 = new hydra.util.Lazy<>(() -> hydra.inference.Inference.extendContext(
      java.util.List.of((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(var, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()), dom, (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
      cx));
    hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(vdomResult));
    return hydra.lib.eithers.Bind.apply(
      hydra.inference.Inference.inferTypeOfTerm(
        fcx2.get(),
        cx2.get(),
        body,
        "lambda body"),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (result -> {
        hydra.typing.TypeSubst isubst = (result).subst;
        hydra.graph.Graph cx3 = hydra.substitution.Substitution.substInContext(
          isubst,
          cx);
        hydra.context.Context fcx3 = (result).context;
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
        return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(new hydra.typing.InferenceResult(rterm, rtype, isubst, iconstraints, fcx3))));
      }));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfLet(hydra.context.Context fcx0, hydra.graph.Graph cx, hydra.core.Let let0) {
    java.util.List<hydra.core.Binding> bindings0 = (let0).bindings;
    hydra.util.Lazy<java.util.List<hydra.core.Name>> names = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      projected -> projected.name,
      bindings0));
    hydra.util.Lazy<java.util.Set<hydra.core.Name>> nameSet = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(names.get()));
    java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>> toPair = (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>>) (binding -> {
      hydra.core.Name name = (binding).name;
      hydra.core.Term term = (binding).term;
      return (hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>) ((hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>) (new hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>(name, hydra.lib.lists.Filter.apply(
        (java.util.function.Function<hydra.core.Name, Boolean>) (n -> hydra.lib.sets.Member.apply(
          n,
          nameSet.get())),
        hydra.lib.sets.ToList.apply(hydra.rewriting.Rewriting.freeVariablesInTerm(term))))));
    });
    hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Name, java.util.List<hydra.core.Name>>>> adjList = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
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
    hydra.util.Lazy<hydra.context.Context> fcx = new hydra.util.Lazy<>(() -> new hydra.context.Context(hydra.lib.lists.Cons.apply(
      "let",
      (fcx0).trace), (fcx0).messages, (fcx0).other));
    hydra.util.Lazy<java.util.List<java.util.List<hydra.core.Name>>> groups = new hydra.util.Lazy<>(() -> hydra.sorting.Sorting.topologicalSortComponents(adjList.get()));
    hydra.util.Lazy<hydra.core.Term> rewrittenLet = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      createLet,
      body0,
      hydra.lib.lists.Reverse.apply(groups.get())));
    hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> res = (rewrittenLet.get()).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> otherwise(hydra.core.Term instance) {
        return hydra.inference.Inference.inferTypeOfTerm(
          fcx.get(),
          cx,
          rewrittenLet.get(),
          "empty let term");
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.Let l) {
        return hydra.inference.Inference.inferTypeOfLetNormalized(
          fcx.get(),
          cx,
          (l).value);
      }
    });
    java.util.function.Function<hydra.core.Term, hydra.core.Term> restoreLet = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (iterm -> {
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Integer, java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>>>>> helper = new java.util.concurrent.atomic.AtomicReference<>();
      helper.set((java.util.function.Function<Integer, java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>>>>) (level -> (java.util.function.Function<java.util.List<hydra.core.Binding>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>>>) (bins -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>>) (term -> {
        java.util.function.Function<hydra.core.Term, hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>> nonzero = (java.util.function.Function<hydra.core.Term, hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>>) (term2 -> (term2).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term> visit(hydra.core.Term.Let l) {
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
          () -> (hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>) ((hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>) (new hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>(bins, term))),
          () -> (nonzero).apply(term));
      }))));
      hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>> result = new hydra.util.Lazy<>(() -> (((helper.get()).apply(hydra.lib.lists.Length.apply(groups.get()))).apply((java.util.List<hydra.core.Binding>) (java.util.List.<hydra.core.Binding>of()))).apply(iterm));
      hydra.util.Lazy<java.util.List<hydra.core.Binding>> bindingList = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result.get()));
      hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Binding>> bindingMap2 = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Name, hydra.core.Binding>>) (b -> (hydra.util.Pair<hydra.core.Name, hydra.core.Binding>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Binding>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Binding>((b).name, b)))),
        bindingList.get())));
      hydra.util.Lazy<hydra.core.Term> e = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result.get()));
      return new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Name, hydra.util.Maybe<hydra.core.Binding>>) (n -> hydra.lib.maps.Lookup.apply(
          n,
          bindingMap2.get())),
        names.get())), e.get()));
    });
    java.util.function.Function<hydra.typing.InferenceResult, hydra.typing.InferenceResult> rewriteResult = (java.util.function.Function<hydra.typing.InferenceResult, hydra.typing.InferenceResult>) (iresult -> {
      hydra.context.Context fcxR = (iresult).context;
      java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> iconstraints = (iresult).classConstraints;
      hydra.typing.TypeSubst isubst = (iresult).subst;
      hydra.core.Term iterm = (iresult).term;
      hydra.core.Type itype = (iresult).type;
      return new hydra.typing.InferenceResult((restoreLet).apply(iterm), itype, isubst, iconstraints, fcxR);
    });
    return hydra.lib.eithers.Map.apply(
      rewriteResult,
      res);
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfLetNormalized(hydra.context.Context fcx0, hydra.graph.Graph cx0, hydra.core.Let letTerm) {
    java.util.List<hydra.core.Binding> bins0 = (letTerm).bindings;
    hydra.util.Lazy<java.util.List<hydra.core.Name>> bnames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      projected -> projected.name,
      bins0));
    hydra.core.Term body0 = (letTerm).body;
    hydra.util.Lazy<hydra.context.Context> fcx = new hydra.util.Lazy<>(() -> new hydra.context.Context(hydra.lib.lists.Cons.apply(
      "let-normalized",
      (fcx0).trace), (fcx0).messages, (fcx0).other));
    hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.context.Context>> bvarsResult = new hydra.util.Lazy<>(() -> hydra.schemas.Schemas.freshNames(
      hydra.lib.lists.Length.apply(bins0),
      fcx.get()));
    hydra.util.Lazy<java.util.List<hydra.core.Name>> bvars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bvarsResult.get()));
    hydra.util.Lazy<java.util.List<hydra.core.Type>> tbins0 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
      bvars.get()));
    hydra.util.Lazy<hydra.graph.Graph> cx1 = new hydra.util.Lazy<>(() -> hydra.inference.Inference.extendContext(
      hydra.lib.lists.Zip.apply(
        bnames.get(),
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>) (t -> new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()), t, (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))),
          tbins0.get())),
      cx0));
    hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bvarsResult.get()));
    return hydra.lib.eithers.Bind.apply(
      hydra.inference.Inference.inferTypesOfTemporaryBindings(
        fcx2.get(),
        cx1.get(),
        bins0),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (irRp -> {
        hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>> inferredResult = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(irRp));
        hydra.util.Lazy<java.util.List<hydra.core.Term>> bterms1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(inferredResult.get()));
        hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(irRp));
        hydra.util.Lazy<hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>> substAndConstraints = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(inferredResult.get())));
        hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> inferredConstraints = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(substAndConstraints.get()));
        hydra.util.Lazy<hydra.typing.TypeSubst> s1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(substAndConstraints.get()));
        hydra.util.Lazy<java.util.List<hydra.core.Type>> tbins1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(inferredResult.get())));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.Bimap.apply(
            (java.util.function.Function<hydra.context.InContext<hydra.error.UnificationError>, hydra.context.InContext<hydra.error.OtherError>>) (_ic -> (hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError((((java.util.function.Function<hydra.context.InContext<hydra.error.UnificationError>, hydra.error.UnificationError>) (projected -> projected.object)).apply(_ic)).message), ((java.util.function.Function<hydra.context.InContext<hydra.error.UnificationError>, hydra.context.Context>) (projected -> projected.context)).apply(_ic)))),
            (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>) (_a -> _a),
            hydra.unification.Unification.unifyTypeLists(
              fcx3.get(),
              (cx0).schemaTypes,
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (v1 -> hydra.substitution.Substitution.substInType(
                  s1.get(),
                  v1)),
                tbins0.get()),
              tbins1.get(),
              "temporary type bindings")),
          (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (s2 -> hydra.lib.eithers.Bind.apply(
            hydra.checking.Checking.checkTypeSubst(
              fcx3.get(),
              cx0,
              s2),
            (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (ignored -> {
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
              hydra.graph.Graph g2base = hydra.substitution.Substitution.substInContext(
                hydra.substitution.Substitution.composeTypeSubst(
                  s1.get(),
                  s2),
                cx0);
              hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> mergedConstraints = new hydra.util.Lazy<>(() -> hydra.inference.Inference.mergeClassConstraints(
                (g2base).classConstraints,
                allInferredConstraints.get()));
              hydra.graph.Graph g2 = new hydra.graph.Graph((g2base).boundTerms, (g2base).boundTypes, mergedConstraints.get(), (g2base).lambdaVariables, (g2base).metadata, (g2base).primitives, (g2base).schemaTypes, (g2base).typeVariables);
              hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>> tsbins1 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Zip.apply(
                bnames.get(),
                hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>) (t -> hydra.inference.Inference.generalize(
                    g2,
                    hydra.substitution.Substitution.substInType(
                      s2,
                      t))),
                  tbins1.get())));
              return hydra.lib.eithers.Bind.apply(
                hydra.inference.Inference.inferTypeOfTerm(
                  fcx3.get(),
                  hydra.inference.Inference.extendContext(
                    tsbins1.get(),
                    g2),
                  body0,
                  "let body"),
                (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (bodyResult -> {
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
                    (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>, hydra.util.Pair<hydra.core.Name, hydra.core.Term>>) (pair -> {
                      hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
                      hydra.util.Lazy<hydra.core.TypeScheme> ts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
                      return (hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>(name.get(), hydra.inference.Inference.buildTypeApplicationTerm(
                        (ts.get()).variables,
                        new hydra.core.Term.Variable(name.get())))));
                    }),
                    tsbins1.get()))));
                  java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>, hydra.core.Term>, hydra.core.Binding> createBinding = (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>, hydra.core.Term>, hydra.core.Binding>) (bindingPair -> {
                    hydra.util.Lazy<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>> nameTsPair = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindingPair));
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
                  hydra.context.Context fcx4 = (bodyResult).context;
                  hydra.core.Type tbody = (bodyResult).type;
                  return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(new hydra.typing.InferenceResult(new hydra.core.Term.Let(new hydra.core.Let(bins1.get(), body1)), tbody, hydra.substitution.Substitution.composeTypeSubstList(java.util.List.of(
                    s1.get(),
                    s2,
                    sbody)), allConstraints.get(), fcx4))));
                }));
            }))));
      }));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfList(hydra.context.Context fcx, hydra.graph.Graph cx, java.util.List<hydra.core.Term> v1) {
    return hydra.inference.Inference.inferTypeOfCollection(
      fcx,
      cx,
      (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> new hydra.core.Type.List(x)),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (x -> new hydra.core.Term.List(x)),
      "list element",
      v1);
  }
  
  static hydra.typing.InferenceResult inferTypeOfLiteral(hydra.context.Context fcx, hydra.core.Literal lit) {
    return new hydra.typing.InferenceResult(new hydra.core.Term.Literal(lit), new hydra.core.Type.Literal(hydra.reflect.Reflect.literalType(lit)), hydra.substitution.Substitution.idTypeSubst(), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), fcx);
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfMap(hydra.context.Context fcx, hydra.graph.Graph cx, java.util.Map<hydra.core.Term, hydra.core.Term> m) {
    hydra.util.Pair<hydra.core.Name, hydra.context.Context> kvarResult = hydra.schemas.Schemas.freshName(fcx);
    hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(kvarResult));
    hydra.util.Pair<hydra.core.Name, hydra.context.Context> vvarResult = hydra.schemas.Schemas.freshName(fcx2.get());
    hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(vvarResult));
    hydra.util.Lazy<hydra.core.Name> kvar = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(kvarResult));
    hydra.util.Lazy<hydra.core.Name> vvar = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(vvarResult));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.maps.Null.apply(m),
      () -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(hydra.inference.Inference.yield(
        fcx3.get(),
        hydra.inference.Inference.buildTypeApplicationTerm(
          java.util.List.of(
            kvar.get(),
            vvar.get()),
          new hydra.core.Term.Map((java.util.Map<hydra.core.Term, hydra.core.Term>) ((java.util.Map<hydra.core.Term, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Term, hydra.core.Term>apply())))),
        new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(kvar.get()), new hydra.core.Type.Variable(vvar.get()))),
        hydra.substitution.Substitution.idTypeSubst())))),
      () -> hydra.lib.eithers.Bind.apply(
        hydra.inference.Inference.inferMany(
          fcx3.get(),
          cx,
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.core.Term, String>>) (k -> (hydra.util.Pair<hydra.core.Term, String>) ((hydra.util.Pair<hydra.core.Term, String>) (new hydra.util.Pair<hydra.core.Term, String>(k, "map key")))),
            hydra.lib.maps.Keys.apply(m))),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (kRp -> {
          hydra.util.Lazy<hydra.context.Context> fcx4 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(kRp));
          hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>> kResults = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(kRp));
          hydra.util.Lazy<hydra.typing.TypeSubst> ksubst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(kResults.get())));
          hydra.util.Lazy<java.util.List<hydra.core.Term>> kterms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(kResults.get()));
          hydra.util.Lazy<java.util.List<hydra.core.Type>> ktypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(kResults.get())));
          return hydra.lib.eithers.Bind.apply(
            hydra.inference.Inference.inferMany(
              fcx4.get(),
              cx,
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.core.Term, String>>) (v -> (hydra.util.Pair<hydra.core.Term, String>) ((hydra.util.Pair<hydra.core.Term, String>) (new hydra.util.Pair<hydra.core.Term, String>(v, "map value")))),
                hydra.lib.maps.Elems.apply(m))),
            (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (vRp -> {
              hydra.util.Lazy<hydra.context.Context> fcx5 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(vRp));
              hydra.util.Lazy<java.util.List<hydra.typing.TypeConstraint>> kcons = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>) (t -> new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(kvar.get()), t, "map key")),
                ktypes.get()));
              hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>> vResults = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(vRp));
              hydra.util.Lazy<java.util.List<hydra.core.Type>> vtypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(vResults.get())));
              hydra.util.Lazy<java.util.List<hydra.typing.TypeConstraint>> vcons = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>) (t -> new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(vvar.get()), t, "map value")),
                vtypes.get()));
              hydra.util.Lazy<hydra.typing.TypeSubst> vsubst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(vResults.get())));
              hydra.util.Lazy<java.util.List<hydra.core.Term>> vterms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(vResults.get()));
              return hydra.lib.eithers.Bind.apply(
                hydra.inference.Inference.mapConstraints(
                  fcx5.get(),
                  cx,
                  (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst -> hydra.inference.Inference.yield(
                    fcx5.get(),
                    new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
                      kterms.get(),
                      vterms.get()))),
                    new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(kvar.get()), new hydra.core.Type.Variable(vvar.get()))),
                    hydra.substitution.Substitution.composeTypeSubstList(java.util.List.of(
                      ksubst.get(),
                      vsubst.get(),
                      subst)))),
                  hydra.lib.lists.Concat.apply(java.util.List.of(
                    kcons.get(),
                    vcons.get()))),
                (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (mcResult -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(mcResult)))));
            }));
        })));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfOptional(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.util.Maybe<hydra.core.Term> m) {
    java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term> trmCons = (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (terms -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(terms),
      () -> new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())),
      () -> new hydra.core.Term.Maybe(hydra.util.Maybe.just(hydra.lib.lists.Head.apply(terms)))));
    return hydra.inference.Inference.inferTypeOfCollection(
      fcx,
      cx,
      (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> new hydra.core.Type.Maybe(x)),
      trmCons,
      "optional element",
      hydra.lib.maybes.Maybe.apply(
        (java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()),
        (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (hydra.lib.lists.Singleton::apply),
        m));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfPair(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.util.Pair<hydra.core.Term, hydra.core.Term> p) {
    return hydra.lib.eithers.Bind.apply(
      hydra.inference.Inference.inferMany(
        fcx,
        cx,
        java.util.List.of(
          (hydra.util.Pair<hydra.core.Term, String>) ((hydra.util.Pair<hydra.core.Term, String>) (new hydra.util.Pair<hydra.core.Term, String>(hydra.lib.pairs.First.apply(p), "pair first element"))),
          (hydra.util.Pair<hydra.core.Term, String>) ((hydra.util.Pair<hydra.core.Term, String>) (new hydra.util.Pair<hydra.core.Term, String>(hydra.lib.pairs.Second.apply(p), "pair second element"))))),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (rp -> {
        hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(rp));
        hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>> results = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(rp));
        hydra.util.Lazy<java.util.List<hydra.core.Term>> iterms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(results.get()));
        hydra.util.Lazy<hydra.core.Term> ifst = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(iterms.get()));
        hydra.util.Lazy<hydra.core.Term> isnd = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(hydra.lib.lists.Tail.apply(iterms.get())));
        hydra.util.Lazy<hydra.typing.TypeSubst> isubst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(results.get())));
        hydra.util.Lazy<java.util.List<hydra.core.Type>> itypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(results.get())));
        hydra.util.Lazy<hydra.core.Term> pairTerm = new hydra.util.Lazy<>(() -> new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(ifst.get(), isnd.get())))));
        hydra.util.Lazy<hydra.core.Type> tyFst = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(itypes.get()));
        hydra.util.Lazy<hydra.core.Type> tySnd = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(hydra.lib.lists.Tail.apply(itypes.get())));
        hydra.core.Term termWithTypes = new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(pairTerm.get(), tyFst.get())), tySnd.get()));
        return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(hydra.inference.Inference.yield(
          fcx2.get(),
          termWithTypes,
          new hydra.core.Type.Pair(new hydra.core.PairType(tyFst.get(), tySnd.get())),
          isubst.get()))));
      }));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfPrimitive(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Name name) {
    return hydra.lib.maybes.Maybe.apply(
      (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
        "No such primitive: ",
        (name).value)), fcx))))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (scheme -> {
        hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context> tsResult = hydra.schemas.Schemas.instantiateTypeScheme(
          fcx,
          scheme);
        hydra.util.Lazy<hydra.core.TypeScheme> ts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tsResult));
        hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> constraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.apply(
          (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())),
          (ts.get()).constraints));
        hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(tsResult));
        return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(hydra.inference.Inference.yieldCheckedWithConstraints(
          fcx2.get(),
          hydra.inference.Inference.buildTypeApplicationTerm(
            (ts.get()).variables,
            new hydra.core.Term.Function(new hydra.core.Function.Primitive(name))),
          (ts.get()).type,
          hydra.substitution.Substitution.idTypeSubst(),
          constraints.get()))));
      }),
      hydra.lib.maybes.Map.apply(
        projected -> projected.type,
        hydra.lib.maps.Lookup.apply(
          name,
          (cx).primitives)));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfProjection(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Projection proj) {
    hydra.core.Name fname = (proj).field;
    hydra.core.Name tname = (proj).typeName;
    return hydra.lib.eithers.Bind.apply(
      hydra.schemas.Schemas.requireSchemaType(
        fcx,
        (cx).schemaTypes,
        tname),
      (java.util.function.Function<hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (stRp -> {
        hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(stRp));
        hydra.util.Lazy<hydra.core.TypeScheme> schemaType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(stRp));
        hydra.core.Type stype = (schemaType.get()).type;
        java.util.List<hydra.core.Name> svars = (schemaType.get()).variables;
        return hydra.lib.eithers.Bind.apply(
          hydra.extract.core.Core.recordType(
            fcx2.get(),
            tname,
            stype),
          (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (sfields -> hydra.lib.eithers.Bind.apply(
            hydra.schemas.Schemas.findFieldType(
              fcx2.get(),
              fname,
              sfields),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (ftyp -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(hydra.inference.Inference.yield(
              fcx2.get(),
              hydra.inference.Inference.buildTypeApplicationTerm(
                svars,
                new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(tname, fname))))),
              new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.schemas.Schemas.nominalApplication(
                tname,
                hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
                  svars)), ftyp)),
              hydra.substitution.Substitution.idTypeSubst()))))))));
      }));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfRecord(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Record record) {
    java.util.List<hydra.core.Field> fields = (record).fields;
    hydra.util.Lazy<java.util.List<hydra.core.Name>> fnames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      projected -> projected.name,
      fields));
    hydra.core.Name tname = (record).typeName;
    return hydra.lib.eithers.Bind.apply(
      hydra.schemas.Schemas.requireSchemaType(
        fcx,
        (cx).schemaTypes,
        tname),
      (java.util.function.Function<hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (stRp -> {
        hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(stRp));
        hydra.util.Lazy<hydra.core.TypeScheme> schemaType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(stRp));
        return hydra.lib.eithers.Bind.apply(
          hydra.inference.Inference.inferMany(
            fcx2.get(),
            cx,
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Field, hydra.util.Pair<hydra.core.Term, String>>) (f -> (hydra.util.Pair<hydra.core.Term, String>) ((hydra.util.Pair<hydra.core.Term, String>) (new hydra.util.Pair<hydra.core.Term, String>((f).term, hydra.lib.strings.Cat2.apply(
                "field ",
                ((f).name).value))))),
              fields)),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (rp -> {
            hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(rp));
            hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.typing.TypeSubst>>> results = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(rp));
            hydra.util.Lazy<hydra.typing.TypeSubst> isubst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(results.get())));
            hydra.util.Lazy<java.util.List<hydra.core.Term>> iterms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(results.get()));
            hydra.util.Lazy<java.util.List<hydra.core.Type>> itypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(results.get())));
            hydra.util.Lazy<hydra.core.Type> ityp = new hydra.util.Lazy<>(() -> new hydra.core.Type.Record(new hydra.core.RowType(tname, hydra.lib.lists.ZipWith.apply(
              (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.core.FieldType>>) (n -> (java.util.function.Function<hydra.core.Type, hydra.core.FieldType>) (t -> new hydra.core.FieldType(n, t))),
              fnames.get(),
              itypes.get()))));
            hydra.core.Type stype = (schemaType.get()).type;
            java.util.List<hydra.core.Name> svars = (schemaType.get()).variables;
            return hydra.lib.eithers.Bind.apply(
              hydra.inference.Inference.mapConstraints(
                fcx3.get(),
                cx,
                (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst -> hydra.inference.Inference.yield(
                  fcx3.get(),
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
                java.util.List.of(new hydra.typing.TypeConstraint(stype, ityp.get(), "schema type of record"))),
              (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (mcResult -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(mcResult)))));
          }));
      }));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfSet(hydra.context.Context fcx, hydra.graph.Graph cx, java.util.Set<hydra.core.Term> s) {
    return hydra.inference.Inference.inferTypeOfCollection(
      fcx,
      cx,
      (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> new hydra.core.Type.Set(x)),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (terms -> new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(terms))),
      "set element",
      hydra.lib.sets.ToList.apply(s));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfTerm(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Term term, String desc) {
    hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> new hydra.context.Context(hydra.lib.lists.Cons.apply(
      desc,
      (fcx).trace), (fcx).messages, (fcx).other));
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.Annotated a) {
        return hydra.inference.Inference.inferTypeOfAnnotatedTerm(
          fcx2.get(),
          cx,
          (a).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.Application a) {
        return hydra.inference.Inference.inferTypeOfApplication(
          fcx2.get(),
          cx,
          (a).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.Either e) {
        return hydra.inference.Inference.inferTypeOfEither(
          fcx2.get(),
          cx,
          (e).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.Function f) {
        return hydra.inference.Inference.inferTypeOfFunction(
          fcx2.get(),
          cx,
          (f).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.Let l) {
        return hydra.inference.Inference.inferTypeOfLet(
          fcx2.get(),
          cx,
          (l).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.List els) {
        return hydra.inference.Inference.inferTypeOfList(
          fcx2.get(),
          cx,
          (els).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.Literal l) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(hydra.inference.Inference.inferTypeOfLiteral(
          fcx2.get(),
          (l).value))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.Map m) {
        return hydra.inference.Inference.inferTypeOfMap(
          fcx2.get(),
          cx,
          (m).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.Maybe m) {
        return hydra.inference.Inference.inferTypeOfOptional(
          fcx2.get(),
          cx,
          (m).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.Pair p) {
        return hydra.inference.Inference.inferTypeOfPair(
          fcx2.get(),
          cx,
          (p).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.Record r) {
        return hydra.inference.Inference.inferTypeOfRecord(
          fcx2.get(),
          cx,
          (r).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.Set s) {
        return hydra.inference.Inference.inferTypeOfSet(
          fcx2.get(),
          cx,
          (s).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.TypeApplication tt) {
        return hydra.inference.Inference.inferTypeOfTypeApplication(
          fcx2.get(),
          cx,
          (tt).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.TypeLambda ta) {
        return hydra.inference.Inference.inferTypeOfTypeLambda(
          fcx2.get(),
          cx,
          (ta).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.Union i) {
        return hydra.inference.Inference.inferTypeOfInjection(
          fcx2.get(),
          cx,
          (i).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.Unit ignored) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(hydra.inference.Inference.inferTypeOfUnit(fcx2.get()))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.Variable name) {
        return hydra.inference.Inference.inferTypeOfVariable(
          fcx2.get(),
          cx,
          (name).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> visit(hydra.core.Term.Wrap w) {
        return hydra.inference.Inference.inferTypeOfWrappedTerm(
          fcx2.get(),
          cx,
          (w).value);
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfTypeLambda(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.TypeLambda ta) {
    return hydra.inference.Inference.inferTypeOfTerm(
      fcx,
      cx,
      (ta).body,
      "type abstraction");
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfTypeApplication(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.TypeApplicationTerm tt) {
    return hydra.inference.Inference.inferTypeOfTerm(
      fcx,
      cx,
      (tt).body,
      "type application term");
  }
  
  static hydra.typing.InferenceResult inferTypeOfUnit(hydra.context.Context fcx) {
    return new hydra.typing.InferenceResult(new hydra.core.Term.Unit(), new hydra.core.Type.Unit(), hydra.substitution.Substitution.idTypeSubst(), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), fcx);
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfUnwrap(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Name tname) {
    return hydra.lib.eithers.Bind.apply(
      hydra.schemas.Schemas.requireSchemaType(
        fcx,
        (cx).schemaTypes,
        tname),
      (java.util.function.Function<hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (stRp -> {
        hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(stRp));
        hydra.util.Lazy<hydra.core.TypeScheme> schemaType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(stRp));
        hydra.core.Type stype = (schemaType.get()).type;
        java.util.List<hydra.core.Name> svars = (schemaType.get()).variables;
        return hydra.lib.eithers.Bind.apply(
          hydra.extract.core.Core.wrappedType(
            fcx2.get(),
            tname,
            stype),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (wtyp -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(hydra.inference.Inference.yield(
            fcx2.get(),
            hydra.inference.Inference.buildTypeApplicationTerm(
              svars,
              new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(tname)))),
            new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.schemas.Schemas.nominalApplication(
              tname,
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
                svars)), wtyp)),
            hydra.substitution.Substitution.idTypeSubst()))))));
      }));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfVariable(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Name name) {
    return hydra.lib.maybes.Maybe.apply(
      (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
        "Variable not bound to type: ",
        (name).value)), fcx))))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (scheme -> {
        hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context> tsResult = hydra.schemas.Schemas.instantiateTypeScheme(
          fcx,
          scheme);
        hydra.util.Lazy<hydra.core.TypeScheme> ts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tsResult));
        hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> constraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.apply(
          (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())),
          (ts.get()).constraints));
        hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(tsResult));
        return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(new hydra.typing.InferenceResult(hydra.inference.Inference.buildTypeApplicationTerm(
          (ts.get()).variables,
          new hydra.core.Term.Variable(name)), (ts.get()).type, hydra.substitution.Substitution.idTypeSubst(), constraints.get(), fcx2.get()))));
      }),
      hydra.lib.maps.Lookup.apply(
        name,
        (cx).boundTypes));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> inferTypeOfWrappedTerm(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.WrappedTerm wt) {
    hydra.core.Term term = (wt).body;
    hydra.core.Name tname = (wt).typeName;
    return hydra.lib.eithers.Bind.apply(
      hydra.schemas.Schemas.requireSchemaType(
        fcx,
        (cx).schemaTypes,
        tname),
      (java.util.function.Function<hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (stRp -> {
        hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(stRp));
        hydra.util.Lazy<hydra.core.TypeScheme> schemaType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(stRp));
        return hydra.lib.eithers.Bind.apply(
          hydra.inference.Inference.inferTypeOfTerm(
            fcx2.get(),
            cx,
            term,
            "wrapped term"),
          (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (result -> {
            hydra.context.Context fcx3 = (result).context;
            hydra.typing.TypeSubst isubst = (result).subst;
            hydra.core.Term iterm = (result).term;
            hydra.core.Type itype = (result).type;
            hydra.core.Type ityp = new hydra.core.Type.Wrap(new hydra.core.WrappedType(tname, itype));
            hydra.core.Type stype = (schemaType.get()).type;
            java.util.List<hydra.core.Name> svars = (schemaType.get()).variables;
            return hydra.lib.eithers.Bind.apply(
              hydra.inference.Inference.mapConstraints(
                fcx3,
                cx,
                (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst -> hydra.inference.Inference.yield(
                  fcx3,
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
                java.util.List.of(new hydra.typing.TypeConstraint(stype, ityp, "schema type of wrapper"))),
              (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (mcResult -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(mcResult)))));
          }));
      }));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>> inferTypesOfTemporaryBindings(hydra.context.Context fcx, hydra.graph.Graph cx, java.util.List<hydra.core.Binding> bins) {
    hydra.util.Lazy<hydra.core.Binding> binding = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(bins));
    hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>> dflt = new hydra.util.Lazy<>(() -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>>) (() -> {
      hydra.core.Name k = (binding.get()).name;
      return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>>) (() -> {
        hydra.core.Term v = (binding.get()).term;
        return ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>>) (() -> {
          hydra.util.Lazy<java.util.List<hydra.core.Binding>> tl = new hydra.util.Lazy<>(() -> hydra.lib.lists.Tail.apply(bins));
          return hydra.lib.eithers.Bind.apply(
            hydra.inference.Inference.inferTypeOfTerm(
              fcx,
              cx,
              v,
              hydra.lib.strings.Cat.apply(java.util.List.of(
                "temporary let binding '",
                (k).value,
                "'"))),
            (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>>) (result1 -> {
              java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c1Inferred = (result1).classConstraints;
              hydra.context.Context fcx2 = (result1).context;
              hydra.core.Term j = (result1).term;
              hydra.typing.TypeSubst u = (result1).subst;
              hydra.core.Type u_prime = (result1).type;
              return hydra.lib.eithers.Bind.apply(
                hydra.lib.maybes.Maybe.apply(
                  (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>right((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply()))))),
                  (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (ts -> {
                    hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context> tsResult = hydra.schemas.Schemas.instantiateTypeScheme(
                      fcx2,
                      ts);
                    hydra.util.Lazy<hydra.core.TypeScheme> instantiatedTs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tsResult));
                    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> freshConstraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.apply(
                      (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())),
                      (instantiatedTs.get()).constraints));
                    return hydra.lib.eithers.Bind.apply(
                      hydra.lib.eithers.Bimap.apply(
                        (java.util.function.Function<hydra.context.InContext<hydra.error.UnificationError>, hydra.context.InContext<hydra.error.OtherError>>) (_ic -> (hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError((((java.util.function.Function<hydra.context.InContext<hydra.error.UnificationError>, hydra.error.UnificationError>) (projected -> projected.object)).apply(_ic)).message), ((java.util.function.Function<hydra.context.InContext<hydra.error.UnificationError>, hydra.context.Context>) (projected -> projected.context)).apply(_ic)))),
                        (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>) (_a -> _a),
                        hydra.unification.Unification.unifyTypes(
                          fcx2,
                          (cx).schemaTypes,
                          (instantiatedTs.get()).type,
                          u_prime,
                          "original binding type")),
                      (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (unifySubst -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>right(hydra.substitution.Substitution.substInClassConstraints(
                        unifySubst,
                        freshConstraints.get()))))));
                  }),
                  (binding.get()).type),
                (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>>) (originalBindingConstraints -> {
                  hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> c1 = new hydra.util.Lazy<>(() -> hydra.inference.Inference.mergeClassConstraints(
                    c1Inferred,
                    originalBindingConstraints));
                  return hydra.lib.eithers.Bind.apply(
                    hydra.inference.Inference.inferTypesOfTemporaryBindings(
                      fcx2,
                      hydra.substitution.Substitution.substInContext(
                        u,
                        cx),
                      tl.get()),
                    (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>>) (rp2 -> {
                      hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>> result2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(rp2));
                      hydra.util.Lazy<hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>> restPair = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(result2.get())));
                      hydra.util.Lazy<hydra.typing.TypeSubst> r = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(restPair.get()));
                      java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c1Subst = hydra.substitution.Substitution.substInClassConstraints(
                        r.get(),
                        c1.get());
                      hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> c2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(restPair.get()));
                      hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(rp2));
                      hydra.util.Lazy<java.util.List<hydra.core.Term>> h = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result2.get()));
                      hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> mergedConstraints = new hydra.util.Lazy<>(() -> hydra.inference.Inference.mergeClassConstraints(
                        c1Subst,
                        c2.get()));
                      hydra.util.Lazy<java.util.List<hydra.core.Type>> r_prime = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(result2.get())));
                      return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>((hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>) ((hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>) (new hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>(hydra.lib.lists.Cons.apply(
                        hydra.substitution.Substitution.substTypesInTerm(
                          r.get(),
                          j),
                        h.get()), (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>(hydra.lib.lists.Cons.apply(
                        hydra.substitution.Substitution.substInType(
                          r.get(),
                          u_prime),
                        r_prime.get()), (hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) ((hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (new hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>(hydra.substitution.Substitution.composeTypeSubst(
                        u,
                        r.get()), mergedConstraints.get()))))))))), fcx3.get()))))));
                    }));
                }));
            }));
        })).get();
      })).get();
    })).get());
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(bins),
      () -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>((hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>) ((hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>) (new hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()), (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>((java.util.List<hydra.core.Type>) (java.util.List.<hydra.core.Type>of()), (hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) ((hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (new hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>(hydra.substitution.Substitution.idTypeSubst(), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply()))))))))))), fcx)))))),
      () -> dflt.get());
  }
  
  static Boolean isUnbound(hydra.graph.Graph cx, hydra.core.Name v) {
    return hydra.lib.logic.And.apply(
      hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
        v,
        hydra.inference.Inference.freeVariablesInContext(cx))),
      hydra.lib.logic.Not.apply(hydra.lib.maps.Member.apply(
        v,
        (cx).schemaTypes)));
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T0> mapConstraints(hydra.context.Context flowCx, hydra.graph.Graph cx, java.util.function.Function<hydra.typing.TypeSubst, T0> f, java.util.List<hydra.typing.TypeConstraint> constraints) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.error.UnificationError>, hydra.context.InContext<hydra.error.OtherError>>) (_ic -> (hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError((((java.util.function.Function<hydra.context.InContext<hydra.error.UnificationError>, hydra.error.UnificationError>) (projected -> projected.object)).apply(_ic)).message), ((java.util.function.Function<hydra.context.InContext<hydra.error.UnificationError>, hydra.context.Context>) (projected -> projected.context)).apply(_ic)))),
        (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>) (_a -> _a),
        hydra.unification.Unification.unifyTypeConstraints(
          flowCx,
          (cx).schemaTypes,
          constraints)),
      (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T0>>) (s -> hydra.lib.eithers.Bind.apply(
        hydra.checking.Checking.checkTypeSubst(
          flowCx,
          cx,
          s),
        (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T0>>) (ignored -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T0>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, T0>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, T0>right((f).apply(s))))))));
  }
  
  static <T0> java.util.Map<T0, hydra.core.TypeVariableMetadata> mergeClassConstraints(java.util.Map<T0, hydra.core.TypeVariableMetadata> m1, java.util.Map<T0, hydra.core.TypeVariableMetadata> m2) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Map<T0, hydra.core.TypeVariableMetadata>, java.util.function.Function<hydra.util.Pair<T0, hydra.core.TypeVariableMetadata>, java.util.Map<T0, hydra.core.TypeVariableMetadata>>>) (acc -> (java.util.function.Function<hydra.util.Pair<T0, hydra.core.TypeVariableMetadata>, java.util.Map<T0, hydra.core.TypeVariableMetadata>>) (pair -> {
        hydra.util.Lazy<T0> k = new hydra.util.Lazy<>(() -> hydra.inference.Inference.<T0>mergeClassConstraints_k(pair));
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
  
  static <T0> T0 mergeClassConstraints_k(hydra.util.Pair<T0, hydra.core.TypeVariableMetadata> pair) {
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
  
  static hydra.typing.InferenceResult yield(hydra.context.Context fcx, hydra.core.Term term, hydra.core.Type typ, hydra.typing.TypeSubst subst) {
    return new hydra.typing.InferenceResult(hydra.substitution.Substitution.substTypesInTerm(
      subst,
      term), hydra.substitution.Substitution.substInType(
      subst,
      typ), subst, (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), fcx);
  }
  
  static hydra.typing.InferenceResult yieldChecked(hydra.context.Context fcx, hydra.core.Term term, hydra.core.Type typ, hydra.typing.TypeSubst subst) {
    hydra.core.Term iterm = hydra.substitution.Substitution.substTypesInTerm(
      subst,
      term);
    hydra.core.Type itype = hydra.substitution.Substitution.substInType(
      subst,
      typ);
    return new hydra.typing.InferenceResult(iterm, itype, subst, (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), fcx);
  }
  
  static hydra.typing.InferenceResult yieldCheckedWithConstraints(hydra.context.Context fcx, hydra.core.Term term, hydra.core.Type typ, hydra.typing.TypeSubst subst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> constraints) {
    java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> iconstraints = hydra.substitution.Substitution.substInClassConstraints(
      subst,
      constraints);
    hydra.core.Term iterm = hydra.substitution.Substitution.substTypesInTerm(
      subst,
      term);
    hydra.core.Type itype = hydra.substitution.Substitution.substInType(
      subst,
      typ);
    return new hydra.typing.InferenceResult(iterm, itype, subst, iconstraints, fcx);
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult> yieldDebug(hydra.context.Context fcx, T0 cx, String debugId, hydra.core.Term term, hydra.core.Type typ, hydra.typing.TypeSubst subst) {
    hydra.core.Term rterm = hydra.substitution.Substitution.substTypesInTerm(
      subst,
      term);
    hydra.core.Type rtyp = hydra.substitution.Substitution.substInType(
      subst,
      typ);
    return hydra.lib.eithers.Bind.apply(
      hydra.annotations.Annotations.debugIf(
        fcx,
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
      (java.util.function.Function<java.lang.Void, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>>) (result -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.typing.InferenceResult>right(new hydra.typing.InferenceResult(rterm, rtyp, subst, (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), fcx))))));
  }
  
  static hydra.typing.InferenceResult yieldWithConstraints(hydra.context.Context fcx, hydra.core.Term term, hydra.core.Type typ, hydra.typing.TypeSubst subst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> constraints) {
    return new hydra.typing.InferenceResult(hydra.substitution.Substitution.substTypesInTerm(
      subst,
      term), hydra.substitution.Substitution.substInType(
      subst,
      typ), subst, constraints, fcx);
  }
}
