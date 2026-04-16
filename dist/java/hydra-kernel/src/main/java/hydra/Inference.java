// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Type inference following Algorithm W, extended for nominal terms and types
 */
public interface Inference {
  static <T0> hydra.util.Either<hydra.errors.Error_, T0> atOrFail(Integer i, String desc, java.util.List<T0> xs) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.errors.Error_, T0>left(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
        "atOrFail: ",
        desc)))),
      (java.util.function.Function<T0, hydra.util.Either<hydra.errors.Error_, T0>>) (x -> hydra.util.Either.<hydra.errors.Error_, T0>right(x)),
      hydra.lib.lists.MaybeAt.apply(
        i,
        xs));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.typing.TypeSubst> bindConstraints(T0 flowCx, hydra.graph.Graph cx, java.util.List<hydra.typing.TypeConstraint> constraints) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.errors.UnificationError, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Unification(_e)),
        (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>) (_a -> _a),
        hydra.Unification.unifyTypeConstraints(
          flowCx,
          (cx).schemaTypes,
          constraints)),
      (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.errors.Error_, hydra.typing.TypeSubst>>) (s -> hydra.lib.eithers.Bind.apply(
        hydra.Checking.<T0>checkTypeSubst(
          flowCx,
          cx,
          s),
        (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.errors.Error_, hydra.typing.TypeSubst>>) (ignored -> hydra.util.Either.<hydra.errors.Error_, hydra.typing.TypeSubst>right(s)))));
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
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> new hydra.core.Binding(bname, hydra.Inference.bindUnboundTypeVariables(
              cx,
              bterm), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
            (java.util.function.Function<hydra.core.TypeScheme, hydra.core.Binding>) (ts -> {
              hydra.util.Lazy<java.util.Set<hydra.core.Name>> bvars = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply((ts).variables));
              hydra.util.Lazy<java.util.Set<hydra.core.Name>> excluded = new hydra.util.Lazy<>(() -> hydra.lib.sets.Union.apply(
                svars.get(),
                bvars.get()));
              hydra.util.Lazy<java.util.Set<hydra.core.Name>> inType = new hydra.util.Lazy<>(() -> hydra.lib.sets.Difference.apply(
                hydra.Variables.freeVariablesInType((ts).type),
                excluded.get()));
              hydra.util.Lazy<java.util.Set<hydra.core.Name>> phantoms = new hydra.util.Lazy<>(() -> hydra.lib.sets.Difference.apply(
                hydra.Variables.freeTypeVariablesInTerm(bterm),
                hydra.lib.sets.Union.apply(
                  excluded.get(),
                  inType.get())));
              hydra.util.Lazy<hydra.typing.TypeSubst> phantomSubst = new hydra.util.Lazy<>(() -> new hydra.typing.TypeSubst(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>) (v -> (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>(v, new hydra.core.Type.Unit())))),
                hydra.lib.sets.ToList.apply(phantoms.get())))));
              hydra.core.Term bterm1 = hydra.Substitution.substTypesInTerm(
                phantomSubst.get(),
                bterm);
              hydra.util.Lazy<java.util.List<hydra.core.Name>> unbound = new hydra.util.Lazy<>(() -> hydra.lib.sets.ToList.apply(inType.get()));
              hydra.util.Lazy<hydra.core.Term> bterm2 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, hydra.core.Term>>) (t -> (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (v -> new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(v, t)))),
                bterm1,
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
          (l).value.bindings), hydra.Inference.bindUnboundTypeVariables(
          cx,
          (l).value.body)));
      }
    })));
    return hydra.Rewriting.rewriteTerm(
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

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.core.Term> finalizeInferredTerm(T0 flowCx, hydra.graph.Graph cx, hydra.core.Term term) {
    hydra.core.Term term2 = hydra.Inference.bindUnboundTypeVariables(
      cx,
      term);
    return hydra.lib.eithers.Bind.apply(
      hydra.Checking.<T0>checkForUnboundTypeVariables(
        flowCx,
        cx,
        term2),
      (java.util.function.Function<java.lang.Void, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (ignored -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(hydra.Variables.normalizeTypeVariablesInTerm(term2))));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<T0, hydra.context.Context>> forInferredTerm(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Term term, String desc, java.util.function.Function<hydra.typing.InferenceResult, T0> f) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Inference.inferTypeOfTerm(
        fcx,
        cx,
        term,
        desc),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<T0, hydra.context.Context>>>) (rp -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<T0, hydra.context.Context>>right((hydra.util.Pair<T0, hydra.context.Context>) ((hydra.util.Pair<T0, hydra.context.Context>) (new hydra.util.Pair<T0, hydra.context.Context>((f).apply(rp), (rp).context))))));
  }

  static java.util.Set<hydra.core.Name> freeVariablesInContext(hydra.graph.Graph cx) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.Set<hydra.core.Name>>>) ((java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.Set<hydra.core.Name>>>) (p0 -> p1 -> hydra.lib.sets.Union.apply(
        p0,
        p1))),
      (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      hydra.lib.lists.Map.apply(
        hydra.Variables::freeVariablesInTypeSchemeSimple,
        hydra.lib.maps.Elems.apply((cx).boundTypes)));
  }

  static hydra.util.Pair<hydra.core.Type, hydra.context.Context> freshVariableType(hydra.context.Context cx) {
    hydra.util.Pair<hydra.core.Name, hydra.context.Context> result = hydra.Names.freshName(cx);
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
        hydra.Inference.isUnbound(
          cx,
          v),
        (isTypeVarName).apply(v))),
      hydra.Variables.freeVariablesInTypeOrdered(typ))));
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

  static <T0> hydra.util.Either<hydra.errors.Error_, T0> headOrFail(String desc, java.util.List<T0> xs) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.errors.Error_, T0>left(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
        "headOrFail: ",
        desc)))),
      (java.util.function.Function<T0, hydra.util.Either<hydra.errors.Error_, T0>>) (x -> hydra.util.Either.<hydra.errors.Error_, T0>right(x)),
      hydra.lib.lists.MaybeHead.apply(xs));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>> inferGraphTypes(hydra.context.Context fcx0, java.util.List<hydra.core.Binding> bindings0, hydra.graph.Graph g0) {
    hydra.util.Lazy<hydra.context.Context> fcx = new hydra.util.Lazy<>(() -> new hydra.context.Context(hydra.lib.lists.Cons.apply(
      "graph inference",
      (fcx0).trace), (fcx0).messages, (fcx0).other));
    java.util.function.Function<hydra.core.Let, hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>> fromLetTerm = (java.util.function.Function<hydra.core.Let, hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>>) (l -> {
      java.util.List<hydra.core.Binding> bindings = (l).bindings;
      java.util.Map<hydra.core.Name, hydra.graph.Primitive> prims = (g0).primitives;
      hydra.util.Lazy<hydra.graph.Graph> rawG = new hydra.util.Lazy<>(() -> hydra.Lexical.buildGraph(
        bindings,
        (java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) ((java.util.Map<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.util.Maybe<hydra.core.Term>>apply())),
        prims));
      java.util.Map<hydra.core.Name, hydra.core.TypeScheme> schemaTypes = (g0).schemaTypes;
      hydra.graph.Graph g = new hydra.graph.Graph(rawG.get().boundTerms, rawG.get().boundTypes, rawG.get().classConstraints, rawG.get().lambdaVariables, rawG.get().metadata, rawG.get().primitives, schemaTypes, rawG.get().typeVariables);
      return (hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>) ((hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>) (new hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>(g, bindings)));
    });
    hydra.core.Let let0 = new hydra.core.Let(bindings0, new hydra.core.Term.Unit());
    return hydra.lib.eithers.Bind.apply(
      hydra.Inference.inferTypeOfTerm(
        fcx.get(),
        g0,
        new hydra.core.Term.Let(let0),
        "graph term"),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>>>) (result -> {
        hydra.context.Context fcx2 = (result).context;
        hydra.core.Term term = (result).term;
        return hydra.lib.eithers.Bind.apply(
          hydra.Inference.finalizeInferredTerm(
            fcx2,
            g0,
            term),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>>>) (finalized -> (finalized).accept(new hydra.core.Term.PartialVisitor<>() {
            @Override
            public hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>> visit(hydra.core.Term.Let l) {
              return hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>((fromLetTerm).apply((l).value), fcx2))));
            }

            @Override
            public hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>> visit(hydra.core.Term.Variable ignored) {
              return hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, java.util.List<hydra.core.Binding>>, hydra.context.Context>>left(new hydra.errors.Error_.Other(new hydra.errors.OtherError("Expected inferred graph as let term")));
            }
          })));
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferInGraphContext(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Term term) {
    return hydra.Inference.inferTypeOfTerm(
      fcx,
      cx,
      term,
      "single term");
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>> inferMany(hydra.context.Context fcx, hydra.graph.Graph cx, java.util.List<hydra.util.Pair<hydra.core.Term, String>> pairs) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.Inference.inferMany_emptyResult(
        fcx,
        hydra.Substitution.idTypeSubst()),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, String>, java.util.List<hydra.util.Pair<hydra.core.Term, String>>>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>>) (pairsUc -> {
        hydra.util.Lazy<hydra.util.Pair<hydra.core.Term, String>> headPair = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pairsUc));
        hydra.util.Lazy<String> desc = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(headPair.get()));
        hydra.util.Lazy<hydra.core.Term> e = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(headPair.get()));
        hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Term, String>>> tl = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pairsUc));
        return hydra.lib.eithers.Bind.apply(
          hydra.Inference.inferTypeOfTerm(
            fcx,
            cx,
            e.get(),
            desc.get()),
          (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>>) (result1 -> {
            java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c1 = (result1).classConstraints;
            hydra.core.Term e1 = (result1).term;
            hydra.context.Context fcx2 = (result1).context;
            hydra.typing.TypeSubst s1 = (result1).subst;
            hydra.core.Type t1 = (result1).type;
            return hydra.lib.eithers.Bind.apply(
              hydra.Inference.inferMany(
                fcx2,
                hydra.Substitution.substInContext(
                  s1,
                  cx),
                tl.get()),
              (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>>) (rp2 -> {
                hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>> result2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(rp2));
                hydra.util.Lazy<hydra.typing.TypeSubst> s2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(result2.get()))));
                java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c1Subst = hydra.Substitution.substInClassConstraints(
                  s2.get(),
                  c1);
                hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> c2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(result2.get()))));
                hydra.util.Lazy<java.util.List<hydra.core.Term>> e2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result2.get()));
                hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(rp2));
                hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> mergedConstraints = new hydra.util.Lazy<>(() -> hydra.Inference.mergeClassConstraints(
                  c1Subst,
                  c2.get()));
                hydra.util.Lazy<java.util.List<hydra.core.Type>> t2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(result2.get())));
                return hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>((hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>) ((hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>) (new hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>(hydra.lib.lists.Cons.apply(
                  hydra.Substitution.substTypesInTerm(
                    s2.get(),
                    e1),
                  e2.get()), (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>(hydra.lib.lists.Cons.apply(
                  hydra.Substitution.substInType(
                    s2.get(),
                    t1),
                  t2.get()), (hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) ((hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (new hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>(hydra.Substitution.composeTypeSubst(
                  s1,
                  s2.get()), mergedConstraints.get()))))))))), fcx3.get()))));
              }));
          }));
      }),
      hydra.lib.lists.Uncons.apply(pairs));
  }

  static <T0, T1, T2, T3, T4> hydra.util.Either<T0, hydra.util.Pair<hydra.util.Pair<java.util.List<T1>, hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>>, hydra.context.Context>> inferMany_emptyResult(hydra.context.Context fcx, hydra.typing.TypeSubst hydra_substitution_idTypeSubst) {
    return hydra.util.Either.<T0, hydra.util.Pair<hydra.util.Pair<java.util.List<T1>, hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.Pair<java.util.List<T1>, hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.Pair<java.util.List<T1>, hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.Pair<java.util.List<T1>, hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>>, hydra.context.Context>((hydra.util.Pair<java.util.List<T1>, hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>>) ((hydra.util.Pair<java.util.List<T1>, hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>>) (new hydra.util.Pair<java.util.List<T1>, hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>>((java.util.List<T1>) (java.util.Collections.<T1>emptyList()), (hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>) ((hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>) (new hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>((java.util.List<T2>) (java.util.Collections.<T2>emptyList()), (hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>) ((hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>) (new hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>(hydra_substitution_idTypeSubst, (java.util.Map<T3, T4>) ((java.util.Map<T3, T4>) (hydra.lib.maps.Empty.<T3, T4>apply()))))))))))), fcx))));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>> inferTypeOf(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Term term) {
    hydra.util.Lazy<hydra.core.Term> letTerm = new hydra.util.Lazy<>(() -> new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("ignoredVariableName"), term, (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("ignoredBody")))));
    return hydra.lib.eithers.Bind.apply(
      hydra.Inference.inferTypeOfTerm(
        fcx,
        cx,
        letTerm.get(),
        "infer type of term"),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>>) (result -> {
        hydra.context.Context fcx2 = (result).context;
        return hydra.lib.eithers.Bind.apply(
          hydra.Inference.finalizeInferredTerm(
            fcx2,
            cx,
            (result).term),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>>) (finalized -> hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.let(
              cx,
              finalized),
            (java.util.function.Function<hydra.core.Let, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>>) (letResult -> {
              java.util.List<hydra.core.Binding> bindings = (letResult).bindings;
              return hydra.lib.logic.IfElse.lazy(
                hydra.lib.equality.Equal.apply(
                  1,
                  hydra.lib.lists.Length.apply(bindings)),
                () -> hydra.lib.eithers.Bind.apply(
                  hydra.Inference.headOrFail(
                    "inferTypeOf: single binding expected",
                    bindings),
                  (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>>) (binding -> {
                    hydra.util.Maybe<hydra.core.TypeScheme> mts = (binding).type;
                    hydra.core.Term term1 = (binding).term;
                    return hydra.lib.maybes.Maybe.applyLazy(
                      () -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>left(new hydra.errors.Error_.Other(new hydra.errors.OtherError("Expected a type scheme"))),
                      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>>) (ts -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>((hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>(term1, ts))), fcx2))))),
                      mts);
                  })),
                () -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>>left(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  "Expected a single binding with a type scheme, but got: ",
                  hydra.lib.literals.ShowInt32.apply(hydra.lib.lists.Length.apply(bindings)),
                  " bindings"))))));
            }))));
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfAnnotatedTerm(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.AnnotatedTerm at) {
    java.util.Map<hydra.core.Name, hydra.core.Term> ann = (at).annotation;
    hydra.core.Term term = (at).body;
    return hydra.lib.eithers.Bind.apply(
      hydra.Inference.inferTypeOfTerm(
        fcx,
        cx,
        term,
        "annotated term"),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (result -> {
        hydra.context.Context fcx2 = (result).context;
        java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> iconstraints = (result).classConstraints;
        hydra.typing.TypeSubst isubst = (result).subst;
        hydra.core.Term iterm = (result).term;
        hydra.core.Type itype = (result).type;
        return hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(new hydra.typing.InferenceResult(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(iterm, ann)), itype, isubst, iconstraints, fcx2));
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfApplication(hydra.context.Context fcx0, hydra.graph.Graph cx, hydra.core.Application app) {
    hydra.core.Term e0 = (app).function;
    hydra.core.Term e1 = (app).argument;
    hydra.util.Lazy<hydra.context.Context> fcx = new hydra.util.Lazy<>(() -> new hydra.context.Context(hydra.lib.lists.Cons.apply(
      "application",
      (fcx0).trace), (fcx0).messages, (fcx0).other));
    return hydra.lib.eithers.Bind.apply(
      hydra.Inference.inferTypeOfTerm(
        fcx.get(),
        cx,
        e0,
        "lhs"),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (lhsResult -> {
        hydra.core.Term a = (lhsResult).term;
        java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c0 = (lhsResult).classConstraints;
        hydra.context.Context fcx2 = (lhsResult).context;
        hydra.typing.TypeSubst s0 = (lhsResult).subst;
        hydra.core.Type t0 = (lhsResult).type;
        return hydra.lib.eithers.Bind.apply(
          hydra.Inference.inferTypeOfTerm(
            fcx2,
            hydra.Substitution.substInContext(
              s0,
              cx),
            e1,
            "rhs"),
          (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (rhsResult -> {
            hydra.core.Term b = (rhsResult).term;
            java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c1 = (rhsResult).classConstraints;
            hydra.context.Context fcx3 = (rhsResult).context;
            hydra.util.Pair<hydra.core.Name, hydra.context.Context> vResult = hydra.Names.freshName(fcx3);
            hydra.util.Lazy<hydra.context.Context> fcx4 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(vResult));
            hydra.typing.TypeSubst s1 = (rhsResult).subst;
            hydra.core.Type t1 = (rhsResult).type;
            hydra.util.Lazy<hydra.core.Name> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(vResult));
            return hydra.lib.eithers.Bind.apply(
              hydra.lib.eithers.Bimap.apply(
                (java.util.function.Function<hydra.errors.UnificationError, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Unification(_e)),
                (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>) (_a -> _a),
                hydra.Unification.unifyTypes(
                  fcx4.get(),
                  (cx).schemaTypes,
                  hydra.Substitution.substInType(
                    s1,
                    t0),
                  new hydra.core.Type.Function(new hydra.core.FunctionType(t1, new hydra.core.Type.Variable(v.get()))),
                  "application lhs")),
              (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (s2 -> hydra.lib.eithers.Bind.apply(
                hydra.Checking.checkTypeSubst(
                  fcx4.get(),
                  cx,
                  s2),
                (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (ignored -> {
                  java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c0Subst = hydra.Substitution.substInClassConstraints(
                    s2,
                    hydra.Substitution.substInClassConstraints(
                      s1,
                      c0));
                  java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c1Subst = hydra.Substitution.substInClassConstraints(
                    s2,
                    c1);
                  hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> rConstraints = new hydra.util.Lazy<>(() -> hydra.Inference.mergeClassConstraints(
                    c0Subst,
                    c1Subst));
                  hydra.core.Term rExpr = new hydra.core.Term.Application(new hydra.core.Application(hydra.Substitution.substTypesInTerm(
                    hydra.Substitution.composeTypeSubst(
                      s1,
                      s2),
                    a), hydra.Substitution.substTypesInTerm(
                    s2,
                    b)));
                  hydra.typing.TypeSubst rSubst = hydra.Substitution.composeTypeSubstList(java.util.Arrays.asList(
                    s0,
                    s1,
                    s2));
                  hydra.core.Type rType = hydra.Substitution.substInType(
                    s2,
                    new hydra.core.Type.Variable(v.get()));
                  return hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(new hydra.typing.InferenceResult(rExpr, rType, rSubst, rConstraints.get(), fcx4.get()));
                }))));
          }));
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfCaseStatement(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.CaseStatement caseStmt) {
    java.util.List<hydra.core.Field> cases = (caseStmt).cases;
    hydra.util.Maybe<hydra.core.Term> dflt = (caseStmt).default_;
    hydra.util.Lazy<java.util.List<hydra.core.Name>> fnames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      projected -> projected.name,
      cases));
    hydra.core.Name tname = (caseStmt).typeName;
    return hydra.lib.eithers.Bind.apply(
      hydra.Resolution.requireSchemaType(
        fcx,
        (cx).schemaTypes,
        tname),
      (java.util.function.Function<hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (stRp -> {
        hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(stRp));
        hydra.util.Lazy<hydra.core.TypeScheme> schemaType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(stRp));
        hydra.core.Type stype = schemaType.get().type;
        java.util.List<hydra.core.Name> svars = schemaType.get().variables;
        return hydra.lib.eithers.Bind.apply(
          hydra.extract.Core.unionType(
            tname,
            stype),
          (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (sfields -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.MapMaybe.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (t -> hydra.Inference.inferTypeOfTerm(
                fcx2.get(),
                cx,
                t,
                hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  "case ",
                  (tname).value,
                  ".<default>")))),
              dflt),
            (java.util.function.Function<hydra.util.Maybe<hydra.typing.InferenceResult>, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (dfltRp -> {
              hydra.util.Maybe<hydra.typing.InferenceResult> dfltResult = dfltRp;
              hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
                () -> fcx2.get(),
                hydra.lib.maybes.Map.apply(
                  projected -> projected.context,
                  dfltRp)));
              return hydra.lib.eithers.Bind.apply(
                hydra.Inference.inferMany(
                  fcx3.get(),
                  cx,
                  hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.core.Field, hydra.util.Pair<hydra.core.Term, String>>) (f -> (hydra.util.Pair<hydra.core.Term, String>) ((hydra.util.Pair<hydra.core.Term, String>) (new hydra.util.Pair<hydra.core.Term, String>((f).term, hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                      "case ",
                      (tname).value,
                      ".",
                      (f).name.value)))))),
                    cases)),
                (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (caseRp -> {
                  hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>> caseResults = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(caseRp));
                  hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> caseElemConstraints = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(caseResults.get()))));
                  hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> dfltClassConstraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
                    () -> (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())),
                    hydra.lib.maybes.Map.apply(
                      projected -> projected.classConstraints,
                      dfltResult)));
                  hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> allElemConstraints = new hydra.util.Lazy<>(() -> hydra.Inference.mergeClassConstraints(
                    caseElemConstraints.get(),
                    dfltClassConstraints.get()));
                  hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Type>> caseMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.core.FieldType, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>) (ft -> (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>((ft).name, (ft).type)))),
                    sfields)));
                  hydra.util.Lazy<hydra.context.Context> fcx4 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(caseRp));
                  hydra.util.Pair<hydra.core.Name, hydra.context.Context> codvResult = hydra.Names.freshName(fcx4.get());
                  hydra.util.Lazy<hydra.core.Name> codv = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(codvResult));
                  hydra.core.Type cod = new hydra.core.Type.Variable(codv.get());
                  hydra.util.Lazy<java.util.List<hydra.core.Type>> itypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(caseResults.get())));
                  hydra.util.Lazy<java.util.List<hydra.typing.TypeConstraint>> caseConstraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(hydra.lib.lists.ZipWith.apply(
                    (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.typing.TypeConstraint>>>) (fname -> (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.typing.TypeConstraint>>) (itype -> hydra.lib.maybes.Map.apply(
                      (java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>) (ftype -> new hydra.typing.TypeConstraint(itype, new hydra.core.Type.Function(new hydra.core.FunctionType(ftype, cod)), "case type")),
                      hydra.lib.maps.Lookup.apply(
                        fname,
                        caseMap.get())))),
                    fnames.get(),
                    itypes.get())));
                  hydra.util.Lazy<hydra.typing.TypeSubst> isubst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(caseResults.get()))));
                  hydra.util.Lazy<java.util.List<hydra.typing.TypeConstraint>> dfltConstraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.ToList.apply(hydra.lib.maybes.Map.apply(
                    (java.util.function.Function<hydra.typing.InferenceResult, hydra.typing.TypeConstraint>) (r -> new hydra.typing.TypeConstraint(cod, hydra.Substitution.substInType(
                      isubst.get(),
                      (r).type), "match default")),
                    dfltResult)));
                  hydra.util.Lazy<hydra.context.Context> fcx5 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(codvResult));
                  hydra.util.Lazy<java.util.List<hydra.core.Term>> iterms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(caseResults.get()));
                  return hydra.lib.eithers.Bind.apply(
                    hydra.Inference.mapConstraints(
                      fcx5.get(),
                      cx,
                      (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst -> hydra.Inference.yieldWithConstraints(
                        fcx5.get(),
                        hydra.Inference.buildTypeApplicationTerm(
                          svars,
                          new hydra.core.Term.Cases(new hydra.core.CaseStatement(tname, hydra.lib.maybes.Map.apply(
                            projected -> projected.term,
                            dfltResult), hydra.lib.lists.ZipWith.apply(
                            (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.core.Field>>) (n -> (java.util.function.Function<hydra.core.Term, hydra.core.Field>) (t -> new hydra.core.Field(n, t))),
                            fnames.get(),
                            iterms.get())))),
                        new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.Resolution.nominalApplication(
                          tname,
                          hydra.lib.lists.Map.apply(
                            (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
                            svars)), cod)),
                        hydra.Substitution.composeTypeSubstList(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
                          hydra.lib.maybes.ToList.apply(hydra.lib.maybes.Map.apply(
                            projected -> projected.subst,
                            dfltResult)),
                          java.util.Arrays.asList(
                            isubst.get(),
                            subst)))),
                        hydra.Substitution.substInClassConstraints(
                          subst,
                          allElemConstraints.get()))),
                      hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
                        dfltConstraints.get(),
                        caseConstraints.get()))),
                    (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (mcResult -> hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(mcResult)));
                }));
            }))));
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfCollection(hydra.context.Context fcx, hydra.graph.Graph cx, java.util.function.Function<hydra.core.Type, hydra.core.Type> typCons, java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term> trmCons, String desc, java.util.Set<hydra.core.Name> classNames, java.util.List<hydra.core.Term> els) {
    hydra.util.Pair<hydra.core.Name, hydra.context.Context> varResult = hydra.Names.freshName(fcx);
    hydra.util.Lazy<hydra.core.Name> var = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(varResult));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> classConstraints = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.sets.Null.apply(classNames),
      () -> (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())),
      () -> hydra.lib.maps.Singleton.apply(
        var.get(),
        new hydra.core.TypeVariableMetadata(classNames))));
    hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(varResult));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(els),
      () -> hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(hydra.Inference.yieldWithConstraints(
        fcx2.get(),
        hydra.Inference.buildTypeApplicationTerm(
          java.util.Arrays.asList(var.get()),
          (trmCons).apply((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList()))),
        (typCons).apply(new hydra.core.Type.Variable(var.get())),
        hydra.Substitution.idTypeSubst(),
        classConstraints.get())),
      () -> hydra.lib.eithers.Bind.apply(
        hydra.Inference.inferMany(
          fcx2.get(),
          cx,
          hydra.lib.lists.Zip.apply(
            els,
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<Integer, String>) (i -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                "#",
                hydra.lib.literals.ShowInt32.apply(i)))),
              hydra.lib.math.Range.apply(
                1,
                hydra.lib.math.Add.apply(
                  hydra.lib.lists.Length.apply(els),
                  1))))),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (resultsRp -> {
          hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>> results = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(resultsRp));
          hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> elemConstraints = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(results.get()))));
          hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> allConstraints = new hydra.util.Lazy<>(() -> hydra.Inference.mergeClassConstraints(
            classConstraints.get(),
            elemConstraints.get()));
          hydra.util.Lazy<java.util.List<hydra.core.Type>> types = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(results.get())));
          hydra.util.Lazy<java.util.List<hydra.typing.TypeConstraint>> constraints = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>) (t -> new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(var.get()), t, desc)),
            types.get()));
          hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(resultsRp));
          hydra.util.Lazy<hydra.typing.TypeSubst> subst1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(results.get()))));
          hydra.util.Lazy<java.util.List<hydra.core.Term>> terms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(results.get()));
          return hydra.lib.eithers.Bind.apply(
            hydra.Inference.mapConstraints(
              fcx3.get(),
              cx,
              (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst2 -> {
                hydra.typing.TypeSubst isubst = hydra.Substitution.composeTypeSubst(
                  subst1.get(),
                  subst2);
                hydra.core.Term iterm = (trmCons).apply(terms.get());
                hydra.core.Type itype = (typCons).apply(new hydra.core.Type.Variable(var.get()));
                return hydra.Inference.yieldWithConstraints(
                  fcx3.get(),
                  iterm,
                  itype,
                  isubst,
                  hydra.Substitution.substInClassConstraints(
                    subst2,
                    allConstraints.get()));
              }),
              constraints.get()),
            (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (mcResult -> hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(mcResult)));
        })));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfEither(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.util.Either<hydra.core.Term, hydra.core.Term> e) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (l -> hydra.lib.eithers.Bind.apply(
        hydra.Inference.inferTypeOfTerm(
          fcx,
          cx,
          l,
          "either left value"),
        (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (r1 -> {
          hydra.core.Term iterm = (r1).term;
          hydra.util.Lazy<hydra.core.Term> eitherTerm = new hydra.util.Lazy<>(() -> new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(iterm)));
          hydra.core.Type leftType = (r1).type;
          hydra.context.Context fcx2 = (r1).context;
          hydra.util.Pair<hydra.core.Type, hydra.context.Context> fvResult = hydra.Inference.freshVariableType(fcx2);
          hydra.util.Lazy<hydra.core.Type> rightType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(fvResult));
          hydra.core.Type eitherType = new hydra.core.Type.Either(new hydra.core.EitherType(leftType, rightType.get()));
          hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(fvResult));
          hydra.typing.TypeSubst subst = (r1).subst;
          hydra.core.Term termWithLeftType = new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(eitherTerm.get(), leftType));
          hydra.core.Term termWithBothTypes = new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(termWithLeftType, rightType.get()));
          return hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(hydra.Inference.yieldChecked(
            fcx3.get(),
            termWithBothTypes,
            eitherType,
            subst));
        }))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (r -> hydra.lib.eithers.Bind.apply(
        hydra.Inference.inferTypeOfTerm(
          fcx,
          cx,
          r,
          "either right value"),
        (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (r1 -> {
          hydra.core.Term iterm = (r1).term;
          hydra.util.Lazy<hydra.core.Term> eitherTerm = new hydra.util.Lazy<>(() -> new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(iterm)));
          hydra.context.Context fcx2 = (r1).context;
          hydra.util.Pair<hydra.core.Type, hydra.context.Context> fvResult = hydra.Inference.freshVariableType(fcx2);
          hydra.util.Lazy<hydra.core.Type> leftType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(fvResult));
          hydra.core.Type rightType = (r1).type;
          hydra.core.Type eitherType = new hydra.core.Type.Either(new hydra.core.EitherType(leftType.get(), rightType));
          hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(fvResult));
          hydra.typing.TypeSubst subst = (r1).subst;
          hydra.core.Term termWithLeftType = new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(eitherTerm.get(), leftType.get()));
          hydra.core.Term termWithBothTypes = new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(termWithLeftType, rightType));
          return hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(hydra.Inference.yieldChecked(
            fcx3.get(),
            termWithBothTypes,
            eitherType,
            subst));
        }))),
      e);
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfInjection(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Injection injection) {
    hydra.core.Field field = (injection).field;
    hydra.core.Name fname = (field).name;
    hydra.core.Term term = (field).term;
    hydra.core.Name tname = (injection).typeName;
    return hydra.lib.eithers.Bind.apply(
      hydra.Inference.inferTypeOfTerm(
        fcx,
        cx,
        term,
        "injected term"),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (result -> {
        hydra.context.Context fcx2 = (result).context;
        return hydra.lib.eithers.Bind.apply(
          hydra.Resolution.requireSchemaType(
            fcx2,
            (cx).schemaTypes,
            tname),
          (java.util.function.Function<hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (stRp -> {
            hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(stRp));
            hydra.typing.TypeSubst isubst = (result).subst;
            hydra.core.Term iterm = (result).term;
            hydra.core.Type ityp = (result).type;
            hydra.util.Lazy<hydra.core.TypeScheme> schemaType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(stRp));
            hydra.core.Type stype = schemaType.get().type;
            java.util.List<hydra.core.Name> svars = schemaType.get().variables;
            return hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.unionType(
                tname,
                stype),
              (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (sfields -> hydra.lib.eithers.Bind.apply(
                hydra.Resolution.findFieldType(
                  fcx3.get(),
                  fname,
                  sfields),
                (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (ftyp -> hydra.lib.eithers.Bind.apply(
                  hydra.Inference.mapConstraints(
                    fcx3.get(),
                    cx,
                    (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst -> hydra.Inference.yield(
                      fcx3.get(),
                      hydra.Inference.buildTypeApplicationTerm(
                        svars,
                        new hydra.core.Term.Inject(new hydra.core.Injection(tname, new hydra.core.Field(fname, iterm)))),
                      hydra.Resolution.nominalApplication(
                        tname,
                        hydra.lib.lists.Map.apply(
                          (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
                          svars)),
                      hydra.Substitution.composeTypeSubst(
                        isubst,
                        subst))),
                    java.util.Arrays.asList(new hydra.typing.TypeConstraint(ftyp, ityp, "schema type of injected field"))),
                  (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (mcResult -> hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(mcResult)))))));
          }));
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfLambda(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Lambda lambda) {
    hydra.core.Term body = (lambda).body;
    hydra.util.Pair<hydra.core.Name, hydra.context.Context> vdomResult = hydra.Names.freshName(fcx);
    hydra.util.Lazy<hydra.core.Name> vdom = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(vdomResult));
    hydra.core.Type dom = new hydra.core.Type.Variable(vdom.get());
    hydra.core.Name var = (lambda).parameter;
    hydra.util.Lazy<hydra.graph.Graph> cx2 = new hydra.util.Lazy<>(() -> hydra.Inference.extendContext(
      java.util.Arrays.asList((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(var, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), dom, (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
      cx));
    hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(vdomResult));
    return hydra.lib.eithers.Bind.apply(
      hydra.Inference.inferTypeOfTerm(
        fcx2.get(),
        cx2.get(),
        body,
        "lambda body"),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (result -> {
        hydra.typing.TypeSubst isubst = (result).subst;
        hydra.graph.Graph cx3 = hydra.Substitution.substInContext(
          isubst,
          cx);
        hydra.context.Context fcx3 = (result).context;
        hydra.core.Type icod = (result).type;
        java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> iconstraints = hydra.Substitution.substInClassConstraints(
          isubst,
          (result).classConstraints);
        hydra.core.Term iterm = (result).term;
        hydra.core.Type rdom = hydra.Substitution.substInType(
          isubst,
          dom);
        hydra.core.Term rterm = new hydra.core.Term.Lambda(new hydra.core.Lambda(var, hydra.util.Maybe.just(rdom), iterm));
        hydra.core.Type rtype = new hydra.core.Type.Function(new hydra.core.FunctionType(rdom, icod));
        hydra.util.Lazy<java.util.Set<hydra.core.Name>> vars = new hydra.util.Lazy<>(() -> hydra.lib.sets.Unions.apply(java.util.Arrays.asList(
          hydra.Variables.freeVariablesInType(rdom),
          hydra.Variables.freeVariablesInType(icod),
          hydra.Inference.freeVariablesInContext(hydra.Substitution.substInContext(
            isubst,
            cx2.get())))));
        return hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(new hydra.typing.InferenceResult(rterm, rtype, isubst, iconstraints, fcx3));
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfLet(hydra.context.Context fcx0, hydra.graph.Graph cx, hydra.core.Let let0) {
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
        hydra.lib.sets.ToList.apply(hydra.Variables.freeVariablesInTerm(term))))));
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
    hydra.util.Lazy<java.util.List<java.util.List<hydra.core.Name>>> groups = new hydra.util.Lazy<>(() -> hydra.Sorting.topologicalSortComponents(adjList.get()));
    hydra.util.Lazy<hydra.core.Term> rewrittenLet = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
      createLet,
      body0,
      hydra.lib.lists.Reverse.apply(groups.get())));
    hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> res = rewrittenLet.get().accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> otherwise(hydra.core.Term instance) {
        return hydra.Inference.inferTypeOfTerm(
          fcx.get(),
          cx,
          rewrittenLet.get(),
          "empty let term");
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Let l) {
        return hydra.Inference.inferTypeOfLetNormalized(
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
            java.util.List<hydra.core.Binding> bs = (l).value.bindings;
            hydra.core.Term letBody = (l).value.body;
            return helper.get().apply(hydra.lib.math.Sub.apply(
              level,
              1)).apply(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              bs,
              bins))).apply(letBody);
          }
        }));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            level,
            0),
          () -> (hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>) ((hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>) (new hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>(bins, term))),
          () -> (nonzero).apply(term));
      }))));
      hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Binding>, hydra.core.Term>> result = new hydra.util.Lazy<>(() -> helper.get().apply(hydra.lib.lists.Length.apply(groups.get())).apply((java.util.List<hydra.core.Binding>) (java.util.Collections.<hydra.core.Binding>emptyList())).apply(iterm));
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

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfLetNormalized(hydra.context.Context fcx0, hydra.graph.Graph cx0, hydra.core.Let letTerm) {
    java.util.List<hydra.core.Binding> bins0 = (letTerm).bindings;
    hydra.util.Lazy<java.util.List<hydra.core.Name>> bnames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      projected -> projected.name,
      bins0));
    hydra.core.Term body0 = (letTerm).body;
    hydra.util.Lazy<hydra.context.Context> fcx = new hydra.util.Lazy<>(() -> new hydra.context.Context(hydra.lib.lists.Cons.apply(
      "let-normalized",
      (fcx0).trace), (fcx0).messages, (fcx0).other));
    hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Name>, hydra.context.Context>> bvarsResult = new hydra.util.Lazy<>(() -> hydra.Names.freshNames(
      hydra.lib.lists.Length.apply(bins0),
      fcx.get()));
    hydra.util.Lazy<java.util.List<hydra.core.Name>> bvars = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bvarsResult.get()));
    hydra.util.Lazy<java.util.List<hydra.core.Type>> tbins0 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
      bvars.get()));
    hydra.util.Lazy<hydra.graph.Graph> cx1 = new hydra.util.Lazy<>(() -> hydra.Inference.extendContext(
      hydra.lib.lists.Zip.apply(
        bnames.get(),
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>) (t -> new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), t, (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))),
          tbins0.get())),
      cx0));
    hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bvarsResult.get()));
    return hydra.lib.eithers.Bind.apply(
      hydra.Inference.inferTypesOfTemporaryBindings(
        fcx2.get(),
        cx1.get(),
        bins0),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (irRp -> {
        hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>> inferredResult = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(irRp));
        hydra.util.Lazy<java.util.List<hydra.core.Term>> bterms1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(inferredResult.get()));
        hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(irRp));
        hydra.util.Lazy<hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>> substAndConstraints = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(inferredResult.get())));
        hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> inferredConstraints = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(substAndConstraints.get()));
        hydra.util.Lazy<hydra.typing.TypeSubst> s1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(substAndConstraints.get()));
        hydra.util.Lazy<java.util.List<hydra.core.Type>> tbins1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(inferredResult.get())));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.Bimap.apply(
            (java.util.function.Function<hydra.errors.UnificationError, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Unification(_e)),
            (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>) (_a -> _a),
            hydra.Unification.unifyTypeLists(
              fcx3.get(),
              (cx0).schemaTypes,
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (v1 -> hydra.Substitution.substInType(
                  s1.get(),
                  v1)),
                tbins0.get()),
              tbins1.get(),
              "temporary type bindings")),
          (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (s2 -> hydra.lib.eithers.Bind.apply(
            hydra.Checking.checkTypeSubst(
              fcx3.get(),
              cx0,
              s2),
            (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (ignored -> {
              java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> constraintsWithS2 = hydra.Substitution.substInClassConstraints(
                s2,
                inferredConstraints.get());
              hydra.typing.TypeSubst composedSubst = hydra.Substitution.composeTypeSubst(
                s1.get(),
                s2);
              hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> originalBindingConstraints = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, java.util.function.Function<hydra.core.Binding, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (acc -> (java.util.function.Function<hydra.core.Binding, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (b -> hydra.lib.maybes.Maybe.applyLazy(
                  () -> acc,
                  (java.util.function.Function<hydra.core.TypeScheme, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (ts -> hydra.lib.maybes.Maybe.applyLazy(
                    () -> acc,
                    (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (c -> hydra.Inference.mergeClassConstraints(
                      acc,
                      c)),
                    (ts).constraints)),
                  (b).type))),
                (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())),
                bins0));
              java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> originalConstraintsSubst = hydra.Substitution.substInClassConstraints(
                composedSubst,
                originalBindingConstraints.get());
              hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> allInferredConstraints = new hydra.util.Lazy<>(() -> hydra.Inference.mergeClassConstraints(
                constraintsWithS2,
                originalConstraintsSubst));
              hydra.util.Lazy<java.util.List<hydra.core.Term>> bterms1Subst = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v1 -> hydra.Substitution.substTypesInTerm(
                  s2,
                  v1)),
                bterms1.get()));
              hydra.graph.Graph g2base = hydra.Substitution.substInContext(
                hydra.Substitution.composeTypeSubst(
                  s1.get(),
                  s2),
                cx0);
              hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> mergedConstraints = new hydra.util.Lazy<>(() -> hydra.Inference.mergeClassConstraints(
                (g2base).classConstraints,
                allInferredConstraints.get()));
              hydra.graph.Graph g2 = new hydra.graph.Graph((g2base).boundTerms, (g2base).boundTypes, mergedConstraints.get(), (g2base).lambdaVariables, (g2base).metadata, (g2base).primitives, (g2base).schemaTypes, (g2base).typeVariables);
              hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>> tsbins1 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Zip.apply(
                bnames.get(),
                hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>) (t -> hydra.Inference.generalize(
                    g2,
                    hydra.Substitution.substInType(
                      s2,
                      t))),
                  tbins1.get())));
              return hydra.lib.eithers.Bind.apply(
                hydra.Inference.inferTypeOfTerm(
                  fcx3.get(),
                  hydra.Inference.extendContext(
                    tsbins1.get(),
                    g2),
                  body0,
                  "let body"),
                (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (bodyResult -> {
                  hydra.typing.TypeSubst sbody = (bodyResult).subst;
                  java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> bindingConstraintsSubst = hydra.Substitution.substInClassConstraints(
                    sbody,
                    constraintsWithS2);
                  java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> bodyConstraints = hydra.Substitution.substInClassConstraints(
                    sbody,
                    (bodyResult).classConstraints);
                  hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> allConstraints = new hydra.util.Lazy<>(() -> hydra.Inference.mergeClassConstraints(
                    bindingConstraintsSubst,
                    bodyConstraints));
                  hydra.util.Lazy<hydra.typing.TermSubst> st1 = new hydra.util.Lazy<>(() -> new hydra.typing.TermSubst(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>, hydra.util.Pair<hydra.core.Name, hydra.core.Term>>) (pair -> {
                      hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
                      hydra.util.Lazy<hydra.core.TypeScheme> ts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
                      return (hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>(name.get(), hydra.Inference.buildTypeApplicationTerm(
                        ts.get().variables,
                        new hydra.core.Term.Variable(name.get())))));
                    }),
                    tsbins1.get()))));
                  java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>, hydra.core.Term>, hydra.core.Binding> createBinding = (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>, hydra.core.Term>, hydra.core.Binding>) (bindingPair -> {
                    hydra.util.Lazy<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>> nameTsPair = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(bindingPair));
                    hydra.util.Lazy<hydra.core.TypeScheme> ts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nameTsPair.get()));
                    hydra.core.TypeScheme finalTs = hydra.Substitution.substInTypeScheme(
                      sbody,
                      ts.get());
                    hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(nameTsPair.get()));
                    hydra.util.Lazy<hydra.core.Term> term = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(bindingPair));
                    hydra.util.Lazy<hydra.core.Term> typeLambdaTerm = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
                      (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Name, hydra.core.Term>>) (b -> (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (v -> new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(v, b)))),
                      hydra.Substitution.substituteInTerm(
                        st1.get(),
                        term.get()),
                      hydra.lib.lists.Reverse.apply((finalTs).variables)));
                    return new hydra.core.Binding(name.get(), hydra.Substitution.substTypesInTerm(
                      hydra.Substitution.composeTypeSubst(
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
                  return hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(new hydra.typing.InferenceResult(new hydra.core.Term.Let(new hydra.core.Let(bins1.get(), body1)), tbody, hydra.Substitution.composeTypeSubstList(java.util.Arrays.asList(
                    s1.get(),
                    s2,
                    sbody)), allConstraints.get(), fcx4));
                }));
            }))));
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfList(hydra.context.Context fcx, hydra.graph.Graph cx, java.util.List<hydra.core.Term> v1) {
    return hydra.Inference.inferTypeOfCollection(
      fcx,
      cx,
      (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> new hydra.core.Type.List(x)),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (x -> new hydra.core.Term.List(x)),
      "list element",
      (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      v1);
  }

  static hydra.typing.InferenceResult inferTypeOfLiteral(hydra.context.Context fcx, hydra.core.Literal lit) {
    return new hydra.typing.InferenceResult(new hydra.core.Term.Literal(lit), new hydra.core.Type.Literal(hydra.Reflect.literalType(lit)), hydra.Substitution.idTypeSubst(), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), fcx);
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfMap(hydra.context.Context fcx, hydra.graph.Graph cx, java.util.Map<hydra.core.Term, hydra.core.Term> m) {
    hydra.util.Pair<hydra.core.Name, hydra.context.Context> kvarResult = hydra.Names.freshName(fcx);
    hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(kvarResult));
    hydra.util.Pair<hydra.core.Name, hydra.context.Context> vvarResult = hydra.Names.freshName(fcx2.get());
    hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(vvarResult));
    hydra.util.Lazy<hydra.core.Name> kvar = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(kvarResult));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> keyConstraints = new hydra.util.Lazy<>(() -> hydra.lib.maps.Singleton.apply(
      kvar.get(),
      new hydra.core.TypeVariableMetadata(hydra.lib.sets.Singleton.apply(new hydra.core.Name("ordering")))));
    hydra.util.Lazy<hydra.core.Name> vvar = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(vvarResult));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.maps.Null.apply(m),
      () -> hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(hydra.Inference.yieldWithConstraints(
        fcx3.get(),
        hydra.Inference.buildTypeApplicationTerm(
          java.util.Arrays.asList(
            kvar.get(),
            vvar.get()),
          new hydra.core.Term.Map((java.util.Map<hydra.core.Term, hydra.core.Term>) ((java.util.Map<hydra.core.Term, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Term, hydra.core.Term>apply())))),
        new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(kvar.get()), new hydra.core.Type.Variable(vvar.get()))),
        hydra.Substitution.idTypeSubst(),
        keyConstraints.get())),
      () -> hydra.lib.eithers.Bind.apply(
        hydra.Inference.inferMany(
          fcx3.get(),
          cx,
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.core.Term, String>>) (k -> (hydra.util.Pair<hydra.core.Term, String>) ((hydra.util.Pair<hydra.core.Term, String>) (new hydra.util.Pair<hydra.core.Term, String>(k, "map key")))),
            hydra.lib.maps.Keys.apply(m))),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (kRp -> {
          hydra.util.Lazy<hydra.context.Context> fcx4 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(kRp));
          hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>> kResults = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(kRp));
          hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> kElemConstraints = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(kResults.get()))));
          hydra.util.Lazy<hydra.typing.TypeSubst> ksubst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(kResults.get()))));
          hydra.util.Lazy<java.util.List<hydra.core.Term>> kterms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(kResults.get()));
          hydra.util.Lazy<java.util.List<hydra.core.Type>> ktypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(kResults.get())));
          return hydra.lib.eithers.Bind.apply(
            hydra.Inference.inferMany(
              fcx4.get(),
              hydra.Substitution.substInContext(
                ksubst.get(),
                cx),
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.core.Term, String>>) (v -> (hydra.util.Pair<hydra.core.Term, String>) ((hydra.util.Pair<hydra.core.Term, String>) (new hydra.util.Pair<hydra.core.Term, String>(v, "map value")))),
                hydra.lib.maps.Elems.apply(m))),
            (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (vRp -> {
              hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>> vResults = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(vRp));
              hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> vElemConstraints = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(vResults.get()))));
              hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> allMapConstraints = new hydra.util.Lazy<>(() -> hydra.Inference.mergeClassConstraints(
                keyConstraints.get(),
                hydra.Inference.mergeClassConstraints(
                  kElemConstraints.get(),
                  vElemConstraints.get())));
              hydra.util.Lazy<hydra.context.Context> fcx5 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(vRp));
              hydra.util.Lazy<java.util.List<hydra.typing.TypeConstraint>> kcons = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>) (t -> new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(kvar.get()), t, "map key")),
                ktypes.get()));
              hydra.util.Lazy<java.util.List<hydra.core.Type>> vtypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(vResults.get())));
              hydra.util.Lazy<java.util.List<hydra.typing.TypeConstraint>> vcons = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Type, hydra.typing.TypeConstraint>) (t -> new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(vvar.get()), t, "map value")),
                vtypes.get()));
              hydra.util.Lazy<hydra.typing.TypeSubst> vsubst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(vResults.get()))));
              hydra.util.Lazy<java.util.List<hydra.core.Term>> vterms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(vResults.get()));
              return hydra.lib.eithers.Bind.apply(
                hydra.Inference.mapConstraints(
                  fcx5.get(),
                  cx,
                  (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst -> hydra.Inference.yieldWithConstraints(
                    fcx5.get(),
                    new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(hydra.lib.lists.Zip.apply(
                      kterms.get(),
                      vterms.get()))),
                    new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(kvar.get()), new hydra.core.Type.Variable(vvar.get()))),
                    hydra.Substitution.composeTypeSubstList(java.util.Arrays.asList(
                      ksubst.get(),
                      vsubst.get(),
                      subst)),
                    hydra.Substitution.substInClassConstraints(
                      subst,
                      allMapConstraints.get()))),
                  hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
                    kcons.get(),
                    vcons.get()))),
                (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (mcResult -> hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(mcResult)));
            }));
        })));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfOptional(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.util.Maybe<hydra.core.Term> m) {
    java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term> trmCons = (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (terms -> new hydra.core.Term.Maybe(hydra.lib.lists.MaybeHead.apply(terms)));
    return hydra.Inference.inferTypeOfCollection(
      fcx,
      cx,
      (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> new hydra.core.Type.Maybe(x)),
      trmCons,
      "optional element",
      (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      hydra.lib.maybes.Maybe.applyLazy(
        () -> (java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList()),
        (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (hydra.lib.lists.Singleton::apply),
        m));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfPair(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.util.Pair<hydra.core.Term, hydra.core.Term> p) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Inference.inferMany(
        fcx,
        cx,
        java.util.Arrays.asList(
          (hydra.util.Pair<hydra.core.Term, String>) ((hydra.util.Pair<hydra.core.Term, String>) (new hydra.util.Pair<hydra.core.Term, String>(hydra.lib.pairs.First.apply(p), "pair first element"))),
          (hydra.util.Pair<hydra.core.Term, String>) ((hydra.util.Pair<hydra.core.Term, String>) (new hydra.util.Pair<hydra.core.Term, String>(hydra.lib.pairs.Second.apply(p), "pair second element"))))),
      (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (rp -> {
        hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(rp));
        hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>> results = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(rp));
        hydra.util.Lazy<hydra.typing.TypeSubst> isubst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(results.get()))));
        hydra.util.Lazy<java.util.List<hydra.core.Term>> iterms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(results.get()));
        hydra.util.Lazy<java.util.List<hydra.core.Type>> itypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(results.get())));
        hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> pairElemConstraints = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(results.get()))));
        return hydra.lib.eithers.Bind.apply(
          hydra.Inference.atOrFail(
            0,
            "inferTypeOfPair ifst",
            iterms.get()),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (ifst -> hydra.lib.eithers.Bind.apply(
            hydra.Inference.atOrFail(
              1,
              "inferTypeOfPair isnd",
              iterms.get()),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (isnd -> hydra.lib.eithers.Bind.apply(
              hydra.Inference.atOrFail(
                0,
                "inferTypeOfPair tyFst",
                itypes.get()),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (tyFst -> hydra.lib.eithers.Bind.apply(
                hydra.Inference.atOrFail(
                  1,
                  "inferTypeOfPair tySnd",
                  itypes.get()),
                (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (tySnd -> {
                  hydra.util.Lazy<hydra.core.Term> pairTerm = new hydra.util.Lazy<>(() -> new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(ifst, isnd)))));
                  hydra.core.Term termWithTypes = new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(pairTerm.get(), tyFst)), tySnd));
                  return hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(hydra.Inference.yieldWithConstraints(
                    fcx2.get(),
                    termWithTypes,
                    new hydra.core.Type.Pair(new hydra.core.PairType(tyFst, tySnd)),
                    isubst.get(),
                    pairElemConstraints.get()));
                }))))))));
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfPrimitive(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Name name) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>left(new hydra.errors.Error_.Resolution(new hydra.errors.ResolutionError.NoSuchPrimitive(new hydra.errors.NoSuchPrimitiveError(name)))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (scheme -> {
        hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context> tsResult = hydra.Resolution.instantiateTypeScheme(
          fcx,
          scheme);
        hydra.util.Lazy<hydra.core.TypeScheme> ts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tsResult));
        hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> constraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
          () -> (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())),
          ts.get().constraints));
        hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(tsResult));
        return hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(hydra.Inference.yieldCheckedWithConstraints(
          fcx2.get(),
          hydra.Inference.buildTypeApplicationTerm(
            ts.get().variables,
            new hydra.core.Term.Variable(name)),
          ts.get().type,
          hydra.Substitution.idTypeSubst(),
          constraints.get()));
      }),
      hydra.lib.maybes.Map.apply(
        projected -> projected.type,
        hydra.lib.maps.Lookup.apply(
          name,
          (cx).primitives)));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfProjection(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Projection proj) {
    hydra.core.Name fname = (proj).field;
    hydra.core.Name tname = (proj).typeName;
    return hydra.lib.eithers.Bind.apply(
      hydra.Resolution.requireSchemaType(
        fcx,
        (cx).schemaTypes,
        tname),
      (java.util.function.Function<hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (stRp -> {
        hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(stRp));
        hydra.util.Lazy<hydra.core.TypeScheme> schemaType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(stRp));
        hydra.core.Type stype = schemaType.get().type;
        java.util.List<hydra.core.Name> svars = schemaType.get().variables;
        return hydra.lib.eithers.Bind.apply(
          hydra.extract.Core.recordType(
            tname,
            stype),
          (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (sfields -> hydra.lib.eithers.Bind.apply(
            hydra.Resolution.findFieldType(
              fcx2.get(),
              fname,
              sfields),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (ftyp -> hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(hydra.Inference.yield(
              fcx2.get(),
              hydra.Inference.buildTypeApplicationTerm(
                svars,
                new hydra.core.Term.Project(new hydra.core.Projection(tname, fname))),
              new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.Resolution.nominalApplication(
                tname,
                hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
                  svars)), ftyp)),
              hydra.Substitution.idTypeSubst()))))));
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfRecord(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Record record) {
    java.util.List<hydra.core.Field> fields = (record).fields;
    hydra.util.Lazy<java.util.List<hydra.core.Name>> fnames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      projected -> projected.name,
      fields));
    hydra.core.Name tname = (record).typeName;
    return hydra.lib.eithers.Bind.apply(
      hydra.Resolution.requireSchemaType(
        fcx,
        (cx).schemaTypes,
        tname),
      (java.util.function.Function<hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (stRp -> {
        hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(stRp));
        hydra.util.Lazy<hydra.core.TypeScheme> schemaType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(stRp));
        return hydra.lib.eithers.Bind.apply(
          hydra.Inference.inferMany(
            fcx2.get(),
            cx,
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Field, hydra.util.Pair<hydra.core.Term, String>>) (f -> (hydra.util.Pair<hydra.core.Term, String>) ((hydra.util.Pair<hydra.core.Term, String>) (new hydra.util.Pair<hydra.core.Term, String>((f).term, hydra.lib.strings.Cat2.apply(
                "field ",
                (f).name.value))))),
              fields)),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (rp -> {
            hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(rp));
            hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>> results = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(rp));
            hydra.util.Lazy<hydra.typing.TypeSubst> isubst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(results.get()))));
            hydra.util.Lazy<java.util.List<hydra.core.Term>> iterms = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(results.get()));
            hydra.util.Lazy<java.util.List<hydra.core.Type>> itypes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(results.get())));
            hydra.util.Lazy<hydra.core.Type> ityp = new hydra.util.Lazy<>(() -> new hydra.core.Type.Record(hydra.lib.lists.ZipWith.apply(
              (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Type, hydra.core.FieldType>>) (n -> (java.util.function.Function<hydra.core.Type, hydra.core.FieldType>) (t -> new hydra.core.FieldType(n, t))),
              fnames.get(),
              itypes.get())));
            hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> recElemConstraints = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(results.get()))));
            hydra.core.Type stype = schemaType.get().type;
            java.util.List<hydra.core.Name> svars = schemaType.get().variables;
            return hydra.lib.eithers.Bind.apply(
              hydra.Inference.mapConstraints(
                fcx3.get(),
                cx,
                (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst -> hydra.Inference.yieldWithConstraints(
                  fcx3.get(),
                  hydra.Inference.buildTypeApplicationTerm(
                    svars,
                    new hydra.core.Term.Record(new hydra.core.Record(tname, hydra.lib.lists.ZipWith.apply(
                      (java.util.function.Function<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.core.Field>>) (n -> (java.util.function.Function<hydra.core.Term, hydra.core.Field>) (t -> new hydra.core.Field(n, t))),
                      fnames.get(),
                      iterms.get())))),
                  hydra.Resolution.nominalApplication(
                    tname,
                    hydra.lib.lists.Map.apply(
                      (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
                      svars)),
                  hydra.Substitution.composeTypeSubst(
                    isubst.get(),
                    subst),
                  hydra.Substitution.substInClassConstraints(
                    subst,
                    recElemConstraints.get()))),
                java.util.Arrays.asList(new hydra.typing.TypeConstraint(stype, ityp.get(), "schema type of record"))),
              (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (mcResult -> hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(mcResult)));
          }));
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfSet(hydra.context.Context fcx, hydra.graph.Graph cx, java.util.Set<hydra.core.Term> s) {
    return hydra.Inference.inferTypeOfCollection(
      fcx,
      cx,
      (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (x -> new hydra.core.Type.Set(x)),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (terms -> new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(terms))),
      "set element",
      hydra.lib.sets.Singleton.apply(new hydra.core.Name("ordering")),
      hydra.lib.sets.ToList.apply(s));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfTerm(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Term term, String desc) {
    hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> new hydra.context.Context(hydra.lib.lists.Cons.apply(
      desc,
      (fcx).trace), (fcx).messages, (fcx).other));
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Annotated a) {
        return hydra.Inference.inferTypeOfAnnotatedTerm(
          fcx2.get(),
          cx,
          (a).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Application a) {
        return hydra.Inference.inferTypeOfApplication(
          fcx2.get(),
          cx,
          (a).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Cases c) {
        return hydra.Inference.inferTypeOfCaseStatement(
          fcx2.get(),
          cx,
          (c).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Either e) {
        return hydra.Inference.inferTypeOfEither(
          fcx2.get(),
          cx,
          (e).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Lambda l) {
        return hydra.Inference.inferTypeOfLambda(
          fcx2.get(),
          cx,
          (l).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Let l) {
        return hydra.Inference.inferTypeOfLet(
          fcx2.get(),
          cx,
          (l).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.List els) {
        return hydra.Inference.inferTypeOfList(
          fcx2.get(),
          cx,
          (els).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Literal l) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(hydra.Inference.inferTypeOfLiteral(
          fcx2.get(),
          (l).value));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Map m) {
        return hydra.Inference.inferTypeOfMap(
          fcx2.get(),
          cx,
          (m).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Maybe m) {
        return hydra.Inference.inferTypeOfOptional(
          fcx2.get(),
          cx,
          (m).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Pair p) {
        return hydra.Inference.inferTypeOfPair(
          fcx2.get(),
          cx,
          (p).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Project p) {
        return hydra.Inference.inferTypeOfProjection(
          fcx2.get(),
          cx,
          (p).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Record r) {
        return hydra.Inference.inferTypeOfRecord(
          fcx2.get(),
          cx,
          (r).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Set s) {
        return hydra.Inference.inferTypeOfSet(
          fcx2.get(),
          cx,
          (s).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.TypeApplication tt) {
        return hydra.Inference.inferTypeOfTypeApplication(
          fcx2.get(),
          cx,
          (tt).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.TypeLambda ta) {
        return hydra.Inference.inferTypeOfTypeLambda(
          fcx2.get(),
          cx,
          (ta).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Inject i) {
        return hydra.Inference.inferTypeOfInjection(
          fcx2.get(),
          cx,
          (i).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Unit ignored) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(hydra.Inference.inferTypeOfUnit(fcx2.get()));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Unwrap tname) {
        return hydra.Inference.inferTypeOfUnwrap(
          fcx2.get(),
          cx,
          (tname).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Variable name) {
        return hydra.Inference.inferTypeOfVariable(
          fcx2.get(),
          cx,
          (name).value);
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> visit(hydra.core.Term.Wrap w) {
        return hydra.Inference.inferTypeOfWrappedTerm(
          fcx2.get(),
          cx,
          (w).value);
      }
    });
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfTypeApplication(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.TypeApplicationTerm tt) {
    return hydra.Inference.inferTypeOfTerm(
      fcx,
      cx,
      (tt).body,
      "type application term");
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfTypeLambda(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.TypeLambda ta) {
    return hydra.Inference.inferTypeOfTerm(
      fcx,
      cx,
      (ta).body,
      "type abstraction");
  }

  static hydra.typing.InferenceResult inferTypeOfUnit(hydra.context.Context fcx) {
    return new hydra.typing.InferenceResult(new hydra.core.Term.Unit(), new hydra.core.Type.Unit(), hydra.Substitution.idTypeSubst(), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), fcx);
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfUnwrap(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Name tname) {
    return hydra.lib.eithers.Bind.apply(
      hydra.Resolution.requireSchemaType(
        fcx,
        (cx).schemaTypes,
        tname),
      (java.util.function.Function<hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (stRp -> {
        hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(stRp));
        hydra.util.Lazy<hydra.core.TypeScheme> schemaType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(stRp));
        hydra.core.Type stype = schemaType.get().type;
        java.util.List<hydra.core.Name> svars = schemaType.get().variables;
        return hydra.lib.eithers.Bind.apply(
          hydra.extract.Core.wrappedType(
            tname,
            stype),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (wtyp -> hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(hydra.Inference.yield(
            fcx2.get(),
            hydra.Inference.buildTypeApplicationTerm(
              svars,
              new hydra.core.Term.Unwrap(tname)),
            new hydra.core.Type.Function(new hydra.core.FunctionType(hydra.Resolution.nominalApplication(
              tname,
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
                svars)), wtyp)),
            hydra.Substitution.idTypeSubst()))));
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfVariable(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.Name name) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>left(new hydra.errors.Error_.Resolution(new hydra.errors.ResolutionError.NoSuchBinding(new hydra.errors.NoSuchBindingError(name)))),
        (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (scheme -> {
          hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context> tsResult = hydra.Resolution.instantiateTypeScheme(
            fcx,
            scheme);
          hydra.util.Lazy<hydra.core.TypeScheme> ts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tsResult));
          hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> constraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
            () -> (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())),
            ts.get().constraints));
          hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(tsResult));
          return hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(hydra.Inference.yieldCheckedWithConstraints(
            fcx2.get(),
            hydra.Inference.buildTypeApplicationTerm(
              ts.get().variables,
              new hydra.core.Term.Variable(name)),
            ts.get().type,
            hydra.Substitution.idTypeSubst(),
            constraints.get()));
        }),
        hydra.lib.maybes.Map.apply(
          projected -> projected.type,
          hydra.lib.maps.Lookup.apply(
            name,
            (cx).primitives))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (scheme -> {
        hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context> tsResult = hydra.Resolution.instantiateTypeScheme(
          fcx,
          scheme);
        hydra.util.Lazy<hydra.core.TypeScheme> ts = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tsResult));
        hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> constraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
          () -> (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())),
          ts.get().constraints));
        hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(tsResult));
        return hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(new hydra.typing.InferenceResult(hydra.Inference.buildTypeApplicationTerm(
          ts.get().variables,
          new hydra.core.Term.Variable(name)), ts.get().type, hydra.Substitution.idTypeSubst(), constraints.get(), fcx2.get()));
      }),
      hydra.lib.maps.Lookup.apply(
        name,
        (cx).boundTypes));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> inferTypeOfWrappedTerm(hydra.context.Context fcx, hydra.graph.Graph cx, hydra.core.WrappedTerm wt) {
    hydra.core.Term term = (wt).body;
    hydra.core.Name tname = (wt).typeName;
    return hydra.lib.eithers.Bind.apply(
      hydra.Resolution.requireSchemaType(
        fcx,
        (cx).schemaTypes,
        tname),
      (java.util.function.Function<hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (stRp -> {
        hydra.util.Lazy<hydra.context.Context> fcx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(stRp));
        hydra.util.Lazy<hydra.core.TypeScheme> schemaType = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(stRp));
        return hydra.lib.eithers.Bind.apply(
          hydra.Inference.inferTypeOfTerm(
            fcx2.get(),
            cx,
            term,
            "wrapped term"),
          (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (result -> {
            hydra.context.Context fcx3 = (result).context;
            hydra.typing.TypeSubst isubst = (result).subst;
            hydra.core.Term iterm = (result).term;
            hydra.core.Type itype = (result).type;
            hydra.core.Type ityp = new hydra.core.Type.Wrap(itype);
            hydra.core.Type stype = schemaType.get().type;
            java.util.List<hydra.core.Name> svars = schemaType.get().variables;
            return hydra.lib.eithers.Bind.apply(
              hydra.Inference.mapConstraints(
                fcx3,
                cx,
                (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.InferenceResult>) (subst -> hydra.Inference.yield(
                  fcx3,
                  hydra.Inference.buildTypeApplicationTerm(
                    svars,
                    new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(tname, iterm))),
                  hydra.Resolution.nominalApplication(
                    tname,
                    hydra.lib.lists.Map.apply(
                      (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (x -> new hydra.core.Type.Variable(x)),
                      svars)),
                  hydra.Substitution.composeTypeSubst(
                    isubst,
                    subst))),
                java.util.Arrays.asList(new hydra.typing.TypeConstraint(stype, ityp, "schema type of wrapper"))),
              (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (mcResult -> hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(mcResult)));
          }));
      }));
  }

  static hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>> inferTypesOfTemporaryBindings(hydra.context.Context fcx, hydra.graph.Graph cx, java.util.List<hydra.core.Binding> bins) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.Inference.inferTypesOfTemporaryBindings_emptyResult(
        fcx,
        hydra.Substitution.idTypeSubst()),
      (java.util.function.Function<hydra.util.Pair<hydra.core.Binding, java.util.List<hydra.core.Binding>>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>>) (binsUc -> {
        hydra.util.Lazy<hydra.core.Binding> binding = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(binsUc));
        hydra.core.Name k = binding.get().name;
        hydra.util.Lazy<java.util.List<hydra.core.Binding>> tl = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(binsUc));
        hydra.core.Term v = binding.get().term;
        return hydra.lib.eithers.Bind.apply(
          hydra.Inference.inferTypeOfTerm(
            fcx,
            cx,
            v,
            hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "temporary let binding '",
              (k).value,
              "'"))),
          (java.util.function.Function<hydra.typing.InferenceResult, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>>) (result1 -> {
            java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c1Inferred = (result1).classConstraints;
            hydra.context.Context fcx2 = (result1).context;
            hydra.core.Term j = (result1).term;
            hydra.typing.TypeSubst u = (result1).subst;
            hydra.core.Type u_prime = (result1).type;
            return hydra.lib.eithers.Bind.apply(
              hydra.lib.maybes.Maybe.applyLazy(
                () -> hydra.util.Either.<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>right((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply()))),
                (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (ts -> {
                  hydra.util.Pair<hydra.core.TypeScheme, hydra.context.Context> tsResult = hydra.Resolution.instantiateTypeScheme(
                    fcx2,
                    ts);
                  hydra.util.Lazy<hydra.core.TypeScheme> instantiatedTs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(tsResult));
                  hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> freshConstraints = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
                    () -> (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())),
                    instantiatedTs.get().constraints));
                  return hydra.lib.eithers.Bind.apply(
                    hydra.lib.eithers.Bimap.apply(
                      (java.util.function.Function<hydra.errors.UnificationError, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Unification(_e)),
                      (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>) (_a -> _a),
                      hydra.Unification.unifyTypes(
                        fcx2,
                        (cx).schemaTypes,
                        instantiatedTs.get().type,
                        u_prime,
                        "original binding type")),
                    (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (unifySubst -> hydra.util.Either.<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>right(hydra.Substitution.substInClassConstraints(
                      unifySubst,
                      freshConstraints.get()))));
                }),
                binding.get().type),
              (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>>) (originalBindingConstraints -> {
                hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> c1 = new hydra.util.Lazy<>(() -> hydra.Inference.mergeClassConstraints(
                  c1Inferred,
                  originalBindingConstraints));
                return hydra.lib.eithers.Bind.apply(
                  hydra.Inference.inferTypesOfTemporaryBindings(
                    fcx2,
                    hydra.Substitution.substInContext(
                      u,
                      cx),
                    tl.get()),
                  (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>, hydra.util.Either<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>>) (rp2 -> {
                    hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>> result2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(rp2));
                    hydra.util.Lazy<hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>> restPair = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(result2.get())));
                    hydra.util.Lazy<hydra.typing.TypeSubst> r = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(restPair.get()));
                    java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> c1Subst = hydra.Substitution.substInClassConstraints(
                      r.get(),
                      c1.get());
                    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> c2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(restPair.get()));
                    hydra.util.Lazy<hydra.context.Context> fcx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(rp2));
                    hydra.util.Lazy<java.util.List<hydra.core.Term>> h = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result2.get()));
                    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>> mergedConstraints = new hydra.util.Lazy<>(() -> hydra.Inference.mergeClassConstraints(
                      c1Subst,
                      c2.get()));
                    hydra.util.Lazy<java.util.List<hydra.core.Type>> r_prime = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(result2.get())));
                    return hydra.util.Either.<hydra.errors.Error_, hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>, hydra.context.Context>((hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>) ((hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>) (new hydra.util.Pair<java.util.List<hydra.core.Term>, hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>(hydra.lib.lists.Cons.apply(
                      hydra.Substitution.substTypesInTerm(
                        r.get(),
                        j),
                      h.get()), (hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) ((hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (new hydra.util.Pair<java.util.List<hydra.core.Type>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>(hydra.lib.lists.Cons.apply(
                      hydra.Substitution.substInType(
                        r.get(),
                        u_prime),
                      r_prime.get()), (hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) ((hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (new hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>(hydra.Substitution.composeTypeSubst(
                      u,
                      r.get()), mergedConstraints.get()))))))))), fcx3.get()))));
                  }));
              }));
          }));
      }),
      hydra.lib.lists.Uncons.apply(bins));
  }

  static <T0, T1, T2, T3, T4> hydra.util.Either<T0, hydra.util.Pair<hydra.util.Pair<java.util.List<T1>, hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>>, hydra.context.Context>> inferTypesOfTemporaryBindings_emptyResult(hydra.context.Context fcx, hydra.typing.TypeSubst hydra_substitution_idTypeSubst) {
    return hydra.util.Either.<T0, hydra.util.Pair<hydra.util.Pair<java.util.List<T1>, hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.Pair<java.util.List<T1>, hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.Pair<java.util.List<T1>, hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.Pair<java.util.List<T1>, hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>>, hydra.context.Context>((hydra.util.Pair<java.util.List<T1>, hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>>) ((hydra.util.Pair<java.util.List<T1>, hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>>) (new hydra.util.Pair<java.util.List<T1>, hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>>((java.util.List<T1>) (java.util.Collections.<T1>emptyList()), (hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>) ((hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>) (new hydra.util.Pair<java.util.List<T2>, hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>>((java.util.List<T2>) (java.util.Collections.<T2>emptyList()), (hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>) ((hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>) (new hydra.util.Pair<hydra.typing.TypeSubst, java.util.Map<T3, T4>>(hydra_substitution_idTypeSubst, (java.util.Map<T3, T4>) ((java.util.Map<T3, T4>) (hydra.lib.maps.Empty.<T3, T4>apply()))))))))))), fcx))));
  }

  static Boolean isUnbound(hydra.graph.Graph cx, hydra.core.Name v) {
    return hydra.lib.logic.And.apply(
      hydra.lib.logic.Not.apply(hydra.lib.sets.Member.apply(
        v,
        hydra.Inference.freeVariablesInContext(cx))),
      hydra.lib.logic.Not.apply(hydra.lib.maps.Member.apply(
        v,
        (cx).schemaTypes)));
  }

  static <T0, T1> hydra.util.Either<hydra.errors.Error_, T1> mapConstraints(T0 flowCx, hydra.graph.Graph cx, java.util.function.Function<hydra.typing.TypeSubst, T1> f, java.util.List<hydra.typing.TypeConstraint> constraints) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.errors.UnificationError, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Unification(_e)),
        (java.util.function.Function<hydra.typing.TypeSubst, hydra.typing.TypeSubst>) (_a -> _a),
        hydra.Unification.unifyTypeConstraints(
          flowCx,
          (cx).schemaTypes,
          constraints)),
      (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.errors.Error_, T1>>) (s -> hydra.lib.eithers.Bind.apply(
        hydra.Checking.<T0>checkTypeSubst(
          flowCx,
          cx,
          s),
        (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.errors.Error_, T1>>) (ignored -> hydra.util.Either.<hydra.errors.Error_, T1>right((f).apply(s))))));
  }

  static <T0> java.util.Map<T0, hydra.core.TypeVariableMetadata> mergeClassConstraints(java.util.Map<T0, hydra.core.TypeVariableMetadata> m1, java.util.Map<T0, hydra.core.TypeVariableMetadata> m2) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Map<T0, hydra.core.TypeVariableMetadata>, java.util.function.Function<hydra.util.Pair<T0, hydra.core.TypeVariableMetadata>, java.util.Map<T0, hydra.core.TypeVariableMetadata>>>) (acc -> (java.util.function.Function<hydra.util.Pair<T0, hydra.core.TypeVariableMetadata>, java.util.Map<T0, hydra.core.TypeVariableMetadata>>) (pair -> {
        hydra.util.Lazy<T0> k = new hydra.util.Lazy<>(() -> hydra.Inference.<T0>mergeClassConstraints_k(pair));
        hydra.util.Lazy<hydra.core.TypeVariableMetadata> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.lib.maps.Insert.apply(
            k.get(),
            v.get(),
            acc),
          (java.util.function.Function<hydra.core.TypeVariableMetadata, java.util.Map<T0, hydra.core.TypeVariableMetadata>>) (existing -> {
            hydra.util.Lazy<hydra.core.TypeVariableMetadata> merged = new hydra.util.Lazy<>(() -> new hydra.core.TypeVariableMetadata(hydra.lib.sets.Union.apply(
              (existing).classes,
              v.get().classes)));
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
    return hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "{term=",
      hydra.show.Core.term(term),
      ", type=",
      hydra.show.Core.type(typ),
      ", subst=",
      hydra.show.Typing.typeSubst(subst),
      "}"));
  }

  static hydra.typing.InferenceResult yield(hydra.context.Context fcx, hydra.core.Term term, hydra.core.Type typ, hydra.typing.TypeSubst subst) {
    return new hydra.typing.InferenceResult(hydra.Substitution.substTypesInTerm(
      subst,
      term), hydra.Substitution.substInType(
      subst,
      typ), subst, (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), fcx);
  }

  static hydra.typing.InferenceResult yieldChecked(hydra.context.Context fcx, hydra.core.Term term, hydra.core.Type typ, hydra.typing.TypeSubst subst) {
    hydra.core.Term iterm = hydra.Substitution.substTypesInTerm(
      subst,
      term);
    hydra.core.Type itype = hydra.Substitution.substInType(
      subst,
      typ);
    return new hydra.typing.InferenceResult(iterm, itype, subst, (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), fcx);
  }

  static hydra.typing.InferenceResult yieldCheckedWithConstraints(hydra.context.Context fcx, hydra.core.Term term, hydra.core.Type typ, hydra.typing.TypeSubst subst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> constraints) {
    java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> iconstraints = hydra.Substitution.substInClassConstraints(
      subst,
      constraints);
    hydra.core.Term iterm = hydra.Substitution.substTypesInTerm(
      subst,
      term);
    hydra.core.Type itype = hydra.Substitution.substInType(
      subst,
      typ);
    return new hydra.typing.InferenceResult(iterm, itype, subst, iconstraints, fcx);
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult> yieldDebug(hydra.context.Context fcx, T0 cx, String debugId, hydra.core.Term term, hydra.core.Type typ, hydra.typing.TypeSubst subst) {
    hydra.core.Term rterm = hydra.Substitution.substTypesInTerm(
      subst,
      term);
    hydra.core.Type rtyp = hydra.Substitution.substInType(
      subst,
      typ);
    return hydra.lib.eithers.Bind.apply(
      hydra.Annotations.debugIf(
        fcx,
        debugId,
        hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "\n\tterm: ",
          hydra.show.Core.term(term),
          "\n\ttyp: ",
          hydra.show.Core.type(typ),
          "\n\tsubst: ",
          hydra.show.Typing.typeSubst(subst),
          "\n\trterm: ",
          hydra.show.Core.term(rterm),
          "\n\trtyp: ",
          hydra.show.Core.type(rtyp)))),
      (java.util.function.Function<java.lang.Void, hydra.util.Either<hydra.errors.Error_, hydra.typing.InferenceResult>>) (result -> hydra.util.Either.<hydra.errors.Error_, hydra.typing.InferenceResult>right(new hydra.typing.InferenceResult(rterm, rtyp, subst, (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), fcx))));
  }

  static hydra.typing.InferenceResult yieldWithConstraints(hydra.context.Context fcx, hydra.core.Term term, hydra.core.Type typ, hydra.typing.TypeSubst subst, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata> constraints) {
    return new hydra.typing.InferenceResult(hydra.Substitution.substTypesInTerm(
      subst,
      term), hydra.Substitution.substInType(
      subst,
      typ), subst, constraints, fcx);
  }
}
