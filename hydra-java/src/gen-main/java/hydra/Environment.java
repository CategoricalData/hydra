// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Graph to type environment conversions
 */
public interface Environment {
  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.TypeApplicationTerm> elementAsTypeApplicationTerm(hydra.context.Context cx, hydra.core.Binding el) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.TypeApplicationTerm>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError("missing element type")), cx))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.TypeApplicationTerm>>) (ts -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.TypeApplicationTerm>right(new hydra.core.TypeApplicationTerm((el).term, (ts).type))),
      (el).type);
  }

  static hydra.core.Let graphAsLet(java.util.List<hydra.core.Binding> bindings, hydra.core.Term body) {
    return new hydra.core.Let(bindings, body);
  }

  static hydra.core.Term graphAsTerm(java.util.List<hydra.core.Binding> bindings, hydra.core.Term body) {
    return new hydra.core.Term.Let(hydra.Environment.graphAsLet(
      bindings,
      body));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.DecodingError>, java.util.Map<hydra.core.Name, hydra.core.Type>> graphAsTypes(hydra.context.Context cx, hydra.graph.Graph graph, java.util.List<hydra.core.Binding> els) {
    java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.DecodingError>, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>> toPair = (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.DecodingError>, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>>) (el -> hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.core.Type, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>) (typ -> (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>((el).name, typ)))),
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.errors.DecodingError, hydra.context.InContext<hydra.errors.DecodingError>>) (_wc_e -> (hydra.context.InContext<hydra.errors.DecodingError>) (new hydra.context.InContext<hydra.errors.DecodingError>(_wc_e, cx))),
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_wc_a -> _wc_a),
        hydra.decode.Core.type(
          graph,
          (el).term))));
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.core.Type>>, java.util.Map<hydra.core.Name, hydra.core.Type>>) ((java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.core.Type>>, java.util.Map<hydra.core.Name, hydra.core.Type>>) (hydra.lib.maps.FromList::apply)),
      hydra.lib.eithers.MapList.apply(
        toPair,
        els));
  }

  static hydra.util.Pair<java.util.List<hydra.module.TypeDefinition>, java.util.List<hydra.module.TermDefinition>> partitionDefinitions(java.util.List<hydra.module.Definition> defs) {
    java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.module.TermDefinition>> getTerm = (java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.module.TermDefinition>>) (def -> (def).accept(new hydra.module.Definition.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.module.TermDefinition> visit(hydra.module.Definition.Type ignored) {
        return (hydra.util.Maybe<hydra.module.TermDefinition>) (hydra.util.Maybe.<hydra.module.TermDefinition>nothing());
      }

      @Override
      public hydra.util.Maybe<hydra.module.TermDefinition> visit(hydra.module.Definition.Term td) {
        return hydra.util.Maybe.just((td).value);
      }
    }));
    java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.module.TypeDefinition>> getType = (java.util.function.Function<hydra.module.Definition, hydra.util.Maybe<hydra.module.TypeDefinition>>) (def -> (def).accept(new hydra.module.Definition.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.module.TypeDefinition> visit(hydra.module.Definition.Type td) {
        return hydra.util.Maybe.just((td).value);
      }

      @Override
      public hydra.util.Maybe<hydra.module.TypeDefinition> visit(hydra.module.Definition.Term ignored) {
        return (hydra.util.Maybe<hydra.module.TypeDefinition>) (hydra.util.Maybe.<hydra.module.TypeDefinition>nothing());
      }
    }));
    return (hydra.util.Pair<java.util.List<hydra.module.TypeDefinition>, java.util.List<hydra.module.TermDefinition>>) ((hydra.util.Pair<java.util.List<hydra.module.TypeDefinition>, java.util.List<hydra.module.TermDefinition>>) (new hydra.util.Pair<java.util.List<hydra.module.TypeDefinition>, java.util.List<hydra.module.TermDefinition>>(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      getType,
      defs)), hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      getTerm,
      defs)))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> schemaGraphToTypingEnvironment(hydra.context.Context cx, hydra.graph.Graph g) {
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>> decodeType = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Type>>) (term -> hydra.lib.eithers.Bimap.apply(
      (java.util.function.Function<hydra.errors.Error_, hydra.context.InContext<hydra.errors.Error_>>) (_wc_e -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(_wc_e, cx))),
      (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_wc_a -> _wc_a),
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.errors.DecodingError, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Other(new hydra.errors.OtherError((_e).value))),
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_a -> _a),
        hydra.decode.Core.type(
          g,
          term))));
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.TypeScheme>> decodeTypeScheme = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.TypeScheme>>) (term -> hydra.lib.eithers.Bimap.apply(
      (java.util.function.Function<hydra.errors.Error_, hydra.context.InContext<hydra.errors.Error_>>) (_wc_e -> (hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(_wc_e, cx))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (_wc_a -> _wc_a),
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.errors.DecodingError, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Other(new hydra.errors.OtherError((_e).value))),
        (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (_a -> _a),
        hydra.decode.Core.typeScheme(
          g,
          term))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>>> toTypeScheme = new java.util.concurrent.atomic.AtomicReference<>();
    toTypeScheme.set((java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>>) (vars -> (java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>) (typ -> hydra.Strip.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.TypeScheme otherwise(hydra.core.Type instance) {
        return new hydra.core.TypeScheme(hydra.lib.lists.Reverse.apply(vars), typ, (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()));
      }

      @Override
      public hydra.core.TypeScheme visit(hydra.core.Type.Forall ft) {
        return toTypeScheme.get().apply(hydra.lib.lists.Cons.apply(
          (ft).value.parameter,
          vars)).apply((ft).value.body);
      }
    }))));
    java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>> toPair = (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>>) (el -> {
      java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>>> forTerm = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>>right((hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()));
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>> visit(hydra.core.Term.Record r) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              (r).value.typeName,
              new hydra.core.Name("hydra.core.TypeScheme")),
            () -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Maybe<hydra.core.TypeScheme>>) (hydra.lib.maybes.Pure::apply),
              (decodeTypeScheme).apply((el).term)),
            () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>>right((hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())));
        }

        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>> visit(hydra.core.Term.Union i) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              (i).value.typeName,
              new hydra.core.Name("hydra.core.Type")),
            () -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.TypeScheme>>) (decoded -> hydra.util.Maybe.just(toTypeScheme.get().apply((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList())).apply(decoded))),
              (decodeType).apply((el).term)),
            () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>>right((hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())));
        }
      }));
      return hydra.lib.eithers.Bind.apply(
        hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.TypeScheme>>) (typ -> hydra.util.Maybe.just(hydra.Scoping.fTypeToTypeScheme(typ))),
            (decodeType).apply((el).term)),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.core.TypeScheme>>>) (ts -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              ts,
              new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.TypeScheme")), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))),
            () -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Maybe<hydra.core.TypeScheme>>) (hydra.lib.maybes.Pure::apply),
              (decodeTypeScheme).apply((el).term)),
            () -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                ts,
                new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(new hydra.core.Name("hydra.core.Type")), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))),
              () -> hydra.lib.eithers.Map.apply(
                (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.TypeScheme>>) (decoded -> hydra.util.Maybe.just(toTypeScheme.get().apply((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList())).apply(decoded))),
                (decodeType).apply((el).term)),
              () -> (forTerm).apply(hydra.Strip.deannotateTerm((el).term))))),
          (el).type),
        (java.util.function.Function<hydra.util.Maybe<hydra.core.TypeScheme>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>>) (mts -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>right(hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (ts -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>((el).name, ts)))),
          mts))));
    });
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>) (mpairs -> hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(mpairs))),
      hydra.lib.eithers.MapList.apply(
        toPair,
        hydra.Lexical.graphToBindings(g)));
  }

  static java.util.List<hydra.core.Binding> termAsBindings(hydra.core.Term term) {
    return hydra.Strip.deannotateTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Binding> otherwise(hydra.core.Term instance) {
        return (java.util.List<hydra.core.Binding>) (java.util.Collections.<hydra.core.Binding>emptyList());
      }

      @Override
      public java.util.List<hydra.core.Binding> visit(hydra.core.Term.Let lt) {
        return (lt).value.bindings;
      }
    });
  }

  static java.util.List<hydra.core.Binding> typesToElements(java.util.Map<hydra.core.Name, hydra.core.Type> typeMap) {
    java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, hydra.core.Binding> toElement = (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, hydra.core.Binding>) (pair -> {
      hydra.util.Lazy<hydra.core.Name> name = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
      return new hydra.core.Binding(name.get(), hydra.encode.Core.type(hydra.lib.pairs.Second.apply(pair)), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()));
    });
    return hydra.lib.lists.Map.apply(
      toElement,
      hydra.lib.maps.ToList.apply(typeMap));
  }

  static <T0, T1, T2> T2 withLambdaContext(java.util.function.Function<T0, hydra.graph.Graph> getContext, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T1>> setContext, T0 env, hydra.core.Lambda lam, java.util.function.Function<T1, T2> body) {
    hydra.graph.Graph newContext = hydra.Scoping.extendGraphForLambda(
      (getContext).apply(env),
      lam);
    return (body).apply((setContext).apply(newContext).apply(env));
  }

  static <T0, T1, T2> T2 withLetContext(java.util.function.Function<T0, hydra.graph.Graph> getContext, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T1>> setContext, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>> forBinding, T0 env, hydra.core.Let letrec, java.util.function.Function<T1, T2> body) {
    hydra.graph.Graph newContext = hydra.Scoping.extendGraphForLet(
      forBinding,
      (getContext).apply(env),
      letrec);
    return (body).apply((setContext).apply(newContext).apply(env));
  }

  static <T0, T1, T2> T2 withTypeLambdaContext(java.util.function.Function<T0, hydra.graph.Graph> getContext, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, T1>> setContext, T0 env, hydra.core.TypeLambda tlam, java.util.function.Function<T1, T2> body) {
    hydra.graph.Graph newContext = hydra.Scoping.extendGraphForTypeLambda(
      (getContext).apply(env),
      tlam);
    return (body).apply((setContext).apply(newContext).apply(env));
  }
}
