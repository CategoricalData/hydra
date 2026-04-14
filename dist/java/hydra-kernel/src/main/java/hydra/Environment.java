// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Graph to type environment conversions
 */
public interface Environment {
  static hydra.util.Either<hydra.errors.Error_, hydra.core.TypeApplicationTerm> definitionAsTypeApplicationTerm(hydra.core.Binding el) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.errors.Error_, hydra.core.TypeApplicationTerm>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("typed binding", "untyped binding")))),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.Error_, hydra.core.TypeApplicationTerm>>) (ts -> hydra.util.Either.<hydra.errors.Error_, hydra.core.TypeApplicationTerm>right(new hydra.core.TypeApplicationTerm((el).term, (ts).type))),
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

  static hydra.util.Either<hydra.errors.DecodingError, java.util.Map<hydra.core.Name, hydra.core.Type>> graphAsTypes(hydra.graph.Graph graph, java.util.List<hydra.core.Binding> els) {
    java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>> toPair = (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>>) (el -> hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.core.Type, hydra.util.Pair<hydra.core.Name, hydra.core.Type>>) (typ -> (hydra.util.Pair<hydra.core.Name, hydra.core.Type>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Type>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Type>((el).name, typ)))),
      hydra.decode.Core.type(
        graph,
        (el).term)));
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.core.Type>>, java.util.Map<hydra.core.Name, hydra.core.Type>>) ((java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.Name, hydra.core.Type>>, java.util.Map<hydra.core.Name, hydra.core.Type>>) (hydra.lib.maps.FromList::apply)),
      hydra.lib.eithers.MapList.apply(
        toPair,
        els));
  }

  static hydra.util.Pair<java.util.List<hydra.packaging.TypeDefinition>, java.util.List<hydra.packaging.TermDefinition>> partitionDefinitions(java.util.List<hydra.packaging.Definition> defs) {
    java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.packaging.TermDefinition>> getTerm = (java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.packaging.TermDefinition>>) (def -> (def).accept(new hydra.packaging.Definition.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.packaging.TermDefinition> visit(hydra.packaging.Definition.Type ignored) {
        return (hydra.util.Maybe<hydra.packaging.TermDefinition>) (hydra.util.Maybe.<hydra.packaging.TermDefinition>nothing());
      }

      @Override
      public hydra.util.Maybe<hydra.packaging.TermDefinition> visit(hydra.packaging.Definition.Term td) {
        return hydra.util.Maybe.just((td).value);
      }
    }));
    java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.packaging.TypeDefinition>> getType = (java.util.function.Function<hydra.packaging.Definition, hydra.util.Maybe<hydra.packaging.TypeDefinition>>) (def -> (def).accept(new hydra.packaging.Definition.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.packaging.TypeDefinition> visit(hydra.packaging.Definition.Type td) {
        return hydra.util.Maybe.just((td).value);
      }

      @Override
      public hydra.util.Maybe<hydra.packaging.TypeDefinition> visit(hydra.packaging.Definition.Term ignored) {
        return (hydra.util.Maybe<hydra.packaging.TypeDefinition>) (hydra.util.Maybe.<hydra.packaging.TypeDefinition>nothing());
      }
    }));
    return (hydra.util.Pair<java.util.List<hydra.packaging.TypeDefinition>, java.util.List<hydra.packaging.TermDefinition>>) ((hydra.util.Pair<java.util.List<hydra.packaging.TypeDefinition>, java.util.List<hydra.packaging.TermDefinition>>) (new hydra.util.Pair<java.util.List<hydra.packaging.TypeDefinition>, java.util.List<hydra.packaging.TermDefinition>>(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      getType,
      defs)), hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      getTerm,
      defs)))));
  }

  static java.util.List<hydra.packaging.Definition> reorderDefs(java.util.List<hydra.packaging.Definition> defs) {
    hydra.util.Pair<java.util.List<hydra.packaging.TypeDefinition>, java.util.List<hydra.packaging.TermDefinition>> partitioned = hydra.Environment.partitionDefinitions(defs);
    hydra.util.Lazy<java.util.List<hydra.packaging.TypeDefinition>> typeDefsRaw = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(partitioned));
    hydra.util.Lazy<java.util.List<hydra.packaging.TypeDefinition>> nameFirst = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.packaging.TypeDefinition, Boolean>) (td -> hydra.lib.equality.Equal.apply(
        (td).name,
        new hydra.core.Name("hydra.core.Name"))),
      typeDefsRaw.get()));
    hydra.util.Lazy<java.util.List<hydra.packaging.TypeDefinition>> nameRest = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.packaging.TypeDefinition, Boolean>) (td -> hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
        (td).name,
        new hydra.core.Name("hydra.core.Name")))),
      typeDefsRaw.get()));
    hydra.util.Lazy<java.util.List<hydra.packaging.Definition>> termDefsWrapped = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.TermDefinition, hydra.packaging.Definition>) (td -> new hydra.packaging.Definition.Term(td)),
      hydra.lib.pairs.Second.apply(partitioned)));
    hydra.util.Lazy<java.util.List<hydra.packaging.Definition>> sortedTermDefs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.Sorting.topologicalSortNodes(
      (java.util.function.Function<hydra.packaging.Definition, hydra.core.Name>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
        @Override
        public hydra.core.Name visit(hydra.packaging.Definition.Term td) {
          return (td).value.name;
        }
      })),
      (java.util.function.Function<hydra.packaging.Definition, java.util.List<hydra.core.Name>>) (d -> (d).accept(new hydra.packaging.Definition.PartialVisitor<>() {
        @Override
        public java.util.List<hydra.core.Name> otherwise(hydra.packaging.Definition instance) {
          return (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList());
        }

        @Override
        public java.util.List<hydra.core.Name> visit(hydra.packaging.Definition.Term td) {
          return hydra.lib.sets.ToList.apply(hydra.Variables.freeVariablesInTerm((td).value.term));
        }
      })),
      termDefsWrapped.get())));
    hydra.util.Lazy<java.util.List<hydra.packaging.Definition>> typeDefs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.packaging.TypeDefinition, hydra.packaging.Definition>) (td -> new hydra.packaging.Definition.Type(td)),
        nameFirst.get()),
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.packaging.TypeDefinition, hydra.packaging.Definition>) (td -> new hydra.packaging.Definition.Type(td)),
        nameRest.get()))));
    return hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
      typeDefs.get(),
      sortedTermDefs.get()));
  }

  static hydra.util.Either<hydra.errors.Error_, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> schemaGraphToTypingEnvironment(hydra.graph.Graph g) {
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>> decodeType = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.Type>>) (term -> hydra.lib.eithers.Bimap.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Decoding(_e)),
      (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_a -> _a),
      hydra.decode.Core.type(
        g,
        term)));
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.TypeScheme>> decodeTypeScheme = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.core.TypeScheme>>) (term -> hydra.lib.eithers.Bimap.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.errors.Error_>) (_e -> new hydra.errors.Error_.Decoding(_e)),
      (java.util.function.Function<hydra.core.TypeScheme, hydra.core.TypeScheme>) (_a -> _a),
      hydra.decode.Core.typeScheme(
        g,
        term)));
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
    java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>> toPair = (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>>) (el -> {
      java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.TypeScheme>>> forTerm = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.TypeScheme>>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.TypeScheme>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.core.TypeScheme>>right((hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()));
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.TypeScheme>> visit(hydra.core.Term.Record r) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              (r).value.typeName,
              new hydra.core.Name("hydra.core.TypeScheme")),
            () -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Maybe<hydra.core.TypeScheme>>) (hydra.lib.maybes.Pure::apply),
              (decodeTypeScheme).apply((el).term)),
            () -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.core.TypeScheme>>right((hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())));
        }

        @Override
        public hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.TypeScheme>> visit(hydra.core.Term.Inject i) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              (i).value.typeName,
              new hydra.core.Name("hydra.core.Type")),
            () -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.TypeScheme>>) (decoded -> hydra.util.Maybe.just(toTypeScheme.get().apply((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList())).apply(decoded))),
              (decodeType).apply((el).term)),
            () -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.core.TypeScheme>>right((hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())));
        }
      }));
      return hydra.lib.eithers.Bind.apply(
        hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.core.Type, hydra.util.Maybe<hydra.core.TypeScheme>>) (typ -> hydra.util.Maybe.just(hydra.Scoping.fTypeToTypeScheme(typ))),
            (decodeType).apply((el).term)),
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.core.TypeScheme>>>) (ts -> hydra.lib.logic.IfElse.lazy(
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
        (java.util.function.Function<hydra.util.Maybe<hydra.core.TypeScheme>, hydra.util.Either<hydra.errors.Error_, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>>) (mts -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>right(hydra.lib.maybes.Map.apply(
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

  static java.util.List<hydra.core.Binding> typesToDefinitions(java.util.Map<hydra.core.Name, hydra.core.Type> typeMap) {
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
