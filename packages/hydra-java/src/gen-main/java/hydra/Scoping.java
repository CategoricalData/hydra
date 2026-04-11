// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Graph context extension and type scheme conversion
 */
public interface Scoping {
  static hydra.graph.Graph extendGraphForLambda(hydra.graph.Graph g, hydra.core.Lambda lam) {
    hydra.core.Name var = (lam).parameter;
    return new hydra.graph.Graph((g).boundTerms, hydra.lib.maybes.Maybe.applyLazy(
      () -> (g).boundTypes,
      (java.util.function.Function<hydra.core.Type, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>) (dom -> hydra.lib.maps.Insert.apply(
        var,
        hydra.Scoping.fTypeToTypeScheme(dom),
        (g).boundTypes)),
      (lam).domain), (g).classConstraints, hydra.lib.sets.Insert.apply(
      var,
      (g).lambdaVariables), hydra.lib.maps.Delete.apply(
      var,
      (g).metadata), (g).primitives, (g).schemaTypes, (g).typeVariables);
  }

  static hydra.graph.Graph extendGraphForLet(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>> forBinding, hydra.graph.Graph g, hydra.core.Let letrec) {
    java.util.List<hydra.core.Binding> bindings = (letrec).bindings;
    hydra.graph.Graph g2 = hydra.Scoping.extendGraphWithBindings(
      bindings,
      g);
    return new hydra.graph.Graph(hydra.lib.maps.Union.apply(
      hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Name, hydra.core.Term>>) (b -> (hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>((b).name, (b).term)))),
        bindings)),
      (g).boundTerms), hydra.lib.maps.Union.apply(
      hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>) (b -> hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (ts -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>((b).name, ts)))),
          (b).type)),
        bindings))),
      (g).boundTypes), (g).classConstraints, hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Binding, java.util.Set<hydra.core.Name>>>) (s -> (java.util.function.Function<hydra.core.Binding, java.util.Set<hydra.core.Name>>) (b -> hydra.lib.sets.Delete.apply(
        (b).name,
        s))),
      (g).lambdaVariables,
      bindings), hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.graph.Graph>>) (gAcc -> (java.util.function.Function<hydra.core.Binding, hydra.graph.Graph>) (b -> {
        java.util.Map<hydra.core.Name, hydra.core.Term> m = (gAcc).metadata;
        hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Term>> newMeta = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.lib.maps.Delete.apply(
            (b).name,
            m),
          (java.util.function.Function<hydra.core.Term, java.util.Map<hydra.core.Name, hydra.core.Term>>) (t -> hydra.lib.maps.Insert.apply(
            (b).name,
            t,
            m)),
          (forBinding).apply(gAcc).apply(b)));
        return new hydra.graph.Graph((gAcc).boundTerms, (gAcc).boundTypes, (gAcc).classConstraints, (gAcc).lambdaVariables, newMeta.get(), (gAcc).primitives, (gAcc).schemaTypes, (gAcc).typeVariables);
      })),
      g2,
      bindings).metadata, (g).primitives, (g).schemaTypes, (g).typeVariables);
  }

  static hydra.graph.Graph extendGraphForTypeLambda(hydra.graph.Graph g, hydra.core.TypeLambda tlam) {
    hydra.core.Name name = (tlam).parameter;
    return new hydra.graph.Graph((g).boundTerms, (g).boundTypes, (g).classConstraints, (g).lambdaVariables, (g).metadata, (g).primitives, (g).schemaTypes, hydra.lib.sets.Insert.apply(
      name,
      (g).typeVariables));
  }

  static hydra.graph.Graph extendGraphWithBindings(java.util.List<hydra.core.Binding> bindings, hydra.graph.Graph g) {
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.Term>> newTerms = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.core.Name, hydra.core.Term>>) (b -> (hydra.util.Pair<hydra.core.Name, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Name, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Name, hydra.core.Term>((b).name, (b).term)))),
      bindings)));
    hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>> newTypes = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>>) (b -> hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (ts -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>((b).name, ts)))),
        (b).type)),
      bindings))));
    return new hydra.graph.Graph(hydra.lib.maps.Union.apply(
      newTerms.get(),
      (g).boundTerms), hydra.lib.maps.Union.apply(
      newTypes.get(),
      (g).boundTypes), (g).classConstraints, (g).lambdaVariables, (g).metadata, (g).primitives, (g).schemaTypes, (g).typeVariables);
  }

  static hydra.core.TypeScheme fTypeToTypeScheme(hydra.core.Type typ) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, hydra.core.Type>> stripAnnotations = new java.util.concurrent.atomic.AtomicReference<>();
    stripAnnotations.set((java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return t;
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated at) {
        return stripAnnotations.get().apply((at).value.body);
      }
    })));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>>> gatherForall = new java.util.concurrent.atomic.AtomicReference<>();
    gatherForall.set((java.util.function.Function<java.util.List<hydra.core.Name>, java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>>) (vars -> (java.util.function.Function<hydra.core.Type, hydra.core.TypeScheme>) (typ2 -> stripAnnotations.get().apply(typ2).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.TypeScheme otherwise(hydra.core.Type instance) {
        return new hydra.core.TypeScheme(hydra.lib.lists.Reverse.apply(vars), typ2, (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()));
      }

      @Override
      public hydra.core.TypeScheme visit(hydra.core.Type.Forall ft) {
        return gatherForall.get().apply(hydra.lib.lists.Cons.apply(
          (ft).value.parameter,
          vars)).apply((ft).value.body);
      }
    }))));
    return gatherForall.get().apply((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList())).apply(typ);
  }

  static hydra.core.Type typeSchemeToFType(hydra.core.TypeScheme ts) {
    hydra.core.Type body = (ts).type;
    java.util.List<hydra.core.Name> vars = (ts).variables;
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.core.Name, hydra.core.Type>>) (t -> (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (v -> new hydra.core.Type.Forall(new hydra.core.ForallType(v, t)))),
      body,
      hydra.lib.lists.Reverse.apply(vars));
  }
}
