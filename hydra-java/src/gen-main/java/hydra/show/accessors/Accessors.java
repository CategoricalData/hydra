// Note: this is an automatically generated file. Do not edit.

package hydra.show.accessors;

/**
 * Utilities for working with term accessors.
 */
public interface Accessors {
  static hydra.util.Maybe<String> termAccessor(hydra.accessors.TermAccessor accessor) {
    return ((accessor)).accept(new hydra.accessors.TermAccessor.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.AnnotatedBody ignored) {
        return (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing());
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.ApplicationFunction ignored) {
        return hydra.util.Maybe.just("fun");
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.ApplicationArgument ignored) {
        return hydra.util.Maybe.just("arg");
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.LambdaBody ignored) {
        return hydra.util.Maybe.just("body");
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.UnionCasesDefault ignored) {
        return hydra.util.Maybe.just("default");
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.UnionCasesBranch name) {
        return hydra.util.Maybe.just(hydra.lib.strings.Cat2.apply(
          ".",
          (((name)).value).value));
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.LetBody ignored) {
        return hydra.util.Maybe.just("in");
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.LetBinding name) {
        return hydra.util.Maybe.just(hydra.lib.strings.Cat2.apply(
          (((name)).value).value,
          "="));
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.ListElement i) {
        return hydra.show.accessors.Accessors.termAccessor_idx(((i)).value);
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.MapKey i) {
        return hydra.show.accessors.Accessors.termAccessor_idxSuff(
          ".key",
          ((i)).value);
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.MapValue i) {
        return hydra.show.accessors.Accessors.termAccessor_idxSuff(
          ".value",
          ((i)).value);
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.MaybeTerm ignored) {
        return hydra.util.Maybe.just("just");
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.ProductTerm i) {
        return hydra.show.accessors.Accessors.termAccessor_idx(((i)).value);
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.RecordField name) {
        return hydra.util.Maybe.just(hydra.lib.strings.Cat2.apply(
          ".",
          (((name)).value).value));
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.SetElement i) {
        return hydra.show.accessors.Accessors.termAccessor_idx(((i)).value);
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.SumTerm ignored) {
        return (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing());
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.TypeLambdaBody ignored) {
        return (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing());
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.TypeApplicationTerm ignored) {
        return (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing());
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.InjectionTerm ignored) {
        return (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing());
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.WrappedTerm ignored) {
        return (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing());
      }
    });
  }
  
  static <T0, T1> hydra.util.Maybe<T1> termAccessor_idx(T0 i) {
    return (hydra.util.Maybe<T1>) (hydra.util.Maybe.<T1>nothing());
  }
  
  static <T0> hydra.util.Maybe<String> termAccessor_idxSuff(String suffix, T0 i) {
    return hydra.lib.maybes.Map.apply(
      (java.util.function.Function<String, String>) (s -> hydra.lib.strings.Cat2.apply(
        (s),
        (suffix))),
      hydra.show.accessors.Accessors.<T0, String>termAccessor_idx((i)));
  }
  
  static hydra.accessors.AccessorGraph termToAccessorGraph(java.util.Map<hydra.module.Namespace, String> namespaces, hydra.core.Term term) {
    hydra.accessors.TermAccessor dontCareAccessor = new hydra.accessors.TermAccessor.AnnotatedBody();
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>, java.util.function.Function<hydra.util.Maybe<hydra.accessors.AccessorNode>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>>>>>> helper = new java.util.concurrent.atomic.AtomicReference<>();
    helper.set((java.util.function.Function<java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>, java.util.function.Function<hydra.util.Maybe<hydra.accessors.AccessorNode>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>>>>>) (ids -> (java.util.function.Function<hydra.util.Maybe<hydra.accessors.AccessorNode>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>>>>) (mroot -> (java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>>>) (path -> (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>>) (state -> (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>) (accessorTerm -> {
      hydra.util.Lazy<hydra.accessors.TermAccessor> accessor = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((accessorTerm)));
      hydra.util.Lazy<hydra.core.Term> currentTerm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((accessorTerm)));
      hydra.util.Lazy<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>> nodesEdges = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((state)));
      hydra.util.Lazy<java.util.List<hydra.accessors.AccessorEdge>> edges = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nodesEdges.get()));
      hydra.util.Lazy<java.util.List<hydra.accessors.TermAccessor>> nextPath = new hydra.util.Lazy<>(() -> hydra.lib.lists.Cons.apply(
        accessor.get(),
        (path)));
      hydra.util.Lazy<java.util.List<hydra.accessors.AccessorNode>> nodes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(nodesEdges.get()));
      hydra.util.Lazy<java.util.Set<String>> visited = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((state)));
      return (currentTerm.get()).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>> otherwise(hydra.core.Term instance) {
          return hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>>) (v1 -> (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>) (v2 -> (((((helper.get()).apply((ids))).apply((mroot))).apply(nextPath.get())).apply((v1))).apply((v2)))),
            (state),
            hydra.rewriting.Rewriting.subtermsWithAccessors(currentTerm.get()));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>> visit(hydra.core.Term.Let letExpr) {
          java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>, java.util.function.Function<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>>> addBindingName = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>, java.util.function.Function<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>>>) (nodesVisitedIds -> (java.util.function.Function<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>>) (name -> {
            hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>> currentIds = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((nodesVisitedIds)));
            hydra.util.Lazy<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>> currentNodesVisited = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((nodesVisitedIds)));
            hydra.util.Lazy<java.util.List<hydra.accessors.AccessorNode>> currentNodes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(currentNodesVisited.get()));
            hydra.util.Lazy<java.util.Set<String>> currentVisited = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(currentNodesVisited.get()));
            String rawLabel = hydra.names.Names.compactName(
              (namespaces),
              (name));
            String uniqueLabel = hydra.names.Names.uniqueLabel(
              currentVisited.get(),
              (rawLabel));
            hydra.accessors.AccessorNode node = new hydra.accessors.AccessorNode((name), (rawLabel), (uniqueLabel));
            hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>> newIds = new hydra.util.Lazy<>(() -> hydra.lib.maps.Insert.apply(
              (name),
              (node),
              currentIds.get()));
            hydra.util.Lazy<java.util.List<hydra.accessors.AccessorNode>> newNodes = new hydra.util.Lazy<>(() -> hydra.lib.lists.Cons.apply(
              (node),
              currentNodes.get()));
            hydra.util.Lazy<java.util.Set<String>> newVisited = new hydra.util.Lazy<>(() -> hydra.lib.sets.Insert.apply(
              (uniqueLabel),
              currentVisited.get()));
            return (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>(newNodes.get(), newVisited.get()))), newIds.get())));
          }));
          java.util.List<hydra.core.Binding> bindings = (((letExpr)).value).bindings;
          hydra.util.Lazy<java.util.List<hydra.core.Name>> bindingNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
            projected -> projected.name,
            (bindings)));
          hydra.util.Lazy<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>> nodesVisitedIds1 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
            (addBindingName),
            (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>((java.util.List<hydra.accessors.AccessorNode>) (java.util.List.<hydra.accessors.AccessorNode>of()), visited.get()))), (ids)))),
            bindingNames.get()));
          hydra.util.Lazy<java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>> ids1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nodesVisitedIds1.get()));
          java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.AccessorNode, hydra.core.Binding>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>> addBindingTerm = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.AccessorNode, hydra.core.Binding>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>>) (currentState -> (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.AccessorNode, hydra.core.Binding>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>) (nodeBinding -> {
            hydra.util.Lazy<hydra.core.Binding> binding = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((nodeBinding)));
            hydra.util.Lazy<hydra.accessors.AccessorNode> root = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((nodeBinding)));
            hydra.core.Term term1 = (binding.get()).term;
            return (((((helper.get()).apply(ids1.get())).apply(hydra.util.Maybe.just(root.get()))).apply((java.util.List<hydra.accessors.TermAccessor>) (java.util.List.<hydra.accessors.TermAccessor>of()))).apply((currentState))).apply((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>((dontCareAccessor), (term1)))));
          }));
          hydra.core.Term env = (((letExpr)).value).body;
          hydra.util.Lazy<java.util.List<hydra.accessors.AccessorNode>> nodes1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply(nodesVisitedIds1.get())));
          hydra.util.Lazy<java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.AccessorNode, hydra.core.Binding>>> nodeBindingPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Zip.apply(
            nodes1.get(),
            (bindings)));
          hydra.util.Lazy<java.util.Set<String>> visited1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(nodesVisitedIds1.get())));
          hydra.util.Lazy<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>> stateAfterBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
            (addBindingTerm),
            (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>(hydra.lib.lists.Concat2.apply(
              nodes1.get(),
              nodes.get()), edges.get()))), visited1.get()))),
            nodeBindingPairs.get()));
          return (((((helper.get()).apply(ids1.get())).apply((mroot))).apply(nextPath.get())).apply(stateAfterBindings.get())).apply((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.LetBody(), (env)))));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>> visit(hydra.core.Term.Variable name) {
          return hydra.lib.maybes.Maybe.apply(
            (state),
            (java.util.function.Function<hydra.accessors.AccessorNode, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>) (root -> hydra.lib.maybes.Maybe.apply(
              (state),
              (java.util.function.Function<hydra.accessors.AccessorNode, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>) (node -> {
                hydra.util.Lazy<hydra.accessors.AccessorEdge> edge = new hydra.util.Lazy<>(() -> new hydra.accessors.AccessorEdge((root), new hydra.accessors.AccessorPath(hydra.lib.lists.Reverse.apply(nextPath.get())), (node)));
                hydra.util.Lazy<java.util.List<hydra.accessors.AccessorEdge>> newEdges = new hydra.util.Lazy<>(() -> hydra.lib.lists.Cons.apply(
                  edge.get(),
                  edges.get()));
                return (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>(nodes.get(), newEdges.get()))), visited.get())));
              }),
              hydra.lib.maps.Lookup.apply(
                ((name)).value,
                (ids)))),
            (mroot));
        }
      });
    }))))));
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>> result = new hydra.util.Lazy<>(() -> (((((helper.get()).apply((java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>) ((java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.accessors.AccessorNode>apply())))).apply((hydra.util.Maybe<hydra.accessors.AccessorNode>) (hydra.util.Maybe.<hydra.accessors.AccessorNode>nothing()))).apply((java.util.List<hydra.accessors.TermAccessor>) (java.util.List.<hydra.accessors.TermAccessor>of()))).apply(hydra.show.accessors.Accessors.<hydra.accessors.AccessorNode, hydra.accessors.AccessorEdge, String>termToAccessorGraph_initialState())).apply((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>((dontCareAccessor), (term))))));
    hydra.util.Lazy<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>> finalNodesEdges = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result.get()));
    hydra.util.Lazy<java.util.List<hydra.accessors.AccessorEdge>> finalEdges = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(finalNodesEdges.get()));
    hydra.util.Lazy<java.util.List<hydra.accessors.AccessorNode>> finalNodes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(finalNodesEdges.get()));
    return new hydra.accessors.AccessorGraph(finalNodes.get(), finalEdges.get());
  }
  
  static <T0, T1, T2> hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.List<T1>>, java.util.Set<T2>> termToAccessorGraph_initialState() {
    return (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.List<T1>>, java.util.Set<T2>>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.List<T1>>, java.util.Set<T2>>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.List<T1>>, java.util.Set<T2>>((hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.List<T1>>) ((hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.List<T1>>) (new hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.List<T1>>((java.util.List<T0>) (java.util.List.<T0>of()), (java.util.List<T1>) (java.util.List.<T1>of())))), (java.util.Set<T2>) (hydra.lib.sets.Empty.<T2>apply()))));
  }
}
