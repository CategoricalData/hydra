// Note: this is an automatically generated file. Do not edit.

package hydra.show.accessors;

/**
 * Utilities for working with term accessors.
 */
public interface Accessors {
  static hydra.util.Maybe<String> termAccessor(hydra.accessors.TermAccessor accessor) {
    return (accessor).accept(new hydra.accessors.TermAccessor.PartialVisitor<>() {
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
          (name).value.value));
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.LetBody ignored) {
        return hydra.util.Maybe.just("in");
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.LetBinding name) {
        return hydra.util.Maybe.just(hydra.lib.strings.Cat2.apply(
          (name).value.value,
          "="));
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.ListElement i) {
        return hydra.show.accessors.Accessors.termAccessor_idx((i).value);
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.MapKey i) {
        return hydra.show.accessors.Accessors.termAccessor_idxSuff(
          ".key",
          (i).value);
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.MapValue i) {
        return hydra.show.accessors.Accessors.termAccessor_idxSuff(
          ".value",
          (i).value);
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.MaybeTerm ignored) {
        return hydra.util.Maybe.just("just");
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.ProductTerm i) {
        return hydra.show.accessors.Accessors.termAccessor_idx((i).value);
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.RecordField name) {
        return hydra.util.Maybe.just(hydra.lib.strings.Cat2.apply(
          ".",
          (name).value.value));
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.accessors.TermAccessor.SetElement i) {
        return hydra.show.accessors.Accessors.termAccessor_idx((i).value);
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
        s,
        suffix)),
      hydra.show.accessors.Accessors.<T0, String>termAccessor_idx(i));
  }
  
  static hydra.accessors.AccessorGraph termToAccessorGraph(hydra.util.PersistentMap<hydra.module.Namespace, String> namespaces, hydra.core.Term term) {
    hydra.accessors.TermAccessor dontCareAccessor = new hydra.accessors.TermAccessor.AnnotatedBody();
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>, java.util.function.Function<hydra.util.Maybe<hydra.accessors.AccessorNode>, java.util.function.Function<hydra.util.ConsList<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>, java.util.function.Function<hydra.util.Pair<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>>>>>>> helper = new java.util.concurrent.atomic.AtomicReference<>();
    helper.set((java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>, java.util.function.Function<hydra.util.Maybe<hydra.accessors.AccessorNode>, java.util.function.Function<hydra.util.ConsList<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>, java.util.function.Function<hydra.util.Pair<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>>>>>>) (ids -> (java.util.function.Function<hydra.util.Maybe<hydra.accessors.AccessorNode>, java.util.function.Function<hydra.util.ConsList<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>, java.util.function.Function<hydra.util.Pair<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>>>>>) (mroot -> (java.util.function.Function<hydra.util.ConsList<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>, java.util.function.Function<hydra.util.Pair<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>>>>) (path -> (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>, java.util.function.Function<hydra.util.Pair<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>>>) (state -> (java.util.function.Function<hydra.util.Pair<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>>) (accessorTerm -> {
      hydra.util.Lazy<hydra.accessors.TermAccessor> accessor = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(accessorTerm));
      hydra.util.Lazy<hydra.core.Term> currentTerm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(accessorTerm));
      hydra.util.Lazy<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>> nodesEdges = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(state));
      hydra.util.Lazy<hydra.util.ConsList<hydra.accessors.AccessorEdge>> edges = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nodesEdges.get()));
      hydra.util.Lazy<hydra.util.ConsList<hydra.accessors.TermAccessor>> nextPath = new hydra.util.Lazy<>(() -> hydra.lib.lists.Cons.apply(
        accessor.get(),
        path));
      hydra.util.Lazy<hydra.util.ConsList<hydra.accessors.AccessorNode>> nodes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(nodesEdges.get()));
      hydra.util.Lazy<hydra.util.PersistentSet<String>> visited = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(state));
      return currentTerm.get().accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>> otherwise(hydra.core.Term instance) {
          return hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>, java.util.function.Function<hydra.util.Pair<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>>>) (v1 -> (java.util.function.Function<hydra.util.Pair<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>>) (v2 -> helper.get().apply(ids).apply(mroot).apply(nextPath.get()).apply(v1).apply(v2))),
            state,
            hydra.rewriting.Rewriting.subtermsWithAccessors(currentTerm.get()));
        }
        
        @Override
        public hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>> visit(hydra.core.Term.Let letExpr) {
          java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>>, java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>>>> addBindingName = (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>>, java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>>>>) (nodesVisitedIds -> (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>>>) (name -> {
            hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>> currentIds = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nodesVisitedIds));
            hydra.util.Lazy<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>> currentNodesVisited = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(nodesVisitedIds));
            hydra.util.Lazy<hydra.util.ConsList<hydra.accessors.AccessorNode>> currentNodes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(currentNodesVisited.get()));
            hydra.util.Lazy<hydra.util.PersistentSet<String>> currentVisited = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(currentNodesVisited.get()));
            String rawLabel = hydra.names.Names.compactName(
              namespaces,
              name);
            String uniqueLabel = hydra.names.Names.uniqueLabel(
              currentVisited.get(),
              rawLabel);
            hydra.accessors.AccessorNode node = new hydra.accessors.AccessorNode(name, rawLabel, uniqueLabel);
            hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>> newIds = new hydra.util.Lazy<>(() -> hydra.lib.maps.Insert.apply(
              name,
              node,
              currentIds.get()));
            hydra.util.Lazy<hydra.util.ConsList<hydra.accessors.AccessorNode>> newNodes = new hydra.util.Lazy<>(() -> hydra.lib.lists.Cons.apply(
              node,
              currentNodes.get()));
            hydra.util.Lazy<hydra.util.PersistentSet<String>> newVisited = new hydra.util.Lazy<>(() -> hydra.lib.sets.Insert.apply(
              uniqueLabel,
              currentVisited.get()));
            return (hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>>) ((hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>>) (new hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>>((hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>) ((hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>) (new hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>(newNodes.get(), newVisited.get()))), newIds.get())));
          }));
          hydra.util.ConsList<hydra.core.Binding> bindings = (letExpr).value.bindings;
          hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> bindingNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
            projected -> projected.name,
            bindings));
          hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>>> nodesVisitedIds1 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
            addBindingName,
            (hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>>) ((hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>>) (new hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>>((hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>) ((hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>) (new hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.PersistentSet<String>>((hydra.util.ConsList<hydra.accessors.AccessorNode>) (hydra.util.ConsList.<hydra.accessors.AccessorNode>empty()), visited.get()))), ids))),
            bindingNames.get()));
          hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>> ids1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nodesVisitedIds1.get()));
          java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>, java.util.function.Function<hydra.util.Pair<hydra.accessors.AccessorNode, hydra.core.Binding>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>>> addBindingTerm = (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>, java.util.function.Function<hydra.util.Pair<hydra.accessors.AccessorNode, hydra.core.Binding>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>>>) (currentState -> (java.util.function.Function<hydra.util.Pair<hydra.accessors.AccessorNode, hydra.core.Binding>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>>) (nodeBinding -> {
            hydra.util.Lazy<hydra.core.Binding> binding = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nodeBinding));
            hydra.util.Lazy<hydra.accessors.AccessorNode> root = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(nodeBinding));
            hydra.core.Term term1 = binding.get().term;
            return helper.get().apply(ids1.get()).apply(hydra.util.Maybe.just(root.get())).apply((hydra.util.ConsList<hydra.accessors.TermAccessor>) (hydra.util.ConsList.<hydra.accessors.TermAccessor>empty())).apply(currentState).apply((hydra.util.Pair<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Pair<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Pair<hydra.accessors.TermAccessor, hydra.core.Term>(dontCareAccessor, term1))));
          }));
          hydra.core.Term env = (letExpr).value.body;
          hydra.util.Lazy<hydra.util.ConsList<hydra.accessors.AccessorNode>> nodes1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply(nodesVisitedIds1.get())));
          hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<hydra.accessors.AccessorNode, hydra.core.Binding>>> nodeBindingPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Zip.apply(
            nodes1.get(),
            bindings));
          hydra.util.Lazy<hydra.util.PersistentSet<String>> visited1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(nodesVisitedIds1.get())));
          hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>> stateAfterBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
            addBindingTerm,
            (hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>) ((hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>) (new hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>((hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>) ((hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>) (new hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>(hydra.lib.lists.Concat2.apply(
              nodes1.get(),
              nodes.get()), edges.get()))), visited1.get()))),
            nodeBindingPairs.get()));
          return helper.get().apply(ids1.get()).apply(mroot).apply(nextPath.get()).apply(stateAfterBindings.get()).apply((hydra.util.Pair<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Pair<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Pair<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.LetBody(), env))));
        }
        
        @Override
        public hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>> visit(hydra.core.Term.Variable name) {
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> state,
            (java.util.function.Function<hydra.accessors.AccessorNode, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>>) (root -> hydra.lib.maybes.Maybe.applyLazy(
              () -> state,
              (java.util.function.Function<hydra.accessors.AccessorNode, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>>) (node -> {
                hydra.util.Lazy<hydra.accessors.AccessorEdge> edge = new hydra.util.Lazy<>(() -> new hydra.accessors.AccessorEdge(root, new hydra.accessors.AccessorPath(hydra.lib.lists.Reverse.apply(nextPath.get())), node));
                hydra.util.Lazy<hydra.util.ConsList<hydra.accessors.AccessorEdge>> newEdges = new hydra.util.Lazy<>(() -> hydra.lib.lists.Cons.apply(
                  edge.get(),
                  edges.get()));
                return (hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>) ((hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>) (new hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>((hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>) ((hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>) (new hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>(nodes.get(), newEdges.get()))), visited.get())));
              }),
              hydra.lib.maps.Lookup.apply(
                (name).value,
                ids))),
            mroot);
        }
      });
    }))))));
    hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>, hydra.util.PersistentSet<String>>> result = new hydra.util.Lazy<>(() -> helper.get().apply((hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.accessors.AccessorNode>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.accessors.AccessorNode>apply()))).apply((hydra.util.Maybe<hydra.accessors.AccessorNode>) (hydra.util.Maybe.<hydra.accessors.AccessorNode>nothing())).apply((hydra.util.ConsList<hydra.accessors.TermAccessor>) (hydra.util.ConsList.<hydra.accessors.TermAccessor>empty())).apply(hydra.show.accessors.Accessors.<hydra.accessors.AccessorNode, hydra.accessors.AccessorEdge, String>termToAccessorGraph_initialState()).apply((hydra.util.Pair<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Pair<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Pair<hydra.accessors.TermAccessor, hydra.core.Term>(dontCareAccessor, term)))));
    hydra.util.Lazy<hydra.util.Pair<hydra.util.ConsList<hydra.accessors.AccessorNode>, hydra.util.ConsList<hydra.accessors.AccessorEdge>>> finalNodesEdges = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result.get()));
    hydra.util.Lazy<hydra.util.ConsList<hydra.accessors.AccessorEdge>> finalEdges = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(finalNodesEdges.get()));
    hydra.util.Lazy<hydra.util.ConsList<hydra.accessors.AccessorNode>> finalNodes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(finalNodesEdges.get()));
    return new hydra.accessors.AccessorGraph(finalNodes.get(), finalEdges.get());
  }
  
  static <T0, T1, T2> hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>, hydra.util.PersistentSet<T2>> termToAccessorGraph_initialState() {
    return (hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>, hydra.util.PersistentSet<T2>>) ((hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>, hydra.util.PersistentSet<T2>>) (new hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>, hydra.util.PersistentSet<T2>>((hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>) ((hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>) (new hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>((hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()), (hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty())))), (hydra.util.PersistentSet<T2>) (hydra.lib.sets.Empty.<T2>apply()))));
  }
}
