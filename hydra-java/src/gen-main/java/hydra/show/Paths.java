// Note: this is an automatically generated file. Do not edit.

package hydra.show;

/**
 * Utilities for working with subterm steps and paths.
 */
public interface Paths {
  static hydra.util.Maybe<String> subtermStep(hydra.paths.SubtermStep step) {
    return (step).accept(new hydra.paths.SubtermStep.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.AnnotatedBody ignored) {
        return (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing());
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.ApplicationFunction ignored) {
        return hydra.util.Maybe.just("fun");
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.ApplicationArgument ignored) {
        return hydra.util.Maybe.just("arg");
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.LambdaBody ignored) {
        return hydra.util.Maybe.just("body");
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.UnionCasesDefault ignored) {
        return hydra.util.Maybe.just("default");
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.UnionCasesBranch name) {
        return hydra.util.Maybe.just(hydra.lib.strings.Cat2.apply(
          ".",
          (name).value.value));
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.LetBody ignored) {
        return hydra.util.Maybe.just("in");
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.LetBinding name) {
        return hydra.util.Maybe.just(hydra.lib.strings.Cat2.apply(
          (name).value.value,
          "="));
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.ListElement i) {
        return hydra.show.Paths.subtermStep_idx((i).value);
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.MapKey i) {
        return hydra.show.Paths.subtermStep_idxSuff(
          ".key",
          (i).value);
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.MapValue i) {
        return hydra.show.Paths.subtermStep_idxSuff(
          ".value",
          (i).value);
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.MaybeTerm ignored) {
        return hydra.util.Maybe.just("just");
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.ProductTerm i) {
        return hydra.show.Paths.subtermStep_idx((i).value);
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.RecordField name) {
        return hydra.util.Maybe.just(hydra.lib.strings.Cat2.apply(
          ".",
          (name).value.value));
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.SetElement i) {
        return hydra.show.Paths.subtermStep_idx((i).value);
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.SumTerm ignored) {
        return (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing());
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.TypeLambdaBody ignored) {
        return (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing());
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.TypeApplicationTerm ignored) {
        return (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing());
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.InjectionTerm ignored) {
        return (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing());
      }

      @Override
      public hydra.util.Maybe<String> visit(hydra.paths.SubtermStep.WrappedTerm ignored) {
        return (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing());
      }
    });
  }

  static <T0, T1> hydra.util.Maybe<T1> subtermStep_idx(T0 i) {
    return (hydra.util.Maybe<T1>) (hydra.util.Maybe.<T1>nothing());
  }

  static <T0> hydra.util.Maybe<String> subtermStep_idxSuff(String suffix, T0 i) {
    return hydra.lib.maybes.Map.apply(
      (java.util.function.Function<String, String>) (s -> hydra.lib.strings.Cat2.apply(
        s,
        suffix)),
      hydra.show.Paths.<T0, String>subtermStep_idx(i));
  }

  static hydra.paths.SubtermGraph termToSubtermGraph(hydra.util.PersistentMap<hydra.module.Namespace, String> namespaces, hydra.core.Term term) {
    hydra.paths.SubtermStep dontCareStep = new hydra.paths.SubtermStep.AnnotatedBody();
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>, java.util.function.Function<hydra.util.Maybe<hydra.paths.SubtermNode>, java.util.function.Function<hydra.util.ConsList<hydra.paths.SubtermStep>, java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>, java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>>>>>>> helper = new java.util.concurrent.atomic.AtomicReference<>();
    helper.set((java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>, java.util.function.Function<hydra.util.Maybe<hydra.paths.SubtermNode>, java.util.function.Function<hydra.util.ConsList<hydra.paths.SubtermStep>, java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>, java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>>>>>>) (ids -> (java.util.function.Function<hydra.util.Maybe<hydra.paths.SubtermNode>, java.util.function.Function<hydra.util.ConsList<hydra.paths.SubtermStep>, java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>, java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>>>>>) (mroot -> (java.util.function.Function<hydra.util.ConsList<hydra.paths.SubtermStep>, java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>, java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>>>>) (path -> (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>, java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>>>) (state -> (java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>>) (stepTerm -> {
      hydra.util.Lazy<hydra.core.Term> currentTerm = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(stepTerm));
      hydra.util.Lazy<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>> nodesEdges = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(state));
      hydra.util.Lazy<hydra.util.ConsList<hydra.paths.SubtermEdge>> edges = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nodesEdges.get()));
      hydra.util.Lazy<hydra.paths.SubtermStep> step = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(stepTerm));
      hydra.util.Lazy<hydra.util.ConsList<hydra.paths.SubtermStep>> nextPath = new hydra.util.Lazy<>(() -> hydra.lib.lists.Cons.apply(
        step.get(),
        path));
      hydra.util.Lazy<hydra.util.ConsList<hydra.paths.SubtermNode>> nodes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(nodesEdges.get()));
      hydra.util.Lazy<hydra.util.PersistentSet<String>> visited = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(state));
      return currentTerm.get().accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>> otherwise(hydra.core.Term instance) {
          return hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>, java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>>>) (v1 -> (java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>>) (v2 -> helper.get().apply(ids).apply(mroot).apply(nextPath.get()).apply(v1).apply(v2))),
            state,
            hydra.Rewriting.subtermsWithSteps(currentTerm.get()));
        }

        @Override
        public hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>> visit(hydra.core.Term.Let letExpr) {
          java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>>, java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>>>> addBindingName = (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>>, java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>>>>) (nodesVisitedIds -> (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>>>) (name -> {
            hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>> currentIds = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nodesVisitedIds));
            hydra.util.Lazy<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>> currentNodesVisited = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(nodesVisitedIds));
            hydra.util.Lazy<hydra.util.ConsList<hydra.paths.SubtermNode>> currentNodes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(currentNodesVisited.get()));
            hydra.util.Lazy<hydra.util.PersistentSet<String>> currentVisited = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(currentNodesVisited.get()));
            String rawLabel = hydra.Names.compactName(
              namespaces,
              name);
            String uniqueLabel = hydra.Names.uniqueLabel(
              currentVisited.get(),
              rawLabel);
            hydra.paths.SubtermNode node = new hydra.paths.SubtermNode(name, rawLabel, uniqueLabel);
            hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>> newIds = new hydra.util.Lazy<>(() -> hydra.lib.maps.Insert.apply(
              name,
              node,
              currentIds.get()));
            hydra.util.Lazy<hydra.util.ConsList<hydra.paths.SubtermNode>> newNodes = new hydra.util.Lazy<>(() -> hydra.lib.lists.Cons.apply(
              node,
              currentNodes.get()));
            hydra.util.Lazy<hydra.util.PersistentSet<String>> newVisited = new hydra.util.Lazy<>(() -> hydra.lib.sets.Insert.apply(
              uniqueLabel,
              currentVisited.get()));
            return (hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>>) ((hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>>) (new hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>>((hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>) ((hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>) (new hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>(newNodes.get(), newVisited.get()))), newIds.get())));
          }));
          hydra.util.ConsList<hydra.core.Binding> bindings = (letExpr).value.bindings;
          hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> bindingNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
            projected -> projected.name,
            bindings));
          hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>>> nodesVisitedIds1 = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
            addBindingName,
            (hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>>) ((hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>>) (new hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>, hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>>((hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>) ((hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>) (new hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.PersistentSet<String>>((hydra.util.ConsList<hydra.paths.SubtermNode>) (hydra.util.ConsList.<hydra.paths.SubtermNode>empty()), visited.get()))), ids))),
            bindingNames.get()));
          hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>> ids1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nodesVisitedIds1.get()));
          java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>, java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermNode, hydra.core.Binding>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>>> addBindingTerm = (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>, java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermNode, hydra.core.Binding>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>>>) (currentState -> (java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermNode, hydra.core.Binding>, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>>) (nodeBinding -> {
            hydra.util.Lazy<hydra.core.Binding> binding = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(nodeBinding));
            hydra.util.Lazy<hydra.paths.SubtermNode> root = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(nodeBinding));
            hydra.core.Term term1 = binding.get().term;
            return helper.get().apply(ids1.get()).apply(hydra.util.Maybe.just(root.get())).apply((hydra.util.ConsList<hydra.paths.SubtermStep>) (hydra.util.ConsList.<hydra.paths.SubtermStep>empty())).apply(currentState).apply((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(dontCareStep, term1))));
          }));
          hydra.core.Term env = (letExpr).value.body;
          hydra.util.Lazy<hydra.util.ConsList<hydra.paths.SubtermNode>> nodes1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply(nodesVisitedIds1.get())));
          hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<hydra.paths.SubtermNode, hydra.core.Binding>>> nodeBindingPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Zip.apply(
            nodes1.get(),
            bindings));
          hydra.util.Lazy<hydra.util.PersistentSet<String>> visited1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(nodesVisitedIds1.get())));
          hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>> stateAfterBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
            addBindingTerm,
            (hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>) ((hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>) (new hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>((hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>) ((hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>) (new hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>(hydra.lib.lists.Concat2.apply(
              nodes1.get(),
              nodes.get()), edges.get()))), visited1.get()))),
            nodeBindingPairs.get()));
          return helper.get().apply(ids1.get()).apply(mroot).apply(nextPath.get()).apply(stateAfterBindings.get()).apply((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.LetBody(), env))));
        }

        @Override
        public hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>> visit(hydra.core.Term.Variable name) {
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> state,
            (java.util.function.Function<hydra.paths.SubtermNode, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>>) (root -> hydra.lib.maybes.Maybe.applyLazy(
              () -> state,
              (java.util.function.Function<hydra.paths.SubtermNode, hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>>) (node -> {
                hydra.util.Lazy<hydra.paths.SubtermEdge> edge = new hydra.util.Lazy<>(() -> new hydra.paths.SubtermEdge(root, new hydra.paths.SubtermPath(hydra.lib.lists.Reverse.apply(nextPath.get())), node));
                hydra.util.Lazy<hydra.util.ConsList<hydra.paths.SubtermEdge>> newEdges = new hydra.util.Lazy<>(() -> hydra.lib.lists.Cons.apply(
                  edge.get(),
                  edges.get()));
                return (hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>) ((hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>) (new hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>((hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>) ((hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>) (new hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>(nodes.get(), newEdges.get()))), visited.get())));
              }),
              hydra.lib.maps.Lookup.apply(
                (name).value,
                ids))),
            mroot);
        }
      });
    }))))));
    hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>, hydra.util.PersistentSet<String>>> result = new hydra.util.Lazy<>(() -> helper.get().apply((hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.paths.SubtermNode>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.paths.SubtermNode>apply()))).apply((hydra.util.Maybe<hydra.paths.SubtermNode>) (hydra.util.Maybe.<hydra.paths.SubtermNode>nothing())).apply((hydra.util.ConsList<hydra.paths.SubtermStep>) (hydra.util.ConsList.<hydra.paths.SubtermStep>empty())).apply(hydra.show.Paths.<hydra.paths.SubtermNode, hydra.paths.SubtermEdge, String>termToSubtermGraph_initialState()).apply((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(dontCareStep, term)))));
    hydra.util.Lazy<hydra.util.Pair<hydra.util.ConsList<hydra.paths.SubtermNode>, hydra.util.ConsList<hydra.paths.SubtermEdge>>> finalNodesEdges = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result.get()));
    hydra.util.Lazy<hydra.util.ConsList<hydra.paths.SubtermEdge>> finalEdges = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(finalNodesEdges.get()));
    hydra.util.Lazy<hydra.util.ConsList<hydra.paths.SubtermNode>> finalNodes = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(finalNodesEdges.get()));
    return new hydra.paths.SubtermGraph(finalNodes.get(), finalEdges.get());
  }

  static <T0, T1, T2> hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>, hydra.util.PersistentSet<T2>> termToSubtermGraph_initialState() {
    return (hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>, hydra.util.PersistentSet<T2>>) ((hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>, hydra.util.PersistentSet<T2>>) (new hydra.util.Pair<hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>, hydra.util.PersistentSet<T2>>((hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>) ((hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>) (new hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>((hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()), (hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty())))), (hydra.util.PersistentSet<T2>) (hydra.lib.sets.Empty.<T2>apply()))));
  }
}
