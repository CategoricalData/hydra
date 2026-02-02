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
    hydra.accessors.TermAccessor dontCareAccessor = new hydra.accessors.TermAccessor.AnnotatedBody(true);
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>, java.util.function.Function<hydra.util.Maybe<hydra.accessors.AccessorNode>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>>>>>> helper = new java.util.concurrent.atomic.AtomicReference<>();
    helper.set((java.util.function.Function<java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>, java.util.function.Function<hydra.util.Maybe<hydra.accessors.AccessorNode>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>>>>>) (ids -> (java.util.function.Function<hydra.util.Maybe<hydra.accessors.AccessorNode>, java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>>>>) (mroot -> (java.util.function.Function<java.util.List<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>>>) (path -> (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>>) (state -> (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>) (accessorTerm -> {
      hydra.accessors.TermAccessor accessor = hydra.lib.pairs.First.apply((accessorTerm));
      hydra.core.Term currentTerm = hydra.lib.pairs.Second.apply((accessorTerm));
      hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>> nodesEdges = hydra.lib.pairs.First.apply((state));
      java.util.List<hydra.accessors.AccessorEdge> edges = hydra.lib.pairs.Second.apply((nodesEdges));
      java.util.List<hydra.accessors.TermAccessor> nextPath = hydra.lib.lists.Cons.apply(
        (accessor),
        (path));
      java.util.List<hydra.accessors.AccessorNode> nodes = hydra.lib.pairs.First.apply((nodesEdges));
      java.util.Set<String> visited = hydra.lib.pairs.Second.apply((state));
      return ((currentTerm)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>> otherwise(hydra.core.Term instance) {
          return hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>>) (v1 -> (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>) (v2 -> (((((helper.get()).apply((ids))).apply((mroot))).apply((nextPath))).apply((v1))).apply((v2)))),
            (state),
            hydra.rewriting.Rewriting.subtermsWithAccessors((currentTerm)));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>> visit(hydra.core.Term.Let letExpr) {
          java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>, java.util.function.Function<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>>> addBindingName = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>, java.util.function.Function<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>>>) (nodesVisitedIds -> (java.util.function.Function<hydra.core.Name, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>>) (name -> {
            java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode> currentIds = hydra.lib.pairs.Second.apply((nodesVisitedIds));
            hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>> currentNodesVisited = hydra.lib.pairs.First.apply((nodesVisitedIds));
            java.util.List<hydra.accessors.AccessorNode> currentNodes = hydra.lib.pairs.First.apply((currentNodesVisited));
            java.util.Set<String> currentVisited = hydra.lib.pairs.Second.apply((currentNodesVisited));
            String rawLabel = hydra.names.Names.compactName(
              (namespaces),
              (name));
            String uniqueLabel = hydra.names.Names.uniqueLabel(
              (currentVisited),
              (rawLabel));
            hydra.accessors.AccessorNode node = new hydra.accessors.AccessorNode((name), (rawLabel), (uniqueLabel));
            java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode> newIds = hydra.lib.maps.Insert.apply(
              (name),
              (node),
              (currentIds));
            java.util.List<hydra.accessors.AccessorNode> newNodes = hydra.lib.lists.Cons.apply(
              (node),
              (currentNodes));
            java.util.Set<String> newVisited = hydra.lib.sets.Insert.apply(
              (uniqueLabel),
              (currentVisited));
            return (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>((newNodes), (newVisited)))), (newIds))));
          }));
          java.util.List<hydra.core.Binding> bindings = (((letExpr)).value).bindings;
          java.util.List<hydra.core.Name> bindingNames = hydra.lib.lists.Map.apply(
            projected -> projected.name,
            (bindings));
          hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>> nodesVisitedIds1 = hydra.lib.lists.Foldl.apply(
            (addBindingName),
            (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>, java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>>((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.Set<String>>((java.util.List<hydra.accessors.AccessorNode>) (java.util.List.<hydra.accessors.AccessorNode>of()), (visited)))), (ids)))),
            (bindingNames));
          java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode> ids1 = hydra.lib.pairs.Second.apply((nodesVisitedIds1));
          java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.AccessorNode, hydra.core.Binding>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>> addBindingTerm = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.AccessorNode, hydra.core.Binding>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>>) (currentState -> (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.accessors.AccessorNode, hydra.core.Binding>, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>) (nodeBinding -> {
            hydra.core.Binding binding = hydra.lib.pairs.Second.apply((nodeBinding));
            hydra.accessors.AccessorNode root = hydra.lib.pairs.First.apply((nodeBinding));
            hydra.core.Term term1 = ((binding)).term;
            return (((((helper.get()).apply((ids1))).apply(hydra.util.Maybe.just((root)))).apply((java.util.List<hydra.accessors.TermAccessor>) (java.util.List.<hydra.accessors.TermAccessor>of()))).apply((currentState))).apply((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>((dontCareAccessor), (term1)))));
          }));
          hydra.core.Term env = (((letExpr)).value).body;
          java.util.List<hydra.accessors.AccessorNode> nodes1 = hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply((nodesVisitedIds1)));
          java.util.List<hydra.util.Tuple.Tuple2<hydra.accessors.AccessorNode, hydra.core.Binding>> nodeBindingPairs = hydra.lib.lists.Zip.apply(
            (nodes1),
            (bindings));
          java.util.Set<String> visited1 = hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply((nodesVisitedIds1)));
          hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>> stateAfterBindings = hydra.lib.lists.Foldl.apply(
            (addBindingTerm),
            (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>(hydra.lib.lists.Concat2.apply(
              (nodes1),
              (nodes)), (edges)))), (visited1)))),
            (nodeBindingPairs));
          return (((((helper.get()).apply((ids1))).apply((mroot))).apply((nextPath))).apply((stateAfterBindings))).apply((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>(new hydra.accessors.TermAccessor.LetBody(true), (env)))));
        }
        
        @Override
        public hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>> visit(hydra.core.Term.Variable name) {
          return hydra.lib.maybes.Maybe.apply(
            (state),
            (java.util.function.Function<hydra.accessors.AccessorNode, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>) (root -> hydra.lib.maybes.Maybe.apply(
              (state),
              (java.util.function.Function<hydra.accessors.AccessorNode, hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>>) (node -> {
                hydra.accessors.AccessorEdge edge = new hydra.accessors.AccessorEdge((root), new hydra.accessors.AccessorPath(hydra.lib.lists.Reverse.apply((nextPath))), (node));
                java.util.List<hydra.accessors.AccessorEdge> newEdges = hydra.lib.lists.Cons.apply(
                  (edge),
                  (edges));
                return (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>>((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>) ((hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>) (new hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>((nodes), (newEdges)))), (visited))));
              }),
              hydra.lib.maps.Lookup.apply(
                ((name)).value,
                (ids)))),
            (mroot));
        }
      });
    }))))));
    hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>>, java.util.Set<String>> result = (((((helper.get()).apply((java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>) ((java.util.Map<hydra.core.Name, hydra.accessors.AccessorNode>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.accessors.AccessorNode>apply())))).apply((hydra.util.Maybe<hydra.accessors.AccessorNode>) (hydra.util.Maybe.<hydra.accessors.AccessorNode>nothing()))).apply((java.util.List<hydra.accessors.TermAccessor>) (java.util.List.<hydra.accessors.TermAccessor>of()))).apply(hydra.show.accessors.Accessors.<hydra.accessors.AccessorNode, hydra.accessors.AccessorEdge, String>termToAccessorGraph_initialState())).apply((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.accessors.TermAccessor, hydra.core.Term>((dontCareAccessor), (term)))));
    hydra.util.Tuple.Tuple2<java.util.List<hydra.accessors.AccessorNode>, java.util.List<hydra.accessors.AccessorEdge>> finalNodesEdges = hydra.lib.pairs.First.apply((result));
    java.util.List<hydra.accessors.AccessorEdge> finalEdges = hydra.lib.pairs.Second.apply((finalNodesEdges));
    java.util.List<hydra.accessors.AccessorNode> finalNodes = hydra.lib.pairs.First.apply((finalNodesEdges));
    return new hydra.accessors.AccessorGraph((finalNodes), (finalEdges));
  }
  
  static <T0, T1, T2> hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.List<T1>>, java.util.Set<T2>> termToAccessorGraph_initialState() {
    return (hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.List<T1>>, java.util.Set<T2>>) ((hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.List<T1>>, java.util.Set<T2>>) (new hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.List<T1>>, java.util.Set<T2>>((hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.List<T1>>) ((hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.List<T1>>) (new hydra.util.Tuple.Tuple2<java.util.List<T0>, java.util.List<T1>>((java.util.List<T0>) (java.util.List.<T0>of()), (java.util.List<T1>) (java.util.List.<T1>of())))), (java.util.Set<T2>) (hydra.lib.sets.Empty.<T2>apply()))));
  }
}
