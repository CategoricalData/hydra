// Note: this is an automatically generated file. Do not edit.

package hydra.eval.lib.flows;

/**
 * Evaluation-level implementations of Flow functions for the Hydra interpreter.
 */
public interface Flows {
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> apply(hydra.core.Term flowFun, hydra.core.Term flowArg) {
    return hydra.lib.flows.Pure.apply(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.bind"))), (flowFun))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.bind"))), (flowArg))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.pure"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))))))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> bind(hydra.core.Term flowTerm, hydra.core.Term funTerm) {
    return ((flowTerm)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "flow term",
          hydra.show.core.Core.term((flowTerm)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Wrap wrappedTerm) {
        hydra.core.Term innerFun = (((wrappedTerm)).value).body;
        return hydra.lib.flows.Pure.apply(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.compute.Flow"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("s"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("t"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maybes.maybe"))), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.compute.FlowState"), java.util.List.of(
          new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))),
          new hydra.core.Field(new hydra.core.Name("state"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.compute.FlowState"), new hydra.core.Name("state"))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application((innerFun), new hydra.core.Term.Variable(new hydra.core.Name("s")))), new hydra.core.Term.Variable(new hydra.core.Name("t"))))))),
          new hydra.core.Field(new hydra.core.Name("trace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.compute.FlowState"), new hydra.core.Name("trace"))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application((innerFun), new hydra.core.Term.Variable(new hydra.core.Name("s")))), new hydra.core.Term.Variable(new hydra.core.Name("t")))))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.compute.Flow")))), new hydra.core.Term.Application(new hydra.core.Application((funTerm), new hydra.core.Term.Variable(new hydra.core.Name("v")))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.compute.FlowState"), new hydra.core.Name("state"))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application((innerFun), new hydra.core.Term.Variable(new hydra.core.Name("s")))), new hydra.core.Term.Variable(new hydra.core.Name("t")))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.compute.FlowState"), new hydra.core.Name("trace"))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application((innerFun), new hydra.core.Term.Variable(new hydra.core.Name("s")))), new hydra.core.Term.Variable(new hydra.core.Name("t"))))))))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.compute.FlowState"), new hydra.core.Name("value"))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application((innerFun), new hydra.core.Term.Variable(new hydra.core.Name("s")))), new hydra.core.Term.Variable(new hydra.core.Name("t")))))))))))))))));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> foldl(hydra.core.Term funTerm, hydra.core.Term initTerm, hydra.core.Term listTerm) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.list((listTerm)),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (elements -> hydra.lib.flows.Pure.apply(hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.bind"))), (acc))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("accVal"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application((funTerm), new hydra.core.Term.Variable(new hydra.core.Name("accVal")))), (el)))))))))),
        new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.pure"))), (initTerm))),
        (elements)))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> map(hydra.core.Term funTerm, hydra.core.Term flowTerm) {
    return ((flowTerm)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "flow term",
          hydra.show.core.Core.term((flowTerm)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Wrap wrappedTerm) {
        hydra.core.Term innerFun = (((wrappedTerm)).value).body;
        return hydra.lib.flows.Pure.apply(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.compute.Flow"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("s"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("t"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.compute.FlowState"), java.util.List.of(
          new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maybes.map"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application((funTerm), new hydra.core.Term.Variable(new hydra.core.Name("v"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.compute.FlowState"), new hydra.core.Name("value"))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application((innerFun), new hydra.core.Term.Variable(new hydra.core.Name("s")))), new hydra.core.Term.Variable(new hydra.core.Name("t"))))))))),
          new hydra.core.Field(new hydra.core.Name("state"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.compute.FlowState"), new hydra.core.Name("state"))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application((innerFun), new hydra.core.Term.Variable(new hydra.core.Name("s")))), new hydra.core.Term.Variable(new hydra.core.Name("t"))))))),
          new hydra.core.Field(new hydra.core.Name("trace"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.compute.FlowState"), new hydra.core.Name("trace"))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application((innerFun), new hydra.core.Term.Variable(new hydra.core.Name("s")))), new hydra.core.Term.Variable(new hydra.core.Name("t")))))))))))))))))));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> mapElems(hydra.core.Term funTerm, hydra.core.Term mapTerm) {
    return ((mapTerm)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "map value",
          hydra.show.core.Core.term((mapTerm)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Map m) {
        hydra.util.Lazy<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>> pairs = new hydra.util.Lazy<>(() -> hydra.lib.maps.ToList.apply(((m)).value));
        return hydra.lib.flows.Pure.apply(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.bind"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.sequence"))), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.core.Term>) (p -> {
            hydra.util.Lazy<hydra.core.Term> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((p)));
            hydra.util.Lazy<hydra.core.Term> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((p)));
            return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.map"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v'"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(k.get(), new hydra.core.Term.Variable(new hydra.core.Name("v'"))))))))))), new hydra.core.Term.Application(new hydra.core.Application((funTerm), v.get()))));
          }),
          pairs.get())))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("newPairs"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.pure"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maps.fromList"))), new hydra.core.Term.Variable(new hydra.core.Name("newPairs"))))))))))));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> mapKeys(hydra.core.Term funTerm, hydra.core.Term mapTerm) {
    return ((mapTerm)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "map value",
          hydra.show.core.Core.term((mapTerm)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Map m) {
        hydra.util.Lazy<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>> pairs = new hydra.util.Lazy<>(() -> hydra.lib.maps.ToList.apply(((m)).value));
        return hydra.lib.flows.Pure.apply(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.bind"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.sequence"))), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.core.Term>) (p -> {
            hydra.util.Lazy<hydra.core.Term> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((p)));
            hydra.util.Lazy<hydra.core.Term> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((p)));
            return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.map"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("k'"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Variable(new hydra.core.Name("k'")), v.get()))))))))), new hydra.core.Term.Application(new hydra.core.Application((funTerm), k.get()))));
          }),
          pairs.get())))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("newPairs"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.pure"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maps.fromList"))), new hydra.core.Term.Variable(new hydra.core.Name("newPairs"))))))))))));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> mapList(hydra.core.Term funTerm, hydra.core.Term listTerm) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.list((listTerm)),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (elements -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.sequence"))), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application((funTerm), (el)))),
        (elements))))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> mapMaybe(hydra.core.Term funTerm, hydra.core.Term maybeTerm) {
    return ((maybeTerm)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "optional value",
          hydra.show.core.Core.term((maybeTerm)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.logic.ifElse"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maybes.isNothing"))), new hydra.core.Term.Maybe(((m)).value))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("_"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.pure"))), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("_"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.map"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))), new hydra.core.Term.Application(new hydra.core.Application((funTerm), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maybes.fromJust"))), new hydra.core.Term.Maybe(((m)).value)))))))))))), new hydra.core.Term.Unit())));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> mapSet(hydra.core.Term funTerm, hydra.core.Term setTerm) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.set((setTerm)),
      (java.util.function.Function<java.util.Set<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (elements -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.map"))), new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.sets.fromList"))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.flows.sequence"))), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application((funTerm), (el)))),
        hydra.lib.sets.ToList.apply((elements)))))))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> withDefault(hydra.core.Term fallbackTerm, hydra.core.Term flowTerm) {
    return ((flowTerm)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "flow term",
          hydra.show.core.Core.term((flowTerm)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Wrap wrappedTerm) {
        hydra.core.Term innerFun = (((wrappedTerm)).value).body;
        return hydra.lib.flows.Pure.apply(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.compute.Flow"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("s"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("t"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maybes.maybe"))), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.compute.FlowState"), java.util.List.of(
          new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Maybe(hydra.util.Maybe.just((fallbackTerm)))),
          new hydra.core.Field(new hydra.core.Name("state"), new hydra.core.Term.Variable(new hydra.core.Name("s"))),
          new hydra.core.Field(new hydra.core.Name("trace"), new hydra.core.Term.Variable(new hydra.core.Name("t")))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("_"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application((innerFun), new hydra.core.Term.Variable(new hydra.core.Name("s")))), new hydra.core.Term.Variable(new hydra.core.Name("t"))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.compute.FlowState"), new hydra.core.Name("value"))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application((innerFun), new hydra.core.Term.Variable(new hydra.core.Name("s")))), new hydra.core.Term.Variable(new hydra.core.Name("t")))))))))))))))));
      }
    });
  }
}
