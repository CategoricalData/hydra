// Note: this is an automatically generated file. Do not edit.

package hydra.eval.lib.lists;

/**
 * Evaluation-level implementations of List functions for the Hydra interpreter.
 */
public interface Lists {
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> apply(hydra.core.Term funsTerm, hydra.core.Term argsTerm) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.list(funsTerm),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (funs -> hydra.lib.flows.Bind.apply(
        hydra.extract.core.Core.list(argsTerm),
        (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (arguments -> {
          java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>> applyOne = (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (f -> hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (arg -> new hydra.core.Term.Application(new hydra.core.Application(f, arg))),
            arguments));
          return hydra.lib.flows.Pure.apply(new hydra.core.Term.List(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
            applyOne,
            funs))));
        }))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> bind(hydra.core.Term listTerm, hydra.core.Term funTerm) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.list(listTerm),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (elements -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat"))), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, el))),
        elements)))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> dropWhile(hydra.core.Term predTerm, hydra.core.Term listTerm) {
    return hydra.lib.flows.Pure.apply(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.span"))), predTerm)), listTerm)))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> filter(hydra.core.Term predTerm, hydra.core.Term listTerm) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.list(listTerm),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (elements -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat"))), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.logic.ifElse"))), new hydra.core.Term.Application(new hydra.core.Application(predTerm, el)))), new hydra.core.Term.List(hydra.lib.lists.Pure.apply(el)))), new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))))),
        elements)))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> find(hydra.core.Term predTerm, hydra.core.Term listTerm) {
    return hydra.lib.flows.Pure.apply(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.safeHead"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.filter"))), predTerm)), listTerm)))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> foldl(hydra.core.Term funTerm, hydra.core.Term initTerm, hydra.core.Term listTerm) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.list(listTerm),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (elements -> hydra.lib.flows.Pure.apply(hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(funTerm, acc)), el)))),
        initTerm,
        elements))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> map(hydra.core.Term funTerm, hydra.core.Term listTerm) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.list(listTerm),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (elements -> hydra.lib.flows.Pure.apply(new hydra.core.Term.List(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<java.util.List<hydra.core.Term>, java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>>) (acc -> (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (el -> hydra.lib.lists.Cons.apply(
          new hydra.core.Term.Application(new hydra.core.Application(funTerm, el)),
          acc))),
        (java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()),
        elements))))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> partition(hydra.core.Term predTerm, hydra.core.Term listTerm) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.list(listTerm),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (elements -> {
        hydra.util.Lazy<hydra.core.Term> initialState = new hydra.util.Lazy<>(() -> new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of())), new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of())))))));
        hydra.util.Lazy<hydra.core.Term> finalState = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> {
            hydra.core.Term nos = new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), acc));
            hydra.core.Term yeses = new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.first"))), acc));
            return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.logic.ifElse"))), new hydra.core.Term.Application(new hydra.core.Application(predTerm, el)))), new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat2"))), yeses)), new hydra.core.Term.List(java.util.List.of(el)))), nos)))))), new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(yeses, new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat2"))), nos)), new hydra.core.Term.List(java.util.List.of(el))))))))));
          })),
          initialState.get(),
          elements));
        return hydra.lib.flows.Pure.apply(finalState.get());
      }));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> sortOn(hydra.core.Term projTerm, hydra.core.Term listTerm) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.list(listTerm),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (elements -> hydra.lib.flows.Pure.apply(hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (sorted -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (x -> {
          hydra.util.Lazy<hydra.core.Term> splitResult = new hydra.util.Lazy<>(() -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.span"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.equality.lte"))), new hydra.core.Term.Application(new hydra.core.Application(projTerm, new hydra.core.Term.Variable(new hydra.core.Name("y")))))), new hydra.core.Term.Application(new hydra.core.Application(projTerm, x))))))))), sorted)));
          hydra.core.Term after = new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), splitResult.get()));
          hydra.core.Term before = new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.first"))), splitResult.get()));
          return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat2"))), before)), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.cons"))), x)), after))));
        })),
        new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of())),
        elements))));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> span(hydra.core.Term predTerm, hydra.core.Term listTerm) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.list(listTerm),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (elements -> {
        hydra.util.Lazy<hydra.core.Term> initialState = new hydra.util.Lazy<>(() -> new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)), new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of())))))), new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of())))))));
        hydra.util.Lazy<hydra.core.Term> finalState = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> {
            hydra.core.Term takingLeft = new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.first"))), acc));
            hydra.core.Term left = new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), takingLeft));
            hydra.core.Term right = new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), acc));
            hydra.core.Term taking = new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.first"))), takingLeft));
            return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.logic.ifElse"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.logic.and"))), taking)), new hydra.core.Term.Application(new hydra.core.Application(predTerm, el)))))), new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat2"))), left)), new hydra.core.Term.List(java.util.List.of(el)))))))), right)))))), new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)), left)))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat2"))), right)), new hydra.core.Term.List(java.util.List.of(el))))))))));
          })),
          initialState.get(),
          elements));
        return hydra.lib.flows.Pure.apply(new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.first"))), finalState.get())))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), finalState.get())))))));
      }));
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> zipWith(hydra.core.Term funTerm, hydra.core.Term listTerm1, hydra.core.Term listTerm2) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.list(listTerm1),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (elements1 -> hydra.lib.flows.Bind.apply(
        hydra.extract.core.Core.list(listTerm2),
        (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (elements2 -> hydra.lib.flows.Pure.apply(new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.core.Term>) (p -> {
            hydra.util.Lazy<hydra.core.Term> a = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
            hydra.util.Lazy<hydra.core.Term> b = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(p));
            return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(funTerm, a.get())), b.get()));
          }),
          hydra.lib.lists.Zip.apply(
            elements1,
            elements2))))))));
  }
}
