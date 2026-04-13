// Note: this is an automatically generated file. Do not edit.

package hydra.eval.lib;

/**
 * Evaluation-level implementations of Set functions for the Hydra interpreter.
 */
public interface Sets {
  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.core.Term> difference(T0 cx, hydra.graph.Graph g, hydra.core.Term set1Term, hydra.core.Term set2Term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.set(
        g,
        set1Term),
      (java.util.function.Function<java.util.Set<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.ifElse")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.member")), el)), set2Term)))), acc)), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.insert")), el)), acc)))))),
        new hydra.core.Term.Set(hydra.lib.sets.FromList.apply((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList()))),
        hydra.lib.sets.ToList.apply(elements)))));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.core.Term> intersection(T0 cx, hydra.graph.Graph g, hydra.core.Term set1Term, hydra.core.Term set2Term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.set(
        g,
        set1Term),
      (java.util.function.Function<java.util.Set<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.ifElse")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.member")), el)), set2Term)))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.insert")), el)), acc)))), acc)))),
        new hydra.core.Term.Set(hydra.lib.sets.FromList.apply((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList()))),
        hydra.lib.sets.ToList.apply(elements)))));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.core.Term> map(T0 cx, hydra.graph.Graph g, hydra.core.Term fun, hydra.core.Term setTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.set(
        g,
        setTerm),
      (java.util.function.Function<java.util.Set<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.fromList")), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application(fun, el))),
        hydra.lib.sets.ToList.apply(elements))))))));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.core.Term> union(T0 cx, hydra.graph.Graph g, hydra.core.Term set1Term, hydra.core.Term set2Term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.set(
        g,
        set1Term),
      (java.util.function.Function<java.util.Set<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.insert")), el)), acc)))),
        set2Term,
        hydra.lib.sets.ToList.apply(elements)))));
  }

  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.core.Term> unions(T0 cx, hydra.graph.Graph g, hydra.core.Term listTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        g,
        listTerm),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.errors.Error_, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (s -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.union")), acc)), s)))),
        new hydra.core.Term.Set(hydra.lib.sets.FromList.apply((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList()))),
        elements))));
  }
}
