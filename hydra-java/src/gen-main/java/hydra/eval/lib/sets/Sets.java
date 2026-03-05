// Note: this is an automatically generated file. Do not edit.

package hydra.eval.lib.sets;

/**
 * Evaluation-level implementations of Set functions for the Hydra interpreter.
 */
public interface Sets {
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> map(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term fun, hydra.core.Term setTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.set(
        cx,
        g,
        setTerm),
      (java.util.function.Function<java.util.Set<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>>) (elements -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.sets.fromList"))), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application(fun, el))),
        hydra.lib.sets.ToList.apply(elements))))))))));
  }
}
