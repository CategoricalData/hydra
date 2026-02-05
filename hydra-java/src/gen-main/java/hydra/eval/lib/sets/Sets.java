// Note: this is an automatically generated file. Do not edit.

package hydra.eval.lib.sets;

/**
 * Evaluation-level implementations of Set functions for the Hydra interpreter.
 */
public interface Sets {
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> map(hydra.core.Term fun, hydra.core.Term setTerm) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.set((setTerm)),
      (java.util.function.Function<java.util.Set<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (elements -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.sets.fromList"))), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application((fun), (el)))),
        hydra.lib.sets.ToList.apply((elements)))))))));
  }
}
