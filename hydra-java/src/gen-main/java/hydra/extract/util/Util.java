// Note: this is an automatically generated file. Do not edit.

package hydra.extract.util;

/**
 * Extraction and validation for hydra.util types
 */
public interface Util {
  static hydra.compute.Flow<hydra.graph.Graph, hydra.util.Comparison> comparison(hydra.core.Term term) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.unitVariant(
        new hydra.core.Name("hydra.util.Comparison"),
        term),
      (java.util.function.Function<hydra.core.Name, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Comparison>>) (fname -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          (fname).value,
          "equalTo"),
        () -> hydra.lib.flows.Pure.apply(new hydra.util.Comparison.EqualTo()),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (fname).value,
            "lessThan"),
          () -> hydra.lib.flows.Pure.apply(new hydra.util.Comparison.LessThan()),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              (fname).value,
              "greaterThan"),
            () -> hydra.lib.flows.Pure.apply(new hydra.util.Comparison.GreaterThan()),
            () -> hydra.monads.Monads.unexpected(
              "comparison",
              (fname).value))))));
  }
}
