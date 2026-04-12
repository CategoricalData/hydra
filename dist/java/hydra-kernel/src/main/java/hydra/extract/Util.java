// Note: this is an automatically generated file. Do not edit.

package hydra.extract;

/**
 * Extraction and validation for hydra.util types
 */
public interface Util {
  static <T0> hydra.util.Either<hydra.errors.Error_, hydra.util.Comparison> comparison(T0 cx, hydra.graph.Graph graph, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.unitVariant(
        new hydra.core.Name("hydra.util.Comparison"),
        graph,
        term),
      (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.Error_, hydra.util.Comparison>>) (fname -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          (fname).value,
          "equalTo"),
        () -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Comparison>right(new hydra.util.Comparison.EqualTo()),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (fname).value,
            "lessThan"),
          () -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Comparison>right(new hydra.util.Comparison.LessThan()),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              (fname).value,
              "greaterThan"),
            () -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Comparison>right(new hydra.util.Comparison.GreaterThan()),
            () -> hydra.util.Either.<hydra.errors.Error_, hydra.util.Comparison>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("comparison", (fname).value)))))))));
  }
}
