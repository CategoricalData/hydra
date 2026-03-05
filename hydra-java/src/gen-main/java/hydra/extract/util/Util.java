// Note: this is an automatically generated file. Do not edit.

package hydra.extract.util;

/**
 * Extraction and validation for hydra.util types
 */
public interface Util {
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Comparison> comparison(hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.unitVariant(
        cx,
        new hydra.core.Name("hydra.util.Comparison"),
        graph,
        term),
      (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Comparison>>) (fname -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          (fname).value,
          "equalTo"),
        () -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Comparison>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Comparison>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Comparison>right(new hydra.util.Comparison.EqualTo()))),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (fname).value,
            "lessThan"),
          () -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Comparison>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Comparison>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Comparison>right(new hydra.util.Comparison.LessThan()))),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              (fname).value,
              "greaterThan"),
            () -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Comparison>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Comparison>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Comparison>right(new hydra.util.Comparison.GreaterThan()))),
            () -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Comparison>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Comparison>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.util.Comparison>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
              "expected comparison but found ",
              (fname).value)), cx))))))))));
  }
}
