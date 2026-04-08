// Note: this is an automatically generated file. Do not edit.

package hydra.eval.lib;

/**
 * Evaluation-level implementations of Logic functions for the Hydra interpreter.
 */
public interface Logic {
  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> and(T0 cx, T1 g, hydra.core.Term a, hydra.core.Term b) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.ifElse")), a)), b)), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)))));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> not(T0 cx, T1 g, hydra.core.Term a) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.ifElse")), a)), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)))), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> or(T0 cx, T1 g, hydra.core.Term a, hydra.core.Term b) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.ifElse")), a)), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))), b)));
  }
}
