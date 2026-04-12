// Note: this is an automatically generated file. Do not edit.

package hydra.eval.lib;

/**
 * Evaluation-level implementations of Equality functions for the Hydra interpreter.
 */
public interface Equality {
  static <T0, T1, T2, T3> hydra.util.Either<T3, T2> identity(T0 cx, T1 g, T2 x) {
    return hydra.util.Either.<T3, T2>right(x);
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> max(T0 cx, T1 g, hydra.core.Term x, hydra.core.Term y) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.ifElse")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.equality.gte")), x)), y)))), x)), y)));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> min(T0 cx, T1 g, hydra.core.Term x, hydra.core.Term y) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.ifElse")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.equality.lte")), x)), y)))), x)), y)));
  }
}
