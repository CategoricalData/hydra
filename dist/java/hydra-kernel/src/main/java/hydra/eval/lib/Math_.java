// Note: this is an automatically generated file. Do not edit.

package hydra.eval.lib;

/**
 * Evaluation-level implementations of Math functions for the Hydra interpreter.
 */
public interface Math_ {
  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> even(T0 cx, T1 g, hydra.core.Term x) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.equality.equal")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mod")), x)), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> odd(T0 cx, T1 g, hydra.core.Term x) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.not")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.even")), x)))));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> pred(T0 cx, T1 g, hydra.core.Term x) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sub")), x)), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> succ(T0 cx, T1 g, hydra.core.Term x) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), x)), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))));
  }
}
