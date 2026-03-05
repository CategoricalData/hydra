// Note: this is an automatically generated file. Do not edit.

package hydra.eval.lib.pairs;

/**
 * Evaluation-level implementations of Pair functions for the Hydra interpreter.
 */
public interface Pairs {
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> bimap(hydra.context.Context cx, T0 g, hydra.core.Term firstFun, hydra.core.Term secondFun, hydra.core.Term pairTerm) {
    return (pairTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "pair value"),
            " but found "),
          hydra.show.core.Core.term(pairTerm))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term> visit(hydra.core.Term.Pair p) {
        hydra.util.Lazy<hydra.core.Term> fst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((p).value));
        hydra.util.Lazy<hydra.core.Term> snd = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((p).value));
        return (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.core.Term>right(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(firstFun, fst.get())), new hydra.core.Term.Application(new hydra.core.Application(secondFun, snd.get())))))))));
      }
    });
  }
}
