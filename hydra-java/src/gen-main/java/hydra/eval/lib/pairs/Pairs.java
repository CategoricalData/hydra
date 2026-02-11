// Note: this is an automatically generated file. Do not edit.

package hydra.eval.lib.pairs;

/**
 * Evaluation-level implementations of Pair functions for the Hydra interpreter.
 */
public interface Pairs {
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> bimap(hydra.core.Term firstFun, hydra.core.Term secondFun, hydra.core.Term pairTerm) {
    return (pairTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "pair value",
          hydra.show.core.Core.term(pairTerm));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Pair p) {
        hydra.util.Lazy<hydra.core.Term> fst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((p).value));
        hydra.util.Lazy<hydra.core.Term> snd = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((p).value));
        return hydra.lib.flows.Pure.apply(new hydra.core.Term.Pair((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(firstFun, fst.get())), new hydra.core.Term.Application(new hydra.core.Application(secondFun, snd.get())))))));
      }
    });
  }
}
