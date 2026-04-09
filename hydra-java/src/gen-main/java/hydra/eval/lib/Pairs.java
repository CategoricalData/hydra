// Note: this is an automatically generated file. Do not edit.

package hydra.eval.lib;

/**
 * Evaluation-level implementations of Pair functions for the Hydra interpreter.
 */
public interface Pairs {
  static <T0, T1> hydra.util.Either<hydra.errors.Error_, hydra.core.Term> bimap(T0 cx, T1 g, hydra.core.Term firstFun, hydra.core.Term secondFun, hydra.core.Term pairTerm) {
    return (pairTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("pair value", hydra.show.Core.term(pairTerm)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.Term> visit(hydra.core.Term.Pair p) {
        hydra.util.Lazy<hydra.core.Term> fst = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply((p).value));
        hydra.util.Lazy<hydra.core.Term> snd = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply((p).value));
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(firstFun, fst.get())), new hydra.core.Term.Application(new hydra.core.Application(secondFun, snd.get())))))));
      }
    });
  }

  static <T0, T1> hydra.util.Either<hydra.errors.Error_, hydra.core.Term> first(T0 cx, T1 g, hydra.core.Term pairTerm) {
    return (pairTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("pair value", hydra.show.Core.term(pairTerm)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.Term> visit(hydra.core.Term.Pair p) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(hydra.lib.pairs.First.apply((p).value));
      }
    });
  }

  static <T0, T1> hydra.util.Either<hydra.errors.Error_, hydra.core.Term> second(T0 cx, T1 g, hydra.core.Term pairTerm) {
    return (pairTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>left(new hydra.errors.Error_.Extraction(new hydra.errors.ExtractionError.UnexpectedShape(new hydra.errors.UnexpectedShapeError("pair value", hydra.show.Core.term(pairTerm)))));
      }

      @Override
      public hydra.util.Either<hydra.errors.Error_, hydra.core.Term> visit(hydra.core.Term.Pair p) {
        return hydra.util.Either.<hydra.errors.Error_, hydra.core.Term>right(hydra.lib.pairs.Second.apply((p).value));
      }
    });
  }
}
