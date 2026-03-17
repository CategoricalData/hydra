// Note: this is an automatically generated file. Do not edit.

package hydra.eval.lib.maybes;

/**
 * Evaluation-level implementations of Maybe functions for the Hydra interpreter.
 */
public interface Maybes {
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> apply(hydra.context.Context cx, T0 g, hydra.core.Term funOptTerm, hydra.core.Term argOptTerm) {
    return (funOptTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "optional function"),
            " but found "),
          hydra.show.core.Core.term(funOptTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe mf) {
        return (argOptTerm).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
            return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                hydra.lib.strings.Cat2.apply(
                  "expected ",
                  "optional value"),
                " but found "),
              hydra.show.core.Core.term(argOptTerm)))), cx)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe mx) {
            return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Maybe(hydra.lib.maybes.Bind.apply(
              (mf).value,
              (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Term>>) (f -> hydra.lib.maybes.Map.apply(
                (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (x -> new hydra.core.Term.Application(new hydra.core.Application(f, x))),
                (mx).value)))));
          }
        });
      }
    });
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> bind(hydra.context.Context cx, T0 g, hydra.core.Term optTerm, hydra.core.Term funTerm) {
    return (optTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "optional value"),
            " but found "),
          hydra.show.core.Core.term(optTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(hydra.lib.maybes.Maybe.applyLazy(
          () -> new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())),
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, val))),
          (m).value));
      }
    });
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> cases(hydra.context.Context cx, T0 g, hydra.core.Term optTerm, hydra.core.Term defaultTerm, hydra.core.Term funTerm) {
    return (optTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "optional value"),
            " but found "),
          hydra.show.core.Core.term(optTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(hydra.lib.maybes.Maybe.applyLazy(
          () -> defaultTerm,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, val))),
          (m).value));
      }
    });
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> compose(T0 cx, T1 g, hydra.core.Term funF, hydra.core.Term funG, hydra.core.Term xTerm) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maybes.bind"))), new hydra.core.Term.Application(new hydra.core.Application(funF, xTerm)))), funG)));
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> map(hydra.context.Context cx, T0 g, hydra.core.Term funTerm, hydra.core.Term optTerm) {
    return (optTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "optional value"),
            " but found "),
          hydra.show.core.Core.term(optTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, val))),
          (m).value)));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> mapMaybe(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term funTerm, hydra.core.Term listTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.list(
        cx,
        g,
        listTerm),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maybes.cat"))), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, el))),
        elements)))))));
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> maybe(hydra.context.Context cx, T0 g, hydra.core.Term defaultTerm, hydra.core.Term funTerm, hydra.core.Term optTerm) {
    return (optTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "optional value"),
            " but found "),
          hydra.show.core.Core.term(optTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(hydra.lib.maybes.Maybe.applyLazy(
          () -> defaultTerm,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, val))),
          (m).value));
      }
    });
  }
}
