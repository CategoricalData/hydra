// Note: this is an automatically generated file. Do not edit.

package hydra.eval.lib;

/**
 * Evaluation-level implementations of Maybe functions for the Hydra interpreter.
 */
public interface Maybes {
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> apply(hydra.context.Context cx, T0 g, hydra.core.Term funOptTerm, hydra.core.Term argOptTerm) {
    return (funOptTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "optional function"),
            " but found "),
          hydra.show.Core.term(funOptTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe mf) {
        return (argOptTerm).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                hydra.lib.strings.Cat2.apply(
                  "expected ",
                  "optional value"),
                " but found "),
              hydra.show.Core.term(argOptTerm)))), cx)));
          }

          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe mx) {
            return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(new hydra.core.Term.Maybe(hydra.lib.maybes.Bind.apply(
              (mf).value,
              (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Term>>) (f -> hydra.lib.maybes.Map.apply(
                (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (x -> new hydra.core.Term.Application(new hydra.core.Application(f, x))),
                (mx).value)))));
          }
        });
      }
    });
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> bind(hydra.context.Context cx, T0 g, hydra.core.Term optTerm, hydra.core.Term funTerm) {
    return (optTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "optional value"),
            " but found "),
          hydra.show.Core.term(optTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.maybes.Maybe.applyLazy(
          () -> new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())),
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, val))),
          (m).value));
      }
    });
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> cases(hydra.context.Context cx, T0 g, hydra.core.Term optTerm, hydra.core.Term defaultTerm, hydra.core.Term funTerm) {
    return (optTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "optional value"),
            " but found "),
          hydra.show.Core.term(optTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.maybes.Maybe.applyLazy(
          () -> defaultTerm,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, val))),
          (m).value));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.core.Term>> cat(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term listTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        listTerm),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.core.Term>>>) (elements -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, java.util.List<hydra.core.Term>>right(hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<java.util.List<hydra.core.Term>, java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>>) (acc -> (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (el -> (el).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public java.util.List<hydra.core.Term> otherwise(hydra.core.Term instance) {
            return acc;
          }

          @Override
          public java.util.List<hydra.core.Term> visit(hydra.core.Term.Maybe m) {
            return hydra.lib.maybes.Maybe.applyLazy(
              () -> acc,
              (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (v -> hydra.lib.lists.Concat2.apply(
                acc,
                hydra.lib.lists.Pure.apply(v))),
              (m).value);
          }
        }))),
        (java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList()),
        elements))));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> compose(T0 cx, T1 g, hydra.core.Term funF, hydra.core.Term funG, hydra.core.Term xTerm) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maybes.bind")), new hydra.core.Term.Application(new hydra.core.Application(funF, xTerm)))), funG)));
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> fromJust(hydra.context.Context cx, T0 g, hydra.core.Term optTerm) {
    return (optTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "optional value"),
            " but found "),
          hydra.show.Core.term(optTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "expected ",
                "Just value"),
              " but found "),
            hydra.show.Core.term(optTerm)))), cx))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (val -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(val)),
          (m).value);
      }
    });
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> fromMaybe(hydra.context.Context cx, T0 g, hydra.core.Term defaultTerm, hydra.core.Term optTerm) {
    return (optTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "optional value"),
            " but found "),
          hydra.show.Core.term(optTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.maybes.Maybe.applyLazy(
          () -> defaultTerm,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> val),
          (m).value));
      }
    });
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> isJust(hydra.context.Context cx, T0 g, hydra.core.Term optTerm) {
    return (optTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "optional value"),
            " but found "),
          hydra.show.Core.term(optTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.maybes.Maybe.applyLazy(
          () -> new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)),
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (ignored -> new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true))),
          (m).value));
      }
    });
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> isNothing(hydra.context.Context cx, T0 g, hydra.core.Term optTerm) {
    return (optTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "optional value"),
            " but found "),
          hydra.show.Core.term(optTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.maybes.Maybe.applyLazy(
          () -> new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)),
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (ignored -> new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false))),
          (m).value));
      }
    });
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> map(hydra.context.Context cx, T0 g, hydra.core.Term funTerm, hydra.core.Term optTerm) {
    return (optTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "optional value"),
            " but found "),
          hydra.show.Core.term(optTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, val))),
          (m).value)));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> mapMaybe(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term funTerm, hydra.core.Term listTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        listTerm),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maybes.cat")), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, el))),
        elements)))))));
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> maybe(hydra.context.Context cx, T0 g, hydra.core.Term defaultTerm, hydra.core.Term funTerm, hydra.core.Term optTerm) {
    return (optTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "optional value"),
            " but found "),
          hydra.show.Core.term(optTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.maybes.Maybe.applyLazy(
          () -> defaultTerm,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, val))),
          (m).value));
      }
    });
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> pure(T0 cx, T1 g, hydra.core.Term x) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Maybe(hydra.util.Maybe.just(x)));
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> toList(hydra.context.Context cx, T0 g, hydra.core.Term optTerm) {
    return (optTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "optional value"),
            " but found "),
          hydra.show.Core.term(optTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(new hydra.core.Term.List(hydra.lib.maybes.Maybe.applyLazy(
          () -> (java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList()),
          (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (val -> hydra.lib.lists.Pure.apply(val)),
          (m).value)));
      }
    });
  }
}
