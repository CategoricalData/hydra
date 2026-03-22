// Note: this is an automatically generated file. Do not edit.

package hydra.eval.lib;

/**
 * Evaluation-level implementations of Either functions for the Hydra interpreter.
 */
public interface Eithers {
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> bind(hydra.context.Context cx, T0 g, hydra.core.Term eitherTerm, hydra.core.Term funTerm) {
    return (eitherTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "either value"),
            " but found "),
          hydra.show.Core.term(eitherTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Either e) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(val))),
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, val))),
          (e).value));
      }
    });
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> bimap(hydra.context.Context cx, T0 g, hydra.core.Term leftFun, hydra.core.Term rightFun, hydra.core.Term eitherTerm) {
    return (eitherTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "either value"),
            " but found "),
          hydra.show.Core.term(eitherTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Either e) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Application(new hydra.core.Application(leftFun, val))))),
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(rightFun, val))))),
          (e).value));
      }
    });
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> either(hydra.context.Context cx, T0 g, hydra.core.Term leftFun, hydra.core.Term rightFun, hydra.core.Term eitherTerm) {
    return (eitherTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "either value"),
            " but found "),
          hydra.show.Core.term(eitherTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Either e) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Application(new hydra.core.Application(leftFun, val))),
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Application(new hydra.core.Application(rightFun, val))),
          (e).value));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> foldl(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term funTerm, hydra.core.Term initTerm, hydra.core.Term listTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        listTerm),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("a"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(funTerm, new hydra.core.Term.Variable(new hydra.core.Name("a")))), el))))))), acc)))),
        new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(initTerm)),
        elements))));
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> map(hydra.context.Context cx, T0 g, hydra.core.Term rightFun, hydra.core.Term eitherTerm) {
    return (eitherTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "either value"),
            " but found "),
          hydra.show.Core.term(eitherTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Either e) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(val))),
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(rightFun, val))))),
          (e).value));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> mapList(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term funTerm, hydra.core.Term listTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        listTerm),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("accErr"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("accErr"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("ys"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.cons"))), new hydra.core.Term.Variable(new hydra.core.Name("y")))), new hydra.core.Term.Variable(new hydra.core.Name("ys"))))))))))), acc))))))), new hydra.core.Term.Application(new hydra.core.Application(funTerm, el)))))),
        new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.List((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())))),
        hydra.lib.lists.Reverse.apply(elements)))));
  }

  static <T0> hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> mapMaybe(hydra.context.Context cx, T0 g, hydra.core.Term funTerm, hydra.core.Term maybeTerm) {
    return (maybeTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.errors.Error_>) (new hydra.context.InContext<hydra.errors.Error_>(new hydra.errors.Error_.Other(new hydra.errors.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "maybe value"),
            " but found "),
          hydra.show.Core.term(maybeTerm)))), cx)));
      }

      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> visit(hydra.core.Term.Maybe opt) {
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.maybes.Maybe.applyLazy(
          () -> new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))),
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Variable(new hydra.core.Name("y"))))))))))), new hydra.core.Term.Application(new hydra.core.Application(funTerm, val))))),
          (opt).value));
      }
    });
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> mapSet(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term funTerm, hydra.core.Term setTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.set(
        cx,
        g,
        setTerm),
      (java.util.function.Function<hydra.util.PersistentSet<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("err"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("err"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.eithers.either"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("accErr"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(new hydra.core.Term.Variable(new hydra.core.Name("accErr"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("ys"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.sets.insert"))), new hydra.core.Term.Variable(new hydra.core.Name("y")))), new hydra.core.Term.Variable(new hydra.core.Name("ys"))))))))))), acc))))))), new hydra.core.Term.Application(new hydra.core.Application(funTerm, el)))))),
        new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(new hydra.core.Term.Set(hydra.lib.sets.FromList.apply((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty()))))),
        hydra.lib.sets.ToList.apply(elements)))));
  }
}
