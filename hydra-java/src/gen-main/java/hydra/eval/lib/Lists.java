// Note: this is an automatically generated file. Do not edit.

package hydra.eval.lib;

/**
 * Evaluation-level implementations of List functions for the Hydra interpreter.
 */
public interface Lists {
  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> apply(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term funsTerm, hydra.core.Term argsTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        funsTerm),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (funs -> hydra.lib.eithers.Bind.apply(
        hydra.extract.Core.list(
          cx,
          g,
          argsTerm),
        (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (arguments -> {
          java.util.function.Function<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>> applyOne = (java.util.function.Function<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>>) (f -> hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (arg -> new hydra.core.Term.Application(new hydra.core.Application(f, arg))),
            arguments));
          return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(new hydra.core.Term.List(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
            applyOne,
            funs))));
        }))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> bind(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term listTerm, hydra.core.Term funTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        listTerm),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat"))), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, el))),
        elements)))))));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> concat2(T0 cx, T1 g, hydra.core.Term list1, hydra.core.Term list2) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat"))), new hydra.core.Term.List(hydra.util.ConsList.of(
      list1,
      list2)))));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> dropWhile(T0 cx, T1 g, hydra.core.Term predTerm, hydra.core.Term listTerm) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.span"))), predTerm)), listTerm)))));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> elem(T0 cx, T1 g, hydra.core.Term x, hydra.core.Term listTerm) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maybes.isJust"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.find"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.equality.equal"))), x)))), listTerm)))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> filter(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term predTerm, hydra.core.Term listTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        listTerm),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat"))), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.logic.ifElse"))), new hydra.core.Term.Application(new hydra.core.Application(predTerm, el)))), new hydra.core.Term.List(hydra.lib.lists.Pure.apply(el)))), new hydra.core.Term.List((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty()))))),
        elements)))))));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> find(T0 cx, T1 g, hydra.core.Term predTerm, hydra.core.Term listTerm) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.safeHead"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.filter"))), predTerm)), listTerm)))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> foldl(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term funTerm, hydra.core.Term initTerm, hydra.core.Term listTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        listTerm),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements -> hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (el -> hydra.lib.eithers.Bind.apply(
          acc,
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (reducedAcc -> hydra.Reduction.reduceTerm(
            cx,
            g,
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(funTerm, reducedAcc)), el))))))),
        hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(initTerm),
        elements)));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> foldr(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term funTerm, hydra.core.Term initTerm, hydra.core.Term listTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        listTerm),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements -> hydra.lib.lists.Foldr.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>>) (el -> (java.util.function.Function<hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (acc -> hydra.lib.eithers.Bind.apply(
          acc,
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (reducedAcc -> hydra.Reduction.reduceTerm(
            cx,
            g,
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(funTerm, el)), reducedAcc))))))),
        hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(initTerm),
        elements)));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> group(T0 cx, T1 g, hydra.core.Term listTerm) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("foldResult"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.logic.ifElse"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.null"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.first"))), new hydra.core.Term.Variable(new hydra.core.Name("foldResult")))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), new hydra.core.Term.Variable(new hydra.core.Name("foldResult")))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat2"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), new hydra.core.Term.Variable(new hydra.core.Name("foldResult")))))), new hydra.core.Term.List(hydra.util.ConsList.of(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.first"))), new hydra.core.Term.Variable(new hydra.core.Name("foldResult"))))))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.foldl"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("acc"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("el"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maybes.maybe"))), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.List(hydra.util.ConsList.of(new hydra.core.Term.Variable(new hydra.core.Name("el")))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), new hydra.core.Term.Variable(new hydra.core.Name("acc")))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("h"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.logic.ifElse"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.equality.equal"))), new hydra.core.Term.Variable(new hydra.core.Name("el")))), new hydra.core.Term.Variable(new hydra.core.Name("h")))))), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat2"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.first"))), new hydra.core.Term.Variable(new hydra.core.Name("acc")))))), new hydra.core.Term.List(hydra.util.ConsList.of(new hydra.core.Term.Variable(new hydra.core.Name("el")))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), new hydra.core.Term.Variable(new hydra.core.Name("acc")))))))))), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.List(hydra.util.ConsList.of(new hydra.core.Term.Variable(new hydra.core.Name("el")))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat2"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), new hydra.core.Term.Variable(new hydra.core.Name("acc")))))), new hydra.core.Term.List(hydra.util.ConsList.of(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.first"))), new hydra.core.Term.Variable(new hydra.core.Name("acc"))))))))))))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.safeHead"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.first"))), new hydra.core.Term.Variable(new hydra.core.Name("acc")))))))))))))))), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.List((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())), new hydra.core.Term.List((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())))))))), listTerm)))));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> intercalate(T0 cx, T1 g, hydra.core.Term sep, hydra.core.Term listsTerm) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.intersperse"))), sep)), listsTerm)))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> intersperse(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term sep, hydra.core.Term listTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        listTerm),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(elements),
        () -> new hydra.core.Term.List((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())),
        () -> new hydra.core.Term.List(hydra.lib.lists.Cons.apply(
          hydra.lib.lists.Head.apply(elements),
          hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>>) (el -> hydra.util.ConsList.of(
              sep,
              el)),
            hydra.lib.lists.Tail.apply(elements)))))))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> map(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term funTerm, hydra.core.Term listTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        listTerm),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(new hydra.core.Term.List(hydra.lib.lists.Reverse.apply(hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.util.ConsList<hydra.core.Term>>) (el -> hydra.lib.lists.Cons.apply(
          new hydra.core.Term.Application(new hydra.core.Application(funTerm, el)),
          acc))),
        (hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty()),
        elements))))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> maybeHead(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term listTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        listTerm),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(elements),
        () -> new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())),
        () -> new hydra.core.Term.Maybe(hydra.util.Maybe.just(hydra.lib.lists.Head.apply(elements)))))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> nub(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term listTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        listTerm),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.foldl"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("acc"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.logic.ifElse"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.elem"))), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Variable(new hydra.core.Name("acc")))))), new hydra.core.Term.Variable(new hydra.core.Name("acc")))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat2"))), new hydra.core.Term.Variable(new hydra.core.Name("acc")))), new hydra.core.Term.List(hydra.util.ConsList.of(new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))))))))), new hydra.core.Term.List((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())))), listTerm)))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> partition(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term predTerm, hydra.core.Term listTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        listTerm),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements -> {
        hydra.util.Lazy<hydra.core.Term> initialState = new hydra.util.Lazy<>(() -> new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.List((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())), new hydra.core.Term.List((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())))))));
        hydra.util.Lazy<hydra.core.Term> finalState = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> {
            hydra.core.Term nos = new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), acc));
            hydra.core.Term yeses = new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.first"))), acc));
            return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.logic.ifElse"))), new hydra.core.Term.Application(new hydra.core.Application(predTerm, el)))), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat2"))), yeses)), new hydra.core.Term.List(hydra.util.ConsList.of(el)))), nos)))))), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(yeses, new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat2"))), nos)), new hydra.core.Term.List(hydra.util.ConsList.of(el))))))))));
          })),
          initialState.get(),
          elements));
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(finalState.get());
      }));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> pure(T0 cx, T1 g, hydra.core.Term x) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.List(hydra.util.ConsList.of(x)));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> replicate(T0 cx, T1 g, hydra.core.Term n, hydra.core.Term x) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.map"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("_"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), x))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.math.range"))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))), n)))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> safeHead(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term listTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        listTerm),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(elements),
        () -> new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())),
        () -> new hydra.core.Term.Maybe(hydra.util.Maybe.just(hydra.lib.lists.Head.apply(elements)))))));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> singleton(T0 cx, T1 g, hydra.core.Term x) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.List(hydra.util.ConsList.of(x)));
  }

  static <T0, T1, T2> hydra.util.Either<T2, hydra.core.Term> sort(T0 cx, T1 g, hydra.core.Term listTerm) {
    return hydra.util.Either.<T2, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.sortOn"))), new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.equality.identity"))))), listTerm)));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> sortOn(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term projTerm, hydra.core.Term listTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        listTerm),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(hydra.lib.lists.Foldl.apply(
        (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (sorted -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (x -> {
          hydra.util.Lazy<hydra.core.Term> splitResult = new hydra.util.Lazy<>(() -> new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.span"))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.equality.lte"))), new hydra.core.Term.Application(new hydra.core.Application(projTerm, new hydra.core.Term.Variable(new hydra.core.Name("y")))))), new hydra.core.Term.Application(new hydra.core.Application(projTerm, x))))))))), sorted)));
          hydra.core.Term after = new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), splitResult.get()));
          hydra.core.Term before = new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.first"))), splitResult.get()));
          return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat2"))), before)), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.cons"))), x)), after))));
        })),
        new hydra.core.Term.List((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())),
        elements))));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> span(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term predTerm, hydra.core.Term listTerm) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        listTerm),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements -> {
        hydra.util.Lazy<hydra.core.Term> initialState = new hydra.util.Lazy<>(() -> new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)), new hydra.core.Term.List((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())))))), new hydra.core.Term.List((hydra.util.ConsList<hydra.core.Term>) (hydra.util.ConsList.<hydra.core.Term>empty())))))));
        hydra.util.Lazy<hydra.core.Term> finalState = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> {
            hydra.core.Term takingLeft = new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.first"))), acc));
            hydra.core.Term left = new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), takingLeft));
            hydra.core.Term right = new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), acc));
            hydra.core.Term taking = new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.first"))), takingLeft));
            return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.logic.ifElse"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.logic.and"))), taking)), new hydra.core.Term.Application(new hydra.core.Application(predTerm, el)))))), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat2"))), left)), new hydra.core.Term.List(hydra.util.ConsList.of(el)))))))), right)))))), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)), left)))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat2"))), right)), new hydra.core.Term.List(hydra.util.ConsList.of(el))))))))));
          })),
          initialState.get(),
          elements));
        return hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.first"))), finalState.get())))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.pairs.second"))), finalState.get())))))));
      }));
  }

  static hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term> zipWith(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term funTerm, hydra.core.Term listTerm1, hydra.core.Term listTerm2) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.Core.list(
        cx,
        g,
        listTerm1),
      (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements1 -> hydra.lib.eithers.Bind.apply(
        hydra.extract.Core.list(
          cx,
          g,
          listTerm2),
        (java.util.function.Function<hydra.util.ConsList<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>>) (elements2 -> hydra.util.Either.<hydra.context.InContext<hydra.errors.Error_>, hydra.core.Term>right(new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.core.Term>) (p -> {
            hydra.util.Lazy<hydra.core.Term> a = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
            hydra.util.Lazy<hydra.core.Term> b = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(p));
            return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(funTerm, a.get())), b.get()));
          }),
          hydra.lib.lists.Zip.apply(
            elements1,
            elements2))))))));
  }
}
