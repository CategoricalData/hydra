// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Core rewrite and fold combinators for terms and types
 */
public interface Rewriting {
  static hydra.core.Term applyInsideTypeLambdasAndAnnotations(java.util.function.Function<hydra.core.Term, hydra.core.Term> f, hydra.core.Term term0) {
    return (term0).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term otherwise(hydra.core.Term instance) {
        return (f).apply(term0);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(hydra.Rewriting.applyInsideTypeLambdasAndAnnotations(
          f,
          (at).value.body), (at).value.annotation));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda tl) {
        return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda((tl).value.parameter, hydra.Rewriting.applyInsideTypeLambdasAndAnnotations(
          f,
          (tl).value.body)));
      }
    });
  }

  static <T0> T0 foldOverTerm(hydra.coders.TraversalOrder order, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T0>> fld, T0 b0, hydra.core.Term term) {
    return (order).accept(new hydra.coders.TraversalOrder.PartialVisitor<>() {
      @Override
      public T0 visit(hydra.coders.TraversalOrder.Pre ignored) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T0>>) (v1 -> (java.util.function.Function<hydra.core.Term, T0>) (v2 -> hydra.Rewriting.<T0>foldOverTerm(
            order,
            fld,
            v1,
            v2))),
          (fld).apply(b0).apply(term),
          hydra.Rewriting.subterms(term));
      }

      @Override
      public T0 visit(hydra.coders.TraversalOrder.Post ignored) {
        return (fld).apply(hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T0>>) (v1 -> (java.util.function.Function<hydra.core.Term, T0>) (v2 -> hydra.Rewriting.<T0>foldOverTerm(
            order,
            fld,
            v1,
            v2))),
          b0,
          hydra.Rewriting.subterms(term))).apply(term);
      }
    });
  }

  static <T0> T0 foldOverType(hydra.coders.TraversalOrder order, java.util.function.Function<T0, java.util.function.Function<hydra.core.Type, T0>> fld, T0 b0, hydra.core.Type typ) {
    return (order).accept(new hydra.coders.TraversalOrder.PartialVisitor<>() {
      @Override
      public T0 visit(hydra.coders.TraversalOrder.Pre ignored) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<T0, java.util.function.Function<hydra.core.Type, T0>>) (v1 -> (java.util.function.Function<hydra.core.Type, T0>) (v2 -> hydra.Rewriting.<T0>foldOverType(
            order,
            fld,
            v1,
            v2))),
          (fld).apply(b0).apply(typ),
          hydra.Rewriting.subtypes(typ));
      }

      @Override
      public T0 visit(hydra.coders.TraversalOrder.Post ignored) {
        return (fld).apply(hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<T0, java.util.function.Function<hydra.core.Type, T0>>) (v1 -> (java.util.function.Function<hydra.core.Type, T0>) (v2 -> hydra.Rewriting.<T0>foldOverType(
            order,
            fld,
            v1,
            v2))),
          b0,
          hydra.Rewriting.subtypes(typ))).apply(typ);
      }
    });
  }

  static <T0> T0 foldTermWithGraphAndPath(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T0>>, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T0>>>>> f, hydra.graph.Graph cx0, T0 val0, hydra.core.Term term0) {
    return hydra.lib.pairs.First.apply(hydra.Rewriting.<T0>foldTermWithGraphAndPath_result(
      f,
      cx0,
      term0,
      val0));
  }

  static <T0, T1> hydra.util.Pair<T0, T1> foldTermWithGraphAndPath_r(java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, T1>>> recurse, hydra.core.Term subterm, T0 valIn) {
    return (recurse).apply(valIn).apply(subterm);
  }

  static <T0, T1> T0 foldTermWithGraphAndPath_recurseForUser(java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, T1>>> recurse, T0 valIn, hydra.core.Term subterm) {
    return hydra.lib.pairs.First.apply(hydra.Rewriting.<T0, T1>foldTermWithGraphAndPath_r(
      recurse,
      subterm,
      valIn));
  }

  static <T0> hydra.util.Pair<T0, hydra.core.Term> foldTermWithGraphAndPath_result(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T0>>, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T0>>>>> f, hydra.graph.Graph cx0, hydra.core.Term term0, T0 val0) {
    return hydra.Rewriting.<T0>rewriteAndFoldTermWithGraphAndPath(
      (java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>>>) (v1 -> (java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>>) (v2 -> (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>) (v3 -> (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>) (v4 -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>) (v5 -> hydra.Rewriting.foldTermWithGraphAndPath_wrapper(
        f,
        v1,
        v2,
        v3,
        v4,
        v5)))))),
      cx0,
      val0,
      term0);
  }

  static <T0, T1> hydra.util.Pair<T0, hydra.core.Term> foldTermWithGraphAndPath_wrapper(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T0>>, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T0>>>>> f, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, T1>>> recurse, java.util.List<hydra.paths.SubtermStep> path, hydra.graph.Graph cx, T0 val, hydra.core.Term term) {
    return (hydra.util.Pair<T0, hydra.core.Term>) ((hydra.util.Pair<T0, hydra.core.Term>) (new hydra.util.Pair<T0, hydra.core.Term>((f).apply((java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, T0>>) (v1 -> (java.util.function.Function<hydra.core.Term, T0>) (v2 -> hydra.Rewriting.<T0, T1>foldTermWithGraphAndPath_recurseForUser(
      recurse,
      v1,
      v2)))).apply(path).apply(cx).apply(val).apply(term), term)));
  }

  static hydra.core.Type mapBeneathTypeAnnotations(java.util.function.Function<hydra.core.Type, hydra.core.Type> f, hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return (f).apply(t);
      }

      @Override
      public hydra.core.Type visit(hydra.core.Type.Annotated at) {
        return new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(hydra.Rewriting.mapBeneathTypeAnnotations(
          f,
          (at).value.body), (at).value.annotation));
      }
    });
  }

  static <T0> hydra.util.Pair<T0, hydra.core.Term> rewriteAndFoldTerm(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>> f, T0 term0, hydra.core.Term v1) {
    return hydra.Rewriting.<T0>rewriteAndFoldTerm_recurse(
      f,
      term0,
      v1);
  }

  static <T0> hydra.util.Pair<T0, hydra.core.Term> rewriteAndFoldTermWithGraph(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>> f, hydra.graph.Graph cx0, T0 val0, hydra.core.Term term0) {
    hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term>> result = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T0>rewriteAndFoldTermWithGraph_result(
      f,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>>) (p0 -> p1 -> hydra.Scoping.extendGraphForLambda(
        p0,
        p1)),
      (java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>>) (p0 -> p1 -> p2 -> hydra.Scoping.extendGraphForLet(
        p0,
        p1,
        p2)),
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>>) (p0 -> p1 -> hydra.Scoping.extendGraphForTypeLambda(
        p0,
        p1)),
      cx0,
      term0,
      val0));
    return (hydra.util.Pair<T0, hydra.core.Term>) ((hydra.util.Pair<T0, hydra.core.Term>) (new hydra.util.Pair<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply(result.get())), hydra.lib.pairs.Second.apply(result.get()))));
  }

  static <T0> hydra.util.Pair<T0, hydra.core.Term> rewriteAndFoldTermWithGraphAndPath(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>>> f, hydra.graph.Graph cx0, T0 val0, hydra.core.Term term0) {
    hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>> result = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T0>rewriteAndFoldTermWithGraphAndPath_result(
      f,
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>>) (p0 -> p1 -> hydra.Scoping.extendGraphForLambda(
        p0,
        p1)),
      (java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>>) (p0 -> p1 -> p2 -> hydra.Scoping.extendGraphForLet(
        p0,
        p1,
        p2)),
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>>) (p0 -> p1 -> hydra.Scoping.extendGraphForTypeLambda(
        p0,
        p1)),
      cx0,
      term0,
      val0));
    return (hydra.util.Pair<T0, hydra.core.Term>) ((hydra.util.Pair<T0, hydra.core.Term>) (new hydra.util.Pair<T0, hydra.core.Term>(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result.get())), hydra.lib.pairs.Second.apply(result.get()))));
  }

  static <T0> hydra.util.Pair<T0, hydra.core.Term> rewriteAndFoldTermWithGraphAndPath_fResult(hydra.graph.Graph cx1, java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>>> f, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>> recurseForUser, hydra.core.Term term, T0 val) {
    return (f).apply(recurseForUser).apply(path).apply(cx1).apply(val).apply(term);
  }

  static <T0, T1> hydra.util.Pair<T0, hydra.core.Term> rewriteAndFoldTermWithGraphAndPath_recurseForUser(hydra.graph.Graph cx1, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T1, T0>, hydra.core.Term>>>> recurse, T0 valIn, hydra.core.Term termIn) {
    hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<T1, T0>, hydra.core.Term>> result = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T0, T1>rewriteAndFoldTermWithGraphAndPath_result2(
      cx1,
      path,
      recurse,
      termIn,
      valIn));
    return (hydra.util.Pair<T0, hydra.core.Term>) ((hydra.util.Pair<T0, hydra.core.Term>) (new hydra.util.Pair<T0, hydra.core.Term>(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result.get())), hydra.lib.pairs.Second.apply(result.get()))));
  }

  static <T0> hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term> rewriteAndFoldTermWithGraphAndPath_result(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>>> f, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>> hydra_scoping_extendGraphForLambda, java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>> hydra_scoping_extendGraphForLet, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>> hydra_scoping_extendGraphForTypeLambda, hydra.graph.Graph cx0, hydra.core.Term term0, T0 val0) {
    return hydra.Rewriting.rewriteAndFoldTermWithPath(
      (java.util.function.Function<java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>>>>, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>>>>>) (v1 -> (java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>>>>) (v2 -> (java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>>>) (v3 -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>>) (v4 -> hydra.Rewriting.rewriteAndFoldTermWithGraphAndPath_wrapper(
        f,
        hydra_scoping_extendGraphForLambda,
        hydra_scoping_extendGraphForLet,
        hydra_scoping_extendGraphForTypeLambda,
        v1,
        v2,
        v3,
        v4))))),
      (hydra.util.Pair<hydra.graph.Graph, T0>) ((hydra.util.Pair<hydra.graph.Graph, T0>) (new hydra.util.Pair<hydra.graph.Graph, T0>(cx0, val0))),
      term0);
  }

  static <T0, T1> hydra.util.Pair<hydra.util.Pair<T1, T0>, hydra.core.Term> rewriteAndFoldTermWithGraphAndPath_result2(hydra.graph.Graph cx1, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T1, T0>, hydra.core.Term>>>> recurse, hydra.core.Term termIn, T0 valIn) {
    return (recurse).apply(path).apply((hydra.util.Pair<hydra.graph.Graph, T0>) ((hydra.util.Pair<hydra.graph.Graph, T0>) (new hydra.util.Pair<hydra.graph.Graph, T0>(cx1, valIn)))).apply(termIn);
  }

  static <T0> T0 rewriteAndFoldTermWithGraphAndPath_val(hydra.util.Pair<hydra.graph.Graph, T0> cxAndVal) {
    return hydra.lib.pairs.Second.apply(cxAndVal);
  }

  static <T0, T1> hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term> rewriteAndFoldTermWithGraphAndPath_wrapper(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>>> f, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>> hydra_scoping_extendGraphForLambda, java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>> hydra_scoping_extendGraphForLet, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>> hydra_scoping_extendGraphForTypeLambda, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<hydra.util.Pair<hydra.graph.Graph, T0>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T1, T0>, hydra.core.Term>>>> recurse, java.util.List<hydra.paths.SubtermStep> path, hydra.util.Pair<hydra.graph.Graph, T0> cxAndVal, hydra.core.Term term) {
    hydra.util.Lazy<hydra.graph.Graph> cx = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(cxAndVal));
    hydra.util.Lazy<hydra.graph.Graph> cx1 = new hydra.util.Lazy<>(() -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.graph.Graph otherwise(hydra.core.Term instance) {
        return cx.get();
      }

      @Override
      public hydra.graph.Graph visit(hydra.core.Term.Lambda l) {
        return (hydra_scoping_extendGraphForLambda).apply(cx.get()).apply((l).value);
      }

      @Override
      public hydra.graph.Graph visit(hydra.core.Term.Let l) {
        return (hydra_scoping_extendGraphForLet).apply((java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (ignored -> (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>) (_2 -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))).apply(cx.get()).apply((l).value);
      }

      @Override
      public hydra.graph.Graph visit(hydra.core.Term.TypeLambda tl) {
        return (hydra_scoping_extendGraphForTypeLambda).apply(cx.get()).apply((tl).value);
      }
    }));
    hydra.util.Lazy<hydra.util.Pair<T0, hydra.core.Term>> fResult = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T0>rewriteAndFoldTermWithGraphAndPath_fResult(
      cx1.get(),
      f,
      path,
      (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>) (v2 -> hydra.Rewriting.<T0, T1>rewriteAndFoldTermWithGraphAndPath_recurseForUser(
        cx1.get(),
        path,
        recurse,
        v1,
        v2))),
      term,
      hydra.Rewriting.<T0>rewriteAndFoldTermWithGraphAndPath_val(cxAndVal)));
    return (hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.Pair<hydra.graph.Graph, T0>, hydra.core.Term>((hydra.util.Pair<hydra.graph.Graph, T0>) ((hydra.util.Pair<hydra.graph.Graph, T0>) (new hydra.util.Pair<hydra.graph.Graph, T0>(cx.get(), hydra.lib.pairs.First.apply(fResult.get())))), hydra.lib.pairs.Second.apply(fResult.get()))));
  }

  static <T0> hydra.util.Pair<T0, hydra.core.Term> rewriteAndFoldTermWithGraph_fResult(hydra.graph.Graph cx1, java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>> f, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>> recurseForUser, hydra.core.Term term, T0 val) {
    return (f).apply(recurseForUser).apply(cx1).apply(val).apply(term);
  }

  static <T0, T1> hydra.util.Pair<T0, hydra.core.Term> rewriteAndFoldTermWithGraph_recurseForUser(hydra.graph.Graph cx1, java.util.function.Function<hydra.util.Pair<T0, hydra.graph.Graph>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T0, T1>, hydra.core.Term>>> lowLevelRecurse, T0 newVal, hydra.core.Term subterm) {
    hydra.util.Lazy<hydra.util.Pair<hydra.util.Pair<T0, T1>, hydra.core.Term>> result = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T0, T1>rewriteAndFoldTermWithGraph_result2(
      cx1,
      lowLevelRecurse,
      newVal,
      subterm));
    return (hydra.util.Pair<T0, hydra.core.Term>) ((hydra.util.Pair<T0, hydra.core.Term>) (new hydra.util.Pair<T0, hydra.core.Term>(hydra.lib.pairs.First.apply(hydra.lib.pairs.First.apply(result.get())), hydra.lib.pairs.Second.apply(result.get()))));
  }

  static <T0> hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term> rewriteAndFoldTermWithGraph_result(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>> f, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>> hydra_scoping_extendGraphForLambda, java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>> hydra_scoping_extendGraphForLet, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>> hydra_scoping_extendGraphForTypeLambda, hydra.graph.Graph cx0, hydra.core.Term term0, T0 val0) {
    return hydra.Rewriting.rewriteAndFoldTerm(
      (java.util.function.Function<java.util.function.Function<hydra.util.Pair<T0, hydra.graph.Graph>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term>>>, java.util.function.Function<hydra.util.Pair<T0, hydra.graph.Graph>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.util.Pair<T0, hydra.graph.Graph>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term>>>) (v2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term>>) (v3 -> hydra.Rewriting.rewriteAndFoldTermWithGraph_wrapper(
        f,
        hydra_scoping_extendGraphForLambda,
        hydra_scoping_extendGraphForLet,
        hydra_scoping_extendGraphForTypeLambda,
        v1,
        v2,
        v3)))),
      (hydra.util.Pair<T0, hydra.graph.Graph>) ((hydra.util.Pair<T0, hydra.graph.Graph>) (new hydra.util.Pair<T0, hydra.graph.Graph>(val0, cx0))),
      term0);
  }

  static <T0, T1> hydra.util.Pair<hydra.util.Pair<T0, T1>, hydra.core.Term> rewriteAndFoldTermWithGraph_result2(hydra.graph.Graph cx1, java.util.function.Function<hydra.util.Pair<T0, hydra.graph.Graph>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T0, T1>, hydra.core.Term>>> lowLevelRecurse, T0 newVal, hydra.core.Term subterm) {
    return (lowLevelRecurse).apply((hydra.util.Pair<T0, hydra.graph.Graph>) ((hydra.util.Pair<T0, hydra.graph.Graph>) (new hydra.util.Pair<T0, hydra.graph.Graph>(newVal, cx1)))).apply(subterm);
  }

  static <T0> T0 rewriteAndFoldTermWithGraph_val(hydra.util.Pair<T0, hydra.graph.Graph> valAndCx) {
    return hydra.lib.pairs.First.apply(valAndCx);
  }

  static <T0, T1> hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term> rewriteAndFoldTermWithGraph_wrapper(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>> f, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>> hydra_scoping_extendGraphForLambda, java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>> hydra_scoping_extendGraphForLet, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>> hydra_scoping_extendGraphForTypeLambda, java.util.function.Function<hydra.util.Pair<T0, hydra.graph.Graph>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.util.Pair<T0, T1>, hydra.core.Term>>> lowLevelRecurse, hydra.util.Pair<T0, hydra.graph.Graph> valAndCx, hydra.core.Term term) {
    hydra.util.Lazy<hydra.graph.Graph> cx = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(valAndCx));
    hydra.util.Lazy<hydra.graph.Graph> cx1 = new hydra.util.Lazy<>(() -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.graph.Graph otherwise(hydra.core.Term instance) {
        return cx.get();
      }

      @Override
      public hydra.graph.Graph visit(hydra.core.Term.Lambda l) {
        return (hydra_scoping_extendGraphForLambda).apply(cx.get()).apply((l).value);
      }

      @Override
      public hydra.graph.Graph visit(hydra.core.Term.Let l) {
        return (hydra_scoping_extendGraphForLet).apply((java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (ignored -> (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>) (_2 -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))).apply(cx.get()).apply((l).value);
      }

      @Override
      public hydra.graph.Graph visit(hydra.core.Term.TypeLambda tl) {
        return (hydra_scoping_extendGraphForTypeLambda).apply(cx.get()).apply((tl).value);
      }
    }));
    hydra.util.Lazy<hydra.util.Pair<T0, hydra.core.Term>> fResult = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T0>rewriteAndFoldTermWithGraph_fResult(
      cx1.get(),
      f,
      (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>) (v2 -> hydra.Rewriting.<T0, T1>rewriteAndFoldTermWithGraph_recurseForUser(
        cx1.get(),
        lowLevelRecurse,
        v1,
        v2))),
      term,
      hydra.Rewriting.<T0>rewriteAndFoldTermWithGraph_val(valAndCx)));
    return (hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term>) ((hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term>) (new hydra.util.Pair<hydra.util.Pair<T0, hydra.graph.Graph>, hydra.core.Term>((hydra.util.Pair<T0, hydra.graph.Graph>) ((hydra.util.Pair<T0, hydra.graph.Graph>) (new hydra.util.Pair<T0, hydra.graph.Graph>(hydra.lib.pairs.First.apply(fResult.get()), cx.get()))), hydra.lib.pairs.Second.apply(fResult.get()))));
  }

  static <T0> hydra.util.Pair<T0, hydra.core.Term> rewriteAndFoldTermWithPath(java.util.function.Function<java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>> f, T0 term0, hydra.core.Term v1) {
    return hydra.Rewriting.<T0>rewriteAndFoldTermWithPath_recurse(
      f,
      (java.util.List<hydra.paths.SubtermStep>) (java.util.Collections.<hydra.paths.SubtermStep>emptyList()),
      term0,
      v1);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_dflt(hydra.core.Term term0, T1 val0) {
    return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(val0, term0)));
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Binding> rewriteAndFoldTermWithPath_forBindingWithAccessor(java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, T1 val, hydra.core.Binding binding) {
    hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> r = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_r(
      binding,
      path,
      recurse,
      val));
    return (hydra.util.Pair<T1, hydra.core.Binding>) ((hydra.util.Pair<T1, hydra.core.Binding>) (new hydra.util.Pair<T1, hydra.core.Binding>(hydra.lib.pairs.First.apply(r.get()), new hydra.core.Binding((binding).name, hydra.lib.pairs.Second.apply(r.get()), (binding).type))));
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Field> rewriteAndFoldTermWithPath_forFieldWithAccessor(java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, java.util.function.Function<hydra.core.Name, hydra.paths.SubtermStep> mkAccessor, T1 val, hydra.core.Field field) {
    hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> r = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_r3(
      field,
      mkAccessor,
      path,
      recurse,
      val));
    return (hydra.util.Pair<T1, hydra.core.Field>) ((hydra.util.Pair<T1, hydra.core.Field>) (new hydra.util.Pair<T1, hydra.core.Field>(hydra.lib.pairs.First.apply(r.get()), new hydra.core.Field((field).name, hydra.lib.pairs.Second.apply(r.get())))));
  }

  static <T1> hydra.util.Pair<T1, java.util.List<hydra.core.Field>> rewriteAndFoldTermWithPath_forFieldsWithAccessor(java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.function.Function<hydra.core.Name, hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Field, hydra.util.Pair<T1, hydra.core.Field>>>> forFieldWithAccessor, java.util.function.Function<hydra.core.Name, hydra.paths.SubtermStep> mkAccessor, T1 v1, java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Field>> v2) {
    return hydra.Rewriting.rewriteAndFoldTermWithPath_forManyWithAccessors(
      path,
      (java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Field, hydra.util.Pair<T1, hydra.core.Field>>>>) (path1 -> (java.util.function.Function<T1, java.util.function.Function<hydra.core.Field, hydra.util.Pair<T1, hydra.core.Field>>>) (val1 -> (java.util.function.Function<hydra.core.Field, hydra.util.Pair<T1, hydra.core.Field>>) (field1 -> (forFieldWithAccessor).apply(mkAccessor).apply(val1).apply(field1)))),
      (java.util.function.Function<java.util.List<hydra.core.Field>, java.util.List<hydra.core.Field>>) (x -> x),
      v1,
      v2);
  }

  static <T2, T3, T4, T5> hydra.util.Pair<T2, T5> rewriteAndFoldTermWithPath_forManyWithAccessors(java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T2, java.util.function.Function<T3, hydra.util.Pair<T2, T4>>>> rec, java.util.function.Function<java.util.List<T4>, T5> cons, T2 val, java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, T3>> accessorTermPairs) {
    hydra.util.Lazy<hydra.util.Pair<T2, java.util.List<T4>>> rr = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T2, T3, T4>rewriteAndFoldTermWithPath_rr5(
      accessorTermPairs,
      path,
      rec,
      val));
    return (hydra.util.Pair<T2, T5>) ((hydra.util.Pair<T2, T5>) (new hydra.util.Pair<T2, T5>(hydra.lib.pairs.First.apply(rr.get()), (cons).apply(hydra.lib.lists.Reverse.apply(hydra.lib.pairs.Second.apply(rr.get()))))));
  }

  static <T1> hydra.util.Pair<T1, hydra.util.Pair<hydra.core.Term, hydra.core.Term>> rewriteAndFoldTermWithPath_forPairWithAccessors(java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, hydra.paths.SubtermStep keyAccessor, hydra.paths.SubtermStep valAccessor, T1 val, hydra.util.Pair<hydra.core.Term, hydra.core.Term> kv) {
    hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rk = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rk2(
      keyAccessor,
      kv,
      path,
      recurse,
      val));
    hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rv = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rv2(
      kv,
      path,
      recurse,
      rk.get(),
      valAccessor));
    return (hydra.util.Pair<T1, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) ((hydra.util.Pair<T1, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) (new hydra.util.Pair<T1, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>(hydra.lib.pairs.First.apply(rv.get()), (hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(hydra.lib.pairs.Second.apply(rk.get()), hydra.lib.pairs.Second.apply(rv.get())))))));
  }

  static <T2, T3, T4, T5, T6> hydra.util.Pair<T4, T6> rewriteAndFoldTermWithPath_forSingleWithAccessor(java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T2, java.util.function.Function<T3, hydra.util.Pair<T4, T5>>>> rec, java.util.function.Function<T5, T6> cons, hydra.paths.SubtermStep accessor, T2 val, T3 term) {
    hydra.util.Lazy<hydra.util.Pair<T4, T5>> r = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T2, T3, T4, T5>rewriteAndFoldTermWithPath_r4(
      accessor,
      path,
      rec,
      term,
      val));
    return (hydra.util.Pair<T4, T6>) ((hydra.util.Pair<T4, T6>) (new hydra.util.Pair<T4, T6>(hydra.lib.pairs.First.apply(r.get()), (cons).apply(hydra.lib.pairs.Second.apply(r.get())))));
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_fsub(java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, java.util.List<hydra.paths.SubtermStep> path, T1 val0, hydra.core.Term term0) {
    hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> dflt = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_dflt(
      term0,
      val0));
    return (term0).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<T1, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return dflt.get();
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Annotated at) {
        return hydra.Rewriting.rewriteAndFoldTermWithPath_forSingleWithAccessor(
          path,
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(t, (at).value.annotation))),
          new hydra.paths.SubtermStep.AnnotatedBody(),
          val0,
          (at).value.body);
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Application a) {
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rlhs = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rlhs(
          (a).value,
          path,
          recurse,
          val0));
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rrhs = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rrhs(
          (a).value,
          path,
          recurse,
          rlhs.get()));
        return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(hydra.lib.pairs.First.apply(rrhs.get()), new hydra.core.Term.Application(new hydra.core.Application(hydra.lib.pairs.Second.apply(rlhs.get()), hydra.lib.pairs.Second.apply(rrhs.get()))))));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Cases cs) {
        hydra.util.Lazy<hydra.util.Maybe<hydra.util.Pair<T1, hydra.core.Term>>> rmd = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rmd(
          (cs).value,
          path,
          recurse,
          val0));
        hydra.util.Lazy<hydra.util.Pair<T1, java.util.List<hydra.core.Term>>> rcases = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rcases(
          path,
          (cs).value,
          recurse,
          hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_val1(
            rmd.get(),
            val0)));
        return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(hydra.lib.pairs.First.apply(rcases.get()), new hydra.core.Term.Cases(new hydra.core.CaseStatement((cs).value.typeName, hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.util.Pair<T1, hydra.core.Term>, hydra.core.Term>) ((java.util.function.Function<hydra.util.Pair<T1, hydra.core.Term>, hydra.core.Term>) (hydra.lib.pairs.Second::apply)),
          rmd.get()), hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Term>, hydra.core.Field>) (ft -> new hydra.core.Field(hydra.lib.pairs.First.apply(ft), hydra.lib.pairs.Second.apply(ft))),
          hydra.lib.lists.Zip.apply(
            hydra.lib.lists.Map.apply(
              projected -> projected.name,
              (cs).value.cases),
            hydra.lib.pairs.Second.apply(rcases.get()))))))));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>) (l -> {
            hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rl = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rl(
              l,
              path,
              recurse,
              val0));
            return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(hydra.lib.pairs.First.apply(rl.get()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(hydra.lib.pairs.Second.apply(rl.get()))))));
          }),
          (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>) (r -> {
            hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rr = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rr(
              path,
              r,
              recurse,
              val0));
            return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(hydra.lib.pairs.First.apply(rr.get()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(hydra.lib.pairs.Second.apply(rr.get()))))));
          }),
          (e).value);
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Lambda l) {
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rl = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rl2(
          (l).value,
          path,
          recurse,
          val0));
        return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(hydra.lib.pairs.First.apply(rl.get()), new hydra.core.Term.Lambda(new hydra.core.Lambda((l).value.parameter, (l).value.domain, hydra.lib.pairs.Second.apply(rl.get()))))));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Let l) {
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> renv = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_renv(
          (l).value,
          path,
          recurse,
          val0));
        hydra.util.Lazy<hydra.util.Pair<T1, java.util.List<hydra.core.Binding>>> rbindings = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rbindings(
          (java.util.function.Function<T1, java.util.function.Function<hydra.core.Binding, hydra.util.Pair<T1, hydra.core.Binding>>>) (v1 -> (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<T1, hydra.core.Binding>>) (v2 -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_forBindingWithAccessor(
            path,
            recurse,
            v1,
            v2))),
          (l).value,
          renv.get()));
        return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(hydra.lib.pairs.First.apply(rbindings.get()), new hydra.core.Term.Let(new hydra.core.Let(hydra.lib.lists.Reverse.apply(hydra.lib.pairs.Second.apply(rbindings.get())), hydra.lib.pairs.Second.apply(renv.get()))))));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.List els) {
        Integer idx = 0;
        hydra.util.Lazy<hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>> rr = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rr2(
          (els).value,
          idx,
          path,
          recurse,
          val0));
        return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(rr.get())), new hydra.core.Term.List(hydra.lib.lists.Reverse.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(rr.get())))))));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Map m) {
        Integer idx = 0;
        hydra.util.Lazy<hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>>> rr = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rr3(
          idx,
          (m).value,
          path,
          recurse,
          val0));
        return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(rr.get())), new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(hydra.lib.lists.Reverse.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(rr.get()))))))));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> dflt.get(),
          (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>) (t -> hydra.Rewriting.rewriteAndFoldTermWithPath_forSingleWithAccessor(
            path,
            recurse,
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t1 -> new hydra.core.Term.Maybe(hydra.util.Maybe.just(t1))),
            new hydra.paths.SubtermStep.MaybeTerm(),
            val0,
            t)),
          (mt).value);
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Pair p) {
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rf = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rf(
          (p).value,
          path,
          recurse,
          val0));
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rs = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rs(
          (p).value,
          path,
          recurse,
          rf.get()));
        return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(hydra.lib.pairs.First.apply(rs.get()), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(hydra.lib.pairs.Second.apply(rf.get()), hydra.lib.pairs.Second.apply(rs.get()))))))));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Project p) {
        return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(val0, new hydra.core.Term.Project((p).value))));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Record r) {
        hydra.util.Lazy<hydra.util.Pair<T1, java.util.List<hydra.core.Term>>> rfields = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rfields(
          path,
          (r).value,
          recurse,
          val0));
        return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(hydra.lib.pairs.First.apply(rfields.get()), new hydra.core.Term.Record(new hydra.core.Record((r).value.typeName, hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Term>, hydra.core.Field>) (ft -> new hydra.core.Field(hydra.lib.pairs.First.apply(ft), hydra.lib.pairs.Second.apply(ft))),
          hydra.lib.lists.Zip.apply(
            hydra.lib.lists.Map.apply(
              projected -> projected.name,
              (r).value.fields),
            hydra.lib.pairs.Second.apply(rfields.get()))))))));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Set els) {
        Integer idx = 0;
        hydra.util.Lazy<hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>> rr = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rr4(
          (els).value,
          idx,
          path,
          recurse,
          val0));
        return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(rr.get())), new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(hydra.lib.lists.Reverse.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(rr.get()))))))));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.TypeApplication ta) {
        return hydra.Rewriting.rewriteAndFoldTermWithPath_forSingleWithAccessor(
          path,
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(t, (ta).value.type))),
          new hydra.paths.SubtermStep.TypeApplicationTerm(),
          val0,
          (ta).value.body);
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.TypeLambda tl) {
        return hydra.Rewriting.rewriteAndFoldTermWithPath_forSingleWithAccessor(
          path,
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda((tl).value.parameter, t))),
          new hydra.paths.SubtermStep.TypeLambdaBody(),
          val0,
          (tl).value.body);
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Inject inj) {
        return hydra.Rewriting.rewriteAndFoldTermWithPath_forSingleWithAccessor(
          path,
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Inject(new hydra.core.Injection((inj).value.typeName, new hydra.core.Field((inj).value.field.name, t)))),
          new hydra.paths.SubtermStep.InjectionTerm(),
          val0,
          (inj).value.field.term);
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Unwrap n) {
        return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(val0, new hydra.core.Term.Unwrap((n).value))));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Wrap wt) {
        return hydra.Rewriting.rewriteAndFoldTermWithPath_forSingleWithAccessor(
          path,
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Wrap(new hydra.core.WrappedTerm((wt).value.typeName, t))),
          new hydra.paths.SubtermStep.WrappedTerm(),
          val0,
          (wt).value.body);
      }
    });
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_r(hydra.core.Binding binding, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, T1 val) {
    return (recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList(new hydra.paths.SubtermStep.LetBinding((binding).name)))).apply(val).apply((binding).term);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_r2(hydra.core.Term el, java.util.List<hydra.paths.SubtermStep> path, hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>> r, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse) {
    return (recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList(new hydra.paths.SubtermStep.ListElement(hydra.lib.pairs.First.apply(r))))).apply(hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(r))).apply(el);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_r22(hydra.core.Term el, java.util.List<hydra.paths.SubtermStep> path, hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>> r, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse) {
    return (recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList(new hydra.paths.SubtermStep.SetElement(hydra.lib.pairs.First.apply(r))))).apply(hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(r))).apply(el);
  }

  static <T2, T3, T4> hydra.util.Pair<T2, T4> rewriteAndFoldTermWithPath_r23(hydra.util.Pair<hydra.paths.SubtermStep, T3> atp, java.util.List<hydra.paths.SubtermStep> path, hydra.util.Pair<T2, java.util.List<T4>> r, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T2, java.util.function.Function<T3, hydra.util.Pair<T2, T4>>>> rec) {
    return (rec).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList(hydra.lib.pairs.First.apply(atp)))).apply(hydra.lib.pairs.First.apply(r)).apply(hydra.lib.pairs.Second.apply(atp));
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_r3(hydra.core.Field field, java.util.function.Function<hydra.core.Name, hydra.paths.SubtermStep> mkAccessor, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, T1 val) {
    return (recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList((mkAccessor).apply((field).name)))).apply(val).apply((field).term);
  }

  static <T2, T3, T4, T5> hydra.util.Pair<T4, T5> rewriteAndFoldTermWithPath_r4(hydra.paths.SubtermStep accessor, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T2, java.util.function.Function<T3, hydra.util.Pair<T4, T5>>>> rec, T3 term, T2 val) {
    return (rec).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList(accessor))).apply(val).apply(term);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Binding> rewriteAndFoldTermWithPath_rb(hydra.core.Binding binding, java.util.function.Function<T1, java.util.function.Function<hydra.core.Binding, hydra.util.Pair<T1, hydra.core.Binding>>> forBindingWithAccessor, hydra.util.Pair<T1, java.util.List<hydra.core.Binding>> r) {
    return (forBindingWithAccessor).apply(hydra.lib.pairs.First.apply(r)).apply(binding);
  }

  static <T1> hydra.util.Pair<T1, java.util.List<hydra.core.Binding>> rewriteAndFoldTermWithPath_rbindings(java.util.function.Function<T1, java.util.function.Function<hydra.core.Binding, hydra.util.Pair<T1, hydra.core.Binding>>> forBindingWithAccessor, hydra.core.Let l, hydra.util.Pair<T1, hydra.core.Term> renv) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Pair<T1, java.util.List<hydra.core.Binding>>, java.util.function.Function<hydra.core.Binding, hydra.util.Pair<T1, java.util.List<hydra.core.Binding>>>>) (r -> (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<T1, java.util.List<hydra.core.Binding>>>) (binding -> {
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Binding>> rb = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rb(
          binding,
          forBindingWithAccessor,
          r));
        return (hydra.util.Pair<T1, java.util.List<hydra.core.Binding>>) ((hydra.util.Pair<T1, java.util.List<hydra.core.Binding>>) (new hydra.util.Pair<T1, java.util.List<hydra.core.Binding>>(hydra.lib.pairs.First.apply(rb.get()), hydra.lib.lists.Cons.apply(
          hydra.lib.pairs.Second.apply(rb.get()),
          hydra.lib.pairs.Second.apply(r)))));
      })),
      (hydra.util.Pair<T1, java.util.List<hydra.core.Binding>>) ((hydra.util.Pair<T1, java.util.List<hydra.core.Binding>>) (new hydra.util.Pair<T1, java.util.List<hydra.core.Binding>>(hydra.lib.pairs.First.apply(renv), (java.util.List<hydra.core.Binding>) (java.util.Collections.<hydra.core.Binding>emptyList())))),
      (l).bindings);
  }

  static <T1> hydra.util.Pair<T1, java.util.List<hydra.core.Term>> rewriteAndFoldTermWithPath_rcases(java.util.List<hydra.paths.SubtermStep> path, hydra.core.CaseStatement cs, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, T1 val1) {
    return hydra.Rewriting.rewriteAndFoldTermWithPath_forManyWithAccessors(
      path,
      recurse,
      (java.util.function.Function<java.util.List<hydra.core.Term>, java.util.List<hydra.core.Term>>) (x -> x),
      val1,
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Field, hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>) (f -> (hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.UnionCasesBranch((f).name), (f).term)))),
        (cs).cases));
  }

  static <T0> hydra.util.Pair<T0, hydra.core.Term> rewriteAndFoldTermWithPath_recurse(java.util.function.Function<java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>> f, java.util.List<hydra.paths.SubtermStep> v1, T0 v2, hydra.core.Term v3) {
    return (f).apply((java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>) (v12 -> (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>) (v22 -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>) (v32 -> hydra.Rewriting.<T0>rewriteAndFoldTermWithPath_fsub(
      (java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>>) (v13 -> (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>) (v23 -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>) (v33 -> hydra.Rewriting.<T0>rewriteAndFoldTermWithPath_recurse(
        f,
        v13,
        v23,
        v33)))),
      v12,
      v22,
      v32))))).apply(v1).apply(v2).apply(v3);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_renv(hydra.core.Let l, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, T1 val0) {
    return (recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList(new hydra.paths.SubtermStep.LetBody()))).apply(val0).apply((l).body);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_rf(hydra.util.Pair<hydra.core.Term, hydra.core.Term> p, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, T1 val0) {
    return (recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList(new hydra.paths.SubtermStep.ProductTerm(0)))).apply(val0).apply(hydra.lib.pairs.First.apply(p));
  }

  static <T1> hydra.util.Pair<T1, java.util.List<hydra.core.Term>> rewriteAndFoldTermWithPath_rfields(java.util.List<hydra.paths.SubtermStep> path, hydra.core.Record r, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, T1 val0) {
    return hydra.Rewriting.rewriteAndFoldTermWithPath_forManyWithAccessors(
      path,
      recurse,
      (java.util.function.Function<java.util.List<hydra.core.Term>, java.util.List<hydra.core.Term>>) (x -> x),
      val0,
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Field, hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>) (f -> (hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.RecordField((f).name), (f).term)))),
        (r).fields));
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_rk(hydra.util.Pair<hydra.core.Term, hydra.core.Term> kv, java.util.List<hydra.paths.SubtermStep> path, hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>> r, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse) {
    return (recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList(new hydra.paths.SubtermStep.MapKey(hydra.lib.pairs.First.apply(r))))).apply(hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(r))).apply(hydra.lib.pairs.First.apply(kv));
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_rk2(hydra.paths.SubtermStep keyAccessor, hydra.util.Pair<hydra.core.Term, hydra.core.Term> kv, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, T1 val) {
    return (recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList(keyAccessor))).apply(val).apply(hydra.lib.pairs.First.apply(kv));
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_rl(hydra.core.Term l, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, T1 val0) {
    return (recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList(new hydra.paths.SubtermStep.SumTerm()))).apply(val0).apply(l);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_rl2(hydra.core.Lambda l, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, T1 val0) {
    return (recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList(new hydra.paths.SubtermStep.LambdaBody()))).apply(val0).apply((l).body);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_rlhs(hydra.core.Application a, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, T1 val0) {
    return (recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList(new hydra.paths.SubtermStep.ApplicationFunction()))).apply(val0).apply((a).function);
  }

  static <T1> hydra.util.Maybe<hydra.util.Pair<T1, hydra.core.Term>> rewriteAndFoldTermWithPath_rmd(hydra.core.CaseStatement cs, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, T1 val0) {
    return hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>) (def -> (recurse).apply(hydra.lib.lists.Concat2.apply(
        path,
        java.util.Arrays.asList(new hydra.paths.SubtermStep.UnionCasesDefault()))).apply(val0).apply(def)),
      (cs).default_);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_rr(java.util.List<hydra.paths.SubtermStep> path, hydra.core.Term r, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, T1 val0) {
    return (recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList(new hydra.paths.SubtermStep.SumTerm()))).apply(val0).apply(r);
  }

  static <T1> hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>> rewriteAndFoldTermWithPath_rr2(java.util.List<hydra.core.Term> els, Integer idx, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, T1 val0) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>>>) (r -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>>) (el -> {
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> r2 = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_r2(
          el,
          path,
          r,
          recurse));
        return (hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>) ((hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>) (new hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>(hydra.lib.math.Add.apply(
          hydra.lib.pairs.First.apply(r),
          1), (hydra.util.Pair<T1, java.util.List<hydra.core.Term>>) ((hydra.util.Pair<T1, java.util.List<hydra.core.Term>>) (new hydra.util.Pair<T1, java.util.List<hydra.core.Term>>(hydra.lib.pairs.First.apply(r2.get()), hydra.lib.lists.Cons.apply(
          hydra.lib.pairs.Second.apply(r2.get()),
          hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(r)))))))));
      })),
      (hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>) ((hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>) (new hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>(idx, (hydra.util.Pair<T1, java.util.List<hydra.core.Term>>) ((hydra.util.Pair<T1, java.util.List<hydra.core.Term>>) (new hydra.util.Pair<T1, java.util.List<hydra.core.Term>>(val0, (java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList()))))))),
      els);
  }

  static <T1> hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>> rewriteAndFoldTermWithPath_rr3(Integer idx, java.util.Map<hydra.core.Term, hydra.core.Term> m, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, T1 val0) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>>, java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>>>>) (r -> (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>>>) (kv -> {
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rk = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rk(
          kv,
          path,
          r,
          recurse));
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rv = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_rv(
          kv,
          path,
          r,
          recurse,
          rk.get()));
        return (hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>>) ((hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>>) (new hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>>(hydra.lib.math.Add.apply(
          hydra.lib.pairs.First.apply(r),
          1), (hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>) ((hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>) (new hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>(hydra.lib.pairs.First.apply(rv.get()), hydra.lib.lists.Cons.apply(
          (hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(hydra.lib.pairs.Second.apply(rk.get()), hydra.lib.pairs.Second.apply(rv.get())))),
          hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(r)))))))));
      })),
      (hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>>) ((hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>>) (new hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>>(idx, (hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>) ((hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>) (new hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>(val0, (java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) (java.util.Collections.<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>emptyList()))))))),
      hydra.lib.maps.ToList.apply(m));
  }

  static <T1> hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>> rewriteAndFoldTermWithPath_rr4(java.util.Set<hydra.core.Term> els, Integer idx, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, T1 val0) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>, java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>>>) (r -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>>) (el -> {
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> r2 = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTermWithPath_r22(
          el,
          path,
          r,
          recurse));
        return (hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>) ((hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>) (new hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>(hydra.lib.math.Add.apply(
          hydra.lib.pairs.First.apply(r),
          1), (hydra.util.Pair<T1, java.util.List<hydra.core.Term>>) ((hydra.util.Pair<T1, java.util.List<hydra.core.Term>>) (new hydra.util.Pair<T1, java.util.List<hydra.core.Term>>(hydra.lib.pairs.First.apply(r2.get()), hydra.lib.lists.Cons.apply(
          hydra.lib.pairs.Second.apply(r2.get()),
          hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(r)))))))));
      })),
      (hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>) ((hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>) (new hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.core.Term>>>(idx, (hydra.util.Pair<T1, java.util.List<hydra.core.Term>>) ((hydra.util.Pair<T1, java.util.List<hydra.core.Term>>) (new hydra.util.Pair<T1, java.util.List<hydra.core.Term>>(val0, (java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList()))))))),
      hydra.lib.sets.ToList.apply(els));
  }

  static <T2, T3, T4> hydra.util.Pair<T2, java.util.List<T4>> rewriteAndFoldTermWithPath_rr5(java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, T3>> accessorTermPairs, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T2, java.util.function.Function<T3, hydra.util.Pair<T2, T4>>>> rec, T2 val) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Pair<T2, java.util.List<T4>>, java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, T3>, hydra.util.Pair<T2, java.util.List<T4>>>>) (r -> (java.util.function.Function<hydra.util.Pair<hydra.paths.SubtermStep, T3>, hydra.util.Pair<T2, java.util.List<T4>>>) (atp -> {
        hydra.util.Lazy<hydra.util.Pair<T2, T4>> r2 = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T2, T3, T4>rewriteAndFoldTermWithPath_r23(
          atp,
          path,
          r,
          rec));
        return (hydra.util.Pair<T2, java.util.List<T4>>) ((hydra.util.Pair<T2, java.util.List<T4>>) (new hydra.util.Pair<T2, java.util.List<T4>>(hydra.lib.pairs.First.apply(r2.get()), hydra.lib.lists.Cons.apply(
          hydra.lib.pairs.Second.apply(r2.get()),
          hydra.lib.pairs.Second.apply(r)))));
      })),
      (hydra.util.Pair<T2, java.util.List<T4>>) ((hydra.util.Pair<T2, java.util.List<T4>>) (new hydra.util.Pair<T2, java.util.List<T4>>(val, (java.util.List<T4>) (java.util.Collections.<T4>emptyList())))),
      accessorTermPairs);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_rrhs(hydra.core.Application a, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, hydra.util.Pair<T1, hydra.core.Term> rlhs) {
    return (recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList(new hydra.paths.SubtermStep.ApplicationArgument()))).apply(hydra.lib.pairs.First.apply(rlhs)).apply((a).argument);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_rs(hydra.util.Pair<hydra.core.Term, hydra.core.Term> p, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, hydra.util.Pair<T1, hydra.core.Term> rf) {
    return (recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList(new hydra.paths.SubtermStep.ProductTerm(1)))).apply(hydra.lib.pairs.First.apply(rf)).apply(hydra.lib.pairs.Second.apply(p));
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_rv(hydra.util.Pair<hydra.core.Term, hydra.core.Term> kv, java.util.List<hydra.paths.SubtermStep> path, hydra.util.Pair<Integer, hydra.util.Pair<T1, java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>> r, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, hydra.util.Pair<T1, hydra.core.Term> rk) {
    return (recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList(new hydra.paths.SubtermStep.MapValue(hydra.lib.pairs.First.apply(r))))).apply(hydra.lib.pairs.First.apply(rk)).apply(hydra.lib.pairs.Second.apply(kv));
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTermWithPath_rv2(hydra.util.Pair<hydra.core.Term, hydra.core.Term> kv, java.util.List<hydra.paths.SubtermStep> path, java.util.function.Function<java.util.List<hydra.paths.SubtermStep>, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>>> recurse, hydra.util.Pair<T1, hydra.core.Term> rk, hydra.paths.SubtermStep valAccessor) {
    return (recurse).apply(hydra.lib.lists.Concat2.apply(
      path,
      java.util.Arrays.asList(valAccessor))).apply(hydra.lib.pairs.First.apply(rk)).apply(hydra.lib.pairs.Second.apply(kv));
  }

  static <T1> T1 rewriteAndFoldTermWithPath_val1(hydra.util.Maybe<hydra.util.Pair<T1, hydra.core.Term>> rmd, T1 val0) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> val0,
      (java.util.function.Function<hydra.util.Pair<T1, hydra.core.Term>, T1>) ((java.util.function.Function<hydra.util.Pair<T1, hydra.core.Term>, T1>) (hydra.lib.pairs.First::apply)),
      rmd);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTerm_dflt(hydra.core.Term term0, T1 val0) {
    return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(val0, term0)));
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Binding> rewriteAndFoldTerm_forBinding(java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, T1 val, hydra.core.Binding binding) {
    hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> r = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTerm_r(
      binding,
      recurse,
      val));
    return (hydra.util.Pair<T1, hydra.core.Binding>) ((hydra.util.Pair<T1, hydra.core.Binding>) (new hydra.util.Pair<T1, hydra.core.Binding>(hydra.lib.pairs.First.apply(r.get()), new hydra.core.Binding((binding).name, hydra.lib.pairs.Second.apply(r.get()), (binding).type))));
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Field> rewriteAndFoldTerm_forField(java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, T1 val, hydra.core.Field field) {
    hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> r = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTerm_r2(
      field,
      recurse,
      val));
    return (hydra.util.Pair<T1, hydra.core.Field>) ((hydra.util.Pair<T1, hydra.core.Field>) (new hydra.util.Pair<T1, hydra.core.Field>(hydra.lib.pairs.First.apply(r.get()), new hydra.core.Field((field).name, hydra.lib.pairs.Second.apply(r.get())))));
  }

  static <T1> hydra.util.Pair<T1, java.util.List<hydra.core.Field>> rewriteAndFoldTerm_forFields(java.util.function.Function<T1, java.util.function.Function<hydra.core.Field, hydra.util.Pair<T1, hydra.core.Field>>> forField, T1 v1, java.util.List<hydra.core.Field> v2) {
    return hydra.Rewriting.rewriteAndFoldTerm_forMany(
      forField,
      (java.util.function.Function<java.util.List<hydra.core.Field>, java.util.List<hydra.core.Field>>) (x -> x),
      v1,
      v2);
  }

  static <T2, T3, T4, T5> hydra.util.Pair<T2, T5> rewriteAndFoldTerm_forMany(java.util.function.Function<T2, java.util.function.Function<T3, hydra.util.Pair<T2, T4>>> rec, java.util.function.Function<java.util.List<T4>, T5> cons, T2 val, java.util.List<T3> els) {
    hydra.util.Lazy<hydra.util.Pair<T2, java.util.List<T4>>> rr = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T2, T3, T4>rewriteAndFoldTerm_rr2(
      els,
      rec,
      val));
    return (hydra.util.Pair<T2, T5>) ((hydra.util.Pair<T2, T5>) (new hydra.util.Pair<T2, T5>(hydra.lib.pairs.First.apply(rr.get()), (cons).apply(hydra.lib.lists.Reverse.apply(hydra.lib.pairs.Second.apply(rr.get()))))));
  }

  static <T1> hydra.util.Pair<T1, hydra.util.Pair<hydra.core.Term, hydra.core.Term>> rewriteAndFoldTerm_forPair(java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, T1 val, hydra.util.Pair<hydra.core.Term, hydra.core.Term> kv) {
    hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rk = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTerm_rk(
      kv,
      recurse,
      val));
    hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rv = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTerm_rv(
      kv,
      recurse,
      rk.get()));
    return (hydra.util.Pair<T1, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) ((hydra.util.Pair<T1, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) (new hydra.util.Pair<T1, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>(hydra.lib.pairs.First.apply(rv.get()), (hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(hydra.lib.pairs.Second.apply(rk.get()), hydra.lib.pairs.Second.apply(rv.get())))))));
  }

  static <T2, T3, T4, T5, T6> hydra.util.Pair<T4, T6> rewriteAndFoldTerm_forSingle(java.util.function.Function<T2, java.util.function.Function<T3, hydra.util.Pair<T4, T5>>> rec, java.util.function.Function<T5, T6> cons, T2 val, T3 term) {
    hydra.util.Lazy<hydra.util.Pair<T4, T5>> r = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T2, T3, T4, T5>rewriteAndFoldTerm_r3(
      rec,
      term,
      val));
    return (hydra.util.Pair<T4, T6>) ((hydra.util.Pair<T4, T6>) (new hydra.util.Pair<T4, T6>(hydra.lib.pairs.First.apply(r.get()), (cons).apply(hydra.lib.pairs.Second.apply(r.get())))));
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTerm_fsub(java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, T1 val0, hydra.core.Term term0) {
    hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> dflt = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTerm_dflt(
      term0,
      val0));
    java.util.function.Function<T1, java.util.function.Function<hydra.core.Field, hydra.util.Pair<T1, hydra.core.Field>>> forField = (java.util.function.Function<T1, java.util.function.Function<hydra.core.Field, hydra.util.Pair<T1, hydra.core.Field>>>) (v1 -> (java.util.function.Function<hydra.core.Field, hydra.util.Pair<T1, hydra.core.Field>>) (v2 -> hydra.Rewriting.<T1>rewriteAndFoldTerm_forField(
      recurse,
      v1,
      v2)));
    return (term0).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Pair<T1, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return dflt.get();
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Annotated at) {
        return hydra.Rewriting.rewriteAndFoldTerm_forSingle(
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(t, (at).value.annotation))),
          val0,
          (at).value.body);
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Application a) {
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rlhs = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTerm_rlhs(
          (a).value,
          recurse,
          val0));
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rrhs = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTerm_rrhs(
          (a).value,
          recurse,
          rlhs.get()));
        return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(hydra.lib.pairs.First.apply(rrhs.get()), new hydra.core.Term.Application(new hydra.core.Application(hydra.lib.pairs.Second.apply(rlhs.get()), hydra.lib.pairs.Second.apply(rrhs.get()))))));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Cases cs) {
        hydra.util.Lazy<hydra.util.Maybe<hydra.util.Pair<T1, hydra.core.Term>>> rmd = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTerm_rmd(
          (cs).value,
          recurse,
          val0));
        hydra.util.Lazy<hydra.util.Pair<T1, java.util.List<hydra.core.Field>>> rcases = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTerm_rcases(
          (cs).value,
          (java.util.function.Function<T1, java.util.function.Function<java.util.List<hydra.core.Field>, hydra.util.Pair<T1, java.util.List<hydra.core.Field>>>>) (v1 -> (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.util.Pair<T1, java.util.List<hydra.core.Field>>>) (v2 -> hydra.Rewriting.<T1>rewriteAndFoldTerm_forFields(
            forField,
            v1,
            v2))),
          hydra.Rewriting.<T1>rewriteAndFoldTerm_val1(
            rmd.get(),
            val0)));
        return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(hydra.lib.pairs.First.apply(rcases.get()), new hydra.core.Term.Cases(new hydra.core.CaseStatement((cs).value.typeName, hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.util.Pair<T1, hydra.core.Term>, hydra.core.Term>) ((java.util.function.Function<hydra.util.Pair<T1, hydra.core.Term>, hydra.core.Term>) (hydra.lib.pairs.Second::apply)),
          rmd.get()), hydra.lib.pairs.Second.apply(rcases.get()))))));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>) (l -> {
            hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rl = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTerm_rl(
              l,
              recurse,
              val0));
            return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(hydra.lib.pairs.First.apply(rl.get()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(hydra.lib.pairs.Second.apply(rl.get()))))));
          }),
          (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>) (r -> {
            hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rr = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTerm_rr(
              r,
              recurse,
              val0));
            return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(hydra.lib.pairs.First.apply(rr.get()), new hydra.core.Term.Either(hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(hydra.lib.pairs.Second.apply(rr.get()))))));
          }),
          (e).value);
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Lambda l) {
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rl = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTerm_rl2(
          (l).value,
          recurse,
          val0));
        return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(hydra.lib.pairs.First.apply(rl.get()), new hydra.core.Term.Lambda(new hydra.core.Lambda((l).value.parameter, (l).value.domain, hydra.lib.pairs.Second.apply(rl.get()))))));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Let l) {
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> renv = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTerm_renv(
          (l).value,
          recurse,
          val0));
        return hydra.Rewriting.rewriteAndFoldTerm_forMany(
          (java.util.function.Function<T1, java.util.function.Function<hydra.core.Binding, hydra.util.Pair<T1, hydra.core.Binding>>>) (v1 -> (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<T1, hydra.core.Binding>>) (v2 -> hydra.Rewriting.<T1>rewriteAndFoldTerm_forBinding(
            recurse,
            v1,
            v2))),
          (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.core.Term>) (bins -> new hydra.core.Term.Let(new hydra.core.Let(bins, hydra.lib.pairs.Second.apply(renv.get())))),
          hydra.lib.pairs.First.apply(renv.get()),
          (l).value.bindings);
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.List els) {
        return hydra.Rewriting.rewriteAndFoldTerm_forMany(
          recurse,
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (x -> new hydra.core.Term.List(x)),
          val0,
          (els).value);
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Map m) {
        return hydra.Rewriting.rewriteAndFoldTerm_forMany(
          (java.util.function.Function<T1, java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Pair<T1, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Pair<T1, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>) (v2 -> hydra.Rewriting.<T1>rewriteAndFoldTerm_forPair(
            recurse,
            v1,
            v2))),
          (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>, hydra.core.Term>) (pairs -> new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(pairs))),
          val0,
          hydra.lib.maps.ToList.apply((m).value));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> dflt.get(),
          (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>) (t -> hydra.Rewriting.rewriteAndFoldTerm_forSingle(
            recurse,
            (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t1 -> new hydra.core.Term.Maybe(hydra.util.Maybe.just(t1))),
            val0,
            t)),
          (mt).value);
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Pair p) {
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rf = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTerm_rf(
          (p).value,
          recurse,
          val0));
        hydra.util.Lazy<hydra.util.Pair<T1, hydra.core.Term>> rs = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T1>rewriteAndFoldTerm_rs(
          (p).value,
          recurse,
          rf.get()));
        return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(hydra.lib.pairs.First.apply(rs.get()), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(hydra.lib.pairs.Second.apply(rf.get()), hydra.lib.pairs.Second.apply(rs.get()))))))));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Project p) {
        return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(val0, new hydra.core.Term.Project((p).value))));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Record r) {
        return hydra.Rewriting.rewriteAndFoldTerm_forMany(
          forField,
          (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.core.Term>) (fields -> new hydra.core.Term.Record(new hydra.core.Record((r).value.typeName, fields))),
          val0,
          (r).value.fields);
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Set els) {
        return hydra.Rewriting.rewriteAndFoldTerm_forMany(
          recurse,
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (e -> new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(e))),
          val0,
          hydra.lib.sets.ToList.apply((els).value));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.TypeApplication ta) {
        return hydra.Rewriting.rewriteAndFoldTerm_forSingle(
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(t, (ta).value.type))),
          val0,
          (ta).value.body);
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.TypeLambda tl) {
        return hydra.Rewriting.rewriteAndFoldTerm_forSingle(
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda((tl).value.parameter, t))),
          val0,
          (tl).value.body);
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Inject inj) {
        return hydra.Rewriting.rewriteAndFoldTerm_forSingle(
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Inject(new hydra.core.Injection((inj).value.typeName, new hydra.core.Field((inj).value.field.name, t)))),
          val0,
          (inj).value.field.term);
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Unwrap n) {
        return (hydra.util.Pair<T1, hydra.core.Term>) ((hydra.util.Pair<T1, hydra.core.Term>) (new hydra.util.Pair<T1, hydra.core.Term>(val0, new hydra.core.Term.Unwrap((n).value))));
      }

      @Override
      public hydra.util.Pair<T1, hydra.core.Term> visit(hydra.core.Term.Wrap wt) {
        return hydra.Rewriting.rewriteAndFoldTerm_forSingle(
          recurse,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (t -> new hydra.core.Term.Wrap(new hydra.core.WrappedTerm((wt).value.typeName, t))),
          val0,
          (wt).value.body);
      }
    });
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTerm_r(hydra.core.Binding binding, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, T1 val) {
    return (recurse).apply(val).apply((binding).term);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTerm_r2(hydra.core.Field field, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, T1 val) {
    return (recurse).apply(val).apply((field).term);
  }

  static <T2, T3, T4> hydra.util.Pair<T2, T4> rewriteAndFoldTerm_r22(T3 el, hydra.util.Pair<T2, java.util.List<T4>> r, java.util.function.Function<T2, java.util.function.Function<T3, hydra.util.Pair<T2, T4>>> rec) {
    return (rec).apply(hydra.lib.pairs.First.apply(r)).apply(el);
  }

  static <T2, T3, T4, T5> hydra.util.Pair<T4, T5> rewriteAndFoldTerm_r3(java.util.function.Function<T2, java.util.function.Function<T3, hydra.util.Pair<T4, T5>>> rec, T3 term, T2 val) {
    return (rec).apply(val).apply(term);
  }

  static <T1> hydra.util.Pair<T1, java.util.List<hydra.core.Field>> rewriteAndFoldTerm_rcases(hydra.core.CaseStatement cs, java.util.function.Function<T1, java.util.function.Function<java.util.List<hydra.core.Field>, hydra.util.Pair<T1, java.util.List<hydra.core.Field>>>> forFields, T1 val1) {
    return (forFields).apply(val1).apply((cs).cases);
  }

  static <T0> hydra.util.Pair<T0, hydra.core.Term> rewriteAndFoldTerm_recurse(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>> f, T0 v1, hydra.core.Term v2) {
    return (f).apply((java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>) (v22 -> hydra.Rewriting.<T0>rewriteAndFoldTerm_fsub(
      (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>>) (v13 -> (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T0, hydra.core.Term>>) (v23 -> hydra.Rewriting.<T0>rewriteAndFoldTerm_recurse(
        f,
        v13,
        v23))),
      v12,
      v22)))).apply(v1).apply(v2);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTerm_renv(hydra.core.Let l, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, T1 val0) {
    return (recurse).apply(val0).apply((l).body);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTerm_rf(hydra.util.Pair<hydra.core.Term, hydra.core.Term> p, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, T1 val0) {
    return (recurse).apply(val0).apply(hydra.lib.pairs.First.apply(p));
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTerm_rk(hydra.util.Pair<hydra.core.Term, hydra.core.Term> kv, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, T1 val) {
    return (recurse).apply(val).apply(hydra.lib.pairs.First.apply(kv));
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTerm_rl(hydra.core.Term l, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, T1 val0) {
    return (recurse).apply(val0).apply(l);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTerm_rl2(hydra.core.Lambda l, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, T1 val0) {
    return (recurse).apply(val0).apply((l).body);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTerm_rlhs(hydra.core.Application a, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, T1 val0) {
    return (recurse).apply(val0).apply((a).function);
  }

  static <T1> hydra.util.Maybe<hydra.util.Pair<T1, hydra.core.Term>> rewriteAndFoldTerm_rmd(hydra.core.CaseStatement cs, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, T1 val0) {
    return hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>) (v1 -> (recurse).apply(val0).apply(v1)),
      (cs).default_);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTerm_rr(hydra.core.Term r, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, T1 val0) {
    return (recurse).apply(val0).apply(r);
  }

  static <T2, T3, T4> hydra.util.Pair<T2, java.util.List<T4>> rewriteAndFoldTerm_rr2(java.util.List<T3> els, java.util.function.Function<T2, java.util.function.Function<T3, hydra.util.Pair<T2, T4>>> rec, T2 val) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Pair<T2, java.util.List<T4>>, java.util.function.Function<T3, hydra.util.Pair<T2, java.util.List<T4>>>>) (r -> (java.util.function.Function<T3, hydra.util.Pair<T2, java.util.List<T4>>>) (el -> {
        hydra.util.Lazy<hydra.util.Pair<T2, T4>> r2 = new hydra.util.Lazy<>(() -> hydra.Rewriting.<T2, T3, T4>rewriteAndFoldTerm_r22(
          el,
          r,
          rec));
        return (hydra.util.Pair<T2, java.util.List<T4>>) ((hydra.util.Pair<T2, java.util.List<T4>>) (new hydra.util.Pair<T2, java.util.List<T4>>(hydra.lib.pairs.First.apply(r2.get()), hydra.lib.lists.Cons.apply(
          hydra.lib.pairs.Second.apply(r2.get()),
          hydra.lib.pairs.Second.apply(r)))));
      })),
      (hydra.util.Pair<T2, java.util.List<T4>>) ((hydra.util.Pair<T2, java.util.List<T4>>) (new hydra.util.Pair<T2, java.util.List<T4>>(val, (java.util.List<T4>) (java.util.Collections.<T4>emptyList())))),
      els);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTerm_rrhs(hydra.core.Application a, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, hydra.util.Pair<T1, hydra.core.Term> rlhs) {
    return (recurse).apply(hydra.lib.pairs.First.apply(rlhs)).apply((a).argument);
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTerm_rs(hydra.util.Pair<hydra.core.Term, hydra.core.Term> p, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, hydra.util.Pair<T1, hydra.core.Term> rf) {
    return (recurse).apply(hydra.lib.pairs.First.apply(rf)).apply(hydra.lib.pairs.Second.apply(p));
  }

  static <T1> hydra.util.Pair<T1, hydra.core.Term> rewriteAndFoldTerm_rv(hydra.util.Pair<hydra.core.Term, hydra.core.Term> kv, java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.util.Pair<T1, hydra.core.Term>>> recurse, hydra.util.Pair<T1, hydra.core.Term> rk) {
    return (recurse).apply(hydra.lib.pairs.First.apply(rk)).apply(hydra.lib.pairs.Second.apply(kv));
  }

  static <T1> T1 rewriteAndFoldTerm_val1(hydra.util.Maybe<hydra.util.Pair<T1, hydra.core.Term>> rmd, T1 val0) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> val0,
      (java.util.function.Function<hydra.util.Pair<T1, hydra.core.Term>, T1>) ((java.util.function.Function<hydra.util.Pair<T1, hydra.core.Term>, T1>) (hydra.lib.pairs.First::apply)),
      rmd);
  }

  static hydra.core.Term rewriteTerm(java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> f, hydra.core.Term term0) {
    java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>> fsub = (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.core.Term>, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (recurse -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (term -> {
      java.util.function.Function<hydra.core.Field, hydra.core.Field> forField = (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (f2 -> new hydra.core.Field((f2).name, (recurse).apply((f2).term)));
      java.util.function.Function<hydra.core.Let, hydra.core.Let> forLet = (java.util.function.Function<hydra.core.Let, hydra.core.Let>) (lt -> {
        java.util.function.Function<hydra.core.Binding, hydra.core.Binding> mapBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, (recurse).apply((b).term), (b).type));
        return new hydra.core.Let(hydra.lib.lists.Map.apply(
          mapBinding,
          (lt).bindings), (recurse).apply((lt).body));
      });
      java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, java.util.Map<hydra.core.Term, hydra.core.Term>> forMap = (java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, java.util.Map<hydra.core.Term, hydra.core.Term>>) (m -> {
        java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>> forPair = (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) (p -> (hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>((recurse).apply(hydra.lib.pairs.First.apply(p)), (recurse).apply(hydra.lib.pairs.Second.apply(p))))));
        return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          forPair,
          hydra.lib.maps.ToList.apply(m)));
      });
      return (term).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term visit(hydra.core.Term.Annotated at) {
          return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm((recurse).apply((at).value.body), (at).value.annotation));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Application a) {
          return new hydra.core.Term.Application(new hydra.core.Application((recurse).apply((a).value.function), (recurse).apply((a).value.argument)));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Cases cs) {
          return new hydra.core.Term.Cases(new hydra.core.CaseStatement((cs).value.typeName, hydra.lib.maybes.Map.apply(
            recurse,
            (cs).value.default_), hydra.lib.lists.Map.apply(
            forField,
            (cs).value.cases)));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Either e) {
          return new hydra.core.Term.Either(hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (l -> hydra.util.Either.<hydra.core.Term, hydra.core.Term>left((recurse).apply(l))),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (r -> hydra.util.Either.<hydra.core.Term, hydra.core.Term>right((recurse).apply(r))),
            (e).value));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Lambda l) {
          return new hydra.core.Term.Lambda(new hydra.core.Lambda((l).value.parameter, (l).value.domain, (recurse).apply((l).value.body)));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Let lt) {
          return new hydra.core.Term.Let((forLet).apply((lt).value));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.List els) {
          return new hydra.core.Term.List(hydra.lib.lists.Map.apply(
            recurse,
            (els).value));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Literal v) {
          return new hydra.core.Term.Literal((v).value);
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Map m) {
          return new hydra.core.Term.Map((forMap).apply((m).value));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Maybe m) {
          return new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
            recurse,
            (m).value));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Pair p) {
          return new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>((recurse).apply(hydra.lib.pairs.First.apply((p).value)), (recurse).apply(hydra.lib.pairs.Second.apply((p).value))))));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Project p) {
          return new hydra.core.Term.Project((p).value);
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Record r) {
          return new hydra.core.Term.Record(new hydra.core.Record((r).value.typeName, hydra.lib.lists.Map.apply(
            forField,
            (r).value.fields)));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Set s) {
          return new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
            recurse,
            hydra.lib.sets.ToList.apply((s).value))));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
          return new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm((recurse).apply((tt).value.body), (tt).value.type));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.TypeLambda ta) {
          return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda((ta).value.parameter, (recurse).apply((ta).value.body)));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Inject i) {
          return new hydra.core.Term.Inject(new hydra.core.Injection((i).value.typeName, (forField).apply((i).value.field)));
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Unit ignored) {
          return new hydra.core.Term.Unit();
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Unwrap n) {
          return new hydra.core.Term.Unwrap((n).value);
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Variable v) {
          return new hydra.core.Term.Variable((v).value);
        }

        @Override
        public hydra.core.Term visit(hydra.core.Term.Wrap wt) {
          return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm((wt).value.typeName, (recurse).apply((wt).value.body)));
        }
      });
    }));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Term, hydra.core.Term>> recurse = new java.util.concurrent.atomic.AtomicReference<>();
    recurse.set((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v1 -> (f).apply((java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v12 -> (fsub).apply(recurse.get()).apply(v12))).apply(v1)));
    return recurse.get().apply(term0);
  }

  static <T0> hydra.util.Either<T0, hydra.core.Term> rewriteTermM(java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.core.Term>>, java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.core.Term>>> f, hydra.core.Term term0) {
    return hydra.Rewriting.<T0>rewriteTermM_recurse(
      f,
      term0);
  }

  static <T1> hydra.util.Either<T1, hydra.core.Field> rewriteTermM_forField(java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>> recurse, hydra.core.Field field) {
    return hydra.lib.eithers.Bind.apply(
      (recurse).apply((field).term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Field>>) (t -> hydra.util.Either.<T1, hydra.core.Field>right(new hydra.core.Field((field).name, t))));
  }

  static <T1> hydra.util.Either<T1, hydra.util.Pair<hydra.core.Term, hydra.core.Term>> rewriteTermM_forPair(java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>> recurse, hydra.util.Pair<hydra.core.Term, hydra.core.Term> kv) {
    return hydra.lib.eithers.Bind.apply(
      (recurse).apply(hydra.lib.pairs.First.apply(kv)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>) (k -> hydra.lib.eithers.Bind.apply(
        (recurse).apply(hydra.lib.pairs.Second.apply(kv)),
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>) (v -> hydra.util.Either.<T1, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>right((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(k, v))))))));
  }

  static <T1> hydra.util.Either<T1, hydra.core.Term> rewriteTermM_fsub(java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>> recurse, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Field, hydra.util.Either<T1, hydra.core.Field>> forField = (java.util.function.Function<hydra.core.Field, hydra.util.Either<T1, hydra.core.Field>>) (v1 -> hydra.Rewriting.<T1>rewriteTermM_forField(
      recurse,
      v1));
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Annotated at) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply((at).value.body),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>) (ex -> hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(ex, (at).value.annotation)))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Application app) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply((app).value.function),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>) (lhs -> hydra.lib.eithers.Bind.apply(
            (recurse).apply((app).value.argument),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>) (rhs -> hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(lhs, rhs)))))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Cases cs) {
        java.util.List<hydra.core.Field> csCases = (cs).value.cases;
        hydra.util.Maybe<hydra.core.Term> def = (cs).value.default_;
        hydra.core.Name n = (cs).value.typeName;
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<T1, hydra.util.Maybe<hydra.core.Term>>right((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.util.Maybe<hydra.core.Term>>>) (t -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maybes.Pure::apply),
              (recurse).apply(t))),
            def),
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.util.Either<T1, hydra.core.Term>>) (rdef -> hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.core.Term>) (rcases -> new hydra.core.Term.Cases(new hydra.core.CaseStatement(n, rdef, rcases))),
            hydra.lib.eithers.MapList.apply(
              forField,
              csCases))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.util.Either<hydra.core.Term, hydra.core.Term>>>) (l -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (x -> hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(x)),
              (recurse).apply(l))),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.util.Either<hydra.core.Term, hydra.core.Term>>>) (r -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (x -> hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(x)),
              (recurse).apply(r))),
            (e).value),
          (java.util.function.Function<hydra.util.Either<hydra.core.Term, hydra.core.Term>, hydra.util.Either<T1, hydra.core.Term>>) (re -> hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.Either(re))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Lambda l) {
        hydra.core.Term body = (l).value.body;
        hydra.util.Maybe<hydra.core.Type> d = (l).value.domain;
        hydra.core.Name v = (l).value.parameter;
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply(body),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>) (rbody -> hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.Lambda(new hydra.core.Lambda(v, d, rbody)))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Let lt) {
        java.util.List<hydra.core.Binding> bindings = (lt).value.bindings;
        hydra.core.Term env = (lt).value.body;
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Binding, hydra.util.Either<T1, hydra.core.Binding>>) (v1 -> hydra.Rewriting.<T1>rewriteTermM_mapBinding(
              recurse,
              v1)),
            bindings),
          (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Either<T1, hydra.core.Term>>) (rbindings -> hydra.lib.eithers.Bind.apply(
            (recurse).apply(env),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>) (renv -> hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.Let(new hydra.core.Let(rbindings, renv)))))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.List els) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            recurse,
            (els).value),
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<T1, hydra.core.Term>>) (rels -> hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.List(rels))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Literal v) {
        return hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.Literal((v).value));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Map m) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<T1, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>) (v1 -> hydra.Rewriting.<T1>rewriteTermM_forPair(
              recurse,
              v1)),
            hydra.lib.maps.ToList.apply((m).value)),
          (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>, hydra.util.Either<T1, hydra.core.Term>>) (pairs -> hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(pairs)))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapMaybe.apply(
            recurse,
            (m).value),
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.util.Either<T1, hydra.core.Term>>) (rm -> hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.Maybe(rm))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Pair p) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply(hydra.lib.pairs.First.apply((p).value)),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>) (rf -> hydra.lib.eithers.Bind.apply(
            (recurse).apply(hydra.lib.pairs.Second.apply((p).value)),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>) (rs -> hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(rf, rs)))))))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Project p) {
        return hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.Project((p).value));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Record r) {
        java.util.List<hydra.core.Field> fields = (r).value.fields;
        hydra.core.Name n = (r).value.typeName;
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.core.Term>) (rfields -> new hydra.core.Term.Record(new hydra.core.Record(n, rfields))),
          hydra.lib.eithers.MapList.apply(
            forField,
            fields));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Set s) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            recurse,
            hydra.lib.sets.ToList.apply((s).value)),
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<T1, hydra.core.Term>>) (rlist -> hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(rlist)))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.TypeApplication tt) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply((tt).value.body),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>) (t -> hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(t, (tt).value.type)))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.TypeLambda tl) {
        hydra.core.Term body = (tl).value.body;
        hydra.core.Name v = (tl).value.parameter;
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply(body),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>) (rbody -> hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(v, rbody)))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Inject i) {
        hydra.core.Field field = (i).value.field;
        hydra.core.Name n = (i).value.typeName;
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.core.Field, hydra.core.Term>) (rfield -> new hydra.core.Term.Inject(new hydra.core.Injection(n, rfield))),
          (forField).apply(field));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Unit ignored) {
        return hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.Unit());
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Unwrap n) {
        return hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.Unwrap((n).value));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Variable v) {
        return hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.Variable((v).value));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Term> visit(hydra.core.Term.Wrap wt) {
        hydra.core.Name name = (wt).value.typeName;
        hydra.core.Term t = (wt).value.body;
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply(t),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>) (rt -> hydra.util.Either.<T1, hydra.core.Term>right(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(name, rt)))));
      }
    });
  }

  static <T1> hydra.util.Either<T1, hydra.core.Binding> rewriteTermM_mapBinding(java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>> recurse, hydra.core.Binding b) {
    return hydra.lib.eithers.Bind.apply(
      (recurse).apply((b).term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Binding>>) (v -> hydra.util.Either.<T1, hydra.core.Binding>right(new hydra.core.Binding((b).name, v, (b).type))));
  }

  static <T0> hydra.util.Either<T0, hydra.core.Term> rewriteTermM_recurse(java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.core.Term>>, java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.core.Term>>> f, hydra.core.Term v1) {
    return (f).apply((java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.core.Term>>) (v12 -> hydra.Rewriting.<T0>rewriteTermM_fsub(
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.core.Term>>) (v13 -> hydra.Rewriting.<T0>rewriteTermM_recurse(
        f,
        v13)),
      v12))).apply(v1);
  }

  static <T0> hydra.core.Term rewriteTermWithContext(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.core.Term>>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> f, T0 cx0, hydra.core.Term term0) {
    return hydra.Rewriting.<T0>rewriteTermWithContext_rewrite(
      f,
      cx0,
      term0);
  }

  static <T0, T1> hydra.util.Either<T1, hydra.core.Term> rewriteTermWithContextM(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>>> f, T0 cx0, hydra.core.Term term0) {
    return hydra.Rewriting.<T0, T1>rewriteTermWithContextM_rewrite(
      f,
      cx0,
      term0);
  }

  static <T3> hydra.util.Either<T3, hydra.core.Field> rewriteTermWithContextM_forField(java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Term>> recurse, hydra.core.Field field) {
    return hydra.lib.eithers.Bind.apply(
      (recurse).apply((field).term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Field>>) (t -> hydra.util.Either.<T3, hydra.core.Field>right(new hydra.core.Field((field).name, t))));
  }

  static <T3> hydra.util.Either<T3, hydra.util.Pair<hydra.core.Term, hydra.core.Term>> rewriteTermWithContextM_forPair(java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Term>> recurse, hydra.util.Pair<hydra.core.Term, hydra.core.Term> kv) {
    return hydra.lib.eithers.Bind.apply(
      (recurse).apply(hydra.lib.pairs.First.apply(kv)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>) (k -> hydra.lib.eithers.Bind.apply(
        (recurse).apply(hydra.lib.pairs.Second.apply(kv)),
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>) (v -> hydra.util.Either.<T3, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>right((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(k, v))))))));
  }

  static <T2, T3> hydra.util.Either<T3, hydra.core.Term> rewriteTermWithContextM_forSubterms(java.util.function.Function<T2, java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Term>>> recurse0, T2 cx, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Term>> recurse = (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Term>>) (v1 -> hydra.Rewriting.<T2, T3>rewriteTermWithContextM_recurse(
      cx,
      recurse0,
      v1));
    java.util.function.Function<hydra.core.Field, hydra.util.Either<T3, hydra.core.Field>> forField = (java.util.function.Function<hydra.core.Field, hydra.util.Either<T3, hydra.core.Field>>) (v1 -> hydra.Rewriting.<T3>rewriteTermWithContextM_forField(
      recurse,
      v1));
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Annotated at) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply((at).value.body),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Term>>) (ex -> hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm(ex, (at).value.annotation)))));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Application app) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply((app).value.function),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Term>>) (lhs -> hydra.lib.eithers.Bind.apply(
            (recurse).apply((app).value.argument),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Term>>) (rhs -> hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(lhs, rhs)))))));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Cases cs) {
        java.util.List<hydra.core.Field> csCases = (cs).value.cases;
        hydra.util.Maybe<hydra.core.Term> def = (cs).value.default_;
        hydra.core.Name n = (cs).value.typeName;
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<T3, hydra.util.Maybe<hydra.core.Term>>right((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.util.Maybe<hydra.core.Term>>>) (t -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Term>>) (hydra.lib.maybes.Pure::apply),
              (recurse).apply(t))),
            def),
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.util.Either<T3, hydra.core.Term>>) (rdef -> hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.core.Term>) (rcases -> new hydra.core.Term.Cases(new hydra.core.CaseStatement(n, rdef, rcases))),
            hydra.lib.eithers.MapList.apply(
              forField,
              csCases))));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.util.Either<hydra.core.Term, hydra.core.Term>>>) (l -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (x -> hydra.util.Either.<hydra.core.Term, hydra.core.Term>left(x)),
              (recurse).apply(l))),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.util.Either<hydra.core.Term, hydra.core.Term>>>) (r -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (x -> hydra.util.Either.<hydra.core.Term, hydra.core.Term>right(x)),
              (recurse).apply(r))),
            (e).value),
          (java.util.function.Function<hydra.util.Either<hydra.core.Term, hydra.core.Term>, hydra.util.Either<T3, hydra.core.Term>>) (re -> hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.Either(re))));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Lambda l) {
        hydra.core.Term body = (l).value.body;
        hydra.util.Maybe<hydra.core.Type> d = (l).value.domain;
        hydra.core.Name v = (l).value.parameter;
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply(body),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Term>>) (rbody -> hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.Lambda(new hydra.core.Lambda(v, d, rbody)))));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Let lt) {
        java.util.List<hydra.core.Binding> bindings = (lt).value.bindings;
        hydra.core.Term body = (lt).value.body;
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Binding, hydra.util.Either<T3, hydra.core.Binding>>) (v1 -> hydra.Rewriting.<T3>rewriteTermWithContextM_mapBinding(
              recurse,
              v1)),
            bindings),
          (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Either<T3, hydra.core.Term>>) (rbindings -> hydra.lib.eithers.Bind.apply(
            (recurse).apply(body),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Term>>) (rbody -> hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.Let(new hydra.core.Let(rbindings, rbody)))))));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.List els) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            recurse,
            (els).value),
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<T3, hydra.core.Term>>) (rels -> hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.List(rels))));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Literal v) {
        return hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.Literal((v).value));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Map m) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<T3, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>) (v1 -> hydra.Rewriting.<T3>rewriteTermWithContextM_forPair(
              recurse,
              v1)),
            hydra.lib.maps.ToList.apply((m).value)),
          (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>, hydra.util.Either<T3, hydra.core.Term>>) (pairs -> hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(pairs)))));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapMaybe.apply(
            recurse,
            (m).value),
          (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.util.Either<T3, hydra.core.Term>>) (rm -> hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.Maybe(rm))));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Pair p) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply(hydra.lib.pairs.First.apply((p).value)),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Term>>) (rfirst -> hydra.lib.eithers.Bind.apply(
            (recurse).apply(hydra.lib.pairs.Second.apply((p).value)),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Term>>) (rsecond -> hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(rfirst, rsecond)))))))));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Project p) {
        return hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.Project((p).value));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Record r) {
        java.util.List<hydra.core.Field> fields = (r).value.fields;
        hydra.core.Name n = (r).value.typeName;
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.core.Term>) (rfields -> new hydra.core.Term.Record(new hydra.core.Record(n, rfields))),
          hydra.lib.eithers.MapList.apply(
            forField,
            fields));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Set s) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            recurse,
            hydra.lib.sets.ToList.apply((s).value)),
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<T3, hydra.core.Term>>) (rlist -> hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(rlist)))));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.TypeApplication tt) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply((tt).value.body),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Term>>) (t -> hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm(t, (tt).value.type)))));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.TypeLambda tl) {
        hydra.core.Term body = (tl).value.body;
        hydra.core.Name v = (tl).value.parameter;
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply(body),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Term>>) (rbody -> hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda(v, rbody)))));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Inject i) {
        hydra.core.Field field = (i).value.field;
        hydra.core.Name n = (i).value.typeName;
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.core.Field, hydra.core.Term>) (rfield -> new hydra.core.Term.Inject(new hydra.core.Injection(n, rfield))),
          (forField).apply(field));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Unit ignored) {
        return hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.Unit());
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Unwrap n) {
        return hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.Unwrap((n).value));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Variable v) {
        return hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.Variable((v).value));
      }

      @Override
      public hydra.util.Either<T3, hydra.core.Term> visit(hydra.core.Term.Wrap wt) {
        hydra.core.Name name = (wt).value.typeName;
        hydra.core.Term t = (wt).value.body;
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply(t),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Term>>) (rt -> hydra.util.Either.<T3, hydra.core.Term>right(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(name, rt)))));
      }
    });
  }

  static <T3> hydra.util.Either<T3, hydra.core.Binding> rewriteTermWithContextM_mapBinding(java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Term>> recurse, hydra.core.Binding b) {
    return hydra.lib.eithers.Bind.apply(
      (recurse).apply((b).term),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Binding>>) (v -> hydra.util.Either.<T3, hydra.core.Binding>right(new hydra.core.Binding((b).name, v, (b).type))));
  }

  static <T2, T3> hydra.util.Either<T3, hydra.core.Term> rewriteTermWithContextM_recurse(T2 cx, java.util.function.Function<T2, java.util.function.Function<hydra.core.Term, hydra.util.Either<T3, hydra.core.Term>>> recurse0, hydra.core.Term v1) {
    return (recurse0).apply(cx).apply(v1);
  }

  static <T0, T1> hydra.util.Either<T1, hydra.core.Term> rewriteTermWithContextM_rewrite(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>>> f, T0 cx, hydra.core.Term term) {
    return (f).apply((java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>) (v2 -> hydra.Rewriting.<T0, T1>rewriteTermWithContextM_forSubterms(
      (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.core.Term>>) (v22 -> hydra.Rewriting.<T0, T1>rewriteTermWithContextM_rewrite(
        f,
        v12,
        v22))),
      v1,
      v2)))).apply(cx).apply(term);
  }

  static <T1> hydra.core.Term rewriteTermWithContext_forSubterms(java.util.function.Function<T1, java.util.function.Function<hydra.core.Term, hydra.core.Term>> recurse0, T1 cx, hydra.core.Term term) {
    java.util.function.Function<hydra.core.Term, hydra.core.Term> recurse = (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v1 -> (recurse0).apply(cx).apply(v1));
    java.util.function.Function<hydra.core.Field, hydra.core.Field> forField = (java.util.function.Function<hydra.core.Field, hydra.core.Field>) (field -> new hydra.core.Field((field).name, (recurse).apply((field).term)));
    java.util.function.Function<hydra.core.Let, hydra.core.Let> forLet = (java.util.function.Function<hydra.core.Let, hydra.core.Let>) (lt -> {
      java.util.function.Function<hydra.core.Binding, hydra.core.Binding> mapBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Binding>) (b -> new hydra.core.Binding((b).name, (recurse).apply((b).term), (b).type));
      return new hydra.core.Let(hydra.lib.lists.Map.apply(
        mapBinding,
        (lt).bindings), (recurse).apply((lt).body));
    });
    java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, java.util.Map<hydra.core.Term, hydra.core.Term>> forMap = (java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, java.util.Map<hydra.core.Term, hydra.core.Term>>) (m -> {
      java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>> forPair = (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) (p -> (hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>((recurse).apply(hydra.lib.pairs.First.apply(p)), (recurse).apply(hydra.lib.pairs.Second.apply(p))))));
      return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        forPair,
        hydra.lib.maps.ToList.apply(m)));
    });
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.core.Term.Annotated at) {
        return new hydra.core.Term.Annotated(new hydra.core.AnnotatedTerm((recurse).apply((at).value.body), (at).value.annotation));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Application a) {
        return new hydra.core.Term.Application(new hydra.core.Application((recurse).apply((a).value.function), (recurse).apply((a).value.argument)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Cases cs) {
        return new hydra.core.Term.Cases(new hydra.core.CaseStatement((cs).value.typeName, hydra.lib.maybes.Map.apply(
          recurse,
          (cs).value.default_), hydra.lib.lists.Map.apply(
          forField,
          (cs).value.cases)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Either e) {
        return new hydra.core.Term.Either(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (l -> hydra.util.Either.<hydra.core.Term, hydra.core.Term>left((recurse).apply(l))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.core.Term, hydra.core.Term>>) (r -> hydra.util.Either.<hydra.core.Term, hydra.core.Term>right((recurse).apply(r))),
          (e).value));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Lambda l) {
        return new hydra.core.Term.Lambda(new hydra.core.Lambda((l).value.parameter, (l).value.domain, (recurse).apply((l).value.body)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Let lt) {
        return new hydra.core.Term.Let((forLet).apply((lt).value));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.List els) {
        return new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          recurse,
          (els).value));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Literal v) {
        return new hydra.core.Term.Literal((v).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Map m) {
        return new hydra.core.Term.Map((forMap).apply((m).value));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Maybe m) {
        return new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
          recurse,
          (m).value));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Pair p) {
        return new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>((recurse).apply(hydra.lib.pairs.First.apply((p).value)), (recurse).apply(hydra.lib.pairs.Second.apply((p).value))))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Project p) {
        return new hydra.core.Term.Project((p).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Record r) {
        return new hydra.core.Term.Record(new hydra.core.Record((r).value.typeName, hydra.lib.lists.Map.apply(
          forField,
          (r).value.fields)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Set s) {
        return new hydra.core.Term.Set(hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
          recurse,
          hydra.lib.sets.ToList.apply((s).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeApplication tt) {
        return new hydra.core.Term.TypeApplication(new hydra.core.TypeApplicationTerm((recurse).apply((tt).value.body), (tt).value.type));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.TypeLambda ta) {
        return new hydra.core.Term.TypeLambda(new hydra.core.TypeLambda((ta).value.parameter, (recurse).apply((ta).value.body)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Inject i) {
        return new hydra.core.Term.Inject(new hydra.core.Injection((i).value.typeName, (forField).apply((i).value.field)));
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Unit ignored) {
        return new hydra.core.Term.Unit();
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Unwrap n) {
        return new hydra.core.Term.Unwrap((n).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Variable v) {
        return new hydra.core.Term.Variable((v).value);
      }

      @Override
      public hydra.core.Term visit(hydra.core.Term.Wrap wt) {
        return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm((wt).value.typeName, (recurse).apply((wt).value.body)));
      }
    });
  }

  static <T0> hydra.core.Term rewriteTermWithContext_rewrite(java.util.function.Function<java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.core.Term>>, java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.core.Term>>> f, T0 cx, hydra.core.Term term) {
    return (f).apply((java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v2 -> hydra.Rewriting.<T0>rewriteTermWithContext_forSubterms(
      (java.util.function.Function<T0, java.util.function.Function<hydra.core.Term, hydra.core.Term>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (v22 -> hydra.Rewriting.<T0>rewriteTermWithContext_rewrite(
        f,
        v12,
        v22))),
      v1,
      v2)))).apply(cx).apply(term);
  }

  static <T0> T0 rewriteTermWithGraph(java.util.function.Function<java.util.function.Function<hydra.core.Term, T0>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>>> f, hydra.graph.Graph cx0, hydra.core.Term term0) {
    return hydra.Rewriting.<T0>rewriteTermWithGraph_rewrite(
      (java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>>>) (v1 -> (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>>) (v2 -> (java.util.function.Function<hydra.core.Term, T0>) (v3 -> hydra.Rewriting.<T0>rewriteTermWithGraph_f2(
        f,
        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>>) (p0 -> p1 -> hydra.Scoping.extendGraphForLambda(
          p0,
          p1)),
        (java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>>) (p0 -> p1 -> p2 -> hydra.Scoping.extendGraphForLet(
          p0,
          p1,
          p2)),
        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>>) (p0 -> p1 -> hydra.Scoping.extendGraphForTypeLambda(
          p0,
          p1)),
        v1,
        v2,
        v3)))),
      cx0,
      term0);
  }

  static <T0> T0 rewriteTermWithGraph_f2(java.util.function.Function<java.util.function.Function<hydra.core.Term, T0>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>>> f, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Lambda, hydra.graph.Graph>> hydra_scoping_extendGraphForLambda, java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Let, hydra.graph.Graph>>> hydra_scoping_extendGraphForLet, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.TypeLambda, hydra.graph.Graph>> hydra_scoping_extendGraphForTypeLambda, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>> recurse, hydra.graph.Graph cx, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public T0 otherwise(hydra.core.Term instance) {
        return (f).apply((java.util.function.Function<hydra.core.Term, T0>) (v1 -> hydra.Rewriting.<T0>rewriteTermWithGraph_recurse1(
          cx,
          recurse,
          v1))).apply(cx).apply(term);
      }

      @Override
      public T0 visit(hydra.core.Term.Lambda l) {
        hydra.graph.Graph cx1 = (hydra_scoping_extendGraphForLambda).apply(cx).apply((l).value);
        return (f).apply((java.util.function.Function<hydra.core.Term, T0>) (v1 -> hydra.Rewriting.<T0>rewriteTermWithGraph_recurse2(
          cx1,
          recurse,
          v1))).apply(cx1).apply(term);
      }

      @Override
      public T0 visit(hydra.core.Term.Let l) {
        hydra.util.Lazy<hydra.graph.Graph> cx1 = new hydra.util.Lazy<>(() -> (hydra_scoping_extendGraphForLet).apply((java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>>) (ignored -> (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.core.Term>>) (_2 -> (hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))).apply(cx).apply((l).value));
        return (f).apply((java.util.function.Function<hydra.core.Term, T0>) (v1 -> hydra.Rewriting.<T0>rewriteTermWithGraph_recurse22(
          cx1.get(),
          recurse,
          v1))).apply(cx1.get()).apply(term);
      }

      @Override
      public T0 visit(hydra.core.Term.TypeLambda tl) {
        hydra.graph.Graph cx1 = (hydra_scoping_extendGraphForTypeLambda).apply(cx).apply((tl).value);
        return (f).apply((java.util.function.Function<hydra.core.Term, T0>) (v1 -> hydra.Rewriting.<T0>rewriteTermWithGraph_recurse23(
          cx1,
          recurse,
          v1))).apply(cx1).apply(term);
      }
    });
  }

  static <T0> T0 rewriteTermWithGraph_recurse1(hydra.graph.Graph cx, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>> recurse, hydra.core.Term term) {
    return (recurse).apply(cx).apply(term);
  }

  static <T0> T0 rewriteTermWithGraph_recurse2(hydra.graph.Graph cx1, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>> recurse, hydra.core.Term term) {
    return (recurse).apply(cx1).apply(term);
  }

  static <T0> T0 rewriteTermWithGraph_recurse22(hydra.graph.Graph cx1, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>> recurse, hydra.core.Term term) {
    return (recurse).apply(cx1).apply(term);
  }

  static <T0> T0 rewriteTermWithGraph_recurse23(hydra.graph.Graph cx1, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>> recurse, hydra.core.Term term) {
    return (recurse).apply(cx1).apply(term);
  }

  static <T0> T0 rewriteTermWithGraph_rewrite(java.util.function.Function<java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>>> f2, hydra.graph.Graph cx, hydra.core.Term term) {
    return (f2).apply((java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, T0>>) (v1 -> (java.util.function.Function<hydra.core.Term, T0>) (v2 -> hydra.Rewriting.<T0>rewriteTermWithGraph_rewrite(
      f2,
      v1,
      v2)))).apply(cx).apply(term);
  }

  static hydra.core.Type rewriteType(java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>> f, hydra.core.Type typ0) {
    java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>> fsub = (java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.core.Type>, java.util.function.Function<hydra.core.Type, hydra.core.Type>>) (recurse -> (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (typ -> {
      java.util.function.Function<hydra.core.FieldType, hydra.core.FieldType> forField = (java.util.function.Function<hydra.core.FieldType, hydra.core.FieldType>) (field -> new hydra.core.FieldType((field).name, (recurse).apply((field).type)));
      return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
        @Override
        public hydra.core.Type visit(hydra.core.Type.Annotated at) {
          return new hydra.core.Type.Annotated(new hydra.core.AnnotatedType((recurse).apply((at).value.body), (at).value.annotation));
        }

        @Override
        public hydra.core.Type visit(hydra.core.Type.Application app) {
          return new hydra.core.Type.Application(new hydra.core.ApplicationType((recurse).apply((app).value.function), (recurse).apply((app).value.argument)));
        }

        @Override
        public hydra.core.Type visit(hydra.core.Type.Either et) {
          return new hydra.core.Type.Either(new hydra.core.EitherType((recurse).apply((et).value.left), (recurse).apply((et).value.right)));
        }

        @Override
        public hydra.core.Type visit(hydra.core.Type.Pair pt) {
          return new hydra.core.Type.Pair(new hydra.core.PairType((recurse).apply((pt).value.first), (recurse).apply((pt).value.second)));
        }

        @Override
        public hydra.core.Type visit(hydra.core.Type.Function fun) {
          return new hydra.core.Type.Function(new hydra.core.FunctionType((recurse).apply((fun).value.domain), (recurse).apply((fun).value.codomain)));
        }

        @Override
        public hydra.core.Type visit(hydra.core.Type.Forall lt) {
          return new hydra.core.Type.Forall(new hydra.core.ForallType((lt).value.parameter, (recurse).apply((lt).value.body)));
        }

        @Override
        public hydra.core.Type visit(hydra.core.Type.List t) {
          return new hydra.core.Type.List((recurse).apply((t).value));
        }

        @Override
        public hydra.core.Type visit(hydra.core.Type.Literal lt) {
          return new hydra.core.Type.Literal((lt).value);
        }

        @Override
        public hydra.core.Type visit(hydra.core.Type.Map mt) {
          return new hydra.core.Type.Map(new hydra.core.MapType((recurse).apply((mt).value.keys), (recurse).apply((mt).value.values)));
        }

        @Override
        public hydra.core.Type visit(hydra.core.Type.Maybe t) {
          return new hydra.core.Type.Maybe((recurse).apply((t).value));
        }

        @Override
        public hydra.core.Type visit(hydra.core.Type.Record rt) {
          return new hydra.core.Type.Record(hydra.lib.lists.Map.apply(
            forField,
            (rt).value));
        }

        @Override
        public hydra.core.Type visit(hydra.core.Type.Set t) {
          return new hydra.core.Type.Set((recurse).apply((t).value));
        }

        @Override
        public hydra.core.Type visit(hydra.core.Type.Union rt) {
          return new hydra.core.Type.Union(hydra.lib.lists.Map.apply(
            forField,
            (rt).value));
        }

        @Override
        public hydra.core.Type visit(hydra.core.Type.Unit ignored) {
          return new hydra.core.Type.Unit();
        }

        @Override
        public hydra.core.Type visit(hydra.core.Type.Variable v) {
          return new hydra.core.Type.Variable((v).value);
        }

        @Override
        public hydra.core.Type visit(hydra.core.Type.Void_ ignored) {
          return new hydra.core.Type.Void_();
        }

        @Override
        public hydra.core.Type visit(hydra.core.Type.Wrap wt) {
          return new hydra.core.Type.Wrap((recurse).apply((wt).value));
        }
      });
    }));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.core.Type, hydra.core.Type>> recurse = new java.util.concurrent.atomic.AtomicReference<>();
    recurse.set((java.util.function.Function<hydra.core.Type, hydra.core.Type>) (v1 -> (f).apply((java.util.function.Function<hydra.core.Type, hydra.core.Type>) (v12 -> (fsub).apply(recurse.get()).apply(v12))).apply(v1)));
    return recurse.get().apply(typ0);
  }

  static <T0> hydra.util.Either<T0, hydra.core.Type> rewriteTypeM(java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.util.Either<T0, hydra.core.Type>>, java.util.function.Function<hydra.core.Type, hydra.util.Either<T0, hydra.core.Type>>> f, hydra.core.Type typ0) {
    return hydra.Rewriting.<T0>rewriteTypeM_recurse(
      f,
      typ0);
  }

  static <T1> hydra.util.Either<T1, hydra.core.FieldType> rewriteTypeM_forField(java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>> recurse, hydra.core.FieldType f) {
    return hydra.lib.eithers.Bind.apply(
      (recurse).apply((f).type),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.FieldType>>) (t -> hydra.util.Either.<T1, hydra.core.FieldType>right(new hydra.core.FieldType((f).name, t))));
  }

  static <T1> hydra.util.Either<T1, hydra.core.FieldType> rewriteTypeM_forField2(java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>> recurse, hydra.core.FieldType f) {
    return hydra.lib.eithers.Bind.apply(
      (recurse).apply((f).type),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.FieldType>>) (t -> hydra.util.Either.<T1, hydra.core.FieldType>right(new hydra.core.FieldType((f).name, t))));
  }

  static <T1> hydra.util.Either<T1, hydra.core.Type> rewriteTypeM_fsub(java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>> recurse, hydra.core.Type typ) {
    return (typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T1, hydra.core.Type> visit(hydra.core.Type.Annotated at) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply((at).value.body),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>>) (t -> hydra.util.Either.<T1, hydra.core.Type>right(new hydra.core.Type.Annotated(new hydra.core.AnnotatedType(t, (at).value.annotation)))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Type> visit(hydra.core.Type.Application at) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply((at).value.function),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>>) (lhs -> hydra.lib.eithers.Bind.apply(
            (recurse).apply((at).value.argument),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>>) (rhs -> hydra.util.Either.<T1, hydra.core.Type>right(new hydra.core.Type.Application(new hydra.core.ApplicationType(lhs, rhs)))))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Type> visit(hydra.core.Type.Either et) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply((et).value.left),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>>) (left -> hydra.lib.eithers.Bind.apply(
            (recurse).apply((et).value.right),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>>) (right -> hydra.util.Either.<T1, hydra.core.Type>right(new hydra.core.Type.Either(new hydra.core.EitherType(left, right)))))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Type> visit(hydra.core.Type.Pair pt) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply((pt).value.first),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>>) (pairFirst -> hydra.lib.eithers.Bind.apply(
            (recurse).apply((pt).value.second),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>>) (pairSecond -> hydra.util.Either.<T1, hydra.core.Type>right(new hydra.core.Type.Pair(new hydra.core.PairType(pairFirst, pairSecond)))))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Type> visit(hydra.core.Type.Function ft) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply((ft).value.domain),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>>) (dom -> hydra.lib.eithers.Bind.apply(
            (recurse).apply((ft).value.codomain),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>>) (cod -> hydra.util.Either.<T1, hydra.core.Type>right(new hydra.core.Type.Function(new hydra.core.FunctionType(dom, cod)))))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Type> visit(hydra.core.Type.Forall ft) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply((ft).value.body),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>>) (b -> hydra.util.Either.<T1, hydra.core.Type>right(new hydra.core.Type.Forall(new hydra.core.ForallType((ft).value.parameter, b)))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Type> visit(hydra.core.Type.List t) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply((t).value),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>>) (rt -> hydra.util.Either.<T1, hydra.core.Type>right(new hydra.core.Type.List(rt))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Type> visit(hydra.core.Type.Literal lt) {
        return hydra.util.Either.<T1, hydra.core.Type>right(new hydra.core.Type.Literal((lt).value));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Type> visit(hydra.core.Type.Map mt) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply((mt).value.keys),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>>) (kt -> hydra.lib.eithers.Bind.apply(
            (recurse).apply((mt).value.values),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>>) (vt -> hydra.util.Either.<T1, hydra.core.Type>right(new hydra.core.Type.Map(new hydra.core.MapType(kt, vt)))))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Type> visit(hydra.core.Type.Maybe t) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply((t).value),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>>) (rt -> hydra.util.Either.<T1, hydra.core.Type>right(new hydra.core.Type.Maybe(rt))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Type> visit(hydra.core.Type.Record rt) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<T1, hydra.core.FieldType>>) (v1 -> hydra.Rewriting.<T1>rewriteTypeM_forField(
              recurse,
              v1)),
            (rt).value),
          (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<T1, hydra.core.Type>>) (rfields -> hydra.util.Either.<T1, hydra.core.Type>right(new hydra.core.Type.Record(rfields))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Type> visit(hydra.core.Type.Set t) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply((t).value),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>>) (rt -> hydra.util.Either.<T1, hydra.core.Type>right(new hydra.core.Type.Set(rt))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Type> visit(hydra.core.Type.Union rt) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<T1, hydra.core.FieldType>>) (v1 -> hydra.Rewriting.<T1>rewriteTypeM_forField2(
              recurse,
              v1)),
            (rt).value),
          (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.util.Either<T1, hydra.core.Type>>) (rfields -> hydra.util.Either.<T1, hydra.core.Type>right(new hydra.core.Type.Union(rfields))));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Type> visit(hydra.core.Type.Unit ignored) {
        return hydra.util.Either.<T1, hydra.core.Type>right(new hydra.core.Type.Unit());
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Type> visit(hydra.core.Type.Variable v) {
        return hydra.util.Either.<T1, hydra.core.Type>right(new hydra.core.Type.Variable((v).value));
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Type> visit(hydra.core.Type.Void_ ignored) {
        return hydra.util.Either.<T1, hydra.core.Type>right(new hydra.core.Type.Void_());
      }

      @Override
      public hydra.util.Either<T1, hydra.core.Type> visit(hydra.core.Type.Wrap wt) {
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply((wt).value),
          (java.util.function.Function<hydra.core.Type, hydra.util.Either<T1, hydra.core.Type>>) (t -> hydra.util.Either.<T1, hydra.core.Type>right(new hydra.core.Type.Wrap(t))));
      }
    });
  }

  static <T0> hydra.util.Either<T0, hydra.core.Type> rewriteTypeM_recurse(java.util.function.Function<java.util.function.Function<hydra.core.Type, hydra.util.Either<T0, hydra.core.Type>>, java.util.function.Function<hydra.core.Type, hydra.util.Either<T0, hydra.core.Type>>> f, hydra.core.Type v1) {
    return (f).apply((java.util.function.Function<hydra.core.Type, hydra.util.Either<T0, hydra.core.Type>>) (v12 -> hydra.Rewriting.<T0>rewriteTypeM_fsub(
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<T0, hydra.core.Type>>) (v13 -> hydra.Rewriting.<T0>rewriteTypeM_recurse(
        f,
        v13)),
      v12))).apply(v1);
  }

  static java.util.List<hydra.core.Term> subterms(hydra.core.Term v1) {
    return (v1).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Annotated at) {
        return java.util.Arrays.asList((at).value.body);
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Application p) {
        return java.util.Arrays.asList(
          (p).value.function,
          (p).value.argument);
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Cases cs) {
        return hydra.lib.lists.Concat2.apply(
          hydra.lib.maybes.Maybe.applyLazy(
            () -> (java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList()),
            (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (t -> java.util.Arrays.asList(t)),
            (cs).value.default_),
          hydra.lib.lists.Map.apply(
            projected -> projected.term,
            (cs).value.cases));
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Either e) {
        return hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (l -> java.util.Arrays.asList(l)),
          (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (r -> java.util.Arrays.asList(r)),
          (e).value);
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Lambda l) {
        return java.util.Arrays.asList((l).value.body);
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Let lt) {
        return hydra.lib.lists.Cons.apply(
          (lt).value.body,
          hydra.lib.lists.Map.apply(
            projected -> projected.term,
            (lt).value.bindings));
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.List l) {
        return (l).value;
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Literal ignored) {
        return (java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList());
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Map m) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, java.util.List<hydra.core.Term>>) (p -> java.util.Arrays.asList(
            hydra.lib.pairs.First.apply(p),
            hydra.lib.pairs.Second.apply(p))),
          hydra.lib.maps.ToList.apply((m).value)));
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> (java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList()),
          (java.util.function.Function<hydra.core.Term, java.util.List<hydra.core.Term>>) (t -> java.util.Arrays.asList(t)),
          (m).value);
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Pair p) {
        return java.util.Arrays.asList(
          hydra.lib.pairs.First.apply((p).value),
          hydra.lib.pairs.Second.apply((p).value));
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Project ignored) {
        return (java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList());
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Record rt) {
        return hydra.lib.lists.Map.apply(
          projected -> projected.term,
          (rt).value.fields);
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Set l) {
        return hydra.lib.sets.ToList.apply((l).value);
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.TypeApplication ta) {
        return java.util.Arrays.asList((ta).value.body);
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.TypeLambda ta) {
        return java.util.Arrays.asList((ta).value.body);
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Inject ut) {
        return java.util.Arrays.asList((ut).value.field.term);
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Unit ignored) {
        return (java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList());
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Unwrap ignored) {
        return (java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList());
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Variable ignored) {
        return (java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList());
      }

      @Override
      public java.util.List<hydra.core.Term> visit(hydra.core.Term.Wrap n) {
        return java.util.Arrays.asList((n).value.body);
      }
    });
  }

  static java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> subtermsWithSteps(hydra.core.Term v1) {
    return (v1).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Annotated at) {
        return java.util.Arrays.asList((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.AnnotatedBody(), (at).value.body))));
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Application p) {
        return java.util.Arrays.asList(
          (hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.ApplicationFunction(), (p).value.function))),
          (hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.ApplicationArgument(), (p).value.argument))));
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Cases cs) {
        return hydra.lib.lists.Concat2.apply(
          hydra.lib.maybes.Maybe.applyLazy(
            () -> (java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>) (java.util.Collections.<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>emptyList()),
            (java.util.function.Function<hydra.core.Term, java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>>) (t -> java.util.Arrays.asList((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.UnionCasesDefault(), t))))),
            (cs).value.default_),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Field, hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>) (f -> (hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.UnionCasesBranch((f).name), (f).term)))),
            (cs).value.cases));
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Either e) {
        return (java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>) (java.util.Collections.<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>emptyList());
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Lambda l) {
        return java.util.Arrays.asList((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.LambdaBody(), (l).value.body))));
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Let lt) {
        return hydra.lib.lists.Cons.apply(
          (hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.LetBody(), (lt).value.body))),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.Binding, hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>) (b -> (hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.LetBinding((b).name), (b).term)))),
            (lt).value.bindings));
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.List l) {
        return hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>) (e -> (hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.ListElement(0), e)))),
          (l).value);
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Literal ignored) {
        return (java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>) (java.util.Collections.<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>emptyList());
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Map m) {
        return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>>) (p -> java.util.Arrays.asList(
            (hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.MapKey(0), hydra.lib.pairs.First.apply(p)))),
            (hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.MapValue(0), hydra.lib.pairs.Second.apply(p)))))),
          hydra.lib.maps.ToList.apply((m).value)));
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> (java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>) (java.util.Collections.<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>emptyList()),
          (java.util.function.Function<hydra.core.Term, java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>>) (t -> java.util.Arrays.asList((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.MaybeTerm(), t))))),
          (m).value);
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Pair p) {
        return (java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>) (java.util.Collections.<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>emptyList());
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Project ignored) {
        return (java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>) (java.util.Collections.<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>emptyList());
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Record rt) {
        return hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Field, hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>) (f -> (hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.RecordField((f).name), (f).term)))),
          (rt).value.fields);
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Set s) {
        return hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Term, hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>) (e -> (hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.ListElement(0), e)))),
          hydra.lib.sets.ToList.apply((s).value));
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.TypeApplication ta) {
        return java.util.Arrays.asList((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.TypeApplicationTerm(), (ta).value.body))));
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.TypeLambda ta) {
        return java.util.Arrays.asList((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.TypeLambdaBody(), (ta).value.body))));
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Inject ut) {
        return java.util.Arrays.asList((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.InjectionTerm(), (ut).value.field.term))));
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Unit ignored) {
        return (java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>) (java.util.Collections.<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>emptyList());
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Unwrap ignored) {
        return (java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>) (java.util.Collections.<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>emptyList());
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Variable ignored) {
        return (java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>) (java.util.Collections.<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>>emptyList());
      }

      @Override
      public java.util.List<hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>> visit(hydra.core.Term.Wrap n) {
        return java.util.Arrays.asList((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) ((hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>) (new hydra.util.Pair<hydra.paths.SubtermStep, hydra.core.Term>(new hydra.paths.SubtermStep.WrappedTerm(), (n).value.body))));
      }
    });
  }

  static java.util.List<hydra.core.Type> subtypes(hydra.core.Type v1) {
    return (v1).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Annotated at) {
        return java.util.Arrays.asList((at).value.body);
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Application at) {
        return java.util.Arrays.asList(
          (at).value.function,
          (at).value.argument);
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Either et) {
        return java.util.Arrays.asList(
          (et).value.left,
          (et).value.right);
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Pair pt) {
        return java.util.Arrays.asList(
          (pt).value.first,
          (pt).value.second);
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Function ft) {
        return java.util.Arrays.asList(
          (ft).value.domain,
          (ft).value.codomain);
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Forall lt) {
        return java.util.Arrays.asList((lt).value.body);
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.List lt) {
        return java.util.Arrays.asList((lt).value);
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Literal ignored) {
        return (java.util.List<hydra.core.Type>) (java.util.Collections.<hydra.core.Type>emptyList());
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Map mt) {
        return java.util.Arrays.asList(
          (mt).value.keys,
          (mt).value.values);
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Maybe ot) {
        return java.util.Arrays.asList((ot).value);
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Record rt) {
        return hydra.lib.lists.Map.apply(
          projected -> projected.type,
          (rt).value);
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Set st) {
        return java.util.Arrays.asList((st).value);
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Union rt) {
        return hydra.lib.lists.Map.apply(
          projected -> projected.type,
          (rt).value);
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Unit ignored) {
        return (java.util.List<hydra.core.Type>) (java.util.Collections.<hydra.core.Type>emptyList());
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Variable ignored) {
        return (java.util.List<hydra.core.Type>) (java.util.Collections.<hydra.core.Type>emptyList());
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Void_ ignored) {
        return (java.util.List<hydra.core.Type>) (java.util.Collections.<hydra.core.Type>emptyList());
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Wrap nt) {
        return java.util.Arrays.asList((nt).value);
      }
    });
  }
}
