// Note: this is an automatically generated file. Do not edit.

package hydra.eval.lib.maps;

/**
 * Evaluation-level implementations of Map functions for the Hydra interpreter.
 */
public interface Maps {
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> alter(hydra.context.Context cx, T0 g, hydra.core.Term funTerm, hydra.core.Term keyTerm, hydra.core.Term mapTerm) {
    return (mapTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "map value"),
            " but found "),
          hydra.show.core.Core.term(mapTerm)))), cx)));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Term.Map m) {
        hydra.util.Lazy<hydra.util.Maybe<hydra.core.Term>> currentVal = new hydra.util.Lazy<>(() -> hydra.lib.maps.Lookup.apply(
          keyTerm,
          (m).value));
        hydra.core.Term newVal = new hydra.core.Term.Application(new hydra.core.Application(funTerm, new hydra.core.Term.Maybe(currentVal.get())));
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maybes.maybe"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maps.delete"))), keyTerm)), mapTerm)))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("newV"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maps.insert"))), keyTerm)), new hydra.core.Term.Variable(new hydra.core.Name("newV")))), mapTerm))))))), newVal)));
      }
    });
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> bimap(hydra.context.Context cx, T0 g, hydra.core.Term keyFun, hydra.core.Term valFun, hydra.core.Term mapTerm) {
    return (mapTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "map value"),
            " but found "),
          hydra.show.core.Core.term(mapTerm)))), cx)));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Term.Map m) {
        hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>> pairs = new hydra.util.Lazy<>(() -> hydra.lib.maps.ToList.apply((m).value));
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) (p -> {
            hydra.util.Lazy<hydra.core.Term> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
            hydra.util.Lazy<hydra.core.Term> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(p));
            return (hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(keyFun, k.get())), new hydra.core.Term.Application(new hydra.core.Application(valFun, v.get())))));
          }),
          pairs.get()))));
      }
    });
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> filter(hydra.context.Context cx, T0 g, hydra.core.Term valPred, hydra.core.Term mapTerm) {
    return (mapTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "map value"),
            " but found "),
          hydra.show.core.Core.term(mapTerm)))), cx)));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Term.Map m) {
        hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>> pairs = new hydra.util.Lazy<>(() -> hydra.lib.maps.ToList.apply((m).value));
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maps.fromList"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat"))), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.core.Term>) (p -> {
            hydra.util.Lazy<hydra.core.Term> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(p));
            return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.logic.ifElse"))), new hydra.core.Term.Application(new hydra.core.Application(valPred, v.get())))), new hydra.core.Term.List(hydra.lib.lists.Pure.apply(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(hydra.lib.pairs.First.apply(p), v.get())))))))), new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))));
          }),
          pairs.get())))))));
      }
    });
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> filterWithKey(hydra.context.Context cx, T0 g, hydra.core.Term pred, hydra.core.Term mapTerm) {
    return (mapTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "map value"),
            " but found "),
          hydra.show.core.Core.term(mapTerm)))), cx)));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Term.Map m) {
        hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>> pairs = new hydra.util.Lazy<>(() -> hydra.lib.maps.ToList.apply((m).value));
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maps.fromList"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat"))), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.core.Term>) (p -> {
            hydra.util.Lazy<hydra.core.Term> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
            hydra.util.Lazy<hydra.core.Term> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(p));
            return new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.logic.ifElse"))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(pred, k.get())), v.get())))), new hydra.core.Term.List(hydra.lib.lists.Pure.apply(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(k.get(), v.get())))))))), new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.List.<hydra.core.Term>of()))));
          }),
          pairs.get())))))));
      }
    });
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> map(hydra.context.Context cx, T0 g, hydra.core.Term valFun, hydra.core.Term mapTerm) {
    return (mapTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "map value"),
            " but found "),
          hydra.show.core.Core.term(mapTerm)))), cx)));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Term.Map m) {
        hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>> pairs = new hydra.util.Lazy<>(() -> hydra.lib.maps.ToList.apply((m).value));
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) (p -> {
            hydra.util.Lazy<hydra.core.Term> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
            hydra.util.Lazy<hydra.core.Term> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(p));
            return (hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(k.get(), new hydra.core.Term.Application(new hydra.core.Application(valFun, v.get())))));
          }),
          pairs.get()))));
      }
    });
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> mapKeys(hydra.context.Context cx, T0 g, hydra.core.Term keyFun, hydra.core.Term mapTerm) {
    return (mapTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "expected ",
              "map value"),
            " but found "),
          hydra.show.core.Core.term(mapTerm)))), cx)));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.core.Term.Map m) {
        hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>> pairs = new hydra.util.Lazy<>(() -> hydra.lib.maps.ToList.apply((m).value));
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) (p -> {
            hydra.util.Lazy<hydra.core.Term> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(p));
            hydra.util.Lazy<hydra.core.Term> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(p));
            return (hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(keyFun, k.get())), v.get())));
          }),
          pairs.get()))));
      }
    });
  }
}
