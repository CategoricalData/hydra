// Note: this is an automatically generated file. Do not edit.

package hydra.validate.core;

/**
 * Validation functions for core terms
 */
public interface Core {
  static hydra.util.Maybe<hydra.error.core.InvalidTermError> checkDuplicateBindings(hydra.accessors.AccessorPath path, hydra.util.ConsList<hydra.core.Binding> bindings) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> names = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      projected -> projected.name,
      bindings));
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.Name>> dup = new hydra.util.Lazy<>(() -> hydra.validate.core.Core.findDuplicate(names.get()));
    return hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.error.core.InvalidTermError>) (name -> new hydra.error.core.InvalidTermError.DuplicateBinding(new hydra.error.core.DuplicateBindingError(path, name))),
      dup.get());
  }

  static hydra.util.Maybe<hydra.error.core.InvalidTermError> checkDuplicateFields(hydra.accessors.AccessorPath path, hydra.util.ConsList<hydra.core.Name> names) {
    hydra.util.Lazy<hydra.util.Maybe<hydra.core.Name>> dup = new hydra.util.Lazy<>(() -> hydra.validate.core.Core.findDuplicate(names));
    return hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.core.Name, hydra.error.core.InvalidTermError>) (name -> new hydra.error.core.InvalidTermError.DuplicateField(new hydra.error.core.DuplicateFieldError(path, name))),
      dup.get());
  }

  static hydra.util.Maybe<hydra.error.core.InvalidTermError> checkTerm(hydra.accessors.AccessorPath path, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTermError> otherwise(hydra.core.Term instance) {
        return (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing());
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Let lt) {
        return hydra.validate.core.Core.checkDuplicateBindings(
          path,
          (lt).value.bindings);
      }

      @Override
      public hydra.util.Maybe<hydra.error.core.InvalidTermError> visit(hydra.core.Term.Record rec) {
        return hydra.validate.core.Core.checkDuplicateFields(
          path,
          hydra.lib.lists.Map.apply(
            projected -> projected.name,
            (rec).value.fields));
      }
    });
  }

  static <T0> hydra.util.Maybe<T0> findDuplicate(hydra.util.ConsList<T0> names) {
    return hydra.lib.pairs.Second.apply(hydra.validate.core.Core.<T0>findDuplicate_result(names));
  }

  static <T0> hydra.util.Pair<hydra.util.PersistentSet<T0>, hydra.util.Maybe<T0>> findDuplicate_result(hydra.util.ConsList<T0> names) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.util.PersistentSet<T0>, hydra.util.Maybe<T0>>, java.util.function.Function<T0, hydra.util.Pair<hydra.util.PersistentSet<T0>, hydra.util.Maybe<T0>>>>) (acc -> (java.util.function.Function<T0, hydra.util.Pair<hydra.util.PersistentSet<T0>, hydra.util.Maybe<T0>>>) (name -> {
        hydra.util.Lazy<hydra.util.PersistentSet<T0>> seen = new hydra.util.Lazy<>(() -> hydra.validate.core.Core.<T0>findDuplicate_seen(acc));
        return hydra.lib.maybes.Cases.applyLazy(
          hydra.validate.core.Core.<T0>findDuplicate_dup(acc),
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.sets.Member.apply(
              name,
              seen.get()),
            () -> (hydra.util.Pair<hydra.util.PersistentSet<T0>, hydra.util.Maybe<T0>>) ((hydra.util.Pair<hydra.util.PersistentSet<T0>, hydra.util.Maybe<T0>>) (new hydra.util.Pair<hydra.util.PersistentSet<T0>, hydra.util.Maybe<T0>>(seen.get(), hydra.util.Maybe.just(name)))),
            () -> (hydra.util.Pair<hydra.util.PersistentSet<T0>, hydra.util.Maybe<T0>>) ((hydra.util.Pair<hydra.util.PersistentSet<T0>, hydra.util.Maybe<T0>>) (new hydra.util.Pair<hydra.util.PersistentSet<T0>, hydra.util.Maybe<T0>>(hydra.lib.sets.Insert.apply(
              name,
              seen.get()), (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()))))),
          (java.util.function.Function<T0, hydra.util.Pair<hydra.util.PersistentSet<T0>, hydra.util.Maybe<T0>>>) (ignored -> acc));
      })),
      (hydra.util.Pair<hydra.util.PersistentSet<T0>, hydra.util.Maybe<T0>>) ((hydra.util.Pair<hydra.util.PersistentSet<T0>, hydra.util.Maybe<T0>>) (new hydra.util.Pair<hydra.util.PersistentSet<T0>, hydra.util.Maybe<T0>>((hydra.util.PersistentSet<T0>) (hydra.lib.sets.Empty.<T0>apply()), (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing())))),
      names);
  }

  static <T0> hydra.util.PersistentSet<T0> findDuplicate_seen(hydra.util.Pair<hydra.util.PersistentSet<T0>, hydra.util.Maybe<T0>> acc) {
    return hydra.lib.pairs.First.apply(acc);
  }

  static <T0> hydra.util.Maybe<T0> findDuplicate_dup(hydra.util.Pair<hydra.util.PersistentSet<T0>, hydra.util.Maybe<T0>> acc) {
    return hydra.lib.pairs.Second.apply(acc);
  }

  static hydra.util.Maybe<hydra.error.core.InvalidTermError> term(hydra.graph.Graph g, hydra.core.Term t) {
    return hydra.rewriting.Rewriting.foldTermWithGraphAndPath(
      (java.util.function.Function<java.util.function.Function<hydra.util.Maybe<hydra.error.core.InvalidTermError>, java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.error.core.InvalidTermError>>>, java.util.function.Function<hydra.util.ConsList<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.util.Maybe<hydra.error.core.InvalidTermError>, java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.error.core.InvalidTermError>>>>>>) (recurse -> (java.util.function.Function<hydra.util.ConsList<hydra.accessors.TermAccessor>, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.util.Maybe<hydra.error.core.InvalidTermError>, java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.error.core.InvalidTermError>>>>>) (path -> (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.util.Maybe<hydra.error.core.InvalidTermError>, java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.error.core.InvalidTermError>>>>) (cx -> (java.util.function.Function<hydra.util.Maybe<hydra.error.core.InvalidTermError>, java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.error.core.InvalidTermError>>>) (acc -> (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.error.core.InvalidTermError>>) (trm -> hydra.lib.maybes.Cases.applyLazy(
        acc,
        () -> ((java.util.function.Supplier<hydra.util.Maybe<hydra.error.core.InvalidTermError>>) (() -> {
          hydra.util.Maybe<hydra.error.core.InvalidTermError> checkResult = hydra.validate.core.Core.checkTerm(
            new hydra.accessors.AccessorPath(path),
            trm);
          return hydra.lib.maybes.Cases.applyLazy(
            checkResult,
            () -> (recurse).apply((hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing())).apply(trm),
            (java.util.function.Function<hydra.error.core.InvalidTermError, hydra.util.Maybe<hydra.error.core.InvalidTermError>>) (err -> hydra.util.Maybe.just(err)));
        })).get(),
        (java.util.function.Function<hydra.error.core.InvalidTermError, hydra.util.Maybe<hydra.error.core.InvalidTermError>>) (ignored -> acc))))))),
      g,
      (hydra.util.Maybe<hydra.error.core.InvalidTermError>) (hydra.util.Maybe.<hydra.error.core.InvalidTermError>nothing()),
      t);
  }
}
