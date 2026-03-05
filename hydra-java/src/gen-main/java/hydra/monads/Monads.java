// Note: this is an automatically generated file. Do not edit.

package hydra.monads;

/**
 * Miscellaneous helper functions.
 */
public interface Monads {
  static hydra.context.Context emptyContext() {
    return new hydra.context.Context((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())));
  }
  
  static <T0> java.util.List<T0> maybeToList(hydra.util.Maybe<T0> mx) {
    return hydra.lib.maybes.Maybe.apply(
      (java.util.List<T0>) (java.util.List.<T0>of()),
      (java.util.function.Function<T0, java.util.List<T0>>) (hydra.lib.lists.Pure::apply),
      mx);
  }
}
