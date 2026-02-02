// Note: this is an automatically generated file. Do not edit.

package hydra.encode.phantoms;

/**
 * Term encoders for hydra.phantoms
 */
public interface Phantoms {
  static <T0, T1> hydra.core.Term tBinding(T0 a, hydra.phantoms.TBinding<T1> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.phantoms.TBinding"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.core.Core.name(((java.util.function.Function<hydra.phantoms.TBinding<T1>, hydra.core.Name>) (projected -> projected.name)).apply((x)))),
      new hydra.core.Field(new hydra.core.Name("term"), hydra.encode.phantoms.Phantoms.<T0, T1>tTerm(
        (a),
        ((java.util.function.Function<hydra.phantoms.TBinding<T1>, hydra.phantoms.TTerm<T1>>) (projected -> projected.term)).apply((x)))))));
  }
  
  static <T0, T1> hydra.core.Term tTerm(T0 a, hydra.phantoms.TTerm<T1> x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.phantoms.TTerm"), hydra.encode.core.Core.term(((java.util.function.Function<hydra.phantoms.TTerm<T1>, hydra.core.Term>) (wrapped -> ((wrapped)).value)).apply((x)))));
  }
}
