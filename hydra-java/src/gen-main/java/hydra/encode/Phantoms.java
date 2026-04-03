// Note: this is an automatically generated file. Do not edit.

package hydra.encode;

/**
 * Term encoders for hydra.phantoms
 */
public interface Phantoms {
  static <T0, T1> hydra.core.Term tBinding(T0 a, hydra.phantoms.TBinding<T1> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.phantoms.TBinding"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name(((java.util.function.Function<hydra.phantoms.TBinding<T1>, hydra.core.Name>) (projected -> projected.name)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("term"), hydra.encode.Phantoms.<T0, T1>tTerm(
        a,
        ((java.util.function.Function<hydra.phantoms.TBinding<T1>, hydra.phantoms.TTerm<T1>>) (projected -> projected.term)).apply(x))))));
  }

  static <T0, T1> hydra.core.Term tTerm(T0 a, hydra.phantoms.TTerm<T1> x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.phantoms.TTerm"), hydra.encode.Core.term(((java.util.function.Function<hydra.phantoms.TTerm<T1>, hydra.core.Term>) (wrapped -> (wrapped).value)).apply(x))));
  }

  static <T0, T1> hydra.core.Term tTermDefinition(T0 a, hydra.phantoms.TTermDefinition<T1> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.phantoms.TTermDefinition"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), hydra.encode.Core.name(((java.util.function.Function<hydra.phantoms.TTermDefinition<T1>, hydra.core.Name>) (projected -> projected.name)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("term"), hydra.encode.Phantoms.<T0, T1>tTerm(
        a,
        ((java.util.function.Function<hydra.phantoms.TTermDefinition<T1>, hydra.phantoms.TTerm<T1>>) (projected -> projected.term)).apply(x))))));
  }
}
