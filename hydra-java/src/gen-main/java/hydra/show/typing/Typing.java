// Note: this is an automatically generated file. Do not edit.

package hydra.show.typing;

/**
 * String representations of hydra.typing types
 */
public interface Typing {
  static String typeConstraint(hydra.typing.TypeConstraint tc) {
    hydra.core.Type ltyp = (tc).left;
    hydra.core.Type rtyp = (tc).right;
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      hydra.show.core.Core.type(ltyp),
      "\u2261",
      hydra.show.core.Core.type(rtyp)));
  }
  
  static String typeSubst(hydra.typing.TypeSubst ts) {
    java.util.Map<hydra.core.Name, hydra.core.Type> subst = (ts).value;
    hydra.util.Lazy<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>>> pairs = new hydra.util.Lazy<>(() -> hydra.lib.maps.ToList.apply(subst));
    java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>, String> showPair = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>, String>) (pair -> {
      hydra.util.Lazy<String> name = new hydra.util.Lazy<>(() -> (hydra.lib.pairs.First.apply(pair)).value);
      hydra.util.Lazy<hydra.core.Type> typ = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
      return hydra.lib.strings.Cat.apply(java.util.List.of(
        name.get(),
        "\u21A6",
        hydra.show.core.Core.type(typ.get())));
    });
    hydra.util.Lazy<java.util.List<String>> pairStrs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      showPair,
      pairs.get()));
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      "{",
      hydra.lib.strings.Intercalate.apply(
        ",",
        pairStrs.get()),
      "}"));
  }
}
