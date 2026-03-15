// Note: this is an automatically generated file. Do not edit.

package hydra.show.typing;

/**
 * String representations of hydra.typing types
 */
public interface Typing {
  static String typeConstraint(hydra.typing.TypeConstraint tc) {
    hydra.core.Type ltyp = (tc).left;
    hydra.core.Type rtyp = (tc).right;
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      hydra.show.core.Core.type(ltyp),
      "\u2261",
      hydra.show.core.Core.type(rtyp)));
  }
  
  static String typeSubst(hydra.typing.TypeSubst ts) {
    hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> subst = (ts).value;
    hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Type>>> pairs = new hydra.util.Lazy<>(() -> hydra.lib.maps.ToList.apply(subst));
    java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String> showPair = (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (pair -> {
      hydra.util.Lazy<String> name = new hydra.util.Lazy<>(() -> (hydra.lib.pairs.First.apply(pair)).value);
      hydra.util.Lazy<hydra.core.Type> typ = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair));
      return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
        name.get(),
        "\u21A6",
        hydra.show.core.Core.type(typ.get())));
    });
    hydra.util.Lazy<hydra.util.ConsList<String>> pairStrs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      showPair,
      pairs.get()));
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "{",
      hydra.lib.strings.Intercalate.apply(
        ",",
        pairStrs.get()),
      "}"));
  }
}
