// Note: this is an automatically generated file. Do not edit.

package hydra.show.typing;

/**
 * String representations of hydra.typing types
 */
public interface Typing {
  static String typeConstraint(hydra.typing.TypeConstraint tc) {
    hydra.core.Type ltyp = ((hydra.typing.TypeConstraint) ((tc))).left;
    hydra.core.Type rtyp = ((hydra.typing.TypeConstraint) ((tc))).right;
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      hydra.show.core.Core.type((ltyp)),
      "\u2261",
      hydra.show.core.Core.type((rtyp))));
  }
  
  static String typeSubst(hydra.typing.TypeSubst ts) {
    java.util.Map<hydra.core.Name, hydra.core.Type> subst = ((ts)).value;
    java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>> pairs = hydra.lib.maps.ToList.apply((subst));
    java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>, String> showPair = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Type>, String>) (tuple2 -> {
      String name = (((tuple2)).object1).value;
      hydra.core.Type typ = ((tuple2)).object2;
      return hydra.lib.strings.Cat.apply(java.util.List.of(
        (name),
        "\u21A6",
        hydra.show.core.Core.type((typ))));
    });
    java.util.List<String> pairStrs = hydra.lib.lists.Map.apply(
      (showPair),
      (pairs));
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      "{",
      hydra.lib.strings.Intercalate.apply(
        ",",
        (pairStrs)),
      "}"));
  }
}
