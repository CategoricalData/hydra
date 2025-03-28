// Note: this is an automatically generated file. Do not edit.

package hydra.tier3;

/**
 * A module for miscellaneous tier-3 functions and constants.
 */
public interface Tier3 {
  static String traceSummary(hydra.compute.Trace t) {
    java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>, String> toLine = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>, String>) (pair -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "\t",
          ((pair)).object1.value)),
        ": ")),
      hydra.lib.io.ShowTerm.apply(((pair)).object2))));
    java.util.List<String> messageLines = hydra.lib.lists.Nub.apply(((t)).messages);
    java.util.List<String> keyvalLines = hydra.lib.logic.IfElse.apply(
            hydra.lib.maps.IsEmpty.apply(((t)).other),
      java.util.Arrays.asList(),
      hydra.lib.lists.Cons.apply(
        "key/value pairs: ",
        hydra.lib.lists.Map.apply(
          (toLine),
          hydra.lib.maps.ToList.apply(((t)).other)))
      );
    return hydra.lib.strings.Intercalate.apply(
      "\n",
      hydra.lib.lists.Concat2.apply(
        (messageLines),
        (keyvalLines)));
  }
}