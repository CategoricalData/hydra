// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class Merge implements Serializable, Comparable<Merge> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.Merge");

  public static final hydra.core.Name PATTERN_PART = new hydra.core.Name("patternPart");

  public static final hydra.core.Name ACTIONS = new hydra.core.Name("actions");

  public final hydra.ext.cypher.openCypher.PatternPart patternPart;

  public final java.util.List<hydra.ext.cypher.openCypher.MergeAction> actions;

  public Merge (hydra.ext.cypher.openCypher.PatternPart patternPart, java.util.List<hydra.ext.cypher.openCypher.MergeAction> actions) {
    this.patternPart = patternPart;
    this.actions = actions;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Merge)) {
      return false;
    }
    Merge o = (Merge) other;
    return java.util.Objects.equals(
      this.patternPart,
      o.patternPart) && java.util.Objects.equals(
      this.actions,
      o.actions);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(patternPart) + 3 * java.util.Objects.hashCode(actions);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Merge other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      patternPart,
      other.patternPart);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      actions,
      other.actions);
  }

  public Merge withPatternPart(hydra.ext.cypher.openCypher.PatternPart patternPart) {
    return new Merge(patternPart, actions);
  }

  public Merge withActions(java.util.List<hydra.ext.cypher.openCypher.MergeAction> actions) {
    return new Merge(patternPart, actions);
  }
}
