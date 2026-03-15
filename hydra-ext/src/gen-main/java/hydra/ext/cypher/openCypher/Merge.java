// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class Merge implements Serializable, Comparable<Merge> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.Merge");
  
  public static final hydra.core.Name PATTERN_PART = new hydra.core.Name("patternPart");
  
  public static final hydra.core.Name ACTIONS = new hydra.core.Name("actions");
  
  public final hydra.ext.cypher.openCypher.PatternPart patternPart;
  
  public final hydra.util.ConsList<hydra.ext.cypher.openCypher.MergeAction> actions;
  
  public Merge (hydra.ext.cypher.openCypher.PatternPart patternPart, hydra.util.ConsList<hydra.ext.cypher.openCypher.MergeAction> actions) {
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
    cmp = ((Comparable) patternPart).compareTo(other.patternPart);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      actions.hashCode(),
      other.actions.hashCode());
  }
  
  public Merge withPatternPart(hydra.ext.cypher.openCypher.PatternPart patternPart) {
    return new Merge(patternPart, actions);
  }
  
  public Merge withActions(hydra.util.ConsList<hydra.ext.cypher.openCypher.MergeAction> actions) {
    return new Merge(patternPart, actions);
  }
}
