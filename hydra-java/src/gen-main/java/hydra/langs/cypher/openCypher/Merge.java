// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class Merge implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Merge");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN_PART = new hydra.core.Name("patternPart");
  
  public static final hydra.core.Name FIELD_NAME_ACTIONS = new hydra.core.Name("actions");
  
  public final hydra.langs.cypher.openCypher.PatternPart patternPart;
  
  public final java.util.List<hydra.langs.cypher.openCypher.MergeAction> actions;
  
  public Merge (hydra.langs.cypher.openCypher.PatternPart patternPart, java.util.List<hydra.langs.cypher.openCypher.MergeAction> actions) {
    java.util.Objects.requireNonNull((patternPart));
    java.util.Objects.requireNonNull((actions));
    this.patternPart = patternPart;
    this.actions = actions;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Merge)) {
      return false;
    }
    Merge o = (Merge) (other);
    return patternPart.equals(o.patternPart) && actions.equals(o.actions);
  }
  
  @Override
  public int hashCode() {
    return 2 * patternPart.hashCode() + 3 * actions.hashCode();
  }
  
  public Merge withPatternPart(hydra.langs.cypher.openCypher.PatternPart patternPart) {
    java.util.Objects.requireNonNull((patternPart));
    return new Merge(patternPart, actions);
  }
  
  public Merge withActions(java.util.List<hydra.langs.cypher.openCypher.MergeAction> actions) {
    java.util.Objects.requireNonNull((actions));
    return new Merge(patternPart, actions);
  }
}