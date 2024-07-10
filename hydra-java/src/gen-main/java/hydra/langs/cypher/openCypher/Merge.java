// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class Merge implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Merge");
  
  public final hydra.langs.cypher.openCypher.PatternPart patternPart;
  
  public final java.util.List<hydra.langs.cypher.openCypher.MergeAction> actions;
  
  public Merge (hydra.langs.cypher.openCypher.PatternPart patternPart, java.util.List<hydra.langs.cypher.openCypher.MergeAction> actions) {
    if (patternPart == null) {
      throw new IllegalArgumentException("null value for 'patternPart' argument");
    }
    if (actions == null) {
      throw new IllegalArgumentException("null value for 'actions' argument");
    }
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
    if (patternPart == null) {
      throw new IllegalArgumentException("null value for 'patternPart' argument");
    }
    return new Merge(patternPart, actions);
  }
  
  public Merge withActions(java.util.List<hydra.langs.cypher.openCypher.MergeAction> actions) {
    if (actions == null) {
      throw new IllegalArgumentException("null value for 'actions' argument");
    }
    return new Merge(patternPart, actions);
  }
}