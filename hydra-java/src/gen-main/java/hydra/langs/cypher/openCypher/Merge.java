package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class Merge implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Merge");
  
  public final hydra.langs.cypher.openCypher.PatternPart pattern;
  
  public final java.util.List<hydra.langs.cypher.openCypher.MergeAction> actions;
  
  public Merge (hydra.langs.cypher.openCypher.PatternPart pattern, java.util.List<hydra.langs.cypher.openCypher.MergeAction> actions) {
    this.pattern = pattern;
    this.actions = actions;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Merge)) {
      return false;
    }
    Merge o = (Merge) (other);
    return pattern.equals(o.pattern) && actions.equals(o.actions);
  }
  
  @Override
  public int hashCode() {
    return 2 * pattern.hashCode() + 3 * actions.hashCode();
  }
  
  public Merge withPattern(hydra.langs.cypher.openCypher.PatternPart pattern) {
    return new Merge(pattern, actions);
  }
  
  public Merge withActions(java.util.List<hydra.langs.cypher.openCypher.MergeAction> actions) {
    return new Merge(pattern, actions);
  }
}