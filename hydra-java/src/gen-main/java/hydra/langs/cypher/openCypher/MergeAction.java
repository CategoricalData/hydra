// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class MergeAction implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.MergeAction");
  
  public final hydra.langs.cypher.openCypher.MatchOrCreate action;
  
  public final hydra.langs.cypher.openCypher.Set set;
  
  public MergeAction (hydra.langs.cypher.openCypher.MatchOrCreate action, hydra.langs.cypher.openCypher.Set set) {
    java.util.Objects.requireNonNull((action));
    java.util.Objects.requireNonNull((set));
    this.action = action;
    this.set = set;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MergeAction)) {
      return false;
    }
    MergeAction o = (MergeAction) (other);
    return action.equals(o.action) && set.equals(o.set);
  }
  
  @Override
  public int hashCode() {
    return 2 * action.hashCode() + 3 * set.hashCode();
  }
  
  public MergeAction withAction(hydra.langs.cypher.openCypher.MatchOrCreate action) {
    java.util.Objects.requireNonNull((action));
    return new MergeAction(action, set);
  }
  
  public MergeAction withSet(hydra.langs.cypher.openCypher.Set set) {
    java.util.Objects.requireNonNull((set));
    return new MergeAction(action, set);
  }
}