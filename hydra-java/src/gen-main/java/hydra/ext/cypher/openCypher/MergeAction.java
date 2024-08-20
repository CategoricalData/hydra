// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class MergeAction implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/openCypher.MergeAction");
  
  public static final hydra.core.Name FIELD_NAME_ACTION = new hydra.core.Name("action");
  
  public static final hydra.core.Name FIELD_NAME_SET = new hydra.core.Name("set");
  
  public final hydra.ext.cypher.openCypher.MatchOrCreate action;
  
  public final hydra.ext.cypher.openCypher.Set set;
  
  public MergeAction (hydra.ext.cypher.openCypher.MatchOrCreate action, hydra.ext.cypher.openCypher.Set set) {
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
  
  public MergeAction withAction(hydra.ext.cypher.openCypher.MatchOrCreate action) {
    java.util.Objects.requireNonNull((action));
    return new MergeAction(action, set);
  }
  
  public MergeAction withSet(hydra.ext.cypher.openCypher.Set set) {
    java.util.Objects.requireNonNull((set));
    return new MergeAction(action, set);
  }
}
