// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class MergeAction implements Serializable, Comparable<MergeAction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.MergeAction");

  public static final hydra.core.Name ACTION = new hydra.core.Name("action");

  public static final hydra.core.Name SET = new hydra.core.Name("set");

  public final hydra.ext.cypher.openCypher.MatchOrCreate action;

  public final hydra.ext.cypher.openCypher.Set set;

  public MergeAction (hydra.ext.cypher.openCypher.MatchOrCreate action, hydra.ext.cypher.openCypher.Set set) {
    this.action = action;
    this.set = set;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MergeAction)) {
      return false;
    }
    MergeAction o = (MergeAction) other;
    return java.util.Objects.equals(
      this.action,
      o.action) && java.util.Objects.equals(
      this.set,
      o.set);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(action) + 3 * java.util.Objects.hashCode(set);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MergeAction other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      action,
      other.action);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      set,
      other.set);
  }

  public MergeAction withAction(hydra.ext.cypher.openCypher.MatchOrCreate action) {
    return new MergeAction(action, set);
  }

  public MergeAction withSet(hydra.ext.cypher.openCypher.Set set) {
    return new MergeAction(action, set);
  }
}
