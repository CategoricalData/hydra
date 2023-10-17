package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class MergeAction implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.MergeAction");
  
  public final Boolean create;
  
  public final java.util.List<hydra.langs.cypher.openCypher.SetItem> set;
  
  public MergeAction (Boolean create, java.util.List<hydra.langs.cypher.openCypher.SetItem> set) {
    this.create = create;
    this.set = set;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MergeAction)) {
      return false;
    }
    MergeAction o = (MergeAction) (other);
    return create.equals(o.create) && set.equals(o.set);
  }
  
  @Override
  public int hashCode() {
    return 2 * create.hashCode() + 3 * set.hashCode();
  }
  
  public MergeAction withCreate(Boolean create) {
    return new MergeAction(create, set);
  }
  
  public MergeAction withSet(java.util.List<hydra.langs.cypher.openCypher.SetItem> set) {
    return new MergeAction(create, set);
  }
}