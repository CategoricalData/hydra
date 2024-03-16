package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class NodeLabels implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.NodeLabels");
  
  public final java.util.List<hydra.langs.cypher.openCypher.NodeLabel> value;
  
  public NodeLabels (java.util.List<hydra.langs.cypher.openCypher.NodeLabel> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeLabels)) {
      return false;
    }
    NodeLabels o = (NodeLabels) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}