package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class NodeLabel implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.NodeLabel");
  
  public final String value;
  
  public NodeLabel (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeLabel)) {
      return false;
    }
    NodeLabel o = (NodeLabel) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}