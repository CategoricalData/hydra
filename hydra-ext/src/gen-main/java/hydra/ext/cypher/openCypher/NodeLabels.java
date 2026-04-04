// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class NodeLabels implements Serializable, Comparable<NodeLabels> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.NodeLabels");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<hydra.ext.cypher.openCypher.NodeLabel> value;

  public NodeLabels (java.util.List<hydra.ext.cypher.openCypher.NodeLabel> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeLabels)) {
      return false;
    }
    NodeLabels o = (NodeLabels) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NodeLabels other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
