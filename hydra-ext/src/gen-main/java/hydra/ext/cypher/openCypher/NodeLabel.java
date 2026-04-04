// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class NodeLabel implements Serializable, Comparable<NodeLabel> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.NodeLabel");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public NodeLabel (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeLabel)) {
      return false;
    }
    NodeLabel o = (NodeLabel) other;
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
  public int compareTo(NodeLabel other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
