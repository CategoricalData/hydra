// Note: this is an automatically generated file. Do not edit.

package hydra.shacl.model;

import java.io.Serializable;

/**
 * A SHACL node shape. See https://www.w3.org/TR/shacl/#node-shapes
 */
public class NodeShape implements Serializable, Comparable<NodeShape> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shacl.model.NodeShape");

  public static final hydra.core.Name COMMON = new hydra.core.Name("common");

  public final hydra.shacl.model.CommonProperties common;

  public NodeShape (hydra.shacl.model.CommonProperties common) {
    this.common = common;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeShape)) {
      return false;
    }
    NodeShape o = (NodeShape) other;
    return java.util.Objects.equals(
      this.common,
      o.common);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(common);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NodeShape other) {
    return hydra.util.Comparing.compare(
      common,
      other.common);
  }
}
