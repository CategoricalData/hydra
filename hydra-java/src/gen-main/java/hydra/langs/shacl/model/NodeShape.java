// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shacl.model;

import java.io.Serializable;

/**
 * A SHACL node shape. See https://www.w3.org/TR/shacl/#node-shapes
 */
public class NodeShape implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shacl/model.NodeShape");
  
  public final hydra.langs.shacl.model.CommonProperties common;
  
  public NodeShape (hydra.langs.shacl.model.CommonProperties common) {
    java.util.Objects.requireNonNull((common));
    this.common = common;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodeShape)) {
      return false;
    }
    NodeShape o = (NodeShape) (other);
    return common.equals(o.common);
  }
  
  @Override
  public int hashCode() {
    return 2 * common.hashCode();
  }
}