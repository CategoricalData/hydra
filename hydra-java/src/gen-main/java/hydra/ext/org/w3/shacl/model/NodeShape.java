// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.shacl.model;

import java.io.Serializable;

/**
 * A SHACL node shape. See https://www.w3.org/TR/shacl/#node-shapes
 */
public class NodeShape implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/w3/shacl/model.NodeShape");
  
  public static final hydra.core.Name FIELD_NAME_COMMON = new hydra.core.Name("common");
  
  public final hydra.ext.org.w3.shacl.model.CommonProperties common;
  
  public NodeShape (hydra.ext.org.w3.shacl.model.CommonProperties common) {
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