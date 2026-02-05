// Note: this is an automatically generated file. Do not edit.

package hydra.accessors;

import java.io.Serializable;

/**
 * A node in an accessor graph, representing a term or subterm
 */
public class AccessorNode implements Serializable, Comparable<AccessorNode> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.accessors.AccessorNode");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_LABEL = new hydra.core.Name("label");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  /**
   * The qualified name of the term
   */
  public final hydra.core.Name name;
  
  /**
   * A human-readable label for the node
   */
  public final String label;
  
  /**
   * A unique identifier for the node
   */
  public final String id;
  
  public AccessorNode (hydra.core.Name name, String label, String id) {
    this.name = name;
    this.label = label;
    this.id = id;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AccessorNode)) {
      return false;
    }
    AccessorNode o = (AccessorNode) (other);
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.label,
      o.label) && java.util.Objects.equals(
      this.id,
      o.id);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(label) + 5 * java.util.Objects.hashCode(id);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AccessorNode other) {
    int cmp = 0;
    cmp = ((Comparable) (name)).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (label)).compareTo(other.label);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (id)).compareTo(other.id);
  }
  
  public AccessorNode withName(hydra.core.Name name) {
    return new AccessorNode(name, label, id);
  }
  
  public AccessorNode withLabel(String label) {
    return new AccessorNode(name, label, id);
  }
  
  public AccessorNode withId(String id) {
    return new AccessorNode(name, label, id);
  }
}
