// Note: this is an automatically generated file. Do not edit.

package hydra.accessors;

import java.io.Serializable;

/**
 * A node in an accessor graph, representing a term or subterm
 */
public class AccessorNode implements Serializable {
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
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((label));
    java.util.Objects.requireNonNull((id));
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
    return name.equals(o.name) && label.equals(o.label) && id.equals(o.id);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * label.hashCode() + 5 * id.hashCode();
  }
  
  public AccessorNode withName(hydra.core.Name name) {
    java.util.Objects.requireNonNull((name));
    return new AccessorNode(name, label, id);
  }
  
  public AccessorNode withLabel(String label) {
    java.util.Objects.requireNonNull((label));
    return new AccessorNode(name, label, id);
  }
  
  public AccessorNode withId(String id) {
    java.util.Objects.requireNonNull((id));
    return new AccessorNode(name, label, id);
  }
}
