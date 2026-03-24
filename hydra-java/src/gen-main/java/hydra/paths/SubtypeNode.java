// Note: this is an automatically generated file. Do not edit.

package hydra.paths;

import java.io.Serializable;

/**
 * A node in a subtype graph, representing a type or subtype
 */
public class SubtypeNode implements Serializable, Comparable<SubtypeNode> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.paths.SubtypeNode");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  /**
   * The qualified name of the type
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

  public SubtypeNode (hydra.core.Name name, String label, String id) {
    this.name = name;
    this.label = label;
    this.id = id;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubtypeNode)) {
      return false;
    }
    SubtypeNode o = (SubtypeNode) other;
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
  public int compareTo(SubtypeNode other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) label).compareTo(other.label);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) id).compareTo(other.id);
  }

  public SubtypeNode withName(hydra.core.Name name) {
    return new SubtypeNode(name, label, id);
  }

  public SubtypeNode withLabel(String label) {
    return new SubtypeNode(name, label, id);
  }

  public SubtypeNode withId(String id) {
    return new SubtypeNode(name, label, id);
  }
}
