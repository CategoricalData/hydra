// Note: this is an automatically generated file. Do not edit.

package hydra.pg.mapping;

import java.io.Serializable;

/**
 * A mapping specification producing vertices of a specified label
 */
public class VertexSpec implements Serializable, Comparable<VertexSpec> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.mapping.VertexSpec");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");

  /**
   * The label of the target vertices, which must conform to the vertex type associated with that label.
   */
  public final hydra.pg.model.VertexLabel label;

  /**
   * A specification of the id of each target vertex
   */
  public final hydra.pg.mapping.ValueSpec id;

  /**
   * Zero or more property specifications for each target vertex
   */
  public final hydra.util.ConsList<hydra.pg.mapping.PropertySpec> properties;

  public VertexSpec (hydra.pg.model.VertexLabel label, hydra.pg.mapping.ValueSpec id, hydra.util.ConsList<hydra.pg.mapping.PropertySpec> properties) {
    this.label = label;
    this.id = id;
    this.properties = properties;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexSpec)) {
      return false;
    }
    VertexSpec o = (VertexSpec) other;
    return java.util.Objects.equals(
      this.label,
      o.label) && java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.properties,
      o.properties);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(label) + 3 * java.util.Objects.hashCode(id) + 5 * java.util.Objects.hashCode(properties);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VertexSpec other) {
    int cmp = 0;
    cmp = ((Comparable) label).compareTo(other.label);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) id).compareTo(other.id);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) properties).compareTo(other.properties);
  }

  public VertexSpec withLabel(hydra.pg.model.VertexLabel label) {
    return new VertexSpec(label, id, properties);
  }

  public VertexSpec withId(hydra.pg.mapping.ValueSpec id) {
    return new VertexSpec(label, id, properties);
  }

  public VertexSpec withProperties(hydra.util.ConsList<hydra.pg.mapping.PropertySpec> properties) {
    return new VertexSpec(label, id, properties);
  }
}
