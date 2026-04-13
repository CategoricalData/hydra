// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * A vertex
 */
public class Vertex<V> implements Serializable, Comparable<Vertex<V>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.Vertex");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");

  /**
   * The label of the vertex
   */
  public final hydra.pg.model.VertexLabel label;

  /**
   * The unique identifier of the vertex
   */
  public final V id;

  /**
   * A key/value map of vertex properties
   */
  public final java.util.Map<hydra.pg.model.PropertyKey, V> properties;

  public Vertex (hydra.pg.model.VertexLabel label, V id, java.util.Map<hydra.pg.model.PropertyKey, V> properties) {
    this.label = label;
    this.id = id;
    this.properties = properties;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Vertex)) {
      return false;
    }
    Vertex o = (Vertex) other;
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
  public int compareTo(Vertex other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      label,
      other.label);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      id,
      other.id);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      properties,
      other.properties);
  }

  public Vertex withLabel(hydra.pg.model.VertexLabel label) {
    return new Vertex(label, id, properties);
  }

  public Vertex withId(V id) {
    return new Vertex(label, id, properties);
  }

  public Vertex withProperties(java.util.Map<hydra.pg.model.PropertyKey, V> properties) {
    return new Vertex(label, id, properties);
  }
}
