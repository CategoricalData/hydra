// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * An edge which is adjacent to a given vertex. Only the other endpoint of the edge is provided.
 */
public class AdjacentEdge<V> implements Serializable, Comparable<AdjacentEdge<V>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.AdjacentEdge");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name VERTEX = new hydra.core.Name("vertex");

  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");

  /**
   * The label of the edge
   */
  public final hydra.pg.model.EdgeLabel label;

  /**
   * The unique identifier of the edge
   */
  public final V id;

  /**
   * The id of the other vertex adjacent to the edge
   */
  public final V vertex;

  /**
   * A key/value map of edge properties
   */
  public final java.util.Map<hydra.pg.model.PropertyKey, V> properties;

  public AdjacentEdge (hydra.pg.model.EdgeLabel label, V id, V vertex, java.util.Map<hydra.pg.model.PropertyKey, V> properties) {
    this.label = label;
    this.id = id;
    this.vertex = vertex;
    this.properties = properties;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AdjacentEdge)) {
      return false;
    }
    AdjacentEdge o = (AdjacentEdge) other;
    return java.util.Objects.equals(
      this.label,
      o.label) && java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.vertex,
      o.vertex) && java.util.Objects.equals(
      this.properties,
      o.properties);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(label) + 3 * java.util.Objects.hashCode(id) + 5 * java.util.Objects.hashCode(vertex) + 7 * java.util.Objects.hashCode(properties);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AdjacentEdge other) {
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
    cmp = hydra.util.Comparing.compare(
      vertex,
      other.vertex);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      properties,
      other.properties);
  }

  public AdjacentEdge withLabel(hydra.pg.model.EdgeLabel label) {
    return new AdjacentEdge(label, id, vertex, properties);
  }

  public AdjacentEdge withId(V id) {
    return new AdjacentEdge(label, id, vertex, properties);
  }

  public AdjacentEdge withVertex(V vertex) {
    return new AdjacentEdge(label, id, vertex, properties);
  }

  public AdjacentEdge withProperties(java.util.Map<hydra.pg.model.PropertyKey, V> properties) {
    return new AdjacentEdge(label, id, vertex, properties);
  }
}
