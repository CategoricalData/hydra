// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * An edge
 */
public class Edge<V> implements Serializable, Comparable<Edge<V>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.Edge");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name OUT = new hydra.core.Name("out");

  public static final hydra.core.Name IN = new hydra.core.Name("in");

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
   * The id of the out-vertex (tail) of the edge
   */
  public final V out;

  /**
   * The id of the in-vertex (head) of the edge
   */
  public final V in;

  /**
   * A key/value map of edge properties
   */
  public final java.util.Map<hydra.pg.model.PropertyKey, V> properties;

  public Edge (hydra.pg.model.EdgeLabel label, V id, V out, V in, java.util.Map<hydra.pg.model.PropertyKey, V> properties) {
    this.label = label;
    this.id = id;
    this.out = out;
    this.in = in;
    this.properties = properties;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Edge)) {
      return false;
    }
    Edge o = (Edge) other;
    return java.util.Objects.equals(
      this.label,
      o.label) && java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.out,
      o.out) && java.util.Objects.equals(
      this.in,
      o.in) && java.util.Objects.equals(
      this.properties,
      o.properties);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(label) + 3 * java.util.Objects.hashCode(id) + 5 * java.util.Objects.hashCode(out) + 7 * java.util.Objects.hashCode(in) + 11 * java.util.Objects.hashCode(properties);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Edge other) {
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
      out,
      other.out);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      in,
      other.in);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      properties,
      other.properties);
  }

  public Edge withLabel(hydra.pg.model.EdgeLabel label) {
    return new Edge(label, id, out, in, properties);
  }

  public Edge withId(V id) {
    return new Edge(label, id, out, in, properties);
  }

  public Edge withOut(V out) {
    return new Edge(label, id, out, in, properties);
  }

  public Edge withIn(V in) {
    return new Edge(label, id, out, in, properties);
  }

  public Edge withProperties(java.util.Map<hydra.pg.model.PropertyKey, V> properties) {
    return new Edge(label, id, out, in, properties);
  }
}
