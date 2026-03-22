// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * The type of an edge
 */
public class EdgeType<T> implements Serializable, Comparable<EdgeType<T>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.EdgeType");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name OUT = new hydra.core.Name("out");

  public static final hydra.core.Name IN = new hydra.core.Name("in");

  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");

  /**
   * The label of any edge of this edge type
   */
  public final hydra.pg.model.EdgeLabel label;

  /**
   * The type of the id of any edge of this edge type
   */
  public final T id;

  /**
   * The label of the out-vertex (tail) of any edge of this edge type
   */
  public final hydra.pg.model.VertexLabel out;

  /**
   * The label of the in-vertex (head) of any edge of this edge type
   */
  public final hydra.pg.model.VertexLabel in;

  /**
   * A list of property types. The types are ordered for the sake of applications in which property order is significant.
   */
  public final hydra.util.ConsList<hydra.pg.model.PropertyType<T>> properties;

  public EdgeType (hydra.pg.model.EdgeLabel label, T id, hydra.pg.model.VertexLabel out, hydra.pg.model.VertexLabel in, hydra.util.ConsList<hydra.pg.model.PropertyType<T>> properties) {
    this.label = label;
    this.id = id;
    this.out = out;
    this.in = in;
    this.properties = properties;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeType)) {
      return false;
    }
    EdgeType o = (EdgeType) other;
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
  public int compareTo(EdgeType other) {
    int cmp = 0;
    cmp = ((Comparable) label).compareTo(other.label);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) id).compareTo(other.id);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) out).compareTo(other.out);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) in).compareTo(other.in);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) properties).compareTo(other.properties);
  }

  public EdgeType withLabel(hydra.pg.model.EdgeLabel label) {
    return new EdgeType(label, id, out, in, properties);
  }

  public EdgeType withId(T id) {
    return new EdgeType(label, id, out, in, properties);
  }

  public EdgeType withOut(hydra.pg.model.VertexLabel out) {
    return new EdgeType(label, id, out, in, properties);
  }

  public EdgeType withIn(hydra.pg.model.VertexLabel in) {
    return new EdgeType(label, id, out, in, properties);
  }

  public EdgeType withProperties(hydra.util.ConsList<hydra.pg.model.PropertyType<T>> properties) {
    return new EdgeType(label, id, out, in, properties);
  }
}
