// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class EdgeProjectionPattern implements Serializable, Comparable<EdgeProjectionPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.query.EdgeProjectionPattern");

  public static final hydra.core.Name DIRECTION = new hydra.core.Name("direction");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");

  public static final hydra.core.Name VERTEX = new hydra.core.Name("vertex");

  public final hydra.pg.model.Direction direction;

  public final hydra.util.Maybe<hydra.pg.model.EdgeLabel> label;

  public final java.util.List<hydra.pg.query.PropertyPattern> properties;

  public final hydra.util.Maybe<hydra.pg.query.VertexPattern> vertex;

  public EdgeProjectionPattern (hydra.pg.model.Direction direction, hydra.util.Maybe<hydra.pg.model.EdgeLabel> label, java.util.List<hydra.pg.query.PropertyPattern> properties, hydra.util.Maybe<hydra.pg.query.VertexPattern> vertex) {
    this.direction = direction;
    this.label = label;
    this.properties = properties;
    this.vertex = vertex;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeProjectionPattern)) {
      return false;
    }
    EdgeProjectionPattern o = (EdgeProjectionPattern) other;
    return java.util.Objects.equals(
      this.direction,
      o.direction) && java.util.Objects.equals(
      this.label,
      o.label) && java.util.Objects.equals(
      this.properties,
      o.properties) && java.util.Objects.equals(
      this.vertex,
      o.vertex);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(direction) + 3 * java.util.Objects.hashCode(label) + 5 * java.util.Objects.hashCode(properties) + 7 * java.util.Objects.hashCode(vertex);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EdgeProjectionPattern other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      direction,
      other.direction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      label,
      other.label);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      properties,
      other.properties);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      vertex,
      other.vertex);
  }

  public EdgeProjectionPattern withDirection(hydra.pg.model.Direction direction) {
    return new EdgeProjectionPattern(direction, label, properties, vertex);
  }

  public EdgeProjectionPattern withLabel(hydra.util.Maybe<hydra.pg.model.EdgeLabel> label) {
    return new EdgeProjectionPattern(direction, label, properties, vertex);
  }

  public EdgeProjectionPattern withProperties(java.util.List<hydra.pg.query.PropertyPattern> properties) {
    return new EdgeProjectionPattern(direction, label, properties, vertex);
  }

  public EdgeProjectionPattern withVertex(hydra.util.Maybe<hydra.pg.query.VertexPattern> vertex) {
    return new EdgeProjectionPattern(direction, label, properties, vertex);
  }
}
