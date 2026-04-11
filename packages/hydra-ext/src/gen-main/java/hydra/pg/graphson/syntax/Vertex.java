// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public class Vertex implements Serializable, Comparable<Vertex> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.graphson.syntax.Vertex");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  public static final hydra.core.Name IN_EDGES = new hydra.core.Name("inEdges");

  public static final hydra.core.Name OUT_EDGES = new hydra.core.Name("outEdges");

  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");

  public final hydra.pg.graphson.syntax.Value id;

  public final hydra.util.Maybe<hydra.pg.graphson.syntax.VertexLabel> label;

  public final java.util.Map<hydra.pg.graphson.syntax.EdgeLabel, java.util.List<hydra.pg.graphson.syntax.AdjacentEdge>> inEdges;

  public final java.util.Map<hydra.pg.graphson.syntax.EdgeLabel, java.util.List<hydra.pg.graphson.syntax.AdjacentEdge>> outEdges;

  public final java.util.Map<hydra.pg.graphson.syntax.PropertyKey, java.util.List<hydra.pg.graphson.syntax.VertexPropertyValue>> properties;

  public Vertex (hydra.pg.graphson.syntax.Value id, hydra.util.Maybe<hydra.pg.graphson.syntax.VertexLabel> label, java.util.Map<hydra.pg.graphson.syntax.EdgeLabel, java.util.List<hydra.pg.graphson.syntax.AdjacentEdge>> inEdges, java.util.Map<hydra.pg.graphson.syntax.EdgeLabel, java.util.List<hydra.pg.graphson.syntax.AdjacentEdge>> outEdges, java.util.Map<hydra.pg.graphson.syntax.PropertyKey, java.util.List<hydra.pg.graphson.syntax.VertexPropertyValue>> properties) {
    this.id = id;
    this.label = label;
    this.inEdges = inEdges;
    this.outEdges = outEdges;
    this.properties = properties;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Vertex)) {
      return false;
    }
    Vertex o = (Vertex) other;
    return java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.label,
      o.label) && java.util.Objects.equals(
      this.inEdges,
      o.inEdges) && java.util.Objects.equals(
      this.outEdges,
      o.outEdges) && java.util.Objects.equals(
      this.properties,
      o.properties);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(id) + 3 * java.util.Objects.hashCode(label) + 5 * java.util.Objects.hashCode(inEdges) + 7 * java.util.Objects.hashCode(outEdges) + 11 * java.util.Objects.hashCode(properties);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Vertex other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      id,
      other.id);
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
      inEdges,
      other.inEdges);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      outEdges,
      other.outEdges);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      properties,
      other.properties);
  }

  public Vertex withId(hydra.pg.graphson.syntax.Value id) {
    return new Vertex(id, label, inEdges, outEdges, properties);
  }

  public Vertex withLabel(hydra.util.Maybe<hydra.pg.graphson.syntax.VertexLabel> label) {
    return new Vertex(id, label, inEdges, outEdges, properties);
  }

  public Vertex withInEdges(java.util.Map<hydra.pg.graphson.syntax.EdgeLabel, java.util.List<hydra.pg.graphson.syntax.AdjacentEdge>> inEdges) {
    return new Vertex(id, label, inEdges, outEdges, properties);
  }

  public Vertex withOutEdges(java.util.Map<hydra.pg.graphson.syntax.EdgeLabel, java.util.List<hydra.pg.graphson.syntax.AdjacentEdge>> outEdges) {
    return new Vertex(id, label, inEdges, outEdges, properties);
  }

  public Vertex withProperties(java.util.Map<hydra.pg.graphson.syntax.PropertyKey, java.util.List<hydra.pg.graphson.syntax.VertexPropertyValue>> properties) {
    return new Vertex(id, label, inEdges, outEdges, properties);
  }
}
