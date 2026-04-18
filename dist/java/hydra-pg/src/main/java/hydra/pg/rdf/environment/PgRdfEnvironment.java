// Note: this is an automatically generated file. Do not edit.

package hydra.pg.rdf.environment;

import java.io.Serializable;

/**
 * The environment for property graph to RDF mapping
 */
public class PgRdfEnvironment<V> implements Serializable, Comparable<PgRdfEnvironment<V>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.rdf.environment.PgRdfEnvironment");

  public static final hydra.core.Name ENCODE_VERTEX_ID = new hydra.core.Name("encodeVertexId");

  public static final hydra.core.Name ENCODE_VERTEX_LABEL = new hydra.core.Name("encodeVertexLabel");

  public static final hydra.core.Name ENCODE_EDGE_ID = new hydra.core.Name("encodeEdgeId");

  public static final hydra.core.Name ENCODE_EDGE_LABEL = new hydra.core.Name("encodeEdgeLabel");

  public static final hydra.core.Name ENCODE_PROPERTY_KEY = new hydra.core.Name("encodePropertyKey");

  public static final hydra.core.Name ENCODE_PROPERTY_VALUE = new hydra.core.Name("encodePropertyValue");

  /**
   * A function which encodes a vertex id as an RDF IRI
   */
  public final java.util.function.Function<V, hydra.rdf.syntax.Iri> encodeVertexId;

  /**
   * A function which encodes a vertex label as an RDF IRI
   */
  public final java.util.function.Function<hydra.pg.model.VertexLabel, hydra.rdf.syntax.Iri> encodeVertexLabel;

  /**
   * A function which encodes an edge id as an RDF IRI
   */
  public final java.util.function.Function<V, hydra.rdf.syntax.Iri> encodeEdgeId;

  /**
   * A function which encodes an edge label as an RDF IRI
   */
  public final java.util.function.Function<hydra.pg.model.EdgeLabel, hydra.rdf.syntax.Iri> encodeEdgeLabel;

  /**
   * A function which encodes a property key as an RDF IRI
   */
  public final java.util.function.Function<hydra.pg.model.PropertyKey, hydra.rdf.syntax.Iri> encodePropertyKey;

  /**
   * A function which encodes a property value as an RDF literal
   */
  public final java.util.function.Function<V, hydra.rdf.syntax.Literal> encodePropertyValue;

  public PgRdfEnvironment (java.util.function.Function<V, hydra.rdf.syntax.Iri> encodeVertexId, java.util.function.Function<hydra.pg.model.VertexLabel, hydra.rdf.syntax.Iri> encodeVertexLabel, java.util.function.Function<V, hydra.rdf.syntax.Iri> encodeEdgeId, java.util.function.Function<hydra.pg.model.EdgeLabel, hydra.rdf.syntax.Iri> encodeEdgeLabel, java.util.function.Function<hydra.pg.model.PropertyKey, hydra.rdf.syntax.Iri> encodePropertyKey, java.util.function.Function<V, hydra.rdf.syntax.Literal> encodePropertyValue) {
    this.encodeVertexId = encodeVertexId;
    this.encodeVertexLabel = encodeVertexLabel;
    this.encodeEdgeId = encodeEdgeId;
    this.encodeEdgeLabel = encodeEdgeLabel;
    this.encodePropertyKey = encodePropertyKey;
    this.encodePropertyValue = encodePropertyValue;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PgRdfEnvironment)) {
      return false;
    }
    PgRdfEnvironment o = (PgRdfEnvironment) other;
    return java.util.Objects.equals(
      this.encodeVertexId,
      o.encodeVertexId) && java.util.Objects.equals(
      this.encodeVertexLabel,
      o.encodeVertexLabel) && java.util.Objects.equals(
      this.encodeEdgeId,
      o.encodeEdgeId) && java.util.Objects.equals(
      this.encodeEdgeLabel,
      o.encodeEdgeLabel) && java.util.Objects.equals(
      this.encodePropertyKey,
      o.encodePropertyKey) && java.util.Objects.equals(
      this.encodePropertyValue,
      o.encodePropertyValue);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(encodeVertexId) + 3 * java.util.Objects.hashCode(encodeVertexLabel) + 5 * java.util.Objects.hashCode(encodeEdgeId) + 7 * java.util.Objects.hashCode(encodeEdgeLabel) + 11 * java.util.Objects.hashCode(encodePropertyKey) + 13 * java.util.Objects.hashCode(encodePropertyValue);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PgRdfEnvironment other) {
    int cmp = 0;
    cmp = Integer.compare(
      encodeVertexId.hashCode(),
      other.encodeVertexId.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      encodeVertexLabel.hashCode(),
      other.encodeVertexLabel.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      encodeEdgeId.hashCode(),
      other.encodeEdgeId.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      encodeEdgeLabel.hashCode(),
      other.encodeEdgeLabel.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      encodePropertyKey.hashCode(),
      other.encodePropertyKey.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      encodePropertyValue.hashCode(),
      other.encodePropertyValue.hashCode());
  }

  public PgRdfEnvironment withEncodeVertexId(java.util.function.Function<V, hydra.rdf.syntax.Iri> encodeVertexId) {
    return new PgRdfEnvironment(encodeVertexId, encodeVertexLabel, encodeEdgeId, encodeEdgeLabel, encodePropertyKey, encodePropertyValue);
  }

  public PgRdfEnvironment withEncodeVertexLabel(java.util.function.Function<hydra.pg.model.VertexLabel, hydra.rdf.syntax.Iri> encodeVertexLabel) {
    return new PgRdfEnvironment(encodeVertexId, encodeVertexLabel, encodeEdgeId, encodeEdgeLabel, encodePropertyKey, encodePropertyValue);
  }

  public PgRdfEnvironment withEncodeEdgeId(java.util.function.Function<V, hydra.rdf.syntax.Iri> encodeEdgeId) {
    return new PgRdfEnvironment(encodeVertexId, encodeVertexLabel, encodeEdgeId, encodeEdgeLabel, encodePropertyKey, encodePropertyValue);
  }

  public PgRdfEnvironment withEncodeEdgeLabel(java.util.function.Function<hydra.pg.model.EdgeLabel, hydra.rdf.syntax.Iri> encodeEdgeLabel) {
    return new PgRdfEnvironment(encodeVertexId, encodeVertexLabel, encodeEdgeId, encodeEdgeLabel, encodePropertyKey, encodePropertyValue);
  }

  public PgRdfEnvironment withEncodePropertyKey(java.util.function.Function<hydra.pg.model.PropertyKey, hydra.rdf.syntax.Iri> encodePropertyKey) {
    return new PgRdfEnvironment(encodeVertexId, encodeVertexLabel, encodeEdgeId, encodeEdgeLabel, encodePropertyKey, encodePropertyValue);
  }

  public PgRdfEnvironment withEncodePropertyValue(java.util.function.Function<V, hydra.rdf.syntax.Literal> encodePropertyValue) {
    return new PgRdfEnvironment(encodeVertexId, encodeVertexLabel, encodeEdgeId, encodeEdgeLabel, encodePropertyKey, encodePropertyValue);
  }
}
