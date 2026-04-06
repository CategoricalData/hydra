// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.features;

import java.io.Serializable;

/**
 * Features that are related to Edge operations.
 */
public class EdgeFeatures implements Serializable, Comparable<EdgeFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.features.EdgeFeatures");

  public static final hydra.core.Name ELEMENT_FEATURES = new hydra.core.Name("elementFeatures");

  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");

  public static final hydra.core.Name SUPPORTS_ADD_EDGES = new hydra.core.Name("supportsAddEdges");

  public static final hydra.core.Name SUPPORTS_REMOVE_EDGES = new hydra.core.Name("supportsRemoveEdges");

  public static final hydra.core.Name SUPPORTS_UPSERT = new hydra.core.Name("supportsUpsert");

  public final hydra.ext.org.apache.tinkerpop.features.ElementFeatures elementFeatures;

  public final hydra.ext.org.apache.tinkerpop.features.EdgePropertyFeatures properties;

  /**
   * Determines if an Edge can be added to a Vertex.
   */
  public final Boolean supportsAddEdges;

  /**
   * Determines if an Edge can be removed from a Vertex.
   */
  public final Boolean supportsRemoveEdges;

  /**
   * Determines if the Graph implementation uses upsert functionality as opposed to insert functionality for Vertex.addEdge(String, Vertex, Object...).
   */
  public final Boolean supportsUpsert;

  public EdgeFeatures (hydra.ext.org.apache.tinkerpop.features.ElementFeatures elementFeatures, hydra.ext.org.apache.tinkerpop.features.EdgePropertyFeatures properties, Boolean supportsAddEdges, Boolean supportsRemoveEdges, Boolean supportsUpsert) {
    this.elementFeatures = elementFeatures;
    this.properties = properties;
    this.supportsAddEdges = supportsAddEdges;
    this.supportsRemoveEdges = supportsRemoveEdges;
    this.supportsUpsert = supportsUpsert;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeFeatures)) {
      return false;
    }
    EdgeFeatures o = (EdgeFeatures) other;
    return java.util.Objects.equals(
      this.elementFeatures,
      o.elementFeatures) && java.util.Objects.equals(
      this.properties,
      o.properties) && java.util.Objects.equals(
      this.supportsAddEdges,
      o.supportsAddEdges) && java.util.Objects.equals(
      this.supportsRemoveEdges,
      o.supportsRemoveEdges) && java.util.Objects.equals(
      this.supportsUpsert,
      o.supportsUpsert);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(elementFeatures) + 3 * java.util.Objects.hashCode(properties) + 5 * java.util.Objects.hashCode(supportsAddEdges) + 7 * java.util.Objects.hashCode(supportsRemoveEdges) + 11 * java.util.Objects.hashCode(supportsUpsert);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EdgeFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      elementFeatures,
      other.elementFeatures);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      properties,
      other.properties);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsAddEdges,
      other.supportsAddEdges);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      supportsRemoveEdges,
      other.supportsRemoveEdges);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      supportsUpsert,
      other.supportsUpsert);
  }

  public EdgeFeatures withElementFeatures(hydra.ext.org.apache.tinkerpop.features.ElementFeatures elementFeatures) {
    return new EdgeFeatures(elementFeatures, properties, supportsAddEdges, supportsRemoveEdges, supportsUpsert);
  }

  public EdgeFeatures withProperties(hydra.ext.org.apache.tinkerpop.features.EdgePropertyFeatures properties) {
    return new EdgeFeatures(elementFeatures, properties, supportsAddEdges, supportsRemoveEdges, supportsUpsert);
  }

  public EdgeFeatures withSupportsAddEdges(Boolean supportsAddEdges) {
    return new EdgeFeatures(elementFeatures, properties, supportsAddEdges, supportsRemoveEdges, supportsUpsert);
  }

  public EdgeFeatures withSupportsRemoveEdges(Boolean supportsRemoveEdges) {
    return new EdgeFeatures(elementFeatures, properties, supportsAddEdges, supportsRemoveEdges, supportsUpsert);
  }

  public EdgeFeatures withSupportsUpsert(Boolean supportsUpsert) {
    return new EdgeFeatures(elementFeatures, properties, supportsAddEdges, supportsRemoveEdges, supportsUpsert);
  }
}
