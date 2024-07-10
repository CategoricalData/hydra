// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.features;

import java.io.Serializable;

/**
 * Features that are related to Edge operations.
 */
public class EdgeFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/features.EdgeFeatures");
  
  public final hydra.langs.tinkerpop.features.ElementFeatures elementFeatures;
  
  public final hydra.langs.tinkerpop.features.EdgePropertyFeatures properties;
  
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
  
  public EdgeFeatures (hydra.langs.tinkerpop.features.ElementFeatures elementFeatures, hydra.langs.tinkerpop.features.EdgePropertyFeatures properties, Boolean supportsAddEdges, Boolean supportsRemoveEdges, Boolean supportsUpsert) {
    if (elementFeatures == null) {
      throw new IllegalArgumentException("null value for 'elementFeatures' argument");
    }
    if (properties == null) {
      throw new IllegalArgumentException("null value for 'properties' argument");
    }
    if (supportsAddEdges == null) {
      throw new IllegalArgumentException("null value for 'supportsAddEdges' argument");
    }
    if (supportsRemoveEdges == null) {
      throw new IllegalArgumentException("null value for 'supportsRemoveEdges' argument");
    }
    if (supportsUpsert == null) {
      throw new IllegalArgumentException("null value for 'supportsUpsert' argument");
    }
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
    EdgeFeatures o = (EdgeFeatures) (other);
    return elementFeatures.equals(o.elementFeatures) && properties.equals(o.properties) && supportsAddEdges.equals(o.supportsAddEdges) && supportsRemoveEdges.equals(o.supportsRemoveEdges) && supportsUpsert.equals(o.supportsUpsert);
  }
  
  @Override
  public int hashCode() {
    return 2 * elementFeatures.hashCode() + 3 * properties.hashCode() + 5 * supportsAddEdges.hashCode() + 7 * supportsRemoveEdges.hashCode() + 11 * supportsUpsert.hashCode();
  }
  
  public EdgeFeatures withElementFeatures(hydra.langs.tinkerpop.features.ElementFeatures elementFeatures) {
    if (elementFeatures == null) {
      throw new IllegalArgumentException("null value for 'elementFeatures' argument");
    }
    return new EdgeFeatures(elementFeatures, properties, supportsAddEdges, supportsRemoveEdges, supportsUpsert);
  }
  
  public EdgeFeatures withProperties(hydra.langs.tinkerpop.features.EdgePropertyFeatures properties) {
    if (properties == null) {
      throw new IllegalArgumentException("null value for 'properties' argument");
    }
    return new EdgeFeatures(elementFeatures, properties, supportsAddEdges, supportsRemoveEdges, supportsUpsert);
  }
  
  public EdgeFeatures withSupportsAddEdges(Boolean supportsAddEdges) {
    if (supportsAddEdges == null) {
      throw new IllegalArgumentException("null value for 'supportsAddEdges' argument");
    }
    return new EdgeFeatures(elementFeatures, properties, supportsAddEdges, supportsRemoveEdges, supportsUpsert);
  }
  
  public EdgeFeatures withSupportsRemoveEdges(Boolean supportsRemoveEdges) {
    if (supportsRemoveEdges == null) {
      throw new IllegalArgumentException("null value for 'supportsRemoveEdges' argument");
    }
    return new EdgeFeatures(elementFeatures, properties, supportsAddEdges, supportsRemoveEdges, supportsUpsert);
  }
  
  public EdgeFeatures withSupportsUpsert(Boolean supportsUpsert) {
    if (supportsUpsert == null) {
      throw new IllegalArgumentException("null value for 'supportsUpsert' argument");
    }
    return new EdgeFeatures(elementFeatures, properties, supportsAddEdges, supportsRemoveEdges, supportsUpsert);
  }
}