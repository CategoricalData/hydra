// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.features;

import java.io.Serializable;

/**
 * Features that are related to Edge operations.
 */
public class EdgeFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/features.EdgeFeatures");
  
  public static final hydra.core.Name FIELD_NAME_ELEMENT_FEATURES = new hydra.core.Name("elementFeatures");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_ADD_EDGES = new hydra.core.Name("supportsAddEdges");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_REMOVE_EDGES = new hydra.core.Name("supportsRemoveEdges");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_UPSERT = new hydra.core.Name("supportsUpsert");
  
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
    java.util.Objects.requireNonNull((elementFeatures));
    java.util.Objects.requireNonNull((properties));
    java.util.Objects.requireNonNull((supportsAddEdges));
    java.util.Objects.requireNonNull((supportsRemoveEdges));
    java.util.Objects.requireNonNull((supportsUpsert));
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
    java.util.Objects.requireNonNull((elementFeatures));
    return new EdgeFeatures(elementFeatures, properties, supportsAddEdges, supportsRemoveEdges, supportsUpsert);
  }
  
  public EdgeFeatures withProperties(hydra.langs.tinkerpop.features.EdgePropertyFeatures properties) {
    java.util.Objects.requireNonNull((properties));
    return new EdgeFeatures(elementFeatures, properties, supportsAddEdges, supportsRemoveEdges, supportsUpsert);
  }
  
  public EdgeFeatures withSupportsAddEdges(Boolean supportsAddEdges) {
    java.util.Objects.requireNonNull((supportsAddEdges));
    return new EdgeFeatures(elementFeatures, properties, supportsAddEdges, supportsRemoveEdges, supportsUpsert);
  }
  
  public EdgeFeatures withSupportsRemoveEdges(Boolean supportsRemoveEdges) {
    java.util.Objects.requireNonNull((supportsRemoveEdges));
    return new EdgeFeatures(elementFeatures, properties, supportsAddEdges, supportsRemoveEdges, supportsUpsert);
  }
  
  public EdgeFeatures withSupportsUpsert(Boolean supportsUpsert) {
    java.util.Objects.requireNonNull((supportsUpsert));
    return new EdgeFeatures(elementFeatures, properties, supportsAddEdges, supportsRemoveEdges, supportsUpsert);
  }
}