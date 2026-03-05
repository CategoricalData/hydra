// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.features;

import java.io.Serializable;

/**
 * Features that are related to Vertex operations.
 */
public class VertexFeatures implements Serializable, Comparable<VertexFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.features.VertexFeatures");
  
  public static final hydra.core.Name ELEMENT_FEATURES = new hydra.core.Name("elementFeatures");
  
  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");
  
  public static final hydra.core.Name SUPPORTS_ADD_VERTICES = new hydra.core.Name("supportsAddVertices");
  
  public static final hydra.core.Name SUPPORTS_DUPLICATE_MULTI_PROPERTIES = new hydra.core.Name("supportsDuplicateMultiProperties");
  
  public static final hydra.core.Name SUPPORTS_META_PROPERTIES = new hydra.core.Name("supportsMetaProperties");
  
  public static final hydra.core.Name SUPPORTS_MULTI_PROPERTIES = new hydra.core.Name("supportsMultiProperties");
  
  public static final hydra.core.Name SUPPORTS_REMOVE_VERTICES = new hydra.core.Name("supportsRemoveVertices");
  
  public static final hydra.core.Name SUPPORTS_UPSERT = new hydra.core.Name("supportsUpsert");
  
  public final hydra.ext.org.apache.tinkerpop.features.ElementFeatures elementFeatures;
  
  public final hydra.ext.org.apache.tinkerpop.features.VertexPropertyFeatures properties;
  
  /**
   * Determines if a Vertex can be added to the Graph.
   */
  public final Boolean supportsAddVertices;
  
  /**
   * Determines if a Vertex can support non-unique values on the same key.
   */
  public final Boolean supportsDuplicateMultiProperties;
  
  /**
   * Determines if a Vertex can support properties on vertex properties.
   */
  public final Boolean supportsMetaProperties;
  
  /**
   * Determines if a Vertex can support multiple properties with the same key.
   */
  public final Boolean supportsMultiProperties;
  
  /**
   * Determines if a Vertex can be removed from the Graph.
   */
  public final Boolean supportsRemoveVertices;
  
  /**
   * Determines if the Graph implementation uses upsert functionality as opposed to insert functionality for Graph.addVertex(String).
   */
  public final Boolean supportsUpsert;
  
  public VertexFeatures (hydra.ext.org.apache.tinkerpop.features.ElementFeatures elementFeatures, hydra.ext.org.apache.tinkerpop.features.VertexPropertyFeatures properties, Boolean supportsAddVertices, Boolean supportsDuplicateMultiProperties, Boolean supportsMetaProperties, Boolean supportsMultiProperties, Boolean supportsRemoveVertices, Boolean supportsUpsert) {
    this.elementFeatures = elementFeatures;
    this.properties = properties;
    this.supportsAddVertices = supportsAddVertices;
    this.supportsDuplicateMultiProperties = supportsDuplicateMultiProperties;
    this.supportsMetaProperties = supportsMetaProperties;
    this.supportsMultiProperties = supportsMultiProperties;
    this.supportsRemoveVertices = supportsRemoveVertices;
    this.supportsUpsert = supportsUpsert;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexFeatures)) {
      return false;
    }
    VertexFeatures o = (VertexFeatures) other;
    return java.util.Objects.equals(
      this.elementFeatures,
      o.elementFeatures) && java.util.Objects.equals(
      this.properties,
      o.properties) && java.util.Objects.equals(
      this.supportsAddVertices,
      o.supportsAddVertices) && java.util.Objects.equals(
      this.supportsDuplicateMultiProperties,
      o.supportsDuplicateMultiProperties) && java.util.Objects.equals(
      this.supportsMetaProperties,
      o.supportsMetaProperties) && java.util.Objects.equals(
      this.supportsMultiProperties,
      o.supportsMultiProperties) && java.util.Objects.equals(
      this.supportsRemoveVertices,
      o.supportsRemoveVertices) && java.util.Objects.equals(
      this.supportsUpsert,
      o.supportsUpsert);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(elementFeatures) + 3 * java.util.Objects.hashCode(properties) + 5 * java.util.Objects.hashCode(supportsAddVertices) + 7 * java.util.Objects.hashCode(supportsDuplicateMultiProperties) + 11 * java.util.Objects.hashCode(supportsMetaProperties) + 13 * java.util.Objects.hashCode(supportsMultiProperties) + 17 * java.util.Objects.hashCode(supportsRemoveVertices) + 19 * java.util.Objects.hashCode(supportsUpsert);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VertexFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) elementFeatures).compareTo(other.elementFeatures);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) properties).compareTo(other.properties);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) supportsAddVertices).compareTo(other.supportsAddVertices);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) supportsDuplicateMultiProperties).compareTo(other.supportsDuplicateMultiProperties);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) supportsMetaProperties).compareTo(other.supportsMetaProperties);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) supportsMultiProperties).compareTo(other.supportsMultiProperties);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) supportsRemoveVertices).compareTo(other.supportsRemoveVertices);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) supportsUpsert).compareTo(other.supportsUpsert);
  }
  
  public VertexFeatures withElementFeatures(hydra.ext.org.apache.tinkerpop.features.ElementFeatures elementFeatures) {
    return new VertexFeatures(elementFeatures, properties, supportsAddVertices, supportsDuplicateMultiProperties, supportsMetaProperties, supportsMultiProperties, supportsRemoveVertices, supportsUpsert);
  }
  
  public VertexFeatures withProperties(hydra.ext.org.apache.tinkerpop.features.VertexPropertyFeatures properties) {
    return new VertexFeatures(elementFeatures, properties, supportsAddVertices, supportsDuplicateMultiProperties, supportsMetaProperties, supportsMultiProperties, supportsRemoveVertices, supportsUpsert);
  }
  
  public VertexFeatures withSupportsAddVertices(Boolean supportsAddVertices) {
    return new VertexFeatures(elementFeatures, properties, supportsAddVertices, supportsDuplicateMultiProperties, supportsMetaProperties, supportsMultiProperties, supportsRemoveVertices, supportsUpsert);
  }
  
  public VertexFeatures withSupportsDuplicateMultiProperties(Boolean supportsDuplicateMultiProperties) {
    return new VertexFeatures(elementFeatures, properties, supportsAddVertices, supportsDuplicateMultiProperties, supportsMetaProperties, supportsMultiProperties, supportsRemoveVertices, supportsUpsert);
  }
  
  public VertexFeatures withSupportsMetaProperties(Boolean supportsMetaProperties) {
    return new VertexFeatures(elementFeatures, properties, supportsAddVertices, supportsDuplicateMultiProperties, supportsMetaProperties, supportsMultiProperties, supportsRemoveVertices, supportsUpsert);
  }
  
  public VertexFeatures withSupportsMultiProperties(Boolean supportsMultiProperties) {
    return new VertexFeatures(elementFeatures, properties, supportsAddVertices, supportsDuplicateMultiProperties, supportsMetaProperties, supportsMultiProperties, supportsRemoveVertices, supportsUpsert);
  }
  
  public VertexFeatures withSupportsRemoveVertices(Boolean supportsRemoveVertices) {
    return new VertexFeatures(elementFeatures, properties, supportsAddVertices, supportsDuplicateMultiProperties, supportsMetaProperties, supportsMultiProperties, supportsRemoveVertices, supportsUpsert);
  }
  
  public VertexFeatures withSupportsUpsert(Boolean supportsUpsert) {
    return new VertexFeatures(elementFeatures, properties, supportsAddVertices, supportsDuplicateMultiProperties, supportsMetaProperties, supportsMultiProperties, supportsRemoveVertices, supportsUpsert);
  }
}
