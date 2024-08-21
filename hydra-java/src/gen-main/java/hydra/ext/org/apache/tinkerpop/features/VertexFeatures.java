// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.features;

import java.io.Serializable;

/**
 * Features that are related to Vertex operations.
 */
public class VertexFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/features.VertexFeatures");
  
  public static final hydra.core.Name FIELD_NAME_ELEMENT_FEATURES = new hydra.core.Name("elementFeatures");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_ADD_VERTICES = new hydra.core.Name("supportsAddVertices");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_DUPLICATE_MULTI_PROPERTIES = new hydra.core.Name("supportsDuplicateMultiProperties");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_META_PROPERTIES = new hydra.core.Name("supportsMetaProperties");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_MULTI_PROPERTIES = new hydra.core.Name("supportsMultiProperties");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_REMOVE_VERTICES = new hydra.core.Name("supportsRemoveVertices");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_UPSERT = new hydra.core.Name("supportsUpsert");
  
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
    java.util.Objects.requireNonNull((elementFeatures));
    java.util.Objects.requireNonNull((properties));
    java.util.Objects.requireNonNull((supportsAddVertices));
    java.util.Objects.requireNonNull((supportsDuplicateMultiProperties));
    java.util.Objects.requireNonNull((supportsMetaProperties));
    java.util.Objects.requireNonNull((supportsMultiProperties));
    java.util.Objects.requireNonNull((supportsRemoveVertices));
    java.util.Objects.requireNonNull((supportsUpsert));
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
    VertexFeatures o = (VertexFeatures) (other);
    return elementFeatures.equals(o.elementFeatures) && properties.equals(o.properties) && supportsAddVertices.equals(o.supportsAddVertices) && supportsDuplicateMultiProperties.equals(o.supportsDuplicateMultiProperties) && supportsMetaProperties.equals(o.supportsMetaProperties) && supportsMultiProperties.equals(o.supportsMultiProperties) && supportsRemoveVertices.equals(o.supportsRemoveVertices) && supportsUpsert.equals(o.supportsUpsert);
  }
  
  @Override
  public int hashCode() {
    return 2 * elementFeatures.hashCode() + 3 * properties.hashCode() + 5 * supportsAddVertices.hashCode() + 7 * supportsDuplicateMultiProperties.hashCode() + 11 * supportsMetaProperties.hashCode() + 13 * supportsMultiProperties.hashCode() + 17 * supportsRemoveVertices.hashCode() + 19 * supportsUpsert.hashCode();
  }
  
  public VertexFeatures withElementFeatures(hydra.ext.org.apache.tinkerpop.features.ElementFeatures elementFeatures) {
    java.util.Objects.requireNonNull((elementFeatures));
    return new VertexFeatures(elementFeatures, properties, supportsAddVertices, supportsDuplicateMultiProperties, supportsMetaProperties, supportsMultiProperties, supportsRemoveVertices, supportsUpsert);
  }
  
  public VertexFeatures withProperties(hydra.ext.org.apache.tinkerpop.features.VertexPropertyFeatures properties) {
    java.util.Objects.requireNonNull((properties));
    return new VertexFeatures(elementFeatures, properties, supportsAddVertices, supportsDuplicateMultiProperties, supportsMetaProperties, supportsMultiProperties, supportsRemoveVertices, supportsUpsert);
  }
  
  public VertexFeatures withSupportsAddVertices(Boolean supportsAddVertices) {
    java.util.Objects.requireNonNull((supportsAddVertices));
    return new VertexFeatures(elementFeatures, properties, supportsAddVertices, supportsDuplicateMultiProperties, supportsMetaProperties, supportsMultiProperties, supportsRemoveVertices, supportsUpsert);
  }
  
  public VertexFeatures withSupportsDuplicateMultiProperties(Boolean supportsDuplicateMultiProperties) {
    java.util.Objects.requireNonNull((supportsDuplicateMultiProperties));
    return new VertexFeatures(elementFeatures, properties, supportsAddVertices, supportsDuplicateMultiProperties, supportsMetaProperties, supportsMultiProperties, supportsRemoveVertices, supportsUpsert);
  }
  
  public VertexFeatures withSupportsMetaProperties(Boolean supportsMetaProperties) {
    java.util.Objects.requireNonNull((supportsMetaProperties));
    return new VertexFeatures(elementFeatures, properties, supportsAddVertices, supportsDuplicateMultiProperties, supportsMetaProperties, supportsMultiProperties, supportsRemoveVertices, supportsUpsert);
  }
  
  public VertexFeatures withSupportsMultiProperties(Boolean supportsMultiProperties) {
    java.util.Objects.requireNonNull((supportsMultiProperties));
    return new VertexFeatures(elementFeatures, properties, supportsAddVertices, supportsDuplicateMultiProperties, supportsMetaProperties, supportsMultiProperties, supportsRemoveVertices, supportsUpsert);
  }
  
  public VertexFeatures withSupportsRemoveVertices(Boolean supportsRemoveVertices) {
    java.util.Objects.requireNonNull((supportsRemoveVertices));
    return new VertexFeatures(elementFeatures, properties, supportsAddVertices, supportsDuplicateMultiProperties, supportsMetaProperties, supportsMultiProperties, supportsRemoveVertices, supportsUpsert);
  }
  
  public VertexFeatures withSupportsUpsert(Boolean supportsUpsert) {
    java.util.Objects.requireNonNull((supportsUpsert));
    return new VertexFeatures(elementFeatures, properties, supportsAddVertices, supportsDuplicateMultiProperties, supportsMetaProperties, supportsMultiProperties, supportsRemoveVertices, supportsUpsert);
  }
}