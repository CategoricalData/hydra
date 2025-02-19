// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.features;

import java.io.Serializable;

/**
 * A base interface for Edge or Vertex Property features.
 */
public class PropertyFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.features.PropertyFeatures");
  
  public static final hydra.core.Name FIELD_NAME_DATA_TYPE_FEATURES = new hydra.core.Name("dataTypeFeatures");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_PROPERTIES = new hydra.core.Name("supportsProperties");
  
  public final hydra.ext.org.apache.tinkerpop.features.DataTypeFeatures dataTypeFeatures;
  
  /**
   * Determines if an Element allows for the processing of at least one data type defined by the features.
   */
  public final Boolean supportsProperties;
  
  public PropertyFeatures (hydra.ext.org.apache.tinkerpop.features.DataTypeFeatures dataTypeFeatures, Boolean supportsProperties) {
    java.util.Objects.requireNonNull((dataTypeFeatures));
    java.util.Objects.requireNonNull((supportsProperties));
    this.dataTypeFeatures = dataTypeFeatures;
    this.supportsProperties = supportsProperties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyFeatures)) {
      return false;
    }
    PropertyFeatures o = (PropertyFeatures) (other);
    return dataTypeFeatures.equals(o.dataTypeFeatures) && supportsProperties.equals(o.supportsProperties);
  }
  
  @Override
  public int hashCode() {
    return 2 * dataTypeFeatures.hashCode() + 3 * supportsProperties.hashCode();
  }
  
  public PropertyFeatures withDataTypeFeatures(hydra.ext.org.apache.tinkerpop.features.DataTypeFeatures dataTypeFeatures) {
    java.util.Objects.requireNonNull((dataTypeFeatures));
    return new PropertyFeatures(dataTypeFeatures, supportsProperties);
  }
  
  public PropertyFeatures withSupportsProperties(Boolean supportsProperties) {
    java.util.Objects.requireNonNull((supportsProperties));
    return new PropertyFeatures(dataTypeFeatures, supportsProperties);
  }
}