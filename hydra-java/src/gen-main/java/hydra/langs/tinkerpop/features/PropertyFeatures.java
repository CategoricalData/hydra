// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.features;

import java.io.Serializable;

/**
 * A base interface for Edge or Vertex Property features.
 */
public class PropertyFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/features.PropertyFeatures");
  
  public final hydra.langs.tinkerpop.features.DataTypeFeatures dataTypeFeatures;
  
  /**
   * Determines if an Element allows for the processing of at least one data type defined by the features.
   */
  public final Boolean supportsProperties;
  
  public PropertyFeatures (hydra.langs.tinkerpop.features.DataTypeFeatures dataTypeFeatures, Boolean supportsProperties) {
    if (dataTypeFeatures == null) {
      throw new IllegalArgumentException("null value for 'dataTypeFeatures' argument");
    }
    if (supportsProperties == null) {
      throw new IllegalArgumentException("null value for 'supportsProperties' argument");
    }
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
  
  public PropertyFeatures withDataTypeFeatures(hydra.langs.tinkerpop.features.DataTypeFeatures dataTypeFeatures) {
    if (dataTypeFeatures == null) {
      throw new IllegalArgumentException("null value for 'dataTypeFeatures' argument");
    }
    return new PropertyFeatures(dataTypeFeatures, supportsProperties);
  }
  
  public PropertyFeatures withSupportsProperties(Boolean supportsProperties) {
    if (supportsProperties == null) {
      throw new IllegalArgumentException("null value for 'supportsProperties' argument");
    }
    return new PropertyFeatures(dataTypeFeatures, supportsProperties);
  }
}