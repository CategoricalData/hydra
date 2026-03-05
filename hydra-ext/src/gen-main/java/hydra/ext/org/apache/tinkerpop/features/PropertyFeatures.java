// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.features;

import java.io.Serializable;

/**
 * A base interface for Edge or Vertex Property features.
 */
public class PropertyFeatures implements Serializable, Comparable<PropertyFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.features.PropertyFeatures");
  
  public static final hydra.core.Name DATA_TYPE_FEATURES = new hydra.core.Name("dataTypeFeatures");
  
  public static final hydra.core.Name SUPPORTS_PROPERTIES = new hydra.core.Name("supportsProperties");
  
  public final hydra.ext.org.apache.tinkerpop.features.DataTypeFeatures dataTypeFeatures;
  
  /**
   * Determines if an Element allows for the processing of at least one data type defined by the features.
   */
  public final Boolean supportsProperties;
  
  public PropertyFeatures (hydra.ext.org.apache.tinkerpop.features.DataTypeFeatures dataTypeFeatures, Boolean supportsProperties) {
    this.dataTypeFeatures = dataTypeFeatures;
    this.supportsProperties = supportsProperties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyFeatures)) {
      return false;
    }
    PropertyFeatures o = (PropertyFeatures) other;
    return java.util.Objects.equals(
      this.dataTypeFeatures,
      o.dataTypeFeatures) && java.util.Objects.equals(
      this.supportsProperties,
      o.supportsProperties);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(dataTypeFeatures) + 3 * java.util.Objects.hashCode(supportsProperties);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PropertyFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) dataTypeFeatures).compareTo(other.dataTypeFeatures);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) supportsProperties).compareTo(other.supportsProperties);
  }
  
  public PropertyFeatures withDataTypeFeatures(hydra.ext.org.apache.tinkerpop.features.DataTypeFeatures dataTypeFeatures) {
    return new PropertyFeatures(dataTypeFeatures, supportsProperties);
  }
  
  public PropertyFeatures withSupportsProperties(Boolean supportsProperties) {
    return new PropertyFeatures(dataTypeFeatures, supportsProperties);
  }
}
