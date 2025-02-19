// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.features;

import java.io.Serializable;

/**
 * Features that are related to Vertex Property objects.
 */
public class VertexPropertyFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.features.VertexPropertyFeatures");
  
  public static final hydra.core.Name FIELD_NAME_DATA_TYPE_FEATURES = new hydra.core.Name("dataTypeFeatures");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY_FEATURES = new hydra.core.Name("propertyFeatures");
  
  public static final hydra.core.Name FIELD_NAME_ELEMENT_FEATURES = new hydra.core.Name("elementFeatures");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_REMOVE = new hydra.core.Name("supportsRemove");
  
  public final hydra.ext.org.apache.tinkerpop.features.DataTypeFeatures dataTypeFeatures;
  
  public final hydra.ext.org.apache.tinkerpop.features.PropertyFeatures propertyFeatures;
  
  public final hydra.ext.org.apache.tinkerpop.features.ElementFeatures elementFeatures;
  
  /**
   * Determines if a VertexProperty allows properties to be removed.
   */
  public final Boolean supportsRemove;
  
  public VertexPropertyFeatures (hydra.ext.org.apache.tinkerpop.features.DataTypeFeatures dataTypeFeatures, hydra.ext.org.apache.tinkerpop.features.PropertyFeatures propertyFeatures, hydra.ext.org.apache.tinkerpop.features.ElementFeatures elementFeatures, Boolean supportsRemove) {
    java.util.Objects.requireNonNull((dataTypeFeatures));
    java.util.Objects.requireNonNull((propertyFeatures));
    java.util.Objects.requireNonNull((elementFeatures));
    java.util.Objects.requireNonNull((supportsRemove));
    this.dataTypeFeatures = dataTypeFeatures;
    this.propertyFeatures = propertyFeatures;
    this.elementFeatures = elementFeatures;
    this.supportsRemove = supportsRemove;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexPropertyFeatures)) {
      return false;
    }
    VertexPropertyFeatures o = (VertexPropertyFeatures) (other);
    return dataTypeFeatures.equals(o.dataTypeFeatures) && propertyFeatures.equals(o.propertyFeatures) && elementFeatures.equals(o.elementFeatures) && supportsRemove.equals(o.supportsRemove);
  }
  
  @Override
  public int hashCode() {
    return 2 * dataTypeFeatures.hashCode() + 3 * propertyFeatures.hashCode() + 5 * elementFeatures.hashCode() + 7 * supportsRemove.hashCode();
  }
  
  public VertexPropertyFeatures withDataTypeFeatures(hydra.ext.org.apache.tinkerpop.features.DataTypeFeatures dataTypeFeatures) {
    java.util.Objects.requireNonNull((dataTypeFeatures));
    return new VertexPropertyFeatures(dataTypeFeatures, propertyFeatures, elementFeatures, supportsRemove);
  }
  
  public VertexPropertyFeatures withPropertyFeatures(hydra.ext.org.apache.tinkerpop.features.PropertyFeatures propertyFeatures) {
    java.util.Objects.requireNonNull((propertyFeatures));
    return new VertexPropertyFeatures(dataTypeFeatures, propertyFeatures, elementFeatures, supportsRemove);
  }
  
  public VertexPropertyFeatures withElementFeatures(hydra.ext.org.apache.tinkerpop.features.ElementFeatures elementFeatures) {
    java.util.Objects.requireNonNull((elementFeatures));
    return new VertexPropertyFeatures(dataTypeFeatures, propertyFeatures, elementFeatures, supportsRemove);
  }
  
  public VertexPropertyFeatures withSupportsRemove(Boolean supportsRemove) {
    java.util.Objects.requireNonNull((supportsRemove));
    return new VertexPropertyFeatures(dataTypeFeatures, propertyFeatures, elementFeatures, supportsRemove);
  }
}