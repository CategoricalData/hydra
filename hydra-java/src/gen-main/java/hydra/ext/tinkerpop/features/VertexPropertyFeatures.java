package hydra.ext.tinkerpop.features;

/**
 * Features that are related to Vertex Property objects.
 */
public class VertexPropertyFeatures {
  public final hydra.ext.tinkerpop.features.DataTypeFeatures dataTypeFeatures;
  
  public final hydra.ext.tinkerpop.features.PropertyFeatures propertyFeatures;
  
  public final hydra.ext.tinkerpop.features.ElementFeatures elementFeatures;
  
  /**
   * Determines if a VertexProperty allows properties to be removed.
   */
  public final Boolean supportsRemove;
  
  public VertexPropertyFeatures (hydra.ext.tinkerpop.features.DataTypeFeatures dataTypeFeatures, hydra.ext.tinkerpop.features.PropertyFeatures propertyFeatures, hydra.ext.tinkerpop.features.ElementFeatures elementFeatures, Boolean supportsRemove) {
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
  
  public VertexPropertyFeatures withDataTypeFeatures(hydra.ext.tinkerpop.features.DataTypeFeatures dataTypeFeatures) {
    return new VertexPropertyFeatures(dataTypeFeatures, propertyFeatures, elementFeatures, supportsRemove);
  }
  
  public VertexPropertyFeatures withPropertyFeatures(hydra.ext.tinkerpop.features.PropertyFeatures propertyFeatures) {
    return new VertexPropertyFeatures(dataTypeFeatures, propertyFeatures, elementFeatures, supportsRemove);
  }
  
  public VertexPropertyFeatures withElementFeatures(hydra.ext.tinkerpop.features.ElementFeatures elementFeatures) {
    return new VertexPropertyFeatures(dataTypeFeatures, propertyFeatures, elementFeatures, supportsRemove);
  }
  
  public VertexPropertyFeatures withSupportsRemove(Boolean supportsRemove) {
    return new VertexPropertyFeatures(dataTypeFeatures, propertyFeatures, elementFeatures, supportsRemove);
  }
}