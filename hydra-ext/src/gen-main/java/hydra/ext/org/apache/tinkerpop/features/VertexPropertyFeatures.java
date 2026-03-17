// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.features;

import java.io.Serializable;

/**
 * Features that are related to Vertex Property objects.
 */
public class VertexPropertyFeatures implements Serializable, Comparable<VertexPropertyFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.features.VertexPropertyFeatures");

  public static final hydra.core.Name DATA_TYPE_FEATURES = new hydra.core.Name("dataTypeFeatures");

  public static final hydra.core.Name PROPERTY_FEATURES = new hydra.core.Name("propertyFeatures");

  public static final hydra.core.Name ELEMENT_FEATURES = new hydra.core.Name("elementFeatures");

  public static final hydra.core.Name SUPPORTS_REMOVE = new hydra.core.Name("supportsRemove");

  public final hydra.ext.org.apache.tinkerpop.features.DataTypeFeatures dataTypeFeatures;

  public final hydra.ext.org.apache.tinkerpop.features.PropertyFeatures propertyFeatures;

  public final hydra.ext.org.apache.tinkerpop.features.ElementFeatures elementFeatures;

  /**
   * Determines if a VertexProperty allows properties to be removed.
   */
  public final Boolean supportsRemove;

  public VertexPropertyFeatures (hydra.ext.org.apache.tinkerpop.features.DataTypeFeatures dataTypeFeatures, hydra.ext.org.apache.tinkerpop.features.PropertyFeatures propertyFeatures, hydra.ext.org.apache.tinkerpop.features.ElementFeatures elementFeatures, Boolean supportsRemove) {
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
    VertexPropertyFeatures o = (VertexPropertyFeatures) other;
    return java.util.Objects.equals(
      this.dataTypeFeatures,
      o.dataTypeFeatures) && java.util.Objects.equals(
      this.propertyFeatures,
      o.propertyFeatures) && java.util.Objects.equals(
      this.elementFeatures,
      o.elementFeatures) && java.util.Objects.equals(
      this.supportsRemove,
      o.supportsRemove);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(dataTypeFeatures) + 3 * java.util.Objects.hashCode(propertyFeatures) + 5 * java.util.Objects.hashCode(elementFeatures) + 7 * java.util.Objects.hashCode(supportsRemove);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VertexPropertyFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) dataTypeFeatures).compareTo(other.dataTypeFeatures);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) propertyFeatures).compareTo(other.propertyFeatures);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) elementFeatures).compareTo(other.elementFeatures);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) supportsRemove).compareTo(other.supportsRemove);
  }

  public VertexPropertyFeatures withDataTypeFeatures(hydra.ext.org.apache.tinkerpop.features.DataTypeFeatures dataTypeFeatures) {
    return new VertexPropertyFeatures(dataTypeFeatures, propertyFeatures, elementFeatures, supportsRemove);
  }

  public VertexPropertyFeatures withPropertyFeatures(hydra.ext.org.apache.tinkerpop.features.PropertyFeatures propertyFeatures) {
    return new VertexPropertyFeatures(dataTypeFeatures, propertyFeatures, elementFeatures, supportsRemove);
  }

  public VertexPropertyFeatures withElementFeatures(hydra.ext.org.apache.tinkerpop.features.ElementFeatures elementFeatures) {
    return new VertexPropertyFeatures(dataTypeFeatures, propertyFeatures, elementFeatures, supportsRemove);
  }

  public VertexPropertyFeatures withSupportsRemove(Boolean supportsRemove) {
    return new VertexPropertyFeatures(dataTypeFeatures, propertyFeatures, elementFeatures, supportsRemove);
  }
}
