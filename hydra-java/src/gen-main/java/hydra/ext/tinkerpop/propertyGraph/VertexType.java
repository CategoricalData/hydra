// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.propertyGraph;

import java.io.Serializable;

/**
 * The type of a vertex
 */
public class VertexType<T> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/propertyGraph.VertexType");
  
  public static final hydra.core.Name FIELD_NAME_LABEL = new hydra.core.Name("label");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  /**
   * The label of any vertex of this vertex type
   */
  public final hydra.ext.tinkerpop.propertyGraph.VertexLabel label;
  
  /**
   * The type of the id of any vertex of this vertex type
   */
  public final T id;
  
  /**
   * A list of property types. The types are ordered for the sake of applications in which property order is significant.
   */
  public final java.util.List<hydra.ext.tinkerpop.propertyGraph.PropertyType<T>> properties;
  
  public VertexType (hydra.ext.tinkerpop.propertyGraph.VertexLabel label, T id, java.util.List<hydra.ext.tinkerpop.propertyGraph.PropertyType<T>> properties) {
    java.util.Objects.requireNonNull((label));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((properties));
    this.label = label;
    this.id = id;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexType)) {
      return false;
    }
    VertexType o = (VertexType) (other);
    return label.equals(o.label) && id.equals(o.id) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * label.hashCode() + 3 * id.hashCode() + 5 * properties.hashCode();
  }
  
  public VertexType withLabel(hydra.ext.tinkerpop.propertyGraph.VertexLabel label) {
    java.util.Objects.requireNonNull((label));
    return new VertexType(label, id, properties);
  }
  
  public VertexType withId(T id) {
    java.util.Objects.requireNonNull((id));
    return new VertexType(label, id, properties);
  }
  
  public VertexType withProperties(java.util.List<hydra.ext.tinkerpop.propertyGraph.PropertyType<T>> properties) {
    java.util.Objects.requireNonNull((properties));
    return new VertexType(label, id, properties);
  }
}
