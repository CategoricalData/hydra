// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * The type of a vertex
 */
public class VertexType<T> implements Serializable, Comparable<VertexType<T>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.VertexType");
  
  public static final hydra.core.Name LABEL = new hydra.core.Name("label");
  
  public static final hydra.core.Name ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");
  
  /**
   * The label of any vertex of this vertex type
   */
  public final hydra.pg.model.VertexLabel label;
  
  /**
   * The type of the id of any vertex of this vertex type
   */
  public final T id;
  
  /**
   * A list of property types. The types are ordered for the sake of applications in which property order is significant.
   */
  public final hydra.util.ConsList<hydra.pg.model.PropertyType<T>> properties;
  
  public VertexType (hydra.pg.model.VertexLabel label, T id, hydra.util.ConsList<hydra.pg.model.PropertyType<T>> properties) {
    this.label = label;
    this.id = id;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexType)) {
      return false;
    }
    VertexType o = (VertexType) other;
    return java.util.Objects.equals(
      this.label,
      o.label) && java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.properties,
      o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(label) + 3 * java.util.Objects.hashCode(id) + 5 * java.util.Objects.hashCode(properties);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VertexType other) {
    int cmp = 0;
    cmp = ((Comparable) label).compareTo(other.label);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) id).compareTo(other.id);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      properties.hashCode(),
      other.properties.hashCode());
  }
  
  public VertexType withLabel(hydra.pg.model.VertexLabel label) {
    return new VertexType(label, id, properties);
  }
  
  public VertexType withId(T id) {
    return new VertexType(label, id, properties);
  }
  
  public VertexType withProperties(hydra.util.ConsList<hydra.pg.model.PropertyType<T>> properties) {
    return new VertexType(label, id, properties);
  }
}
