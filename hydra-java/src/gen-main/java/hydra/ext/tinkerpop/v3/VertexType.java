package hydra.ext.tinkerpop.v3;

import hydra.core.AtomicType;

/**
 * The type of a vertex, with characteristic id and property types
 */
public class VertexType {
  public final hydra.core.AtomicType id;
  
  public final java.util.Map<hydra.ext.tinkerpop.v3.Key, hydra.ext.tinkerpop.v3.Type> properties;
  
  /**
   * Constructs an immutable VertexType object
   */
  public VertexType(hydra.core.AtomicType id, java.util.Map<hydra.ext.tinkerpop.v3.Key, hydra.ext.tinkerpop.v3.Type> properties) {
    this.id = id;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexType)) {
        return false;
    }
    VertexType o = (VertexType) other;
    return id.equals(o.id)
        && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode()
        + 3 * properties.hashCode();
  }
  
  /**
   * Construct a new immutable VertexType object in which id is overridden
   */
  public VertexType withId(hydra.core.AtomicType id) {
    return new VertexType(id, properties);
  }
  
  /**
   * Construct a new immutable VertexType object in which properties is overridden
   */
  public VertexType withProperties(java.util.Map<hydra.ext.tinkerpop.v3.Key, hydra.ext.tinkerpop.v3.Type> properties) {
    return new VertexType(id, properties);
  }
}
