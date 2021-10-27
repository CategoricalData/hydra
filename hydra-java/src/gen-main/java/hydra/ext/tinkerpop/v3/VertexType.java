package hydra.ext.tinkerpop.v3;

public class VertexType {
  public final hydra.ext.tinkerpop.v3.Value id;
  
  public final java.util.List<hydra.ext.tinkerpop.v3.PropertyType> properties;
  
  /**
   * Constructs an immutable VertexType object
   */
  public VertexType(hydra.ext.tinkerpop.v3.Value id, java.util.List<hydra.ext.tinkerpop.v3.PropertyType> properties) {
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
  public VertexType withId(hydra.ext.tinkerpop.v3.Value id) {
    return new VertexType(id, properties);
  }
  
  /**
   * Construct a new immutable VertexType object in which properties is overridden
   */
  public VertexType withProperties(java.util.List<hydra.ext.tinkerpop.v3.PropertyType> properties) {
    return new VertexType(id, properties);
  }
}
