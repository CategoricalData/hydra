package hydra.ext.tinkerpop.v3;

public class VertexType {
  /**
   * @type hydra/ext/tinkerpop/v3.Value
   */
  public final Value id;
  
  /**
   * @type list: hydra/ext/tinkerpop/v3.PropertyType
   */
  public final java.util.List<PropertyType> properties;
  
  /**
   * Constructs an immutable VertexType object
   */
  public VertexType(Value id, java.util.List<PropertyType> properties) {
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
  public VertexType withId(Value id) {
    return new VertexType(id, properties);
  }
  
  /**
   * Construct a new immutable VertexType object in which properties is overridden
   */
  public VertexType withProperties(java.util.List<PropertyType> properties) {
    return new VertexType(id, properties);
  }
}
