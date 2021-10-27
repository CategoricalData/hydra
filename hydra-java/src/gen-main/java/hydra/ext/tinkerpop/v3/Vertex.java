package hydra.ext.tinkerpop.v3;

public class Vertex {
  /**
   * @type hydra/ext/tinkerpop/v3.Value
   */
  public final Value id;
  
  /**
   * @type list: hydra/ext/tinkerpop/v3.Property
   */
  public final java.util.List<Property> properties;
  
  /**
   * Constructs an immutable Vertex object
   */
  public Vertex(Value id, java.util.List<Property> properties) {
    this.id = id;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Vertex)) {
        return false;
    }
    Vertex o = (Vertex) other;
    return id.equals(o.id)
        && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode()
        + 3 * properties.hashCode();
  }
  
  /**
   * Construct a new immutable Vertex object in which id is overridden
   */
  public Vertex withId(Value id) {
    return new Vertex(id, properties);
  }
  
  /**
   * Construct a new immutable Vertex object in which properties is overridden
   */
  public Vertex withProperties(java.util.List<Property> properties) {
    return new Vertex(id, properties);
  }
}
