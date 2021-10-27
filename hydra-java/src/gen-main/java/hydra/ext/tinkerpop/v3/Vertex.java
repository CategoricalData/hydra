package hydra.ext.tinkerpop.v3;

/**
 * A vertex, comprised of an id and zero or more properties
 */
public class Vertex {
  public final hydra.ext.tinkerpop.v3.Value id;
  
  public final java.util.Map<hydra.ext.tinkerpop.v3.Key, hydra.ext.tinkerpop.v3.Value> properties;
  
  /**
   * Constructs an immutable Vertex object
   */
  public Vertex(hydra.ext.tinkerpop.v3.Value id, java.util.Map<hydra.ext.tinkerpop.v3.Key, hydra.ext.tinkerpop.v3.Value> properties) {
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
  public Vertex withId(hydra.ext.tinkerpop.v3.Value id) {
    return new Vertex(id, properties);
  }
  
  /**
   * Construct a new immutable Vertex object in which properties is overridden
   */
  public Vertex withProperties(java.util.Map<hydra.ext.tinkerpop.v3.Key, hydra.ext.tinkerpop.v3.Value> properties) {
    return new Vertex(id, properties);
  }
}
