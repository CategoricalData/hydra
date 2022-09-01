package hydra.ext.tinkerpop.v3;

/**
 * A vertex
 */
public class Vertex<V, P> {
  public final V id;
  
  public final java.util.Map<hydra.ext.tinkerpop.v3.PropertyKey, P> properties;
  
  public Vertex (V id, java.util.Map<hydra.ext.tinkerpop.v3.PropertyKey, P> properties) {
    this.id = id;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Vertex)) {
      return false;
    }
    Vertex o = (Vertex) (other);
    return id.equals(o.id) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * properties.hashCode();
  }
  
  public Vertex withId(V id) {
    return new Vertex(id, properties);
  }
  
  public Vertex withProperties(java.util.Map<hydra.ext.tinkerpop.v3.PropertyKey, P> properties) {
    return new Vertex(id, properties);
  }
}