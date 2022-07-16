package hydra.ext.tinkerpop.v3;

/**
 * A vertex
 */
public class Vertex {
  public final Id id;
  
  public final Properties properties;
  
  public Vertex (Id id, Properties properties) {
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
  
  public Vertex withId(Id id) {
    return new Vertex(id, properties);
  }
  
  public Vertex withProperties(Properties properties) {
    return new Vertex(id, properties);
  }
}