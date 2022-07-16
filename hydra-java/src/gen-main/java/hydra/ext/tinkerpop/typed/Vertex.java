package hydra.ext.tinkerpop.typed;

/**
 * A vertex, comprised of an id and zero or more properties
 */
public class Vertex {
  public final VertexId id;
  
  public final Label label;
  
  public final java.util.Map<Key, Value> properties;
  
  public Vertex (VertexId id, Label label, java.util.Map<Key, Value> properties) {
    this.id = id;
    this.label = label;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Vertex)) {
      return false;
    }
    Vertex o = (Vertex) (other);
    return id.equals(o.id) && label.equals(o.label) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * label.hashCode() + 5 * properties.hashCode();
  }
  
  public Vertex withId(VertexId id) {
    return new Vertex(id, label, properties);
  }
  
  public Vertex withLabel(Label label) {
    return new Vertex(id, label, properties);
  }
  
  public Vertex withProperties(java.util.Map<Key, Value> properties) {
    return new Vertex(id, label, properties);
  }
}