package hydra.ext.tinkerpop.typed;

/**
 * The type of a vertex, with characteristic id and property types
 */
public class VertexType {
  public final hydra.core.LiteralType id;
  
  public final java.util.Map<Key, Value> properties;
  
  public VertexType (hydra.core.LiteralType id, java.util.Map<Key, Value> properties) {
    this.id = id;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexType)) {
      return false;
    }
    VertexType o = (VertexType) (other);
    return id.equals(o.id) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * properties.hashCode();
  }
  
  public VertexType withId(hydra.core.LiteralType id) {
    return new VertexType(id, properties);
  }
  
  public VertexType withProperties(java.util.Map<Key, Value> properties) {
    return new VertexType(id, properties);
  }
}