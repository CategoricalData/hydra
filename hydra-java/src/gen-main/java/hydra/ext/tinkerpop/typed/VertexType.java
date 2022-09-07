package hydra.ext.tinkerpop.typed;

/**
 * The type of a vertex, with characteristic id and property types
 */
public class VertexType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/tinkerpop/typed.VertexType");
  
  public final hydra.core.LiteralType id;
  
  public final java.util.Map<hydra.ext.tinkerpop.typed.Key, hydra.ext.tinkerpop.typed.Value> properties;
  
  public VertexType (hydra.core.LiteralType id, java.util.Map<hydra.ext.tinkerpop.typed.Key, hydra.ext.tinkerpop.typed.Value> properties) {
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
  
  public VertexType withProperties(java.util.Map<hydra.ext.tinkerpop.typed.Key, hydra.ext.tinkerpop.typed.Value> properties) {
    return new VertexType(id, properties);
  }
}