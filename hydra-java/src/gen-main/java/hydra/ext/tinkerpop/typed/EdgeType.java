package hydra.ext.tinkerpop.typed;

/**
 * The type of an edge, with characteristic id, out-vertex, in-vertex, and property types
 */
public class EdgeType {
  public final hydra.core.LiteralType id;
  
  public final VertexIdType out;
  
  public final VertexIdType in;
  
  public final java.util.Map<Key, Type> properties;
  
  public EdgeType (hydra.core.LiteralType id, VertexIdType out, VertexIdType in, java.util.Map<Key, Type> properties) {
    this.id = id;
    this.out = out;
    this.in = in;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeType)) {
      return false;
    }
    EdgeType o = (EdgeType) (other);
    return id.equals(o.id) && out.equals(o.out) && in.equals(o.in) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * out.hashCode() + 5 * in.hashCode() + 7 * properties.hashCode();
  }
  
  public EdgeType withId(hydra.core.LiteralType id) {
    return new EdgeType(id, out, in, properties);
  }
  
  public EdgeType withOut(VertexIdType out) {
    return new EdgeType(id, out, in, properties);
  }
  
  public EdgeType withIn(VertexIdType in) {
    return new EdgeType(id, out, in, properties);
  }
  
  public EdgeType withProperties(java.util.Map<Key, Type> properties) {
    return new EdgeType(id, out, in, properties);
  }
}