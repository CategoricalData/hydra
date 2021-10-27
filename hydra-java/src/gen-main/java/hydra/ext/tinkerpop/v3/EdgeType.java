package hydra.ext.tinkerpop.v3;

public class EdgeType {
  public final Type id;
  
  public final VertexType out;
  
  public final VertexType in;
  
  public final java.util.List<PropertyType> properties;
  
  /**
   * Constructs an immutable EdgeType object
   */
  public EdgeType(Type id, VertexType out, VertexType in, java.util.List<PropertyType> properties) {
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
    EdgeType o = (EdgeType) other;
    return id.equals(o.id)
        && out.equals(o.out)
        && in.equals(o.in)
        && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode()
        + 3 * out.hashCode()
        + 5 * in.hashCode()
        + 7 * properties.hashCode();
  }
  
  /**
   * Construct a new immutable EdgeType object in which id is overridden
   */
  public EdgeType withId(Type id) {
    return new EdgeType(id, out, in, properties);
  }
  
  /**
   * Construct a new immutable EdgeType object in which out is overridden
   */
  public EdgeType withOut(VertexType out) {
    return new EdgeType(id, out, in, properties);
  }
  
  /**
   * Construct a new immutable EdgeType object in which in is overridden
   */
  public EdgeType withIn(VertexType in) {
    return new EdgeType(id, out, in, properties);
  }
  
  /**
   * Construct a new immutable EdgeType object in which properties is overridden
   */
  public EdgeType withProperties(java.util.List<PropertyType> properties) {
    return new EdgeType(id, out, in, properties);
  }
}
