package hydra.ext.tinkerpop.v3;

public class Edge {
  public final hydra.ext.tinkerpop.v3.Value id;
  
  public final hydra.ext.tinkerpop.v3.Vertex out;
  
  public final hydra.ext.tinkerpop.v3.Vertex in;
  
  public final java.util.List<hydra.ext.tinkerpop.v3.Property> properties;
  
  /**
   * Constructs an immutable Edge object
   */
  public Edge(hydra.ext.tinkerpop.v3.Value id, hydra.ext.tinkerpop.v3.Vertex out, hydra.ext.tinkerpop.v3.Vertex in, java.util.List<hydra.ext.tinkerpop.v3.Property> properties) {
    this.id = id;
    this.out = out;
    this.in = in;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Edge)) {
        return false;
    }
    Edge o = (Edge) other;
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
   * Construct a new immutable Edge object in which id is overridden
   */
  public Edge withId(hydra.ext.tinkerpop.v3.Value id) {
    return new Edge(id, out, in, properties);
  }
  
  /**
   * Construct a new immutable Edge object in which out is overridden
   */
  public Edge withOut(hydra.ext.tinkerpop.v3.Vertex out) {
    return new Edge(id, out, in, properties);
  }
  
  /**
   * Construct a new immutable Edge object in which in is overridden
   */
  public Edge withIn(hydra.ext.tinkerpop.v3.Vertex in) {
    return new Edge(id, out, in, properties);
  }
  
  /**
   * Construct a new immutable Edge object in which properties is overridden
   */
  public Edge withProperties(java.util.List<hydra.ext.tinkerpop.v3.Property> properties) {
    return new Edge(id, out, in, properties);
  }
}
