package hydra.ext.tinkerpop.v3;

/**
 * An edge
 */
public class Edge<V, E, P> {
  public final E id;
  
  public final V out;
  
  public final V in;
  
  public final java.util.Map<hydra.ext.tinkerpop.v3.PropertyKey, P> properties;
  
  public Edge (E id, V out, V in, java.util.Map<hydra.ext.tinkerpop.v3.PropertyKey, P> properties) {
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
    Edge o = (Edge) (other);
    return id.equals(o.id) && out.equals(o.out) && in.equals(o.in) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * out.hashCode() + 5 * in.hashCode() + 7 * properties.hashCode();
  }
  
  public Edge withId(E id) {
    return new Edge(id, out, in, properties);
  }
  
  public Edge withOut(V out) {
    return new Edge(id, out, in, properties);
  }
  
  public Edge withIn(V in) {
    return new Edge(id, out, in, properties);
  }
  
  public Edge withProperties(java.util.Map<hydra.ext.tinkerpop.v3.PropertyKey, P> properties) {
    return new Edge(id, out, in, properties);
  }
}