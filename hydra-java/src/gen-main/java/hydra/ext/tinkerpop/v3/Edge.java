package hydra.ext.tinkerpop.v3;

/**
 * An edge
 */
public class Edge {
  public final hydra.ext.tinkerpop.v3.Id id;
  
  public final hydra.ext.tinkerpop.v3.Properties properties;
  
  public final hydra.ext.tinkerpop.v3.Id out;
  
  public final hydra.ext.tinkerpop.v3.Id in;
  
  public Edge (hydra.ext.tinkerpop.v3.Id id, hydra.ext.tinkerpop.v3.Properties properties, hydra.ext.tinkerpop.v3.Id out, hydra.ext.tinkerpop.v3.Id in) {
    this.id = id;
    this.properties = properties;
    this.out = out;
    this.in = in;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Edge)) {
      return false;
    }
    Edge o = (Edge) (other);
    return id.equals(o.id) && properties.equals(o.properties) && out.equals(o.out) && in.equals(o.in);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * properties.hashCode() + 5 * out.hashCode() + 7 * in.hashCode();
  }
  
  public Edge withId(hydra.ext.tinkerpop.v3.Id id) {
    return new Edge(id, properties, out, in);
  }
  
  public Edge withProperties(hydra.ext.tinkerpop.v3.Properties properties) {
    return new Edge(id, properties, out, in);
  }
  
  public Edge withOut(hydra.ext.tinkerpop.v3.Id out) {
    return new Edge(id, properties, out, in);
  }
  
  public Edge withIn(hydra.ext.tinkerpop.v3.Id in) {
    return new Edge(id, properties, out, in);
  }
}