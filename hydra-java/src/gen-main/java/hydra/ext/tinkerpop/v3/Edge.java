package hydra.ext.tinkerpop.v3;

/**
 * An edge
 */
public class Edge {
  public final Id id;
  
  public final Properties properties;
  
  public final Id out;
  
  public final Id in;
  
  public Edge (Id id, Properties properties, Id out, Id in) {
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
  
  public Edge withId(Id id) {
    return new Edge(id, properties, out, in);
  }
  
  public Edge withProperties(Properties properties) {
    return new Edge(id, properties, out, in);
  }
  
  public Edge withOut(Id out) {
    return new Edge(id, properties, out, in);
  }
  
  public Edge withIn(Id in) {
    return new Edge(id, properties, out, in);
  }
}