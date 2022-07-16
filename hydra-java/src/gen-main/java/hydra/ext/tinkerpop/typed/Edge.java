package hydra.ext.tinkerpop.typed;

/**
 * An edge, comprised of an id, an out-vertex and in-vertex id, and zero or more properties
 */
public class Edge {
  public final EdgeId id;
  
  public final Label label;
  
  public final VertexId out;
  
  public final VertexId in;
  
  public final java.util.Map<Key, Value> properties;
  
  public Edge (EdgeId id, Label label, VertexId out, VertexId in, java.util.Map<Key, Value> properties) {
    this.id = id;
    this.label = label;
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
    return id.equals(o.id) && label.equals(o.label) && out.equals(o.out) && in.equals(o.in) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * label.hashCode() + 5 * out.hashCode() + 7 * in.hashCode() + 11 * properties.hashCode();
  }
  
  public Edge withId(EdgeId id) {
    return new Edge(id, label, out, in, properties);
  }
  
  public Edge withLabel(Label label) {
    return new Edge(id, label, out, in, properties);
  }
  
  public Edge withOut(VertexId out) {
    return new Edge(id, label, out, in, properties);
  }
  
  public Edge withIn(VertexId in) {
    return new Edge(id, label, out, in, properties);
  }
  
  public Edge withProperties(java.util.Map<Key, Value> properties) {
    return new Edge(id, label, out, in, properties);
  }
}