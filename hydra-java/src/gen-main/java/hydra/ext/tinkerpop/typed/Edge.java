package hydra.ext.tinkerpop.typed;

/**
 * An edge, comprised of an id, an out-vertex and in-vertex id, and zero or more properties
 */
public class Edge {
  public final hydra.ext.tinkerpop.typed.EdgeId id;
  
  public final hydra.ext.tinkerpop.typed.Label label;
  
  public final hydra.ext.tinkerpop.typed.VertexId out;
  
  public final hydra.ext.tinkerpop.typed.VertexId in;
  
  public final java.util.Map<hydra.ext.tinkerpop.typed.Key, hydra.ext.tinkerpop.typed.Value> properties;
  
  public Edge (hydra.ext.tinkerpop.typed.EdgeId id, hydra.ext.tinkerpop.typed.Label label, hydra.ext.tinkerpop.typed.VertexId out, hydra.ext.tinkerpop.typed.VertexId in, java.util.Map<hydra.ext.tinkerpop.typed.Key, hydra.ext.tinkerpop.typed.Value> properties) {
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
  
  public Edge withId(hydra.ext.tinkerpop.typed.EdgeId id) {
    return new Edge(id, label, out, in, properties);
  }
  
  public Edge withLabel(hydra.ext.tinkerpop.typed.Label label) {
    return new Edge(id, label, out, in, properties);
  }
  
  public Edge withOut(hydra.ext.tinkerpop.typed.VertexId out) {
    return new Edge(id, label, out, in, properties);
  }
  
  public Edge withIn(hydra.ext.tinkerpop.typed.VertexId in) {
    return new Edge(id, label, out, in, properties);
  }
  
  public Edge withProperties(java.util.Map<hydra.ext.tinkerpop.typed.Key, hydra.ext.tinkerpop.typed.Value> properties) {
    return new Edge(id, label, out, in, properties);
  }
}