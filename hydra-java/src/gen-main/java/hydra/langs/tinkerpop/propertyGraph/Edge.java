package hydra.langs.tinkerpop.propertyGraph;

/**
 * An edge
 */
public class Edge<T> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/propertyGraph.Edge");
  
  public final hydra.langs.tinkerpop.propertyGraph.EdgeLabel label;
  
  public final T id;
  
  public final T out;
  
  public final T in;
  
  public final java.util.Map<hydra.langs.tinkerpop.propertyGraph.PropertyKey, T> properties;
  
  public Edge (hydra.langs.tinkerpop.propertyGraph.EdgeLabel label, T id, T out, T in, java.util.Map<hydra.langs.tinkerpop.propertyGraph.PropertyKey, T> properties) {
    this.label = label;
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
    return label.equals(o.label) && id.equals(o.id) && out.equals(o.out) && in.equals(o.in) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * label.hashCode() + 3 * id.hashCode() + 5 * out.hashCode() + 7 * in.hashCode() + 11 * properties.hashCode();
  }
  
  public Edge withLabel(hydra.langs.tinkerpop.propertyGraph.EdgeLabel label) {
    return new Edge(label, id, out, in, properties);
  }
  
  public Edge withId(T id) {
    return new Edge(label, id, out, in, properties);
  }
  
  public Edge withOut(T out) {
    return new Edge(label, id, out, in, properties);
  }
  
  public Edge withIn(T in) {
    return new Edge(label, id, out, in, properties);
  }
  
  public Edge withProperties(java.util.Map<hydra.langs.tinkerpop.propertyGraph.PropertyKey, T> properties) {
    return new Edge(label, id, out, in, properties);
  }
}