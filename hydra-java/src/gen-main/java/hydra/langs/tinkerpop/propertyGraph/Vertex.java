package hydra.langs.tinkerpop.propertyGraph;

/**
 * A vertex
 */
public class Vertex<V, P> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/propertyGraph.Vertex");
  
  public final hydra.langs.tinkerpop.propertyGraph.VertexLabel label;
  
  public final V id;
  
  public final java.util.Map<hydra.langs.tinkerpop.propertyGraph.PropertyKey, P> properties;
  
  public Vertex (hydra.langs.tinkerpop.propertyGraph.VertexLabel label, V id, java.util.Map<hydra.langs.tinkerpop.propertyGraph.PropertyKey, P> properties) {
    this.label = label;
    this.id = id;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Vertex)) {
      return false;
    }
    Vertex o = (Vertex) (other);
    return label.equals(o.label) && id.equals(o.id) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * label.hashCode() + 3 * id.hashCode() + 5 * properties.hashCode();
  }
  
  public Vertex withLabel(hydra.langs.tinkerpop.propertyGraph.VertexLabel label) {
    return new Vertex(label, id, properties);
  }
  
  public Vertex withId(V id) {
    return new Vertex(label, id, properties);
  }
  
  public Vertex withProperties(java.util.Map<hydra.langs.tinkerpop.propertyGraph.PropertyKey, P> properties) {
    return new Vertex(label, id, properties);
  }
}