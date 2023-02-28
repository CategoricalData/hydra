package hydra.langs.tinkerpop.typed;

/**
 * A vertex, comprised of an id and zero or more properties
 */
public class Vertex {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/typed.Vertex");
  
  public final hydra.langs.tinkerpop.typed.VertexId id;
  
  public final hydra.langs.tinkerpop.typed.Label label;
  
  public final java.util.Map<hydra.langs.tinkerpop.typed.Key, hydra.langs.tinkerpop.typed.Value> properties;
  
  public Vertex (hydra.langs.tinkerpop.typed.VertexId id, hydra.langs.tinkerpop.typed.Label label, java.util.Map<hydra.langs.tinkerpop.typed.Key, hydra.langs.tinkerpop.typed.Value> properties) {
    this.id = id;
    this.label = label;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Vertex)) {
      return false;
    }
    Vertex o = (Vertex) (other);
    return id.equals(o.id) && label.equals(o.label) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * label.hashCode() + 5 * properties.hashCode();
  }
  
  public Vertex withId(hydra.langs.tinkerpop.typed.VertexId id) {
    return new Vertex(id, label, properties);
  }
  
  public Vertex withLabel(hydra.langs.tinkerpop.typed.Label label) {
    return new Vertex(id, label, properties);
  }
  
  public Vertex withProperties(java.util.Map<hydra.langs.tinkerpop.typed.Key, hydra.langs.tinkerpop.typed.Value> properties) {
    return new Vertex(id, label, properties);
  }
}