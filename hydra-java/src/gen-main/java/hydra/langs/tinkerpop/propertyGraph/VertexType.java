package hydra.langs.tinkerpop.propertyGraph;

/**
 * The type of a vertex
 */
public class VertexType<T> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/propertyGraph.VertexType");
  
  public final hydra.langs.tinkerpop.propertyGraph.VertexLabel label;
  
  public final java.util.Map<hydra.langs.tinkerpop.propertyGraph.PropertyKey, T> properties;
  
  public VertexType (hydra.langs.tinkerpop.propertyGraph.VertexLabel label, java.util.Map<hydra.langs.tinkerpop.propertyGraph.PropertyKey, T> properties) {
    this.label = label;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexType)) {
      return false;
    }
    VertexType o = (VertexType) (other);
    return label.equals(o.label) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * label.hashCode() + 3 * properties.hashCode();
  }
  
  public VertexType withLabel(hydra.langs.tinkerpop.propertyGraph.VertexLabel label) {
    return new VertexType(label, properties);
  }
  
  public VertexType withProperties(java.util.Map<hydra.langs.tinkerpop.propertyGraph.PropertyKey, T> properties) {
    return new VertexType(label, properties);
  }
}