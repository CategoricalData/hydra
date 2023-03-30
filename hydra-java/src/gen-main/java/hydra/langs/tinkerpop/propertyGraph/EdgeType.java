package hydra.langs.tinkerpop.propertyGraph;

/**
 * The type of an edge
 */
public class EdgeType<T> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/propertyGraph.EdgeType");
  
  public final hydra.langs.tinkerpop.propertyGraph.EdgeLabel label;
  
  public final hydra.langs.tinkerpop.propertyGraph.VertexLabel out;
  
  public final hydra.langs.tinkerpop.propertyGraph.VertexLabel in;
  
  public final java.util.Map<hydra.langs.tinkerpop.propertyGraph.PropertyKey, T> properties;
  
  public EdgeType (hydra.langs.tinkerpop.propertyGraph.EdgeLabel label, hydra.langs.tinkerpop.propertyGraph.VertexLabel out, hydra.langs.tinkerpop.propertyGraph.VertexLabel in, java.util.Map<hydra.langs.tinkerpop.propertyGraph.PropertyKey, T> properties) {
    this.label = label;
    this.out = out;
    this.in = in;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeType)) {
      return false;
    }
    EdgeType o = (EdgeType) (other);
    return label.equals(o.label) && out.equals(o.out) && in.equals(o.in) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * label.hashCode() + 3 * out.hashCode() + 5 * in.hashCode() + 7 * properties.hashCode();
  }
  
  public EdgeType withLabel(hydra.langs.tinkerpop.propertyGraph.EdgeLabel label) {
    return new EdgeType(label, out, in, properties);
  }
  
  public EdgeType withOut(hydra.langs.tinkerpop.propertyGraph.VertexLabel out) {
    return new EdgeType(label, out, in, properties);
  }
  
  public EdgeType withIn(hydra.langs.tinkerpop.propertyGraph.VertexLabel in) {
    return new EdgeType(label, out, in, properties);
  }
  
  public EdgeType withProperties(java.util.Map<hydra.langs.tinkerpop.propertyGraph.PropertyKey, T> properties) {
    return new EdgeType(label, out, in, properties);
  }
}