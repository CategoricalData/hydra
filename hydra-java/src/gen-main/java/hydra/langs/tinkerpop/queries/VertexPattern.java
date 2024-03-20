package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public class VertexPattern implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.VertexPattern");
  
  public final java.util.Optional<hydra.langs.tinkerpop.queries.Variable> variable;
  
  public final java.util.Optional<hydra.langs.tinkerpop.propertyGraph.VertexLabel> label;
  
  public final java.util.List<hydra.langs.tinkerpop.queries.PropertyPattern> properties;
  
  public final java.util.List<hydra.langs.tinkerpop.queries.EdgeProjectionPattern> edges;
  
  public VertexPattern (java.util.Optional<hydra.langs.tinkerpop.queries.Variable> variable, java.util.Optional<hydra.langs.tinkerpop.propertyGraph.VertexLabel> label, java.util.List<hydra.langs.tinkerpop.queries.PropertyPattern> properties, java.util.List<hydra.langs.tinkerpop.queries.EdgeProjectionPattern> edges) {
    this.variable = variable;
    this.label = label;
    this.properties = properties;
    this.edges = edges;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexPattern)) {
      return false;
    }
    VertexPattern o = (VertexPattern) (other);
    return variable.equals(o.variable) && label.equals(o.label) && properties.equals(o.properties) && edges.equals(o.edges);
  }
  
  @Override
  public int hashCode() {
    return 2 * variable.hashCode() + 3 * label.hashCode() + 5 * properties.hashCode() + 7 * edges.hashCode();
  }
  
  public VertexPattern withVariable(java.util.Optional<hydra.langs.tinkerpop.queries.Variable> variable) {
    return new VertexPattern(variable, label, properties, edges);
  }
  
  public VertexPattern withLabel(java.util.Optional<hydra.langs.tinkerpop.propertyGraph.VertexLabel> label) {
    return new VertexPattern(variable, label, properties, edges);
  }
  
  public VertexPattern withProperties(java.util.List<hydra.langs.tinkerpop.queries.PropertyPattern> properties) {
    return new VertexPattern(variable, label, properties, edges);
  }
  
  public VertexPattern withEdges(java.util.List<hydra.langs.tinkerpop.queries.EdgeProjectionPattern> edges) {
    return new VertexPattern(variable, label, properties, edges);
  }
}