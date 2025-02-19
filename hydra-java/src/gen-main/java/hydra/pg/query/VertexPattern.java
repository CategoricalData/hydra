// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class VertexPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.query.VertexPattern");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE = new hydra.core.Name("variable");
  
  public static final hydra.core.Name FIELD_NAME_LABEL = new hydra.core.Name("label");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  public static final hydra.core.Name FIELD_NAME_EDGES = new hydra.core.Name("edges");
  
  public final hydra.util.Opt<hydra.pg.query.Variable> variable;
  
  public final hydra.util.Opt<hydra.pg.model.VertexLabel> label;
  
  public final java.util.List<hydra.pg.query.PropertyPattern> properties;
  
  public final java.util.List<hydra.pg.query.EdgeProjectionPattern> edges;
  
  public VertexPattern (hydra.util.Opt<hydra.pg.query.Variable> variable, hydra.util.Opt<hydra.pg.model.VertexLabel> label, java.util.List<hydra.pg.query.PropertyPattern> properties, java.util.List<hydra.pg.query.EdgeProjectionPattern> edges) {
    java.util.Objects.requireNonNull((variable));
    java.util.Objects.requireNonNull((label));
    java.util.Objects.requireNonNull((properties));
    java.util.Objects.requireNonNull((edges));
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
  
  public VertexPattern withVariable(hydra.util.Opt<hydra.pg.query.Variable> variable) {
    java.util.Objects.requireNonNull((variable));
    return new VertexPattern(variable, label, properties, edges);
  }
  
  public VertexPattern withLabel(hydra.util.Opt<hydra.pg.model.VertexLabel> label) {
    java.util.Objects.requireNonNull((label));
    return new VertexPattern(variable, label, properties, edges);
  }
  
  public VertexPattern withProperties(java.util.List<hydra.pg.query.PropertyPattern> properties) {
    java.util.Objects.requireNonNull((properties));
    return new VertexPattern(variable, label, properties, edges);
  }
  
  public VertexPattern withEdges(java.util.List<hydra.pg.query.EdgeProjectionPattern> edges) {
    java.util.Objects.requireNonNull((edges));
    return new VertexPattern(variable, label, properties, edges);
  }
}