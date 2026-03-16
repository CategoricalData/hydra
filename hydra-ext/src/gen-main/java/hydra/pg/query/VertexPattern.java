// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class VertexPattern implements Serializable, Comparable<VertexPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.query.VertexPattern");
  
  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");
  
  public static final hydra.core.Name LABEL = new hydra.core.Name("label");
  
  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");
  
  public static final hydra.core.Name EDGES = new hydra.core.Name("edges");
  
  public final hydra.util.Maybe<hydra.pg.query.Variable> variable;
  
  public final hydra.util.Maybe<hydra.pg.model.VertexLabel> label;
  
  public final hydra.util.ConsList<hydra.pg.query.PropertyPattern> properties;
  
  public final hydra.util.ConsList<hydra.pg.query.EdgeProjectionPattern> edges;
  
  public VertexPattern (hydra.util.Maybe<hydra.pg.query.Variable> variable, hydra.util.Maybe<hydra.pg.model.VertexLabel> label, hydra.util.ConsList<hydra.pg.query.PropertyPattern> properties, hydra.util.ConsList<hydra.pg.query.EdgeProjectionPattern> edges) {
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
    VertexPattern o = (VertexPattern) other;
    return java.util.Objects.equals(
      this.variable,
      o.variable) && java.util.Objects.equals(
      this.label,
      o.label) && java.util.Objects.equals(
      this.properties,
      o.properties) && java.util.Objects.equals(
      this.edges,
      o.edges);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variable) + 3 * java.util.Objects.hashCode(label) + 5 * java.util.Objects.hashCode(properties) + 7 * java.util.Objects.hashCode(edges);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VertexPattern other) {
    int cmp = 0;
    cmp = ((Comparable) variable).compareTo(other.variable);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) label).compareTo(other.label);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) properties).compareTo(other.properties);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) edges).compareTo(other.edges);
  }
  
  public VertexPattern withVariable(hydra.util.Maybe<hydra.pg.query.Variable> variable) {
    return new VertexPattern(variable, label, properties, edges);
  }
  
  public VertexPattern withLabel(hydra.util.Maybe<hydra.pg.model.VertexLabel> label) {
    return new VertexPattern(variable, label, properties, edges);
  }
  
  public VertexPattern withProperties(hydra.util.ConsList<hydra.pg.query.PropertyPattern> properties) {
    return new VertexPattern(variable, label, properties, edges);
  }
  
  public VertexPattern withEdges(hydra.util.ConsList<hydra.pg.query.EdgeProjectionPattern> edges) {
    return new VertexPattern(variable, label, properties, edges);
  }
}
