// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class NodePattern implements Serializable, Comparable<NodePattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.NodePattern");
  
  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");
  
  public static final hydra.core.Name LABELS = new hydra.core.Name("labels");
  
  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");
  
  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Variable> variable;
  
  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.NodeLabels> labels;
  
  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Properties> properties;
  
  public NodePattern (hydra.util.Maybe<hydra.ext.cypher.openCypher.Variable> variable, hydra.util.Maybe<hydra.ext.cypher.openCypher.NodeLabels> labels, hydra.util.Maybe<hydra.ext.cypher.openCypher.Properties> properties) {
    this.variable = variable;
    this.labels = labels;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodePattern)) {
      return false;
    }
    NodePattern o = (NodePattern) other;
    return java.util.Objects.equals(
      this.variable,
      o.variable) && java.util.Objects.equals(
      this.labels,
      o.labels) && java.util.Objects.equals(
      this.properties,
      o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variable) + 3 * java.util.Objects.hashCode(labels) + 5 * java.util.Objects.hashCode(properties);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NodePattern other) {
    int cmp = 0;
    cmp = Integer.compare(
      variable.hashCode(),
      other.variable.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      labels.hashCode(),
      other.labels.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      properties.hashCode(),
      other.properties.hashCode());
  }
  
  public NodePattern withVariable(hydra.util.Maybe<hydra.ext.cypher.openCypher.Variable> variable) {
    return new NodePattern(variable, labels, properties);
  }
  
  public NodePattern withLabels(hydra.util.Maybe<hydra.ext.cypher.openCypher.NodeLabels> labels) {
    return new NodePattern(variable, labels, properties);
  }
  
  public NodePattern withProperties(hydra.util.Maybe<hydra.ext.cypher.openCypher.Properties> properties) {
    return new NodePattern(variable, labels, properties);
  }
}
