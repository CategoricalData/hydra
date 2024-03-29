package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class NodePattern implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.NodePattern");
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Variable> variable;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.NodeLabels> labels;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Properties> properties;
  
  public NodePattern (java.util.Optional<hydra.langs.cypher.openCypher.Variable> variable, java.util.Optional<hydra.langs.cypher.openCypher.NodeLabels> labels, java.util.Optional<hydra.langs.cypher.openCypher.Properties> properties) {
    this.variable = variable;
    this.labels = labels;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodePattern)) {
      return false;
    }
    NodePattern o = (NodePattern) (other);
    return variable.equals(o.variable) && labels.equals(o.labels) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * variable.hashCode() + 3 * labels.hashCode() + 5 * properties.hashCode();
  }
  
  public NodePattern withVariable(java.util.Optional<hydra.langs.cypher.openCypher.Variable> variable) {
    return new NodePattern(variable, labels, properties);
  }
  
  public NodePattern withLabels(java.util.Optional<hydra.langs.cypher.openCypher.NodeLabels> labels) {
    return new NodePattern(variable, labels, properties);
  }
  
  public NodePattern withProperties(java.util.Optional<hydra.langs.cypher.openCypher.Properties> properties) {
    return new NodePattern(variable, labels, properties);
  }
}