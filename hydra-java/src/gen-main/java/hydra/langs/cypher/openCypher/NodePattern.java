// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class NodePattern implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.NodePattern");
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Variable> variable;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.NodeLabels> labels;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Properties> properties;
  
  public NodePattern (hydra.util.Opt<hydra.langs.cypher.openCypher.Variable> variable, hydra.util.Opt<hydra.langs.cypher.openCypher.NodeLabels> labels, hydra.util.Opt<hydra.langs.cypher.openCypher.Properties> properties) {
    java.util.Objects.requireNonNull((variable));
    java.util.Objects.requireNonNull((labels));
    java.util.Objects.requireNonNull((properties));
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
  
  public NodePattern withVariable(hydra.util.Opt<hydra.langs.cypher.openCypher.Variable> variable) {
    java.util.Objects.requireNonNull((variable));
    return new NodePattern(variable, labels, properties);
  }
  
  public NodePattern withLabels(hydra.util.Opt<hydra.langs.cypher.openCypher.NodeLabels> labels) {
    java.util.Objects.requireNonNull((labels));
    return new NodePattern(variable, labels, properties);
  }
  
  public NodePattern withProperties(hydra.util.Opt<hydra.langs.cypher.openCypher.Properties> properties) {
    java.util.Objects.requireNonNull((properties));
    return new NodePattern(variable, labels, properties);
  }
}