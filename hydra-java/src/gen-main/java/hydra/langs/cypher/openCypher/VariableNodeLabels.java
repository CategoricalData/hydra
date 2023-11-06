package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class VariableNodeLabels implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.VariableNodeLabels");
  
  public final hydra.langs.cypher.openCypher.Variable variable;
  
  public final java.util.List<hydra.langs.cypher.openCypher.NodeLabel> labels;
  
  public VariableNodeLabels (hydra.langs.cypher.openCypher.Variable variable, java.util.List<hydra.langs.cypher.openCypher.NodeLabel> labels) {
    this.variable = variable;
    this.labels = labels;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableNodeLabels)) {
      return false;
    }
    VariableNodeLabels o = (VariableNodeLabels) (other);
    return variable.equals(o.variable) && labels.equals(o.labels);
  }
  
  @Override
  public int hashCode() {
    return 2 * variable.hashCode() + 3 * labels.hashCode();
  }
  
  public VariableNodeLabels withVariable(hydra.langs.cypher.openCypher.Variable variable) {
    return new VariableNodeLabels(variable, labels);
  }
  
  public VariableNodeLabels withLabels(java.util.List<hydra.langs.cypher.openCypher.NodeLabel> labels) {
    return new VariableNodeLabels(variable, labels);
  }
}