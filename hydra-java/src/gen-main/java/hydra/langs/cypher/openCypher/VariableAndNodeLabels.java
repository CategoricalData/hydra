// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class VariableAndNodeLabels implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.VariableAndNodeLabels");
  
  public final hydra.langs.cypher.openCypher.Variable variable;
  
  public final hydra.langs.cypher.openCypher.NodeLabels labels;
  
  public VariableAndNodeLabels (hydra.langs.cypher.openCypher.Variable variable, hydra.langs.cypher.openCypher.NodeLabels labels) {
    if (variable == null) {
      throw new IllegalArgumentException("null value for 'variable' argument");
    }
    if (labels == null) {
      throw new IllegalArgumentException("null value for 'labels' argument");
    }
    this.variable = variable;
    this.labels = labels;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableAndNodeLabels)) {
      return false;
    }
    VariableAndNodeLabels o = (VariableAndNodeLabels) (other);
    return variable.equals(o.variable) && labels.equals(o.labels);
  }
  
  @Override
  public int hashCode() {
    return 2 * variable.hashCode() + 3 * labels.hashCode();
  }
  
  public VariableAndNodeLabels withVariable(hydra.langs.cypher.openCypher.Variable variable) {
    if (variable == null) {
      throw new IllegalArgumentException("null value for 'variable' argument");
    }
    return new VariableAndNodeLabels(variable, labels);
  }
  
  public VariableAndNodeLabels withLabels(hydra.langs.cypher.openCypher.NodeLabels labels) {
    if (labels == null) {
      throw new IllegalArgumentException("null value for 'labels' argument");
    }
    return new VariableAndNodeLabels(variable, labels);
  }
}