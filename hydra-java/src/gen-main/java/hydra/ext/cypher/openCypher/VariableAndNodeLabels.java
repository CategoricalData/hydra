// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class VariableAndNodeLabels implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.VariableAndNodeLabels");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE = new hydra.core.Name("variable");
  
  public static final hydra.core.Name FIELD_NAME_LABELS = new hydra.core.Name("labels");
  
  public final hydra.ext.cypher.openCypher.Variable variable;
  
  public final hydra.ext.cypher.openCypher.NodeLabels labels;
  
  public VariableAndNodeLabels (hydra.ext.cypher.openCypher.Variable variable, hydra.ext.cypher.openCypher.NodeLabels labels) {
    java.util.Objects.requireNonNull((variable));
    java.util.Objects.requireNonNull((labels));
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
  
  public VariableAndNodeLabels withVariable(hydra.ext.cypher.openCypher.Variable variable) {
    java.util.Objects.requireNonNull((variable));
    return new VariableAndNodeLabels(variable, labels);
  }
  
  public VariableAndNodeLabels withLabels(hydra.ext.cypher.openCypher.NodeLabels labels) {
    java.util.Objects.requireNonNull((labels));
    return new VariableAndNodeLabels(variable, labels);
  }
}