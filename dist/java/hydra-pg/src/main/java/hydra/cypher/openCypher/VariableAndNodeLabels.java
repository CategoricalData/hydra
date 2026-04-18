// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class VariableAndNodeLabels implements Serializable, Comparable<VariableAndNodeLabels> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.VariableAndNodeLabels");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public static final hydra.core.Name LABELS = new hydra.core.Name("labels");

  public final hydra.cypher.openCypher.Variable variable;

  public final hydra.cypher.openCypher.NodeLabels labels;

  public VariableAndNodeLabels (hydra.cypher.openCypher.Variable variable, hydra.cypher.openCypher.NodeLabels labels) {
    this.variable = variable;
    this.labels = labels;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableAndNodeLabels)) {
      return false;
    }
    VariableAndNodeLabels o = (VariableAndNodeLabels) other;
    return java.util.Objects.equals(
      this.variable,
      o.variable) && java.util.Objects.equals(
      this.labels,
      o.labels);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variable) + 3 * java.util.Objects.hashCode(labels);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VariableAndNodeLabels other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      variable,
      other.variable);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      labels,
      other.labels);
  }

  public VariableAndNodeLabels withVariable(hydra.cypher.openCypher.Variable variable) {
    return new VariableAndNodeLabels(variable, labels);
  }

  public VariableAndNodeLabels withLabels(hydra.cypher.openCypher.NodeLabels labels) {
    return new VariableAndNodeLabels(variable, labels);
  }
}
