// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class SwitchBlock_Pair implements Serializable, Comparable<SwitchBlock_Pair> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.SwitchBlock_Pair");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENTS = new hydra.core.Name("statements");
  
  public static final hydra.core.Name FIELD_NAME_LABELS = new hydra.core.Name("labels");
  
  public final java.util.List<hydra.ext.java.syntax.SwitchBlockStatementGroup> statements;
  
  public final java.util.List<hydra.ext.java.syntax.SwitchLabel> labels;
  
  public SwitchBlock_Pair (java.util.List<hydra.ext.java.syntax.SwitchBlockStatementGroup> statements, java.util.List<hydra.ext.java.syntax.SwitchLabel> labels) {
    this.statements = statements;
    this.labels = labels;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SwitchBlock_Pair)) {
      return false;
    }
    SwitchBlock_Pair o = (SwitchBlock_Pair) other;
    return java.util.Objects.equals(
      this.statements,
      o.statements) && java.util.Objects.equals(
      this.labels,
      o.labels);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(statements) + 3 * java.util.Objects.hashCode(labels);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SwitchBlock_Pair other) {
    int cmp = 0;
    cmp = Integer.compare(
      statements.hashCode(),
      other.statements.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      labels.hashCode(),
      other.labels.hashCode());
  }
  
  public SwitchBlock_Pair withStatements(java.util.List<hydra.ext.java.syntax.SwitchBlockStatementGroup> statements) {
    return new SwitchBlock_Pair(statements, labels);
  }
  
  public SwitchBlock_Pair withLabels(java.util.List<hydra.ext.java.syntax.SwitchLabel> labels) {
    return new SwitchBlock_Pair(statements, labels);
  }
}
