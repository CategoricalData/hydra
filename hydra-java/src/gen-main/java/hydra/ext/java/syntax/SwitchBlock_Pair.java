// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class SwitchBlock_Pair implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.SwitchBlock.Pair");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENTS = new hydra.core.Name("statements");
  
  public static final hydra.core.Name FIELD_NAME_LABELS = new hydra.core.Name("labels");
  
  public final java.util.List<hydra.ext.java.syntax.SwitchBlockStatementGroup> statements;
  
  public final java.util.List<hydra.ext.java.syntax.SwitchLabel> labels;
  
  public SwitchBlock_Pair (java.util.List<hydra.ext.java.syntax.SwitchBlockStatementGroup> statements, java.util.List<hydra.ext.java.syntax.SwitchLabel> labels) {
    java.util.Objects.requireNonNull((statements));
    java.util.Objects.requireNonNull((labels));
    this.statements = statements;
    this.labels = labels;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SwitchBlock_Pair)) {
      return false;
    }
    SwitchBlock_Pair o = (SwitchBlock_Pair) (other);
    return statements.equals(o.statements) && labels.equals(o.labels);
  }
  
  @Override
  public int hashCode() {
    return 2 * statements.hashCode() + 3 * labels.hashCode();
  }
  
  public SwitchBlock_Pair withStatements(java.util.List<hydra.ext.java.syntax.SwitchBlockStatementGroup> statements) {
    java.util.Objects.requireNonNull((statements));
    return new SwitchBlock_Pair(statements, labels);
  }
  
  public SwitchBlock_Pair withLabels(java.util.List<hydra.ext.java.syntax.SwitchLabel> labels) {
    java.util.Objects.requireNonNull((labels));
    return new SwitchBlock_Pair(statements, labels);
  }
}
