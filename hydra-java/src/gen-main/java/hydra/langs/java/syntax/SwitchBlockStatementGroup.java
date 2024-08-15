// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class SwitchBlockStatementGroup implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.SwitchBlockStatementGroup");
  
  public static final hydra.core.Name FIELD_NAME_LABELS = new hydra.core.Name("labels");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENTS = new hydra.core.Name("statements");
  
  public final java.util.List<hydra.langs.java.syntax.SwitchLabel> labels;
  
  public final java.util.List<hydra.langs.java.syntax.BlockStatement> statements;
  
  public SwitchBlockStatementGroup (java.util.List<hydra.langs.java.syntax.SwitchLabel> labels, java.util.List<hydra.langs.java.syntax.BlockStatement> statements) {
    java.util.Objects.requireNonNull((labels));
    java.util.Objects.requireNonNull((statements));
    this.labels = labels;
    this.statements = statements;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SwitchBlockStatementGroup)) {
      return false;
    }
    SwitchBlockStatementGroup o = (SwitchBlockStatementGroup) (other);
    return labels.equals(o.labels) && statements.equals(o.statements);
  }
  
  @Override
  public int hashCode() {
    return 2 * labels.hashCode() + 3 * statements.hashCode();
  }
  
  public SwitchBlockStatementGroup withLabels(java.util.List<hydra.langs.java.syntax.SwitchLabel> labels) {
    java.util.Objects.requireNonNull((labels));
    return new SwitchBlockStatementGroup(labels, statements);
  }
  
  public SwitchBlockStatementGroup withStatements(java.util.List<hydra.langs.java.syntax.BlockStatement> statements) {
    java.util.Objects.requireNonNull((statements));
    return new SwitchBlockStatementGroup(labels, statements);
  }
}