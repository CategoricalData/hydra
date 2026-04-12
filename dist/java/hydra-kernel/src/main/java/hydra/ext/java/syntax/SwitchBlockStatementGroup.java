// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class SwitchBlockStatementGroup implements Serializable, Comparable<SwitchBlockStatementGroup> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.java.syntax.SwitchBlockStatementGroup");

  public static final hydra.core.Name LABELS = new hydra.core.Name("labels");

  public static final hydra.core.Name STATEMENTS = new hydra.core.Name("statements");

  public final java.util.List<hydra.ext.java.syntax.SwitchLabel> labels;

  public final java.util.List<hydra.ext.java.syntax.BlockStatement> statements;

  public SwitchBlockStatementGroup (java.util.List<hydra.ext.java.syntax.SwitchLabel> labels, java.util.List<hydra.ext.java.syntax.BlockStatement> statements) {
    this.labels = labels;
    this.statements = statements;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SwitchBlockStatementGroup)) {
      return false;
    }
    SwitchBlockStatementGroup o = (SwitchBlockStatementGroup) other;
    return java.util.Objects.equals(
      this.labels,
      o.labels) && java.util.Objects.equals(
      this.statements,
      o.statements);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(labels) + 3 * java.util.Objects.hashCode(statements);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SwitchBlockStatementGroup other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      labels,
      other.labels);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      statements,
      other.statements);
  }

  public SwitchBlockStatementGroup withLabels(java.util.List<hydra.ext.java.syntax.SwitchLabel> labels) {
    return new SwitchBlockStatementGroup(labels, statements);
  }

  public SwitchBlockStatementGroup withStatements(java.util.List<hydra.ext.java.syntax.BlockStatement> statements) {
    return new SwitchBlockStatementGroup(labels, statements);
  }
}
