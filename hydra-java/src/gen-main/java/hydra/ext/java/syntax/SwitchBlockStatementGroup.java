package hydra.ext.java.syntax;

public class SwitchBlockStatementGroup {
  public final java.util.List<SwitchLabel> labels;
  
  public final java.util.List<BlockStatement> statements;
  
  public SwitchBlockStatementGroup (java.util.List<SwitchLabel> labels, java.util.List<BlockStatement> statements) {
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
  
  public SwitchBlockStatementGroup withLabels(java.util.List<SwitchLabel> labels) {
    return new SwitchBlockStatementGroup(labels, statements);
  }
  
  public SwitchBlockStatementGroup withStatements(java.util.List<BlockStatement> statements) {
    return new SwitchBlockStatementGroup(labels, statements);
  }
}