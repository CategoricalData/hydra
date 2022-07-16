package hydra.ext.java.syntax;

public class SwitchBlock_Pair {
  public final java.util.List<SwitchBlockStatementGroup> statements;
  
  public final java.util.List<SwitchLabel> labels;
  
  public SwitchBlock_Pair (java.util.List<SwitchBlockStatementGroup> statements, java.util.List<SwitchLabel> labels) {
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
  
  public SwitchBlock_Pair withStatements(java.util.List<SwitchBlockStatementGroup> statements) {
    return new SwitchBlock_Pair(statements, labels);
  }
  
  public SwitchBlock_Pair withLabels(java.util.List<SwitchLabel> labels) {
    return new SwitchBlock_Pair(statements, labels);
  }
}