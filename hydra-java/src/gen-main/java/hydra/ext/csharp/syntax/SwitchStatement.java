// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class SwitchStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.SwitchStatement");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_BRANCHES = new hydra.core.Name("branches");
  
  public final hydra.ext.csharp.syntax.Expression expression;
  
  public final java.util.List<hydra.ext.csharp.syntax.SwitchSection> branches;
  
  public SwitchStatement (hydra.ext.csharp.syntax.Expression expression, java.util.List<hydra.ext.csharp.syntax.SwitchSection> branches) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((branches));
    this.expression = expression;
    this.branches = branches;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SwitchStatement)) {
      return false;
    }
    SwitchStatement o = (SwitchStatement) (other);
    return expression.equals(o.expression) && branches.equals(o.branches);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * branches.hashCode();
  }
  
  public SwitchStatement withExpression(hydra.ext.csharp.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new SwitchStatement(expression, branches);
  }
  
  public SwitchStatement withBranches(java.util.List<hydra.ext.csharp.syntax.SwitchSection> branches) {
    java.util.Objects.requireNonNull((branches));
    return new SwitchStatement(expression, branches);
  }
}