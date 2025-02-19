// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class IfStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.IfStatement");
  
  public static final hydra.core.Name FIELD_NAME_CONDITION = new hydra.core.Name("condition");
  
  public static final hydra.core.Name FIELD_NAME_IF_BRANCH = new hydra.core.Name("ifBranch");
  
  public static final hydra.core.Name FIELD_NAME_ELSE_BRANCH = new hydra.core.Name("elseBranch");
  
  public final hydra.ext.csharp.syntax.BooleanExpression condition;
  
  public final hydra.ext.csharp.syntax.EmbeddedStatement ifBranch;
  
  public final hydra.ext.csharp.syntax.EmbeddedStatement elseBranch;
  
  public IfStatement (hydra.ext.csharp.syntax.BooleanExpression condition, hydra.ext.csharp.syntax.EmbeddedStatement ifBranch, hydra.ext.csharp.syntax.EmbeddedStatement elseBranch) {
    java.util.Objects.requireNonNull((condition));
    java.util.Objects.requireNonNull((ifBranch));
    java.util.Objects.requireNonNull((elseBranch));
    this.condition = condition;
    this.ifBranch = ifBranch;
    this.elseBranch = elseBranch;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IfStatement)) {
      return false;
    }
    IfStatement o = (IfStatement) (other);
    return condition.equals(o.condition) && ifBranch.equals(o.ifBranch) && elseBranch.equals(o.elseBranch);
  }
  
  @Override
  public int hashCode() {
    return 2 * condition.hashCode() + 3 * ifBranch.hashCode() + 5 * elseBranch.hashCode();
  }
  
  public IfStatement withCondition(hydra.ext.csharp.syntax.BooleanExpression condition) {
    java.util.Objects.requireNonNull((condition));
    return new IfStatement(condition, ifBranch, elseBranch);
  }
  
  public IfStatement withIfBranch(hydra.ext.csharp.syntax.EmbeddedStatement ifBranch) {
    java.util.Objects.requireNonNull((ifBranch));
    return new IfStatement(condition, ifBranch, elseBranch);
  }
  
  public IfStatement withElseBranch(hydra.ext.csharp.syntax.EmbeddedStatement elseBranch) {
    java.util.Objects.requireNonNull((elseBranch));
    return new IfStatement(condition, ifBranch, elseBranch);
  }
}