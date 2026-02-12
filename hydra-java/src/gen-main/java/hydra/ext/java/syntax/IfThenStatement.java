// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class IfThenStatement implements Serializable, Comparable<IfThenStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.IfThenStatement");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENT = new hydra.core.Name("statement");
  
  public final hydra.ext.java.syntax.Expression expression;
  
  public final hydra.ext.java.syntax.Statement statement;
  
  public IfThenStatement (hydra.ext.java.syntax.Expression expression, hydra.ext.java.syntax.Statement statement) {
    this.expression = expression;
    this.statement = statement;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IfThenStatement)) {
      return false;
    }
    IfThenStatement o = (IfThenStatement) other;
    return java.util.Objects.equals(
      this.expression,
      o.expression) && java.util.Objects.equals(
      this.statement,
      o.statement);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression) + 3 * java.util.Objects.hashCode(statement);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(IfThenStatement other) {
    int cmp = 0;
    cmp = ((Comparable) expression).compareTo(other.expression);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) statement).compareTo(other.statement);
  }
  
  public IfThenStatement withExpression(hydra.ext.java.syntax.Expression expression) {
    return new IfThenStatement(expression, statement);
  }
  
  public IfThenStatement withStatement(hydra.ext.java.syntax.Statement statement) {
    return new IfThenStatement(expression, statement);
  }
}
