// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class IfThenStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.IfThenStatement");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENT = new hydra.core.Name("statement");
  
  public final hydra.ext.java.syntax.Expression expression;
  
  public final hydra.ext.java.syntax.Statement statement;
  
  public IfThenStatement (hydra.ext.java.syntax.Expression expression, hydra.ext.java.syntax.Statement statement) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((statement));
    this.expression = expression;
    this.statement = statement;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IfThenStatement)) {
      return false;
    }
    IfThenStatement o = (IfThenStatement) (other);
    return expression.equals(o.expression) && statement.equals(o.statement);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * statement.hashCode();
  }
  
  public IfThenStatement withExpression(hydra.ext.java.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new IfThenStatement(expression, statement);
  }
  
  public IfThenStatement withStatement(hydra.ext.java.syntax.Statement statement) {
    java.util.Objects.requireNonNull((statement));
    return new IfThenStatement(expression, statement);
  }
}