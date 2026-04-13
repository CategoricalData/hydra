// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class IfThenStatement implements Serializable, Comparable<IfThenStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.IfThenStatement");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public static final hydra.core.Name STATEMENT = new hydra.core.Name("statement");

  public final hydra.java.syntax.Expression expression;

  public final hydra.java.syntax.Statement statement;

  public IfThenStatement (hydra.java.syntax.Expression expression, hydra.java.syntax.Statement statement) {
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
    cmp = hydra.util.Comparing.compare(
      expression,
      other.expression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      statement,
      other.statement);
  }

  public IfThenStatement withExpression(hydra.java.syntax.Expression expression) {
    return new IfThenStatement(expression, statement);
  }

  public IfThenStatement withStatement(hydra.java.syntax.Statement statement) {
    return new IfThenStatement(expression, statement);
  }
}
