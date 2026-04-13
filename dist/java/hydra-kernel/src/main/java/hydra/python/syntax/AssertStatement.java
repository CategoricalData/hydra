// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class AssertStatement implements Serializable, Comparable<AssertStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.AssertStatement");

  public static final hydra.core.Name EXPRESSION1 = new hydra.core.Name("expression1");

  public static final hydra.core.Name EXPRESSION2 = new hydra.core.Name("expression2");

  public final hydra.python.syntax.Expression expression1;

  public final hydra.util.Maybe<hydra.python.syntax.Expression> expression2;

  public AssertStatement (hydra.python.syntax.Expression expression1, hydra.util.Maybe<hydra.python.syntax.Expression> expression2) {
    this.expression1 = expression1;
    this.expression2 = expression2;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AssertStatement)) {
      return false;
    }
    AssertStatement o = (AssertStatement) other;
    return java.util.Objects.equals(
      this.expression1,
      o.expression1) && java.util.Objects.equals(
      this.expression2,
      o.expression2);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression1) + 3 * java.util.Objects.hashCode(expression2);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AssertStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      expression1,
      other.expression1);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expression2,
      other.expression2);
  }

  public AssertStatement withExpression1(hydra.python.syntax.Expression expression1) {
    return new AssertStatement(expression1, expression2);
  }

  public AssertStatement withExpression2(hydra.util.Maybe<hydra.python.syntax.Expression> expression2) {
    return new AssertStatement(expression1, expression2);
  }
}
