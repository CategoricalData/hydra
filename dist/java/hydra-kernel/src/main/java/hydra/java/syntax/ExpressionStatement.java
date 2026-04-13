// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class ExpressionStatement implements Serializable, Comparable<ExpressionStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.ExpressionStatement");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.java.syntax.StatementExpression value;

  public ExpressionStatement (hydra.java.syntax.StatementExpression value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExpressionStatement)) {
      return false;
    }
    ExpressionStatement o = (ExpressionStatement) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ExpressionStatement other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
