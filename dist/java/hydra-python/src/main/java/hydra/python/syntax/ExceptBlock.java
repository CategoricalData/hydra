// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class ExceptBlock implements Serializable, Comparable<ExceptBlock> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.ExceptBlock");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.util.Maybe<hydra.python.syntax.ExceptExpression> expression;

  public final hydra.python.syntax.Block body;

  public ExceptBlock (hydra.util.Maybe<hydra.python.syntax.ExceptExpression> expression, hydra.python.syntax.Block body) {
    this.expression = expression;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExceptBlock)) {
      return false;
    }
    ExceptBlock o = (ExceptBlock) other;
    return java.util.Objects.equals(
      this.expression,
      o.expression) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression) + 3 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ExceptBlock other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      expression,
      other.expression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public ExceptBlock withExpression(hydra.util.Maybe<hydra.python.syntax.ExceptExpression> expression) {
    return new ExceptBlock(expression, body);
  }

  public ExceptBlock withBody(hydra.python.syntax.Block body) {
    return new ExceptBlock(expression, body);
  }
}
