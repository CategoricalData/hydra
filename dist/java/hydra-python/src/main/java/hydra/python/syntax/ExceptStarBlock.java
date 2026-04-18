// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class ExceptStarBlock implements Serializable, Comparable<ExceptStarBlock> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.ExceptStarBlock");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public static final hydra.core.Name AS = new hydra.core.Name("as");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.python.syntax.Expression expression;

  public final hydra.util.Maybe<hydra.python.syntax.Name> as;

  public final hydra.python.syntax.Block body;

  public ExceptStarBlock (hydra.python.syntax.Expression expression, hydra.util.Maybe<hydra.python.syntax.Name> as, hydra.python.syntax.Block body) {
    this.expression = expression;
    this.as = as;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExceptStarBlock)) {
      return false;
    }
    ExceptStarBlock o = (ExceptStarBlock) other;
    return java.util.Objects.equals(
      this.expression,
      o.expression) && java.util.Objects.equals(
      this.as,
      o.as) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression) + 3 * java.util.Objects.hashCode(as) + 5 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ExceptStarBlock other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      expression,
      other.expression);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      as,
      other.as);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public ExceptStarBlock withExpression(hydra.python.syntax.Expression expression) {
    return new ExceptStarBlock(expression, as, body);
  }

  public ExceptStarBlock withAs(hydra.util.Maybe<hydra.python.syntax.Name> as) {
    return new ExceptStarBlock(expression, as, body);
  }

  public ExceptStarBlock withBody(hydra.python.syntax.Block body) {
    return new ExceptStarBlock(expression, as, body);
  }
}
