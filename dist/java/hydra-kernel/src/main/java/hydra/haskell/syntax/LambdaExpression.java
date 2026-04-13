// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A lambda expression
 */
public class LambdaExpression implements Serializable, Comparable<LambdaExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.LambdaExpression");

  public static final hydra.core.Name BINDINGS = new hydra.core.Name("bindings");

  public static final hydra.core.Name INNER = new hydra.core.Name("inner");

  /**
   * The patterns binding parameters
   */
  public final java.util.List<hydra.haskell.syntax.Pattern> bindings;

  /**
   * The body of the lambda
   */
  public final hydra.haskell.syntax.Expression inner;

  public LambdaExpression (java.util.List<hydra.haskell.syntax.Pattern> bindings, hydra.haskell.syntax.Expression inner) {
    this.bindings = bindings;
    this.inner = inner;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LambdaExpression)) {
      return false;
    }
    LambdaExpression o = (LambdaExpression) other;
    return java.util.Objects.equals(
      this.bindings,
      o.bindings) && java.util.Objects.equals(
      this.inner,
      o.inner);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(bindings) + 3 * java.util.Objects.hashCode(inner);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LambdaExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      bindings,
      other.bindings);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      inner,
      other.inner);
  }

  public LambdaExpression withBindings(java.util.List<hydra.haskell.syntax.Pattern> bindings) {
    return new LambdaExpression(bindings, inner);
  }

  public LambdaExpression withInner(hydra.haskell.syntax.Expression inner) {
    return new LambdaExpression(bindings, inner);
  }
}
