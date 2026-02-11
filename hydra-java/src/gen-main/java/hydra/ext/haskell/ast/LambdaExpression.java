// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A lambda expression
 */
public class LambdaExpression implements Serializable, Comparable<LambdaExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.LambdaExpression");
  
  public static final hydra.core.Name FIELD_NAME_BINDINGS = new hydra.core.Name("bindings");
  
  public static final hydra.core.Name FIELD_NAME_INNER = new hydra.core.Name("inner");
  
  /**
   * The patterns binding parameters
   */
  public final java.util.List<hydra.ext.haskell.ast.Pattern> bindings;
  
  /**
   * The body of the lambda
   */
  public final hydra.ext.haskell.ast.Expression inner;
  
  public LambdaExpression (java.util.List<hydra.ext.haskell.ast.Pattern> bindings, hydra.ext.haskell.ast.Expression inner) {
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
    cmp = Integer.compare(
      bindings.hashCode(),
      other.bindings.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) inner).compareTo(other.inner);
  }
  
  public LambdaExpression withBindings(java.util.List<hydra.ext.haskell.ast.Pattern> bindings) {
    return new LambdaExpression(bindings, inner);
  }
  
  public LambdaExpression withInner(hydra.ext.haskell.ast.Expression inner) {
    return new LambdaExpression(bindings, inner);
  }
}
