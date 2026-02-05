// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A 'let' expression
 */
public class LetExpression implements Serializable, Comparable<LetExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.LetExpression");
  
  public static final hydra.core.Name FIELD_NAME_BINDINGS = new hydra.core.Name("bindings");
  
  public static final hydra.core.Name FIELD_NAME_INNER = new hydra.core.Name("inner");
  
  /**
   * The local bindings
   */
  public final java.util.List<hydra.ext.haskell.ast.LocalBinding> bindings;
  
  /**
   * The body of the let expression
   */
  public final hydra.ext.haskell.ast.Expression inner;
  
  public LetExpression (java.util.List<hydra.ext.haskell.ast.LocalBinding> bindings, hydra.ext.haskell.ast.Expression inner) {
    this.bindings = bindings;
    this.inner = inner;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetExpression)) {
      return false;
    }
    LetExpression o = (LetExpression) (other);
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
  public int compareTo(LetExpression other) {
    int cmp = 0;
    cmp = Integer.compare(
      bindings.hashCode(),
      other.bindings.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (inner)).compareTo(other.inner);
  }
  
  public LetExpression withBindings(java.util.List<hydra.ext.haskell.ast.LocalBinding> bindings) {
    return new LetExpression(bindings, inner);
  }
  
  public LetExpression withInner(hydra.ext.haskell.ast.Expression inner) {
    return new LetExpression(bindings, inner);
  }
}
