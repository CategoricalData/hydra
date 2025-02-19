// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A lambda expression
 */
public class LambdaExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.LambdaExpression");
  
  public static final hydra.core.Name FIELD_NAME_BINDINGS = new hydra.core.Name("bindings");
  
  public static final hydra.core.Name FIELD_NAME_INNER = new hydra.core.Name("inner");
  
  public final java.util.List<hydra.ext.haskell.ast.Pattern> bindings;
  
  public final hydra.ext.haskell.ast.Expression inner;
  
  public LambdaExpression (java.util.List<hydra.ext.haskell.ast.Pattern> bindings, hydra.ext.haskell.ast.Expression inner) {
    java.util.Objects.requireNonNull((bindings));
    java.util.Objects.requireNonNull((inner));
    this.bindings = bindings;
    this.inner = inner;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LambdaExpression)) {
      return false;
    }
    LambdaExpression o = (LambdaExpression) (other);
    return bindings.equals(o.bindings) && inner.equals(o.inner);
  }
  
  @Override
  public int hashCode() {
    return 2 * bindings.hashCode() + 3 * inner.hashCode();
  }
  
  public LambdaExpression withBindings(java.util.List<hydra.ext.haskell.ast.Pattern> bindings) {
    java.util.Objects.requireNonNull((bindings));
    return new LambdaExpression(bindings, inner);
  }
  
  public LambdaExpression withInner(hydra.ext.haskell.ast.Expression inner) {
    java.util.Objects.requireNonNull((inner));
    return new LambdaExpression(bindings, inner);
  }
}