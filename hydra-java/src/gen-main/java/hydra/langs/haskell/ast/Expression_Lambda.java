// Note: this is an automatically generated file. Do not edit.

package hydra.langs.haskell.ast;

import java.io.Serializable;

/**
 * A lambda expression
 */
public class Expression_Lambda implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/haskell/ast.Expression.Lambda");
  
  public final java.util.List<hydra.langs.haskell.ast.Pattern> bindings;
  
  public final hydra.langs.haskell.ast.Expression inner;
  
  public Expression_Lambda (java.util.List<hydra.langs.haskell.ast.Pattern> bindings, hydra.langs.haskell.ast.Expression inner) {
    if (bindings == null) {
      throw new IllegalArgumentException("null value for 'bindings' argument");
    }
    if (inner == null) {
      throw new IllegalArgumentException("null value for 'inner' argument");
    }
    this.bindings = bindings;
    this.inner = inner;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Expression_Lambda)) {
      return false;
    }
    Expression_Lambda o = (Expression_Lambda) (other);
    return bindings.equals(o.bindings) && inner.equals(o.inner);
  }
  
  @Override
  public int hashCode() {
    return 2 * bindings.hashCode() + 3 * inner.hashCode();
  }
  
  public Expression_Lambda withBindings(java.util.List<hydra.langs.haskell.ast.Pattern> bindings) {
    if (bindings == null) {
      throw new IllegalArgumentException("null value for 'bindings' argument");
    }
    return new Expression_Lambda(bindings, inner);
  }
  
  public Expression_Lambda withInner(hydra.langs.haskell.ast.Expression inner) {
    if (inner == null) {
      throw new IllegalArgumentException("null value for 'inner' argument");
    }
    return new Expression_Lambda(bindings, inner);
  }
}