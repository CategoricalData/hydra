// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A type with a context (type class constraints)
 */
public class ContextType implements Serializable, Comparable<ContextType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.ContextType");
  
  public static final hydra.core.Name FIELD_NAME_CTX = new hydra.core.Name("ctx");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  /**
   * The type class context
   */
  public final hydra.ext.haskell.ast.Assertion ctx;
  
  /**
   * The constrained type
   */
  public final hydra.ext.haskell.ast.Type type;
  
  public ContextType (hydra.ext.haskell.ast.Assertion ctx, hydra.ext.haskell.ast.Type type) {
    this.ctx = ctx;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ContextType)) {
      return false;
    }
    ContextType o = (ContextType) other;
    return java.util.Objects.equals(
      this.ctx,
      o.ctx) && java.util.Objects.equals(
      this.type,
      o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(ctx) + 3 * java.util.Objects.hashCode(type);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ContextType other) {
    int cmp = 0;
    cmp = ((Comparable) ctx).compareTo(other.ctx);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) type).compareTo(other.type);
  }
  
  public ContextType withCtx(hydra.ext.haskell.ast.Assertion ctx) {
    return new ContextType(ctx, type);
  }
  
  public ContextType withType(hydra.ext.haskell.ast.Type type) {
    return new ContextType(ctx, type);
  }
}
