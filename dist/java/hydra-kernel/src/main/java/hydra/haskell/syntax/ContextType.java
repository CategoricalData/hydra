// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A type with a context (type class constraints)
 */
public class ContextType implements Serializable, Comparable<ContextType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.ContextType");

  public static final hydra.core.Name CTX = new hydra.core.Name("ctx");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  /**
   * The type class context
   */
  public final hydra.haskell.syntax.Assertion ctx;

  /**
   * The constrained type
   */
  public final hydra.haskell.syntax.Type type;

  public ContextType (hydra.haskell.syntax.Assertion ctx, hydra.haskell.syntax.Type type) {
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
    cmp = hydra.util.Comparing.compare(
      ctx,
      other.ctx);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      type,
      other.type);
  }

  public ContextType withCtx(hydra.haskell.syntax.Assertion ctx) {
    return new ContextType(ctx, type);
  }

  public ContextType withType(hydra.haskell.syntax.Type type) {
    return new ContextType(ctx, type);
  }
}
