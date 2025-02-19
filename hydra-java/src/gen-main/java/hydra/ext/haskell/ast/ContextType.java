// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public class ContextType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.ContextType");
  
  public static final hydra.core.Name FIELD_NAME_CTX = new hydra.core.Name("ctx");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public final hydra.ext.haskell.ast.Assertion ctx;
  
  public final hydra.ext.haskell.ast.Type type;
  
  public ContextType (hydra.ext.haskell.ast.Assertion ctx, hydra.ext.haskell.ast.Type type) {
    java.util.Objects.requireNonNull((ctx));
    java.util.Objects.requireNonNull((type));
    this.ctx = ctx;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ContextType)) {
      return false;
    }
    ContextType o = (ContextType) (other);
    return ctx.equals(o.ctx) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * ctx.hashCode() + 3 * type.hashCode();
  }
  
  public ContextType withCtx(hydra.ext.haskell.ast.Assertion ctx) {
    java.util.Objects.requireNonNull((ctx));
    return new ContextType(ctx, type);
  }
  
  public ContextType withType(hydra.ext.haskell.ast.Type type) {
    java.util.Objects.requireNonNull((type));
    return new ContextType(ctx, type);
  }
}