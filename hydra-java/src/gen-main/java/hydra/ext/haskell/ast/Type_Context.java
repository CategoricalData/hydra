// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public class Type_Context implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/haskell/ast.Type.Context");
  
  public static final hydra.core.Name FIELD_NAME_CTX = new hydra.core.Name("ctx");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public final hydra.ext.haskell.ast.Assertion ctx;
  
  public final hydra.ext.haskell.ast.Type type;
  
  public Type_Context (hydra.ext.haskell.ast.Assertion ctx, hydra.ext.haskell.ast.Type type) {
    java.util.Objects.requireNonNull((ctx));
    java.util.Objects.requireNonNull((type));
    this.ctx = ctx;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Context)) {
      return false;
    }
    Type_Context o = (Type_Context) (other);
    return ctx.equals(o.ctx) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * ctx.hashCode() + 3 * type.hashCode();
  }
  
  public Type_Context withCtx(hydra.ext.haskell.ast.Assertion ctx) {
    java.util.Objects.requireNonNull((ctx));
    return new Type_Context(ctx, type);
  }
  
  public Type_Context withType(hydra.ext.haskell.ast.Type type) {
    java.util.Objects.requireNonNull((type));
    return new Type_Context(ctx, type);
  }
}
