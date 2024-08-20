// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A type signature expression
 */
public class Expression_TypeSignature implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/haskell/ast.Expression.TypeSignature");
  
  public static final hydra.core.Name FIELD_NAME_INNER = new hydra.core.Name("inner");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public final hydra.ext.haskell.ast.Expression inner;
  
  public final hydra.ext.haskell.ast.Type type;
  
  public Expression_TypeSignature (hydra.ext.haskell.ast.Expression inner, hydra.ext.haskell.ast.Type type) {
    java.util.Objects.requireNonNull((inner));
    java.util.Objects.requireNonNull((type));
    this.inner = inner;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Expression_TypeSignature)) {
      return false;
    }
    Expression_TypeSignature o = (Expression_TypeSignature) (other);
    return inner.equals(o.inner) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * inner.hashCode() + 3 * type.hashCode();
  }
  
  public Expression_TypeSignature withInner(hydra.ext.haskell.ast.Expression inner) {
    java.util.Objects.requireNonNull((inner));
    return new Expression_TypeSignature(inner, type);
  }
  
  public Expression_TypeSignature withType(hydra.ext.haskell.ast.Type type) {
    java.util.Objects.requireNonNull((type));
    return new Expression_TypeSignature(inner, type);
  }
}
