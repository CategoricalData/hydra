// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A type signature expression
 */
public class TypeSignatureExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.TypeSignatureExpression");
  
  public static final hydra.core.Name FIELD_NAME_INNER = new hydra.core.Name("inner");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public final hydra.ext.haskell.ast.Expression inner;
  
  public final hydra.ext.haskell.ast.Type type;
  
  public TypeSignatureExpression (hydra.ext.haskell.ast.Expression inner, hydra.ext.haskell.ast.Type type) {
    java.util.Objects.requireNonNull((inner));
    java.util.Objects.requireNonNull((type));
    this.inner = inner;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeSignatureExpression)) {
      return false;
    }
    TypeSignatureExpression o = (TypeSignatureExpression) (other);
    return inner.equals(o.inner) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * inner.hashCode() + 3 * type.hashCode();
  }
  
  public TypeSignatureExpression withInner(hydra.ext.haskell.ast.Expression inner) {
    java.util.Objects.requireNonNull((inner));
    return new TypeSignatureExpression(inner, type);
  }
  
  public TypeSignatureExpression withType(hydra.ext.haskell.ast.Type type) {
    java.util.Objects.requireNonNull((type));
    return new TypeSignatureExpression(inner, type);
  }
}