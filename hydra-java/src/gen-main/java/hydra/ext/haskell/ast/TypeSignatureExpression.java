// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A type signature expression
 */
public class TypeSignatureExpression implements Serializable, Comparable<TypeSignatureExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.TypeSignatureExpression");
  
  public static final hydra.core.Name FIELD_NAME_INNER = new hydra.core.Name("inner");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  /**
   * The expression being typed
   */
  public final hydra.ext.haskell.ast.Expression inner;
  
  /**
   * The type signature
   */
  public final hydra.ext.haskell.ast.Type type;
  
  public TypeSignatureExpression (hydra.ext.haskell.ast.Expression inner, hydra.ext.haskell.ast.Type type) {
    this.inner = inner;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeSignatureExpression)) {
      return false;
    }
    TypeSignatureExpression o = (TypeSignatureExpression) (other);
    return java.util.Objects.equals(
      this.inner,
      o.inner) && java.util.Objects.equals(
      this.type,
      o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(inner) + 3 * java.util.Objects.hashCode(type);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeSignatureExpression other) {
    int cmp = 0;
    cmp = ((Comparable) (inner)).compareTo(other.inner);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (type)).compareTo(other.type);
  }
  
  public TypeSignatureExpression withInner(hydra.ext.haskell.ast.Expression inner) {
    return new TypeSignatureExpression(inner, type);
  }
  
  public TypeSignatureExpression withType(hydra.ext.haskell.ast.Type type) {
    return new TypeSignatureExpression(inner, type);
  }
}
