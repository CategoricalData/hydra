// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A type signature expression
 */
public class TypeSignatureExpression implements Serializable, Comparable<TypeSignatureExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.TypeSignatureExpression");

  public static final hydra.core.Name INNER = new hydra.core.Name("inner");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  /**
   * The expression being typed
   */
  public final hydra.haskell.syntax.Expression inner;

  /**
   * The type signature
   */
  public final hydra.haskell.syntax.Type type;

  public TypeSignatureExpression (hydra.haskell.syntax.Expression inner, hydra.haskell.syntax.Type type) {
    this.inner = inner;
    this.type = type;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeSignatureExpression)) {
      return false;
    }
    TypeSignatureExpression o = (TypeSignatureExpression) other;
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
    cmp = hydra.util.Comparing.compare(
      inner,
      other.inner);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      type,
      other.type);
  }

  public TypeSignatureExpression withInner(hydra.haskell.syntax.Expression inner) {
    return new TypeSignatureExpression(inner, type);
  }

  public TypeSignatureExpression withType(hydra.haskell.syntax.Type type) {
    return new TypeSignatureExpression(inner, type);
  }
}
