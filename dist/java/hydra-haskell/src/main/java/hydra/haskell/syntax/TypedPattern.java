// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A typed pattern
 */
public class TypedPattern implements Serializable, Comparable<TypedPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.TypedPattern");

  public static final hydra.core.Name INNER = new hydra.core.Name("inner");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  /**
   * The inner pattern
   */
  public final hydra.haskell.syntax.Pattern inner;

  /**
   * The type annotation
   */
  public final hydra.haskell.syntax.Type type;

  public TypedPattern (hydra.haskell.syntax.Pattern inner, hydra.haskell.syntax.Type type) {
    this.inner = inner;
    this.type = type;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypedPattern)) {
      return false;
    }
    TypedPattern o = (TypedPattern) other;
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
  public int compareTo(TypedPattern other) {
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

  public TypedPattern withInner(hydra.haskell.syntax.Pattern inner) {
    return new TypedPattern(inner, type);
  }

  public TypedPattern withType(hydra.haskell.syntax.Type type) {
    return new TypedPattern(inner, type);
  }
}
