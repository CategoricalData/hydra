// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A typed pattern
 */
public class TypedPattern implements Serializable, Comparable<TypedPattern> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.TypedPattern");
  
  public static final hydra.core.Name FIELD_NAME_INNER = new hydra.core.Name("inner");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  /**
   * The inner pattern
   */
  public final hydra.ext.haskell.ast.Pattern inner;
  
  /**
   * The type annotation
   */
  public final hydra.ext.haskell.ast.Type type;
  
  public TypedPattern (hydra.ext.haskell.ast.Pattern inner, hydra.ext.haskell.ast.Type type) {
    this.inner = inner;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypedPattern)) {
      return false;
    }
    TypedPattern o = (TypedPattern) (other);
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
    cmp = ((Comparable) (inner)).compareTo(other.inner);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (type)).compareTo(other.type);
  }
  
  public TypedPattern withInner(hydra.ext.haskell.ast.Pattern inner) {
    return new TypedPattern(inner, type);
  }
  
  public TypedPattern withType(hydra.ext.haskell.ast.Type type) {
    return new TypedPattern(inner, type);
  }
}
