// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public class TypedPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.TypedPattern");
  
  public static final hydra.core.Name FIELD_NAME_INNER = new hydra.core.Name("inner");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public final hydra.ext.haskell.ast.Pattern inner;
  
  public final hydra.ext.haskell.ast.Type type;
  
  public TypedPattern (hydra.ext.haskell.ast.Pattern inner, hydra.ext.haskell.ast.Type type) {
    java.util.Objects.requireNonNull((inner));
    java.util.Objects.requireNonNull((type));
    this.inner = inner;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypedPattern)) {
      return false;
    }
    TypedPattern o = (TypedPattern) (other);
    return inner.equals(o.inner) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * inner.hashCode() + 3 * type.hashCode();
  }
  
  public TypedPattern withInner(hydra.ext.haskell.ast.Pattern inner) {
    java.util.Objects.requireNonNull((inner));
    return new TypedPattern(inner, type);
  }
  
  public TypedPattern withType(hydra.ext.haskell.ast.Type type) {
    java.util.Objects.requireNonNull((type));
    return new TypedPattern(inner, type);
  }
}