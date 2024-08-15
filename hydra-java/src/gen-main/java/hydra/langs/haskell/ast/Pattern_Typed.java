// Note: this is an automatically generated file. Do not edit.

package hydra.langs.haskell.ast;

import java.io.Serializable;

public class Pattern_Typed implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/haskell/ast.Pattern.Typed");
  
  public static final hydra.core.Name FIELD_NAME_INNER = new hydra.core.Name("inner");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public final hydra.langs.haskell.ast.Pattern inner;
  
  public final hydra.langs.haskell.ast.Type type;
  
  public Pattern_Typed (hydra.langs.haskell.ast.Pattern inner, hydra.langs.haskell.ast.Type type) {
    java.util.Objects.requireNonNull((inner));
    java.util.Objects.requireNonNull((type));
    this.inner = inner;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern_Typed)) {
      return false;
    }
    Pattern_Typed o = (Pattern_Typed) (other);
    return inner.equals(o.inner) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * inner.hashCode() + 3 * type.hashCode();
  }
  
  public Pattern_Typed withInner(hydra.langs.haskell.ast.Pattern inner) {
    java.util.Objects.requireNonNull((inner));
    return new Pattern_Typed(inner, type);
  }
  
  public Pattern_Typed withType(hydra.langs.haskell.ast.Type type) {
    java.util.Objects.requireNonNull((type));
    return new Pattern_Typed(inner, type);
  }
}