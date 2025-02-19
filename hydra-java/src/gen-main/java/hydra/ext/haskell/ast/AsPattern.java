// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public class AsPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.AsPattern");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_INNER = new hydra.core.Name("inner");
  
  public final hydra.ext.haskell.ast.Name name;
  
  public final hydra.ext.haskell.ast.Pattern inner;
  
  public AsPattern (hydra.ext.haskell.ast.Name name, hydra.ext.haskell.ast.Pattern inner) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((inner));
    this.name = name;
    this.inner = inner;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AsPattern)) {
      return false;
    }
    AsPattern o = (AsPattern) (other);
    return name.equals(o.name) && inner.equals(o.inner);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * inner.hashCode();
  }
  
  public AsPattern withName(hydra.ext.haskell.ast.Name name) {
    java.util.Objects.requireNonNull((name));
    return new AsPattern(name, inner);
  }
  
  public AsPattern withInner(hydra.ext.haskell.ast.Pattern inner) {
    java.util.Objects.requireNonNull((inner));
    return new AsPattern(name, inner);
  }
}