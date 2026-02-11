// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An 'as' pattern
 */
public class AsPattern implements Serializable, Comparable<AsPattern> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.AsPattern");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_INNER = new hydra.core.Name("inner");
  
  /**
   * The bound name
   */
  public final hydra.ext.haskell.ast.Name name;
  
  /**
   * The inner pattern
   */
  public final hydra.ext.haskell.ast.Pattern inner;
  
  public AsPattern (hydra.ext.haskell.ast.Name name, hydra.ext.haskell.ast.Pattern inner) {
    this.name = name;
    this.inner = inner;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AsPattern)) {
      return false;
    }
    AsPattern o = (AsPattern) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.inner,
      o.inner);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(inner);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AsPattern other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) inner).compareTo(other.inner);
  }
  
  public AsPattern withName(hydra.ext.haskell.ast.Name name) {
    return new AsPattern(name, inner);
  }
  
  public AsPattern withInner(hydra.ext.haskell.ast.Pattern inner) {
    return new AsPattern(name, inner);
  }
}
