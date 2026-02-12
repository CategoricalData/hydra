// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class TryStatement_Simple implements Serializable, Comparable<TryStatement_Simple> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.TryStatement_Simple");
  
  public static final hydra.core.Name FIELD_NAME_BLOCK = new hydra.core.Name("block");
  
  public static final hydra.core.Name FIELD_NAME_CATCHES = new hydra.core.Name("catches");
  
  public final hydra.ext.java.syntax.Block block;
  
  public final hydra.ext.java.syntax.Catches catches;
  
  public TryStatement_Simple (hydra.ext.java.syntax.Block block, hydra.ext.java.syntax.Catches catches) {
    this.block = block;
    this.catches = catches;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TryStatement_Simple)) {
      return false;
    }
    TryStatement_Simple o = (TryStatement_Simple) other;
    return java.util.Objects.equals(
      this.block,
      o.block) && java.util.Objects.equals(
      this.catches,
      o.catches);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(block) + 3 * java.util.Objects.hashCode(catches);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TryStatement_Simple other) {
    int cmp = 0;
    cmp = ((Comparable) block).compareTo(other.block);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) catches).compareTo(other.catches);
  }
  
  public TryStatement_Simple withBlock(hydra.ext.java.syntax.Block block) {
    return new TryStatement_Simple(block, catches);
  }
  
  public TryStatement_Simple withCatches(hydra.ext.java.syntax.Catches catches) {
    return new TryStatement_Simple(block, catches);
  }
}
