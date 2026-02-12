// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class TryStatement_WithFinally implements Serializable, Comparable<TryStatement_WithFinally> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.TryStatement_WithFinally");
  
  public static final hydra.core.Name FIELD_NAME_BLOCK = new hydra.core.Name("block");
  
  public static final hydra.core.Name FIELD_NAME_CATCHES = new hydra.core.Name("catches");
  
  public static final hydra.core.Name FIELD_NAME_FINALLY = new hydra.core.Name("finally");
  
  public final hydra.ext.java.syntax.Block block;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.Catches> catches;
  
  public final hydra.ext.java.syntax.Finally finally_;
  
  public TryStatement_WithFinally (hydra.ext.java.syntax.Block block, hydra.util.Maybe<hydra.ext.java.syntax.Catches> catches, hydra.ext.java.syntax.Finally finally_) {
    this.block = block;
    this.catches = catches;
    this.finally_ = finally_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TryStatement_WithFinally)) {
      return false;
    }
    TryStatement_WithFinally o = (TryStatement_WithFinally) other;
    return java.util.Objects.equals(
      this.block,
      o.block) && java.util.Objects.equals(
      this.catches,
      o.catches) && java.util.Objects.equals(
      this.finally_,
      o.finally_);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(block) + 3 * java.util.Objects.hashCode(catches) + 5 * java.util.Objects.hashCode(finally_);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TryStatement_WithFinally other) {
    int cmp = 0;
    cmp = ((Comparable) block).compareTo(other.block);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      catches.hashCode(),
      other.catches.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) finally_).compareTo(other.finally_);
  }
  
  public TryStatement_WithFinally withBlock(hydra.ext.java.syntax.Block block) {
    return new TryStatement_WithFinally(block, catches, finally_);
  }
  
  public TryStatement_WithFinally withCatches(hydra.util.Maybe<hydra.ext.java.syntax.Catches> catches) {
    return new TryStatement_WithFinally(block, catches, finally_);
  }
  
  public TryStatement_WithFinally withFinally(hydra.ext.java.syntax.Finally finally_) {
    return new TryStatement_WithFinally(block, catches, finally_);
  }
}
