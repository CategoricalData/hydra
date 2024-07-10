// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class TryStatement_WithFinally implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.TryStatement.WithFinally");
  
  public final hydra.langs.java.syntax.Block block;
  
  public final java.util.Optional<hydra.langs.java.syntax.Catches> catches;
  
  public final hydra.langs.java.syntax.Finally finally_;
  
  public TryStatement_WithFinally (hydra.langs.java.syntax.Block block, java.util.Optional<hydra.langs.java.syntax.Catches> catches, hydra.langs.java.syntax.Finally finally_) {
    if (block == null) {
      throw new IllegalArgumentException("null value for 'block' argument");
    }
    if (catches == null) {
      throw new IllegalArgumentException("null value for 'catches' argument");
    }
    if (finally_ == null) {
      throw new IllegalArgumentException("null value for 'finally' argument");
    }
    this.block = block;
    this.catches = catches;
    this.finally_ = finally_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TryStatement_WithFinally)) {
      return false;
    }
    TryStatement_WithFinally o = (TryStatement_WithFinally) (other);
    return block.equals(o.block) && catches.equals(o.catches) && finally_.equals(o.finally_);
  }
  
  @Override
  public int hashCode() {
    return 2 * block.hashCode() + 3 * catches.hashCode() + 5 * finally_.hashCode();
  }
  
  public TryStatement_WithFinally withBlock(hydra.langs.java.syntax.Block block) {
    if (block == null) {
      throw new IllegalArgumentException("null value for 'block' argument");
    }
    return new TryStatement_WithFinally(block, catches, finally_);
  }
  
  public TryStatement_WithFinally withCatches(java.util.Optional<hydra.langs.java.syntax.Catches> catches) {
    if (catches == null) {
      throw new IllegalArgumentException("null value for 'catches' argument");
    }
    return new TryStatement_WithFinally(block, catches, finally_);
  }
  
  public TryStatement_WithFinally withFinally(hydra.langs.java.syntax.Finally finally_) {
    if (finally_ == null) {
      throw new IllegalArgumentException("null value for 'finally' argument");
    }
    return new TryStatement_WithFinally(block, catches, finally_);
  }
}