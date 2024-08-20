// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class TryStatement_WithFinally implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.TryStatement.WithFinally");
  
  public static final hydra.core.Name FIELD_NAME_BLOCK = new hydra.core.Name("block");
  
  public static final hydra.core.Name FIELD_NAME_CATCHES = new hydra.core.Name("catches");
  
  public static final hydra.core.Name FIELD_NAME_FINALLY = new hydra.core.Name("finally");
  
  public final hydra.ext.java.syntax.Block block;
  
  public final hydra.util.Opt<hydra.ext.java.syntax.Catches> catches;
  
  public final hydra.ext.java.syntax.Finally finally_;
  
  public TryStatement_WithFinally (hydra.ext.java.syntax.Block block, hydra.util.Opt<hydra.ext.java.syntax.Catches> catches, hydra.ext.java.syntax.Finally finally_) {
    java.util.Objects.requireNonNull((block));
    java.util.Objects.requireNonNull((catches));
    java.util.Objects.requireNonNull((finally_));
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
  
  public TryStatement_WithFinally withBlock(hydra.ext.java.syntax.Block block) {
    java.util.Objects.requireNonNull((block));
    return new TryStatement_WithFinally(block, catches, finally_);
  }
  
  public TryStatement_WithFinally withCatches(hydra.util.Opt<hydra.ext.java.syntax.Catches> catches) {
    java.util.Objects.requireNonNull((catches));
    return new TryStatement_WithFinally(block, catches, finally_);
  }
  
  public TryStatement_WithFinally withFinally(hydra.ext.java.syntax.Finally finally_) {
    java.util.Objects.requireNonNull((finally_));
    return new TryStatement_WithFinally(block, catches, finally_);
  }
}
