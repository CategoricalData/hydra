// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class TryStatement_Simple implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.TryStatement.Simple");
  
  public static final hydra.core.Name FIELD_NAME_BLOCK = new hydra.core.Name("block");
  
  public static final hydra.core.Name FIELD_NAME_CATCHES = new hydra.core.Name("catches");
  
  public final hydra.ext.java.syntax.Block block;
  
  public final hydra.ext.java.syntax.Catches catches;
  
  public TryStatement_Simple (hydra.ext.java.syntax.Block block, hydra.ext.java.syntax.Catches catches) {
    java.util.Objects.requireNonNull((block));
    java.util.Objects.requireNonNull((catches));
    this.block = block;
    this.catches = catches;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TryStatement_Simple)) {
      return false;
    }
    TryStatement_Simple o = (TryStatement_Simple) (other);
    return block.equals(o.block) && catches.equals(o.catches);
  }
  
  @Override
  public int hashCode() {
    return 2 * block.hashCode() + 3 * catches.hashCode();
  }
  
  public TryStatement_Simple withBlock(hydra.ext.java.syntax.Block block) {
    java.util.Objects.requireNonNull((block));
    return new TryStatement_Simple(block, catches);
  }
  
  public TryStatement_Simple withCatches(hydra.ext.java.syntax.Catches catches) {
    java.util.Objects.requireNonNull((catches));
    return new TryStatement_Simple(block, catches);
  }
}
