package hydra.ext.java.syntax;

public class TryStatement_Simple {
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
    TryStatement_Simple o = (TryStatement_Simple) (other);
    return block.equals(o.block) && catches.equals(o.catches);
  }
  
  @Override
  public int hashCode() {
    return 2 * block.hashCode() + 3 * catches.hashCode();
  }
  
  public TryStatement_Simple withBlock(hydra.ext.java.syntax.Block block) {
    return new TryStatement_Simple(block, catches);
  }
  
  public TryStatement_Simple withCatches(hydra.ext.java.syntax.Catches catches) {
    return new TryStatement_Simple(block, catches);
  }
}