package hydra.ext.java.syntax;

public class TryStatement_Simple {
  public final Block block;
  
  public final Catches catches;
  
  public TryStatement_Simple (Block block, Catches catches) {
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
  
  public TryStatement_Simple withBlock(Block block) {
    return new TryStatement_Simple(block, catches);
  }
  
  public TryStatement_Simple withCatches(Catches catches) {
    return new TryStatement_Simple(block, catches);
  }
}