package hydra.ext.java.syntax;

public class TryStatement_WithFinally {
  public final Block block;
  
  public final java.util.Optional<Catches> catches;
  
  public final Finally finally_;
  
  public TryStatement_WithFinally (Block block, java.util.Optional<Catches> catches, Finally finally_) {
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
  
  public TryStatement_WithFinally withBlock(Block block) {
    return new TryStatement_WithFinally(block, catches, finally_);
  }
  
  public TryStatement_WithFinally withCatches(java.util.Optional<Catches> catches) {
    return new TryStatement_WithFinally(block, catches, finally_);
  }
  
  public TryStatement_WithFinally withFinally(Finally finally_) {
    return new TryStatement_WithFinally(block, catches, finally_);
  }
}