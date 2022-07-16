package hydra.ext.java.syntax;

public class TryWithResourcesStatement {
  public final ResourceSpecification resourceSpecification;
  
  public final Block block;
  
  public final java.util.Optional<Catches> catches;
  
  public final java.util.Optional<Finally> finally_;
  
  public TryWithResourcesStatement (ResourceSpecification resourceSpecification, Block block, java.util.Optional<Catches> catches, java.util.Optional<Finally> finally_) {
    this.resourceSpecification = resourceSpecification;
    this.block = block;
    this.catches = catches;
    this.finally_ = finally_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TryWithResourcesStatement)) {
      return false;
    }
    TryWithResourcesStatement o = (TryWithResourcesStatement) (other);
    return resourceSpecification.equals(o.resourceSpecification) && block.equals(o.block) && catches.equals(o.catches) && finally_.equals(o.finally_);
  }
  
  @Override
  public int hashCode() {
    return 2 * resourceSpecification.hashCode() + 3 * block.hashCode() + 5 * catches.hashCode() + 7 * finally_.hashCode();
  }
  
  public TryWithResourcesStatement withResourceSpecification(ResourceSpecification resourceSpecification) {
    return new TryWithResourcesStatement(resourceSpecification, block, catches, finally_);
  }
  
  public TryWithResourcesStatement withBlock(Block block) {
    return new TryWithResourcesStatement(resourceSpecification, block, catches, finally_);
  }
  
  public TryWithResourcesStatement withCatches(java.util.Optional<Catches> catches) {
    return new TryWithResourcesStatement(resourceSpecification, block, catches, finally_);
  }
  
  public TryWithResourcesStatement withFinally(java.util.Optional<Finally> finally_) {
    return new TryWithResourcesStatement(resourceSpecification, block, catches, finally_);
  }
}