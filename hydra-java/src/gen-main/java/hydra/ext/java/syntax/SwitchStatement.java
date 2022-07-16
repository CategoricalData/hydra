package hydra.ext.java.syntax;

public class SwitchStatement {
  public final Expression cond;
  
  public final SwitchBlock block;
  
  public SwitchStatement (Expression cond, SwitchBlock block) {
    this.cond = cond;
    this.block = block;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SwitchStatement)) {
      return false;
    }
    SwitchStatement o = (SwitchStatement) (other);
    return cond.equals(o.cond) && block.equals(o.block);
  }
  
  @Override
  public int hashCode() {
    return 2 * cond.hashCode() + 3 * block.hashCode();
  }
  
  public SwitchStatement withCond(Expression cond) {
    return new SwitchStatement(cond, block);
  }
  
  public SwitchStatement withBlock(SwitchBlock block) {
    return new SwitchStatement(cond, block);
  }
}