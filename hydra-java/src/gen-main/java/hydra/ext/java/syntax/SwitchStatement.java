package hydra.ext.java.syntax;

public class SwitchStatement {
  public final hydra.ext.java.syntax.Expression cond;
  
  public final hydra.ext.java.syntax.SwitchBlock block;
  
  public SwitchStatement (hydra.ext.java.syntax.Expression cond, hydra.ext.java.syntax.SwitchBlock block) {
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
  
  public SwitchStatement withCond(hydra.ext.java.syntax.Expression cond) {
    return new SwitchStatement(cond, block);
  }
  
  public SwitchStatement withBlock(hydra.ext.java.syntax.SwitchBlock block) {
    return new SwitchStatement(cond, block);
  }
}