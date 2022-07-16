package hydra.ext.java.syntax;

public class CatchClause {
  public final java.util.Optional<CatchFormalParameter> parameter;
  
  public final Block block;
  
  public CatchClause (java.util.Optional<CatchFormalParameter> parameter, Block block) {
    this.parameter = parameter;
    this.block = block;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CatchClause)) {
      return false;
    }
    CatchClause o = (CatchClause) (other);
    return parameter.equals(o.parameter) && block.equals(o.block);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameter.hashCode() + 3 * block.hashCode();
  }
  
  public CatchClause withParameter(java.util.Optional<CatchFormalParameter> parameter) {
    return new CatchClause(parameter, block);
  }
  
  public CatchClause withBlock(Block block) {
    return new CatchClause(parameter, block);
  }
}