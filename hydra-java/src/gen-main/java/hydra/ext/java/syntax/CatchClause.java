package hydra.ext.java.syntax;

public class CatchClause {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.CatchClause");
  
  public final java.util.Optional<hydra.ext.java.syntax.CatchFormalParameter> parameter;
  
  public final hydra.ext.java.syntax.Block block;
  
  public CatchClause (java.util.Optional<hydra.ext.java.syntax.CatchFormalParameter> parameter, hydra.ext.java.syntax.Block block) {
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
  
  public CatchClause withParameter(java.util.Optional<hydra.ext.java.syntax.CatchFormalParameter> parameter) {
    return new CatchClause(parameter, block);
  }
  
  public CatchClause withBlock(hydra.ext.java.syntax.Block block) {
    return new CatchClause(parameter, block);
  }
}