package hydra.ext.java.syntax;

public class SynchronizedStatement {
  public final hydra.ext.java.syntax.Expression expression;
  
  public final hydra.ext.java.syntax.Block block;
  
  public SynchronizedStatement (hydra.ext.java.syntax.Expression expression, hydra.ext.java.syntax.Block block) {
    this.expression = expression;
    this.block = block;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SynchronizedStatement)) {
      return false;
    }
    SynchronizedStatement o = (SynchronizedStatement) (other);
    return expression.equals(o.expression) && block.equals(o.block);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * block.hashCode();
  }
  
  public SynchronizedStatement withExpression(hydra.ext.java.syntax.Expression expression) {
    return new SynchronizedStatement(expression, block);
  }
  
  public SynchronizedStatement withBlock(hydra.ext.java.syntax.Block block) {
    return new SynchronizedStatement(expression, block);
  }
}