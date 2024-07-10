// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class SynchronizedStatement implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.SynchronizedStatement");
  
  public final hydra.langs.java.syntax.Expression expression;
  
  public final hydra.langs.java.syntax.Block block;
  
  public SynchronizedStatement (hydra.langs.java.syntax.Expression expression, hydra.langs.java.syntax.Block block) {
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
    if (block == null) {
      throw new IllegalArgumentException("null value for 'block' argument");
    }
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
  
  public SynchronizedStatement withExpression(hydra.langs.java.syntax.Expression expression) {
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
    return new SynchronizedStatement(expression, block);
  }
  
  public SynchronizedStatement withBlock(hydra.langs.java.syntax.Block block) {
    if (block == null) {
      throw new IllegalArgumentException("null value for 'block' argument");
    }
    return new SynchronizedStatement(expression, block);
  }
}