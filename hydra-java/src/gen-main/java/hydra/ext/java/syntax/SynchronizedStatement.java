// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class SynchronizedStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.SynchronizedStatement");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_BLOCK = new hydra.core.Name("block");
  
  public final hydra.ext.java.syntax.Expression expression;
  
  public final hydra.ext.java.syntax.Block block;
  
  public SynchronizedStatement (hydra.ext.java.syntax.Expression expression, hydra.ext.java.syntax.Block block) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((block));
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
    java.util.Objects.requireNonNull((expression));
    return new SynchronizedStatement(expression, block);
  }
  
  public SynchronizedStatement withBlock(hydra.ext.java.syntax.Block block) {
    java.util.Objects.requireNonNull((block));
    return new SynchronizedStatement(expression, block);
  }
}