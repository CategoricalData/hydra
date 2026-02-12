// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class SynchronizedStatement implements Serializable, Comparable<SynchronizedStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.SynchronizedStatement");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_BLOCK = new hydra.core.Name("block");
  
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
    SynchronizedStatement o = (SynchronizedStatement) other;
    return java.util.Objects.equals(
      this.expression,
      o.expression) && java.util.Objects.equals(
      this.block,
      o.block);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression) + 3 * java.util.Objects.hashCode(block);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SynchronizedStatement other) {
    int cmp = 0;
    cmp = ((Comparable) expression).compareTo(other.expression);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) block).compareTo(other.block);
  }
  
  public SynchronizedStatement withExpression(hydra.ext.java.syntax.Expression expression) {
    return new SynchronizedStatement(expression, block);
  }
  
  public SynchronizedStatement withBlock(hydra.ext.java.syntax.Block block) {
    return new SynchronizedStatement(expression, block);
  }
}
