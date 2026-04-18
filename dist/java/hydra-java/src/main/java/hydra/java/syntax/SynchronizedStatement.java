// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class SynchronizedStatement implements Serializable, Comparable<SynchronizedStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.SynchronizedStatement");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public static final hydra.core.Name BLOCK = new hydra.core.Name("block");

  public final hydra.java.syntax.Expression expression;

  public final hydra.java.syntax.Block block;

  public SynchronizedStatement (hydra.java.syntax.Expression expression, hydra.java.syntax.Block block) {
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
    cmp = hydra.util.Comparing.compare(
      expression,
      other.expression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      block,
      other.block);
  }

  public SynchronizedStatement withExpression(hydra.java.syntax.Expression expression) {
    return new SynchronizedStatement(expression, block);
  }

  public SynchronizedStatement withBlock(hydra.java.syntax.Block block) {
    return new SynchronizedStatement(expression, block);
  }
}
