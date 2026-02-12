// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class MultiplicativeExpression_Binary implements Serializable, Comparable<MultiplicativeExpression_Binary> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.MultiplicativeExpression_Binary");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.java.syntax.MultiplicativeExpression lhs;
  
  public final hydra.ext.java.syntax.UnaryExpression rhs;
  
  public MultiplicativeExpression_Binary (hydra.ext.java.syntax.MultiplicativeExpression lhs, hydra.ext.java.syntax.UnaryExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiplicativeExpression_Binary)) {
      return false;
    }
    MultiplicativeExpression_Binary o = (MultiplicativeExpression_Binary) other;
    return java.util.Objects.equals(
      this.lhs,
      o.lhs) && java.util.Objects.equals(
      this.rhs,
      o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(lhs) + 3 * java.util.Objects.hashCode(rhs);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MultiplicativeExpression_Binary other) {
    int cmp = 0;
    cmp = ((Comparable) lhs).compareTo(other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rhs).compareTo(other.rhs);
  }
  
  public MultiplicativeExpression_Binary withLhs(hydra.ext.java.syntax.MultiplicativeExpression lhs) {
    return new MultiplicativeExpression_Binary(lhs, rhs);
  }
  
  public MultiplicativeExpression_Binary withRhs(hydra.ext.java.syntax.UnaryExpression rhs) {
    return new MultiplicativeExpression_Binary(lhs, rhs);
  }
}
