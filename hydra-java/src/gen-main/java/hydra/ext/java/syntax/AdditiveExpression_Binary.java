// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class AdditiveExpression_Binary implements Serializable, Comparable<AdditiveExpression_Binary> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.AdditiveExpression_Binary");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.java.syntax.AdditiveExpression lhs;
  
  public final hydra.ext.java.syntax.MultiplicativeExpression rhs;
  
  public AdditiveExpression_Binary (hydra.ext.java.syntax.AdditiveExpression lhs, hydra.ext.java.syntax.MultiplicativeExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AdditiveExpression_Binary)) {
      return false;
    }
    AdditiveExpression_Binary o = (AdditiveExpression_Binary) other;
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
  public int compareTo(AdditiveExpression_Binary other) {
    int cmp = 0;
    cmp = ((Comparable) lhs).compareTo(other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rhs).compareTo(other.rhs);
  }
  
  public AdditiveExpression_Binary withLhs(hydra.ext.java.syntax.AdditiveExpression lhs) {
    return new AdditiveExpression_Binary(lhs, rhs);
  }
  
  public AdditiveExpression_Binary withRhs(hydra.ext.java.syntax.MultiplicativeExpression rhs) {
    return new AdditiveExpression_Binary(lhs, rhs);
  }
}
