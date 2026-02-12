// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class RelationalExpression_GreaterThanEqual implements Serializable, Comparable<RelationalExpression_GreaterThanEqual> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.java.syntax.RelationalExpression lhs;
  
  public final hydra.ext.java.syntax.ShiftExpression rhs;
  
  public RelationalExpression_GreaterThanEqual (hydra.ext.java.syntax.RelationalExpression lhs, hydra.ext.java.syntax.ShiftExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationalExpression_GreaterThanEqual)) {
      return false;
    }
    RelationalExpression_GreaterThanEqual o = (RelationalExpression_GreaterThanEqual) other;
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
  public int compareTo(RelationalExpression_GreaterThanEqual other) {
    int cmp = 0;
    cmp = ((Comparable) lhs).compareTo(other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rhs).compareTo(other.rhs);
  }
  
  public RelationalExpression_GreaterThanEqual withLhs(hydra.ext.java.syntax.RelationalExpression lhs) {
    return new RelationalExpression_GreaterThanEqual(lhs, rhs);
  }
  
  public RelationalExpression_GreaterThanEqual withRhs(hydra.ext.java.syntax.ShiftExpression rhs) {
    return new RelationalExpression_GreaterThanEqual(lhs, rhs);
  }
}
