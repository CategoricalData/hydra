// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class RelationalExpression_GreaterThanEqual implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.RelationalExpression.GreaterThanEqual");
  
  public final hydra.langs.java.syntax.RelationalExpression lhs;
  
  public final hydra.langs.java.syntax.ShiftExpression rhs;
  
  public RelationalExpression_GreaterThanEqual (hydra.langs.java.syntax.RelationalExpression lhs, hydra.langs.java.syntax.ShiftExpression rhs) {
    if (lhs == null) {
      throw new IllegalArgumentException("null value for 'lhs' argument");
    }
    if (rhs == null) {
      throw new IllegalArgumentException("null value for 'rhs' argument");
    }
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationalExpression_GreaterThanEqual)) {
      return false;
    }
    RelationalExpression_GreaterThanEqual o = (RelationalExpression_GreaterThanEqual) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public RelationalExpression_GreaterThanEqual withLhs(hydra.langs.java.syntax.RelationalExpression lhs) {
    if (lhs == null) {
      throw new IllegalArgumentException("null value for 'lhs' argument");
    }
    return new RelationalExpression_GreaterThanEqual(lhs, rhs);
  }
  
  public RelationalExpression_GreaterThanEqual withRhs(hydra.langs.java.syntax.ShiftExpression rhs) {
    if (rhs == null) {
      throw new IllegalArgumentException("null value for 'rhs' argument");
    }
    return new RelationalExpression_GreaterThanEqual(lhs, rhs);
  }
}