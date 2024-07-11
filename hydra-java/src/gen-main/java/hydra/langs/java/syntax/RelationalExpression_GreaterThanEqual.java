// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class RelationalExpression_GreaterThanEqual implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.RelationalExpression.GreaterThanEqual");
  
  public final hydra.langs.java.syntax.RelationalExpression lhs;
  
  public final hydra.langs.java.syntax.ShiftExpression rhs;
  
  public RelationalExpression_GreaterThanEqual (hydra.langs.java.syntax.RelationalExpression lhs, hydra.langs.java.syntax.ShiftExpression rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
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
    java.util.Objects.requireNonNull((lhs));
    return new RelationalExpression_GreaterThanEqual(lhs, rhs);
  }
  
  public RelationalExpression_GreaterThanEqual withRhs(hydra.langs.java.syntax.ShiftExpression rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new RelationalExpression_GreaterThanEqual(lhs, rhs);
  }
}