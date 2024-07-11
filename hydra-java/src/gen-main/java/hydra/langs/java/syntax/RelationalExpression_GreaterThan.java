// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class RelationalExpression_GreaterThan implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.RelationalExpression.GreaterThan");
  
  public final hydra.langs.java.syntax.RelationalExpression lhs;
  
  public final hydra.langs.java.syntax.ShiftExpression rhs;
  
  public RelationalExpression_GreaterThan (hydra.langs.java.syntax.RelationalExpression lhs, hydra.langs.java.syntax.ShiftExpression rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationalExpression_GreaterThan)) {
      return false;
    }
    RelationalExpression_GreaterThan o = (RelationalExpression_GreaterThan) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public RelationalExpression_GreaterThan withLhs(hydra.langs.java.syntax.RelationalExpression lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new RelationalExpression_GreaterThan(lhs, rhs);
  }
  
  public RelationalExpression_GreaterThan withRhs(hydra.langs.java.syntax.ShiftExpression rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new RelationalExpression_GreaterThan(lhs, rhs);
  }
}