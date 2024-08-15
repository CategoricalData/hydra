// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class ShiftExpression_Binary implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.ShiftExpression.Binary");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.langs.java.syntax.ShiftExpression lhs;
  
  public final hydra.langs.java.syntax.AdditiveExpression rhs;
  
  public ShiftExpression_Binary (hydra.langs.java.syntax.ShiftExpression lhs, hydra.langs.java.syntax.AdditiveExpression rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShiftExpression_Binary)) {
      return false;
    }
    ShiftExpression_Binary o = (ShiftExpression_Binary) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public ShiftExpression_Binary withLhs(hydra.langs.java.syntax.ShiftExpression lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new ShiftExpression_Binary(lhs, rhs);
  }
  
  public ShiftExpression_Binary withRhs(hydra.langs.java.syntax.AdditiveExpression rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new ShiftExpression_Binary(lhs, rhs);
  }
}