// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class AdditiveExpression_Binary implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.AdditiveExpression.Binary");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.java.syntax.AdditiveExpression lhs;
  
  public final hydra.ext.java.syntax.MultiplicativeExpression rhs;
  
  public AdditiveExpression_Binary (hydra.ext.java.syntax.AdditiveExpression lhs, hydra.ext.java.syntax.MultiplicativeExpression rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AdditiveExpression_Binary)) {
      return false;
    }
    AdditiveExpression_Binary o = (AdditiveExpression_Binary) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public AdditiveExpression_Binary withLhs(hydra.ext.java.syntax.AdditiveExpression lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new AdditiveExpression_Binary(lhs, rhs);
  }
  
  public AdditiveExpression_Binary withRhs(hydra.ext.java.syntax.MultiplicativeExpression rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new AdditiveExpression_Binary(lhs, rhs);
  }
}
