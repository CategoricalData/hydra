// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class MultiplicativeExpression_Binary implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.MultiplicativeExpression.Binary");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.java.syntax.MultiplicativeExpression lhs;
  
  public final hydra.ext.java.syntax.UnaryExpression rhs;
  
  public MultiplicativeExpression_Binary (hydra.ext.java.syntax.MultiplicativeExpression lhs, hydra.ext.java.syntax.UnaryExpression rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiplicativeExpression_Binary)) {
      return false;
    }
    MultiplicativeExpression_Binary o = (MultiplicativeExpression_Binary) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public MultiplicativeExpression_Binary withLhs(hydra.ext.java.syntax.MultiplicativeExpression lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new MultiplicativeExpression_Binary(lhs, rhs);
  }
  
  public MultiplicativeExpression_Binary withRhs(hydra.ext.java.syntax.UnaryExpression rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new MultiplicativeExpression_Binary(lhs, rhs);
  }
}
