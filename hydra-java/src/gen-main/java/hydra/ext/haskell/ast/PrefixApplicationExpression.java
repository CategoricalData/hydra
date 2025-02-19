// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A prefix expression
 */
public class PrefixApplicationExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.PrefixApplicationExpression");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.haskell.ast.Operator operator;
  
  public final hydra.ext.haskell.ast.Expression rhs;
  
  public PrefixApplicationExpression (hydra.ext.haskell.ast.Operator operator, hydra.ext.haskell.ast.Expression rhs) {
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((rhs));
    this.operator = operator;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrefixApplicationExpression)) {
      return false;
    }
    PrefixApplicationExpression o = (PrefixApplicationExpression) (other);
    return operator.equals(o.operator) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * operator.hashCode() + 3 * rhs.hashCode();
  }
  
  public PrefixApplicationExpression withOperator(hydra.ext.haskell.ast.Operator operator) {
    java.util.Objects.requireNonNull((operator));
    return new PrefixApplicationExpression(operator, rhs);
  }
  
  public PrefixApplicationExpression withRhs(hydra.ext.haskell.ast.Expression rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new PrefixApplicationExpression(operator, rhs);
  }
}