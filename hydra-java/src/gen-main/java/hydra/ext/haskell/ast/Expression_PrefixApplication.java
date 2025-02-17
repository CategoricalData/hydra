// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A prefix expression
 */
public class Expression_PrefixApplication implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/haskell/ast.Expression.PrefixApplication");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.haskell.ast.Operator operator;
  
  public final hydra.ext.haskell.ast.Expression rhs;
  
  public Expression_PrefixApplication (hydra.ext.haskell.ast.Operator operator, hydra.ext.haskell.ast.Expression rhs) {
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((rhs));
    this.operator = operator;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Expression_PrefixApplication)) {
      return false;
    }
    Expression_PrefixApplication o = (Expression_PrefixApplication) (other);
    return operator.equals(o.operator) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * operator.hashCode() + 3 * rhs.hashCode();
  }
  
  public Expression_PrefixApplication withOperator(hydra.ext.haskell.ast.Operator operator) {
    java.util.Objects.requireNonNull((operator));
    return new Expression_PrefixApplication(operator, rhs);
  }
  
  public Expression_PrefixApplication withRhs(hydra.ext.haskell.ast.Expression rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Expression_PrefixApplication(operator, rhs);
  }
}
