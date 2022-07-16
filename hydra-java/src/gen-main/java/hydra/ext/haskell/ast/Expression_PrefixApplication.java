package hydra.ext.haskell.ast;

/**
 * A prefix expression
 */
public class Expression_PrefixApplication {
  public final Operator operator;
  
  public final Expression rhs;
  
  public Expression_PrefixApplication (Operator operator, Expression rhs) {
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
  
  public Expression_PrefixApplication withOperator(Operator operator) {
    return new Expression_PrefixApplication(operator, rhs);
  }
  
  public Expression_PrefixApplication withRhs(Expression rhs) {
    return new Expression_PrefixApplication(operator, rhs);
  }
}