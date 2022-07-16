package hydra.ext.haskell.ast;

/**
 * An infix application expression
 */
public class Expression_InfixApplication {
  public final Expression lhs;
  
  public final Operator operator;
  
  public final Expression rhs;
  
  public Expression_InfixApplication (Expression lhs, Operator operator, Expression rhs) {
    this.lhs = lhs;
    this.operator = operator;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Expression_InfixApplication)) {
      return false;
    }
    Expression_InfixApplication o = (Expression_InfixApplication) (other);
    return lhs.equals(o.lhs) && operator.equals(o.operator) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * operator.hashCode() + 5 * rhs.hashCode();
  }
  
  public Expression_InfixApplication withLhs(Expression lhs) {
    return new Expression_InfixApplication(lhs, operator, rhs);
  }
  
  public Expression_InfixApplication withOperator(Operator operator) {
    return new Expression_InfixApplication(lhs, operator, rhs);
  }
  
  public Expression_InfixApplication withRhs(Expression rhs) {
    return new Expression_InfixApplication(lhs, operator, rhs);
  }
}