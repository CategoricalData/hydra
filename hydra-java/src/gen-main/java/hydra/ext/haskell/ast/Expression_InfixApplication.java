package hydra.ext.haskell.ast;

/**
 * An infix application expression
 */
public class Expression_InfixApplication {
  public final hydra.ext.haskell.ast.Expression lhs;
  
  public final hydra.ext.haskell.ast.Operator operator;
  
  public final hydra.ext.haskell.ast.Expression rhs;
  
  public Expression_InfixApplication (hydra.ext.haskell.ast.Expression lhs, hydra.ext.haskell.ast.Operator operator, hydra.ext.haskell.ast.Expression rhs) {
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
  
  public Expression_InfixApplication withLhs(hydra.ext.haskell.ast.Expression lhs) {
    return new Expression_InfixApplication(lhs, operator, rhs);
  }
  
  public Expression_InfixApplication withOperator(hydra.ext.haskell.ast.Operator operator) {
    return new Expression_InfixApplication(lhs, operator, rhs);
  }
  
  public Expression_InfixApplication withRhs(hydra.ext.haskell.ast.Expression rhs) {
    return new Expression_InfixApplication(lhs, operator, rhs);
  }
}