package hydra.ext.haskell.ast;

/**
 * A prefix expression
 */
public class Expression_PrefixApplication {
  public final hydra.ext.haskell.ast.Operator operator;
  
  public final hydra.ext.haskell.ast.Expression rhs;
  
  public Expression_PrefixApplication (hydra.ext.haskell.ast.Operator operator, hydra.ext.haskell.ast.Expression rhs) {
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
    return new Expression_PrefixApplication(operator, rhs);
  }
  
  public Expression_PrefixApplication withRhs(hydra.ext.haskell.ast.Expression rhs) {
    return new Expression_PrefixApplication(operator, rhs);
  }
}