package hydra.ext.haskell.ast;

/**
 * A section expression
 */
public class Expression_Section {
  public final Operator operator;
  
  public final Expression expression;
  
  public Expression_Section (Operator operator, Expression expression) {
    this.operator = operator;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Expression_Section)) {
      return false;
    }
    Expression_Section o = (Expression_Section) (other);
    return operator.equals(o.operator) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * operator.hashCode() + 3 * expression.hashCode();
  }
  
  public Expression_Section withOperator(Operator operator) {
    return new Expression_Section(operator, expression);
  }
  
  public Expression_Section withExpression(Expression expression) {
    return new Expression_Section(operator, expression);
  }
}