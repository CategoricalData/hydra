package hydra.ext.haskell.ast;

/**
 * A section expression
 */
public class Expression_Section {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Expression.Section");
  
  public final hydra.ext.haskell.ast.Operator operator;
  
  public final hydra.ext.haskell.ast.Expression expression;
  
  public Expression_Section (hydra.ext.haskell.ast.Operator operator, hydra.ext.haskell.ast.Expression expression) {
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
  
  public Expression_Section withOperator(hydra.ext.haskell.ast.Operator operator) {
    return new Expression_Section(operator, expression);
  }
  
  public Expression_Section withExpression(hydra.ext.haskell.ast.Expression expression) {
    return new Expression_Section(operator, expression);
  }
}