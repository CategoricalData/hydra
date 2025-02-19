// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A section expression
 */
public class SectionExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.SectionExpression");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.haskell.ast.Operator operator;
  
  public final hydra.ext.haskell.ast.Expression expression;
  
  public SectionExpression (hydra.ext.haskell.ast.Operator operator, hydra.ext.haskell.ast.Expression expression) {
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((expression));
    this.operator = operator;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SectionExpression)) {
      return false;
    }
    SectionExpression o = (SectionExpression) (other);
    return operator.equals(o.operator) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * operator.hashCode() + 3 * expression.hashCode();
  }
  
  public SectionExpression withOperator(hydra.ext.haskell.ast.Operator operator) {
    java.util.Objects.requireNonNull((operator));
    return new SectionExpression(operator, expression);
  }
  
  public SectionExpression withExpression(hydra.ext.haskell.ast.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new SectionExpression(operator, expression);
  }
}