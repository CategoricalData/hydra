// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A section expression
 */
public class SectionExpression implements Serializable, Comparable<SectionExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.SectionExpression");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  /**
   * The operator
   */
  public final hydra.ext.haskell.ast.Operator operator;
  
  /**
   * The operand
   */
  public final hydra.ext.haskell.ast.Expression expression;
  
  public SectionExpression (hydra.ext.haskell.ast.Operator operator, hydra.ext.haskell.ast.Expression expression) {
    this.operator = operator;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SectionExpression)) {
      return false;
    }
    SectionExpression o = (SectionExpression) other;
    return java.util.Objects.equals(
      this.operator,
      o.operator) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(operator) + 3 * java.util.Objects.hashCode(expression);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SectionExpression other) {
    int cmp = 0;
    cmp = ((Comparable) operator).compareTo(other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) expression).compareTo(other.expression);
  }
  
  public SectionExpression withOperator(hydra.ext.haskell.ast.Operator operator) {
    return new SectionExpression(operator, expression);
  }
  
  public SectionExpression withExpression(hydra.ext.haskell.ast.Expression expression) {
    return new SectionExpression(operator, expression);
  }
}
