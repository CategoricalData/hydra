// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class CastExpression_NotPlusMinus implements Serializable, Comparable<CastExpression_NotPlusMinus> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.CastExpression_NotPlusMinus");
  
  public static final hydra.core.Name FIELD_NAME_REF_AND_BOUNDS = new hydra.core.Name("refAndBounds");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.java.syntax.CastExpression_RefAndBounds refAndBounds;
  
  public final hydra.ext.java.syntax.UnaryExpression expression;
  
  public CastExpression_NotPlusMinus (hydra.ext.java.syntax.CastExpression_RefAndBounds refAndBounds, hydra.ext.java.syntax.UnaryExpression expression) {
    this.refAndBounds = refAndBounds;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CastExpression_NotPlusMinus)) {
      return false;
    }
    CastExpression_NotPlusMinus o = (CastExpression_NotPlusMinus) other;
    return java.util.Objects.equals(
      this.refAndBounds,
      o.refAndBounds) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(refAndBounds) + 3 * java.util.Objects.hashCode(expression);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CastExpression_NotPlusMinus other) {
    int cmp = 0;
    cmp = ((Comparable) refAndBounds).compareTo(other.refAndBounds);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) expression).compareTo(other.expression);
  }
  
  public CastExpression_NotPlusMinus withRefAndBounds(hydra.ext.java.syntax.CastExpression_RefAndBounds refAndBounds) {
    return new CastExpression_NotPlusMinus(refAndBounds, expression);
  }
  
  public CastExpression_NotPlusMinus withExpression(hydra.ext.java.syntax.UnaryExpression expression) {
    return new CastExpression_NotPlusMinus(refAndBounds, expression);
  }
}
