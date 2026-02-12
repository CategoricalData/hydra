// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class CastExpression_Lambda implements Serializable, Comparable<CastExpression_Lambda> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.CastExpression_Lambda");
  
  public static final hydra.core.Name FIELD_NAME_REF_AND_BOUNDS = new hydra.core.Name("refAndBounds");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.java.syntax.CastExpression_RefAndBounds refAndBounds;
  
  public final hydra.ext.java.syntax.LambdaExpression expression;
  
  public CastExpression_Lambda (hydra.ext.java.syntax.CastExpression_RefAndBounds refAndBounds, hydra.ext.java.syntax.LambdaExpression expression) {
    this.refAndBounds = refAndBounds;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CastExpression_Lambda)) {
      return false;
    }
    CastExpression_Lambda o = (CastExpression_Lambda) other;
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
  public int compareTo(CastExpression_Lambda other) {
    int cmp = 0;
    cmp = ((Comparable) refAndBounds).compareTo(other.refAndBounds);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) expression).compareTo(other.expression);
  }
  
  public CastExpression_Lambda withRefAndBounds(hydra.ext.java.syntax.CastExpression_RefAndBounds refAndBounds) {
    return new CastExpression_Lambda(refAndBounds, expression);
  }
  
  public CastExpression_Lambda withExpression(hydra.ext.java.syntax.LambdaExpression expression) {
    return new CastExpression_Lambda(refAndBounds, expression);
  }
}
