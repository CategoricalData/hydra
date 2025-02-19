// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class CastExpression_Lambda implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.CastExpression_Lambda");
  
  public static final hydra.core.Name FIELD_NAME_REF_AND_BOUNDS = new hydra.core.Name("refAndBounds");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.ext.java.syntax.CastExpression_RefAndBounds refAndBounds;
  
  public final hydra.ext.java.syntax.LambdaExpression expression;
  
  public CastExpression_Lambda (hydra.ext.java.syntax.CastExpression_RefAndBounds refAndBounds, hydra.ext.java.syntax.LambdaExpression expression) {
    java.util.Objects.requireNonNull((refAndBounds));
    java.util.Objects.requireNonNull((expression));
    this.refAndBounds = refAndBounds;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CastExpression_Lambda)) {
      return false;
    }
    CastExpression_Lambda o = (CastExpression_Lambda) (other);
    return refAndBounds.equals(o.refAndBounds) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * refAndBounds.hashCode() + 3 * expression.hashCode();
  }
  
  public CastExpression_Lambda withRefAndBounds(hydra.ext.java.syntax.CastExpression_RefAndBounds refAndBounds) {
    java.util.Objects.requireNonNull((refAndBounds));
    return new CastExpression_Lambda(refAndBounds, expression);
  }
  
  public CastExpression_Lambda withExpression(hydra.ext.java.syntax.LambdaExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new CastExpression_Lambda(refAndBounds, expression);
  }
}