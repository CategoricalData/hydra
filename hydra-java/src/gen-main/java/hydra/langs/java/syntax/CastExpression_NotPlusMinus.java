// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class CastExpression_NotPlusMinus implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.CastExpression.NotPlusMinus");
  
  public final hydra.langs.java.syntax.CastExpression_RefAndBounds refAndBounds;
  
  public final hydra.langs.java.syntax.UnaryExpression expression;
  
  public CastExpression_NotPlusMinus (hydra.langs.java.syntax.CastExpression_RefAndBounds refAndBounds, hydra.langs.java.syntax.UnaryExpression expression) {
    if (refAndBounds == null) {
      throw new IllegalArgumentException("null value for 'refAndBounds' argument");
    }
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
    this.refAndBounds = refAndBounds;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CastExpression_NotPlusMinus)) {
      return false;
    }
    CastExpression_NotPlusMinus o = (CastExpression_NotPlusMinus) (other);
    return refAndBounds.equals(o.refAndBounds) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * refAndBounds.hashCode() + 3 * expression.hashCode();
  }
  
  public CastExpression_NotPlusMinus withRefAndBounds(hydra.langs.java.syntax.CastExpression_RefAndBounds refAndBounds) {
    if (refAndBounds == null) {
      throw new IllegalArgumentException("null value for 'refAndBounds' argument");
    }
    return new CastExpression_NotPlusMinus(refAndBounds, expression);
  }
  
  public CastExpression_NotPlusMinus withExpression(hydra.langs.java.syntax.UnaryExpression expression) {
    if (expression == null) {
      throw new IllegalArgumentException("null value for 'expression' argument");
    }
    return new CastExpression_NotPlusMinus(refAndBounds, expression);
  }
}