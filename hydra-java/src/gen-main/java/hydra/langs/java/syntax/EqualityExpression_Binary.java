// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class EqualityExpression_Binary implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.EqualityExpression.Binary");
  
  public final hydra.langs.java.syntax.EqualityExpression lhs;
  
  public final hydra.langs.java.syntax.RelationalExpression rhs;
  
  public EqualityExpression_Binary (hydra.langs.java.syntax.EqualityExpression lhs, hydra.langs.java.syntax.RelationalExpression rhs) {
    if (lhs == null) {
      throw new IllegalArgumentException("null value for 'lhs' argument");
    }
    if (rhs == null) {
      throw new IllegalArgumentException("null value for 'rhs' argument");
    }
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EqualityExpression_Binary)) {
      return false;
    }
    EqualityExpression_Binary o = (EqualityExpression_Binary) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public EqualityExpression_Binary withLhs(hydra.langs.java.syntax.EqualityExpression lhs) {
    if (lhs == null) {
      throw new IllegalArgumentException("null value for 'lhs' argument");
    }
    return new EqualityExpression_Binary(lhs, rhs);
  }
  
  public EqualityExpression_Binary withRhs(hydra.langs.java.syntax.RelationalExpression rhs) {
    if (rhs == null) {
      throw new IllegalArgumentException("null value for 'rhs' argument");
    }
    return new EqualityExpression_Binary(lhs, rhs);
  }
}