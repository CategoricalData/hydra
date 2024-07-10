// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class RelationalExpression_InstanceOf implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.RelationalExpression.InstanceOf");
  
  public final hydra.langs.java.syntax.RelationalExpression lhs;
  
  public final hydra.langs.java.syntax.ReferenceType rhs;
  
  public RelationalExpression_InstanceOf (hydra.langs.java.syntax.RelationalExpression lhs, hydra.langs.java.syntax.ReferenceType rhs) {
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
    if (!(other instanceof RelationalExpression_InstanceOf)) {
      return false;
    }
    RelationalExpression_InstanceOf o = (RelationalExpression_InstanceOf) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public RelationalExpression_InstanceOf withLhs(hydra.langs.java.syntax.RelationalExpression lhs) {
    if (lhs == null) {
      throw new IllegalArgumentException("null value for 'lhs' argument");
    }
    return new RelationalExpression_InstanceOf(lhs, rhs);
  }
  
  public RelationalExpression_InstanceOf withRhs(hydra.langs.java.syntax.ReferenceType rhs) {
    if (rhs == null) {
      throw new IllegalArgumentException("null value for 'rhs' argument");
    }
    return new RelationalExpression_InstanceOf(lhs, rhs);
  }
}