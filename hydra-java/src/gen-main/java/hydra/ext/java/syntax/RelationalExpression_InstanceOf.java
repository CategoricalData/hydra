// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class RelationalExpression_InstanceOf implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.RelationalExpression.InstanceOf");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.java.syntax.RelationalExpression lhs;
  
  public final hydra.ext.java.syntax.ReferenceType rhs;
  
  public RelationalExpression_InstanceOf (hydra.ext.java.syntax.RelationalExpression lhs, hydra.ext.java.syntax.ReferenceType rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
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
  
  public RelationalExpression_InstanceOf withLhs(hydra.ext.java.syntax.RelationalExpression lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new RelationalExpression_InstanceOf(lhs, rhs);
  }
  
  public RelationalExpression_InstanceOf withRhs(hydra.ext.java.syntax.ReferenceType rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new RelationalExpression_InstanceOf(lhs, rhs);
  }
}
