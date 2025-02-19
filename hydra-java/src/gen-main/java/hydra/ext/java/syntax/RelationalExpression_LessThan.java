// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class RelationalExpression_LessThan implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.RelationalExpression_LessThan");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.java.syntax.RelationalExpression lhs;
  
  public final hydra.ext.java.syntax.ShiftExpression rhs;
  
  public RelationalExpression_LessThan (hydra.ext.java.syntax.RelationalExpression lhs, hydra.ext.java.syntax.ShiftExpression rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationalExpression_LessThan)) {
      return false;
    }
    RelationalExpression_LessThan o = (RelationalExpression_LessThan) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public RelationalExpression_LessThan withLhs(hydra.ext.java.syntax.RelationalExpression lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new RelationalExpression_LessThan(lhs, rhs);
  }
  
  public RelationalExpression_LessThan withRhs(hydra.ext.java.syntax.ShiftExpression rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new RelationalExpression_LessThan(lhs, rhs);
  }
}