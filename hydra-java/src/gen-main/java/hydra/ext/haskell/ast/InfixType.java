// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public class InfixType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.InfixType");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.haskell.ast.Type lhs;
  
  public final hydra.ext.haskell.ast.Operator operator;
  
  public final hydra.ext.haskell.ast.Operator rhs;
  
  public InfixType (hydra.ext.haskell.ast.Type lhs, hydra.ext.haskell.ast.Operator operator, hydra.ext.haskell.ast.Operator rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.operator = operator;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InfixType)) {
      return false;
    }
    InfixType o = (InfixType) (other);
    return lhs.equals(o.lhs) && operator.equals(o.operator) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * operator.hashCode() + 5 * rhs.hashCode();
  }
  
  public InfixType withLhs(hydra.ext.haskell.ast.Type lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new InfixType(lhs, operator, rhs);
  }
  
  public InfixType withOperator(hydra.ext.haskell.ast.Operator operator) {
    java.util.Objects.requireNonNull((operator));
    return new InfixType(lhs, operator, rhs);
  }
  
  public InfixType withRhs(hydra.ext.haskell.ast.Operator rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new InfixType(lhs, operator, rhs);
  }
}