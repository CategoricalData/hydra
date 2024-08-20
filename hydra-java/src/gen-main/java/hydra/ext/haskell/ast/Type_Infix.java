// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public class Type_Infix implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/haskell/ast.Type.Infix");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.haskell.ast.Type lhs;
  
  public final hydra.ext.haskell.ast.Operator operator;
  
  public final hydra.ext.haskell.ast.Operator rhs;
  
  public Type_Infix (hydra.ext.haskell.ast.Type lhs, hydra.ext.haskell.ast.Operator operator, hydra.ext.haskell.ast.Operator rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.operator = operator;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Infix)) {
      return false;
    }
    Type_Infix o = (Type_Infix) (other);
    return lhs.equals(o.lhs) && operator.equals(o.operator) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * operator.hashCode() + 5 * rhs.hashCode();
  }
  
  public Type_Infix withLhs(hydra.ext.haskell.ast.Type lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new Type_Infix(lhs, operator, rhs);
  }
  
  public Type_Infix withOperator(hydra.ext.haskell.ast.Operator operator) {
    java.util.Objects.requireNonNull((operator));
    return new Type_Infix(lhs, operator, rhs);
  }
  
  public Type_Infix withRhs(hydra.ext.haskell.ast.Operator rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Type_Infix(lhs, operator, rhs);
  }
}
