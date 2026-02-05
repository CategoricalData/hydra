// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An infix type application
 */
public class InfixType implements Serializable, Comparable<InfixType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.InfixType");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  /**
   * The left-hand type
   */
  public final hydra.ext.haskell.ast.Type lhs;
  
  /**
   * The type operator
   */
  public final hydra.ext.haskell.ast.Operator operator;
  
  /**
   * The right-hand operator
   */
  public final hydra.ext.haskell.ast.Operator rhs;
  
  public InfixType (hydra.ext.haskell.ast.Type lhs, hydra.ext.haskell.ast.Operator operator, hydra.ext.haskell.ast.Operator rhs) {
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
    return java.util.Objects.equals(
      this.lhs,
      o.lhs) && java.util.Objects.equals(
      this.operator,
      o.operator) && java.util.Objects.equals(
      this.rhs,
      o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(lhs) + 3 * java.util.Objects.hashCode(operator) + 5 * java.util.Objects.hashCode(rhs);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InfixType other) {
    int cmp = 0;
    cmp = ((Comparable) (lhs)).compareTo(other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (operator)).compareTo(other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (rhs)).compareTo(other.rhs);
  }
  
  public InfixType withLhs(hydra.ext.haskell.ast.Type lhs) {
    return new InfixType(lhs, operator, rhs);
  }
  
  public InfixType withOperator(hydra.ext.haskell.ast.Operator operator) {
    return new InfixType(lhs, operator, rhs);
  }
  
  public InfixType withRhs(hydra.ext.haskell.ast.Operator rhs) {
    return new InfixType(lhs, operator, rhs);
  }
}
