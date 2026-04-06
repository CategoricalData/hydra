// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.syntax;

import java.io.Serializable;

/**
 * An infix type application
 */
public class InfixType implements Serializable, Comparable<InfixType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.haskell.syntax.InfixType");

  public static final hydra.core.Name LHS = new hydra.core.Name("lhs");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  /**
   * The left-hand type
   */
  public final hydra.ext.haskell.syntax.Type lhs;

  /**
   * The type operator
   */
  public final hydra.ext.haskell.syntax.Operator operator;

  /**
   * The right-hand operator
   */
  public final hydra.ext.haskell.syntax.Operator rhs;

  public InfixType (hydra.ext.haskell.syntax.Type lhs, hydra.ext.haskell.syntax.Operator operator, hydra.ext.haskell.syntax.Operator rhs) {
    this.lhs = lhs;
    this.operator = operator;
    this.rhs = rhs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InfixType)) {
      return false;
    }
    InfixType o = (InfixType) other;
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
    cmp = hydra.util.Comparing.compare(
      lhs,
      other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      operator,
      other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      rhs,
      other.rhs);
  }

  public InfixType withLhs(hydra.ext.haskell.syntax.Type lhs) {
    return new InfixType(lhs, operator, rhs);
  }

  public InfixType withOperator(hydra.ext.haskell.syntax.Operator operator) {
    return new InfixType(lhs, operator, rhs);
  }

  public InfixType withRhs(hydra.ext.haskell.syntax.Operator rhs) {
    return new InfixType(lhs, operator, rhs);
  }
}
