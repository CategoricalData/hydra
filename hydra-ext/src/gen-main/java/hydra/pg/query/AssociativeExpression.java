// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class AssociativeExpression implements Serializable, Comparable<AssociativeExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.query.AssociativeExpression");
  
  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name OPERANDS = new hydra.core.Name("operands");
  
  public final hydra.pg.query.BinaryOperator operator;
  
  public final hydra.util.ConsList<hydra.pg.query.Expression> operands;
  
  public AssociativeExpression (hydra.pg.query.BinaryOperator operator, hydra.util.ConsList<hydra.pg.query.Expression> operands) {
    this.operator = operator;
    this.operands = operands;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AssociativeExpression)) {
      return false;
    }
    AssociativeExpression o = (AssociativeExpression) other;
    return java.util.Objects.equals(
      this.operator,
      o.operator) && java.util.Objects.equals(
      this.operands,
      o.operands);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(operator) + 3 * java.util.Objects.hashCode(operands);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AssociativeExpression other) {
    int cmp = 0;
    cmp = ((Comparable) operator).compareTo(other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      operands.hashCode(),
      other.operands.hashCode());
  }
  
  public AssociativeExpression withOperator(hydra.pg.query.BinaryOperator operator) {
    return new AssociativeExpression(operator, operands);
  }
  
  public AssociativeExpression withOperands(hydra.util.ConsList<hydra.pg.query.Expression> operands) {
    return new AssociativeExpression(operator, operands);
  }
}
