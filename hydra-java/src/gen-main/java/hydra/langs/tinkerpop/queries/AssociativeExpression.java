package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public class AssociativeExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.AssociativeExpression");
  
  public final hydra.langs.tinkerpop.queries.BinaryOperator operator;
  
  public final java.util.List<hydra.langs.tinkerpop.queries.Expression> operands;
  
  public AssociativeExpression (hydra.langs.tinkerpop.queries.BinaryOperator operator, java.util.List<hydra.langs.tinkerpop.queries.Expression> operands) {
    this.operator = operator;
    this.operands = operands;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AssociativeExpression)) {
      return false;
    }
    AssociativeExpression o = (AssociativeExpression) (other);
    return operator.equals(o.operator) && operands.equals(o.operands);
  }
  
  @Override
  public int hashCode() {
    return 2 * operator.hashCode() + 3 * operands.hashCode();
  }
  
  public AssociativeExpression withOperator(hydra.langs.tinkerpop.queries.BinaryOperator operator) {
    return new AssociativeExpression(operator, operands);
  }
  
  public AssociativeExpression withOperands(java.util.List<hydra.langs.tinkerpop.queries.Expression> operands) {
    return new AssociativeExpression(operator, operands);
  }
}