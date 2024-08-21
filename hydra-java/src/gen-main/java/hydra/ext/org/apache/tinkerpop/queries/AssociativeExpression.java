// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.queries;

import java.io.Serializable;

public class AssociativeExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/queries.AssociativeExpression");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_OPERANDS = new hydra.core.Name("operands");
  
  public final hydra.ext.org.apache.tinkerpop.queries.BinaryOperator operator;
  
  public final java.util.List<hydra.ext.org.apache.tinkerpop.queries.Expression> operands;
  
  public AssociativeExpression (hydra.ext.org.apache.tinkerpop.queries.BinaryOperator operator, java.util.List<hydra.ext.org.apache.tinkerpop.queries.Expression> operands) {
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((operands));
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
  
  public AssociativeExpression withOperator(hydra.ext.org.apache.tinkerpop.queries.BinaryOperator operator) {
    java.util.Objects.requireNonNull((operator));
    return new AssociativeExpression(operator, operands);
  }
  
  public AssociativeExpression withOperands(java.util.List<hydra.ext.org.apache.tinkerpop.queries.Expression> operands) {
    java.util.Objects.requireNonNull((operands));
    return new AssociativeExpression(operator, operands);
  }
}