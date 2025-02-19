// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class SumLhs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SumLhs");
  
  public static final hydra.core.Name FIELD_NAME_OPERAND = new hydra.core.Name("operand");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public final hydra.ext.python.syntax.Sum operand;
  
  public final hydra.ext.python.syntax.SumOp operator;
  
  public SumLhs (hydra.ext.python.syntax.Sum operand, hydra.ext.python.syntax.SumOp operator) {
    java.util.Objects.requireNonNull((operand));
    java.util.Objects.requireNonNull((operator));
    this.operand = operand;
    this.operator = operator;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SumLhs)) {
      return false;
    }
    SumLhs o = (SumLhs) (other);
    return operand.equals(o.operand) && operator.equals(o.operator);
  }
  
  @Override
  public int hashCode() {
    return 2 * operand.hashCode() + 3 * operator.hashCode();
  }
  
  public SumLhs withOperand(hydra.ext.python.syntax.Sum operand) {
    java.util.Objects.requireNonNull((operand));
    return new SumLhs(operand, operator);
  }
  
  public SumLhs withOperator(hydra.ext.python.syntax.SumOp operator) {
    java.util.Objects.requireNonNull((operator));
    return new SumLhs(operand, operator);
  }
}