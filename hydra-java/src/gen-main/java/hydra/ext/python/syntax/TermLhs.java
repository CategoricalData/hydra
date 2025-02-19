// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TermLhs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TermLhs");
  
  public static final hydra.core.Name FIELD_NAME_OPERAND = new hydra.core.Name("operand");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public final hydra.ext.python.syntax.Term operand;
  
  public final hydra.ext.python.syntax.TermOp operator;
  
  public TermLhs (hydra.ext.python.syntax.Term operand, hydra.ext.python.syntax.TermOp operator) {
    java.util.Objects.requireNonNull((operand));
    java.util.Objects.requireNonNull((operator));
    this.operand = operand;
    this.operator = operator;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TermLhs)) {
      return false;
    }
    TermLhs o = (TermLhs) (other);
    return operand.equals(o.operand) && operator.equals(o.operator);
  }
  
  @Override
  public int hashCode() {
    return 2 * operand.hashCode() + 3 * operator.hashCode();
  }
  
  public TermLhs withOperand(hydra.ext.python.syntax.Term operand) {
    java.util.Objects.requireNonNull((operand));
    return new TermLhs(operand, operator);
  }
  
  public TermLhs withOperator(hydra.ext.python.syntax.TermOp operator) {
    java.util.Objects.requireNonNull((operator));
    return new TermLhs(operand, operator);
  }
}