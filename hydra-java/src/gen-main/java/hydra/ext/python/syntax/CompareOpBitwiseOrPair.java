// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class CompareOpBitwiseOrPair implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.CompareOpBitwiseOrPair");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.python.syntax.CompareOp operator;
  
  public final hydra.ext.python.syntax.BitwiseOr rhs;
  
  public CompareOpBitwiseOrPair (hydra.ext.python.syntax.CompareOp operator, hydra.ext.python.syntax.BitwiseOr rhs) {
    java.util.Objects.requireNonNull((operator));
    java.util.Objects.requireNonNull((rhs));
    this.operator = operator;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CompareOpBitwiseOrPair)) {
      return false;
    }
    CompareOpBitwiseOrPair o = (CompareOpBitwiseOrPair) (other);
    return operator.equals(o.operator) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * operator.hashCode() + 3 * rhs.hashCode();
  }
  
  public CompareOpBitwiseOrPair withOperator(hydra.ext.python.syntax.CompareOp operator) {
    java.util.Objects.requireNonNull((operator));
    return new CompareOpBitwiseOrPair(operator, rhs);
  }
  
  public CompareOpBitwiseOrPair withRhs(hydra.ext.python.syntax.BitwiseOr rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new CompareOpBitwiseOrPair(operator, rhs);
  }
}