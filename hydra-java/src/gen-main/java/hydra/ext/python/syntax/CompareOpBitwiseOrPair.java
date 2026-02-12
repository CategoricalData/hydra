// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class CompareOpBitwiseOrPair implements Serializable, Comparable<CompareOpBitwiseOrPair> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.CompareOpBitwiseOrPair");
  
  public static final hydra.core.Name FIELD_NAME_OPERATOR = new hydra.core.Name("operator");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.python.syntax.CompareOp operator;
  
  public final hydra.ext.python.syntax.BitwiseOr rhs;
  
  public CompareOpBitwiseOrPair (hydra.ext.python.syntax.CompareOp operator, hydra.ext.python.syntax.BitwiseOr rhs) {
    this.operator = operator;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CompareOpBitwiseOrPair)) {
      return false;
    }
    CompareOpBitwiseOrPair o = (CompareOpBitwiseOrPair) other;
    return java.util.Objects.equals(
      this.operator,
      o.operator) && java.util.Objects.equals(
      this.rhs,
      o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(operator) + 3 * java.util.Objects.hashCode(rhs);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CompareOpBitwiseOrPair other) {
    int cmp = 0;
    cmp = ((Comparable) operator).compareTo(other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rhs).compareTo(other.rhs);
  }
  
  public CompareOpBitwiseOrPair withOperator(hydra.ext.python.syntax.CompareOp operator) {
    return new CompareOpBitwiseOrPair(operator, rhs);
  }
  
  public CompareOpBitwiseOrPair withRhs(hydra.ext.python.syntax.BitwiseOr rhs) {
    return new CompareOpBitwiseOrPair(operator, rhs);
  }
}
