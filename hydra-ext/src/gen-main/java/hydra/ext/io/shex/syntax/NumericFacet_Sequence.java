// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class NumericFacet_Sequence implements Serializable, Comparable<NumericFacet_Sequence> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.NumericFacet_Sequence");
  
  public static final hydra.core.Name NUMERIC_RANGE = new hydra.core.Name("NumericRange");
  
  public static final hydra.core.Name NUMERIC_LITERAL = new hydra.core.Name("NumericLiteral");
  
  public final hydra.ext.io.shex.syntax.NumericRange NumericRange;
  
  public final hydra.ext.io.shex.syntax.NumericLiteral NumericLiteral;
  
  public NumericFacet_Sequence (hydra.ext.io.shex.syntax.NumericRange NumericRange, hydra.ext.io.shex.syntax.NumericLiteral NumericLiteral) {
    this.NumericRange = NumericRange;
    this.NumericLiteral = NumericLiteral;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NumericFacet_Sequence)) {
      return false;
    }
    NumericFacet_Sequence o = (NumericFacet_Sequence) other;
    return java.util.Objects.equals(
      this.NumericRange,
      o.NumericRange) && java.util.Objects.equals(
      this.NumericLiteral,
      o.NumericLiteral);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(NumericRange) + 3 * java.util.Objects.hashCode(NumericLiteral);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NumericFacet_Sequence other) {
    int cmp = 0;
    cmp = ((Comparable) NumericRange).compareTo(other.NumericRange);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) NumericLiteral).compareTo(other.NumericLiteral);
  }
  
  public NumericFacet_Sequence withNumericRange(hydra.ext.io.shex.syntax.NumericRange NumericRange) {
    return new NumericFacet_Sequence(NumericRange, NumericLiteral);
  }
  
  public NumericFacet_Sequence withNumericLiteral(hydra.ext.io.shex.syntax.NumericLiteral NumericLiteral) {
    return new NumericFacet_Sequence(NumericRange, NumericLiteral);
  }
}
