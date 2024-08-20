// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class NumericFacet_Sequence implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/io/shex/syntax.NumericFacet.Sequence");
  
  public static final hydra.core.Name FIELD_NAME_NUMERIC_RANGE = new hydra.core.Name("numericRange");
  
  public static final hydra.core.Name FIELD_NAME_NUMERIC_LITERAL = new hydra.core.Name("numericLiteral");
  
  public final hydra.ext.io.shex.syntax.NumericRange numericRange;
  
  public final hydra.ext.io.shex.syntax.NumericLiteral numericLiteral;
  
  public NumericFacet_Sequence (hydra.ext.io.shex.syntax.NumericRange numericRange, hydra.ext.io.shex.syntax.NumericLiteral numericLiteral) {
    java.util.Objects.requireNonNull((numericRange));
    java.util.Objects.requireNonNull((numericLiteral));
    this.numericRange = numericRange;
    this.numericLiteral = numericLiteral;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NumericFacet_Sequence)) {
      return false;
    }
    NumericFacet_Sequence o = (NumericFacet_Sequence) (other);
    return numericRange.equals(o.numericRange) && numericLiteral.equals(o.numericLiteral);
  }
  
  @Override
  public int hashCode() {
    return 2 * numericRange.hashCode() + 3 * numericLiteral.hashCode();
  }
  
  public NumericFacet_Sequence withNumericRange(hydra.ext.io.shex.syntax.NumericRange numericRange) {
    java.util.Objects.requireNonNull((numericRange));
    return new NumericFacet_Sequence(numericRange, numericLiteral);
  }
  
  public NumericFacet_Sequence withNumericLiteral(hydra.ext.io.shex.syntax.NumericLiteral numericLiteral) {
    java.util.Objects.requireNonNull((numericLiteral));
    return new NumericFacet_Sequence(numericRange, numericLiteral);
  }
}