// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class NumericFacet_Sequence2 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.NumericFacet_Sequence2");
  
  public static final hydra.core.Name FIELD_NAME_NUMERIC_LENGTH = new hydra.core.Name("numericLength");
  
  public static final hydra.core.Name FIELD_NAME_INTEGER = new hydra.core.Name("integer");
  
  public final hydra.ext.io.shex.syntax.NumericLength numericLength;
  
  public final hydra.ext.io.shex.syntax.Integer_ integer;
  
  public NumericFacet_Sequence2 (hydra.ext.io.shex.syntax.NumericLength numericLength, hydra.ext.io.shex.syntax.Integer_ integer) {
    java.util.Objects.requireNonNull((numericLength));
    java.util.Objects.requireNonNull((integer));
    this.numericLength = numericLength;
    this.integer = integer;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NumericFacet_Sequence2)) {
      return false;
    }
    NumericFacet_Sequence2 o = (NumericFacet_Sequence2) (other);
    return numericLength.equals(o.numericLength) && integer.equals(o.integer);
  }
  
  @Override
  public int hashCode() {
    return 2 * numericLength.hashCode() + 3 * integer.hashCode();
  }
  
  public NumericFacet_Sequence2 withNumericLength(hydra.ext.io.shex.syntax.NumericLength numericLength) {
    java.util.Objects.requireNonNull((numericLength));
    return new NumericFacet_Sequence2(numericLength, integer);
  }
  
  public NumericFacet_Sequence2 withInteger(hydra.ext.io.shex.syntax.Integer_ integer) {
    java.util.Objects.requireNonNull((integer));
    return new NumericFacet_Sequence2(numericLength, integer);
  }
}