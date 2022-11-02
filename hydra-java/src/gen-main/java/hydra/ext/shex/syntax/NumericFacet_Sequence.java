package hydra.ext.shex.syntax;

public class NumericFacet_Sequence {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.NumericFacet.Sequence");
  
  public final hydra.ext.shex.syntax.NumericRange numericRange;
  
  public final hydra.ext.shex.syntax.NumericLiteral numericLiteral;
  
  public NumericFacet_Sequence (hydra.ext.shex.syntax.NumericRange numericRange, hydra.ext.shex.syntax.NumericLiteral numericLiteral) {
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
  
  public NumericFacet_Sequence withNumericRange(hydra.ext.shex.syntax.NumericRange numericRange) {
    return new NumericFacet_Sequence(numericRange, numericLiteral);
  }
  
  public NumericFacet_Sequence withNumericLiteral(hydra.ext.shex.syntax.NumericLiteral numericLiteral) {
    return new NumericFacet_Sequence(numericRange, numericLiteral);
  }
}