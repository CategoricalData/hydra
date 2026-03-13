// Note: this is an automatically generated file. Do not edit.

package hydra.error;

import java.io.Serializable;

/**
 * An unexpected term variant was encountered
 */
public class UnexpectedTermVariantError implements Serializable, Comparable<UnexpectedTermVariantError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.UnexpectedTermVariantError");
  
  public static final hydra.core.Name EXPECTED_VARIANT = new hydra.core.Name("expectedVariant");
  
  public static final hydra.core.Name ACTUAL_TERM = new hydra.core.Name("actualTerm");
  
  /**
   * The expected term variant
   */
  public final hydra.variants.TermVariant expectedVariant;
  
  /**
   * The actual term that was encountered
   */
  public final hydra.core.Term actualTerm;
  
  public UnexpectedTermVariantError (hydra.variants.TermVariant expectedVariant, hydra.core.Term actualTerm) {
    this.expectedVariant = expectedVariant;
    this.actualTerm = actualTerm;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnexpectedTermVariantError)) {
      return false;
    }
    UnexpectedTermVariantError o = (UnexpectedTermVariantError) other;
    return java.util.Objects.equals(
      this.expectedVariant,
      o.expectedVariant) && java.util.Objects.equals(
      this.actualTerm,
      o.actualTerm);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expectedVariant) + 3 * java.util.Objects.hashCode(actualTerm);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnexpectedTermVariantError other) {
    int cmp = 0;
    cmp = ((Comparable) expectedVariant).compareTo(other.expectedVariant);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) actualTerm).compareTo(other.actualTerm);
  }
  
  public UnexpectedTermVariantError withExpectedVariant(hydra.variants.TermVariant expectedVariant) {
    return new UnexpectedTermVariantError(expectedVariant, actualTerm);
  }
  
  public UnexpectedTermVariantError withActualTerm(hydra.core.Term actualTerm) {
    return new UnexpectedTermVariantError(expectedVariant, actualTerm);
  }
}
