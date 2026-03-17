// Note: this is an automatically generated file. Do not edit.

package hydra.error;

import java.io.Serializable;

/**
 * An unexpected type variant was encountered
 */
public class UnexpectedTypeVariantError implements Serializable, Comparable<UnexpectedTypeVariantError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.UnexpectedTypeVariantError");

  public static final hydra.core.Name EXPECTED_VARIANT = new hydra.core.Name("expectedVariant");

  public static final hydra.core.Name ACTUAL_TYPE = new hydra.core.Name("actualType");

  /**
   * The expected type variant
   */
  public final hydra.variants.TypeVariant expectedVariant;

  /**
   * The actual type that was encountered
   */
  public final hydra.core.Type actualType;

  public UnexpectedTypeVariantError (hydra.variants.TypeVariant expectedVariant, hydra.core.Type actualType) {
    this.expectedVariant = expectedVariant;
    this.actualType = actualType;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnexpectedTypeVariantError)) {
      return false;
    }
    UnexpectedTypeVariantError o = (UnexpectedTypeVariantError) other;
    return java.util.Objects.equals(
      this.expectedVariant,
      o.expectedVariant) && java.util.Objects.equals(
      this.actualType,
      o.actualType);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expectedVariant) + 3 * java.util.Objects.hashCode(actualType);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnexpectedTypeVariantError other) {
    int cmp = 0;
    cmp = ((Comparable) expectedVariant).compareTo(other.expectedVariant);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) actualType).compareTo(other.actualType);
  }

  public UnexpectedTypeVariantError withExpectedVariant(hydra.variants.TypeVariant expectedVariant) {
    return new UnexpectedTypeVariantError(expectedVariant, actualType);
  }

  public UnexpectedTypeVariantError withActualType(hydra.core.Type actualType) {
    return new UnexpectedTypeVariantError(expectedVariant, actualType);
  }
}
