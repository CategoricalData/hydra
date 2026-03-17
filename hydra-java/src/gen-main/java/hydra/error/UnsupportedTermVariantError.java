// Note: this is an automatically generated file. Do not edit.

package hydra.error;

import java.io.Serializable;

/**
 * A term variant that the type checker does not support
 */
public class UnsupportedTermVariantError implements Serializable, Comparable<UnsupportedTermVariantError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.UnsupportedTermVariantError");

  public static final hydra.core.Name TERM_VARIANT = new hydra.core.Name("termVariant");

  /**
   * The unsupported term variant
   */
  public final hydra.variants.TermVariant termVariant;

  public UnsupportedTermVariantError (hydra.variants.TermVariant termVariant) {
    this.termVariant = termVariant;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnsupportedTermVariantError)) {
      return false;
    }
    UnsupportedTermVariantError o = (UnsupportedTermVariantError) other;
    return java.util.Objects.equals(
      this.termVariant,
      o.termVariant);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(termVariant);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnsupportedTermVariantError other) {
    return ((Comparable) termVariant).compareTo(other.termVariant);
  }
}
