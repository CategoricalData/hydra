// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * A floating-point literal
 */
public class FloatLiteral implements Serializable, Comparable<FloatLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.FloatLiteral");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name PRECISION = new hydra.core.Name("precision");

  /**
   * The float value
   */
  public final java.math.BigDecimal value;

  /**
   * Optional precision hint (e.g. 3.14d0 vs 3.14f0 in Common Lisp)
   */
  public final hydra.util.Maybe<String> precision;

  public FloatLiteral (java.math.BigDecimal value, hydra.util.Maybe<String> precision) {
    this.value = value;
    this.precision = precision;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FloatLiteral)) {
      return false;
    }
    FloatLiteral o = (FloatLiteral) other;
    return this.value.compareTo(o.value) == 0 && java.util.Objects.equals(
      this.precision,
      o.precision);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value) + 3 * java.util.Objects.hashCode(precision);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FloatLiteral other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      value,
      other.value);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      precision,
      other.precision);
  }

  public FloatLiteral withValue(java.math.BigDecimal value) {
    return new FloatLiteral(value, precision);
  }

  public FloatLiteral withPrecision(hydra.util.Maybe<String> precision) {
    return new FloatLiteral(value, precision);
  }
}
