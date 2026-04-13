// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * An integer literal
 */
public class IntegerLiteral implements Serializable, Comparable<IntegerLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.IntegerLiteral");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name BIGINT = new hydra.core.Name("bigint");

  /**
   * The integer value
   */
  public final java.math.BigInteger value;

  /**
   * Whether this is explicitly a big integer (e.g. 42N in Clojure)
   */
  public final Boolean bigint;

  public IntegerLiteral (java.math.BigInteger value, Boolean bigint) {
    this.value = value;
    this.bigint = bigint;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IntegerLiteral)) {
      return false;
    }
    IntegerLiteral o = (IntegerLiteral) other;
    return this.value.compareTo(o.value) == 0 && java.util.Objects.equals(
      this.bigint,
      o.bigint);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value) + 3 * java.util.Objects.hashCode(bigint);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(IntegerLiteral other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      value,
      other.value);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      bigint,
      other.bigint);
  }

  public IntegerLiteral withValue(java.math.BigInteger value) {
    return new IntegerLiteral(value, bigint);
  }

  public IntegerLiteral withBigint(Boolean bigint) {
    return new IntegerLiteral(value, bigint);
  }
}
