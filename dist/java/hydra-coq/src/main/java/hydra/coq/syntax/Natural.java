// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * A non-negative arbitrary-precision integer
 */
public class Natural implements Serializable, Comparable<Natural> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Natural");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.math.BigInteger value;

  public Natural (java.math.BigInteger value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Natural)) {
      return false;
    }
    Natural o = (Natural) other;
    return this.value.compareTo(o.value) == 0;
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Natural other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
