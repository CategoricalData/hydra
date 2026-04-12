// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.xml.schema;

import java.io.Serializable;

public class NonNegativeInteger implements Serializable, Comparable<NonNegativeInteger> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.xml.schema.NonNegativeInteger");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.math.BigInteger value;

  public NonNegativeInteger (java.math.BigInteger value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NonNegativeInteger)) {
      return false;
    }
    NonNegativeInteger o = (NonNegativeInteger) other;
    return this.value.compareTo(o.value) == 0;
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NonNegativeInteger other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
