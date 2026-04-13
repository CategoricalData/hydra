// Note: this is an automatically generated file. Do not edit.

package hydra.xml.schema;

import java.io.Serializable;

public class UnsignedLong implements Serializable, Comparable<UnsignedLong> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.xml.schema.UnsignedLong");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.math.BigInteger value;

  public UnsignedLong (java.math.BigInteger value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnsignedLong)) {
      return false;
    }
    UnsignedLong o = (UnsignedLong) other;
    return this.value.compareTo(o.value) == 0;
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnsignedLong other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
