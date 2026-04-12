// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.xml.schema;

import java.io.Serializable;

public class Integer_ implements Serializable, Comparable<Integer_> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.xml.schema.Integer");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.math.BigInteger value;

  public Integer_ (java.math.BigInteger value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Integer_)) {
      return false;
    }
    Integer_ o = (Integer_) other;
    return this.value.compareTo(o.value) == 0;
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Integer_ other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
