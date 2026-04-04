// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class Integer_ implements Serializable, Comparable<Integer_> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.Integer");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public Integer_ (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Integer_)) {
      return false;
    }
    Integer_ o = (Integer_) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Integer_ other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
