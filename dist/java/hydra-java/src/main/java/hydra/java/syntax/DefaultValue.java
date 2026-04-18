// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class DefaultValue implements Serializable, Comparable<DefaultValue> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.DefaultValue");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.java.syntax.ElementValue value;

  public DefaultValue (hydra.java.syntax.ElementValue value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DefaultValue)) {
      return false;
    }
    DefaultValue o = (DefaultValue) other;
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
  public int compareTo(DefaultValue other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
