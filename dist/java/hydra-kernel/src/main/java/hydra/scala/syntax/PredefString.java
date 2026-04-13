// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class PredefString implements Serializable, Comparable<PredefString> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.PredefString");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public PredefString (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PredefString)) {
      return false;
    }
    PredefString o = (PredefString) other;
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
  public int compareTo(PredefString other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
