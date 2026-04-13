// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Quasi implements Serializable, Comparable<Quasi> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Quasi");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.lang.Void value;

  public Quasi (java.lang.Void value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Quasi)) {
      return false;
    }
    Quasi o = (Quasi) other;
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
  public int compareTo(Quasi other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
