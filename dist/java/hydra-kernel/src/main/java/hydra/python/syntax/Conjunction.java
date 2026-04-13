// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class Conjunction implements Serializable, Comparable<Conjunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.Conjunction");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<hydra.python.syntax.Inversion> value;

  public Conjunction (java.util.List<hydra.python.syntax.Inversion> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Conjunction)) {
      return false;
    }
    Conjunction o = (Conjunction) other;
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
  public int compareTo(Conjunction other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
