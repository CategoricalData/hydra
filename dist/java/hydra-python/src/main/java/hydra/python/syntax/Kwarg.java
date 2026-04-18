// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class Kwarg implements Serializable, Comparable<Kwarg> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.Kwarg");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.python.syntax.Name name;

  public final hydra.python.syntax.Expression value;

  public Kwarg (hydra.python.syntax.Name name, hydra.python.syntax.Expression value) {
    this.name = name;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Kwarg)) {
      return false;
    }
    Kwarg o = (Kwarg) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Kwarg other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }

  public Kwarg withName(hydra.python.syntax.Name name) {
    return new Kwarg(name, value);
  }

  public Kwarg withValue(hydra.python.syntax.Expression value) {
    return new Kwarg(name, value);
  }
}
