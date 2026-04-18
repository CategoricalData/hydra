// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class InterfaceType implements Serializable, Comparable<InterfaceType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.InterfaceType");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.java.syntax.ClassType value;

  public InterfaceType (hydra.java.syntax.ClassType value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InterfaceType)) {
      return false;
    }
    InterfaceType o = (InterfaceType) other;
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
  public int compareTo(InterfaceType other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
