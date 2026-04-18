// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class ArrayInitializer implements Serializable, Comparable<ArrayInitializer> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.ArrayInitializer");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<java.util.List<hydra.java.syntax.VariableInitializer>> value;

  public ArrayInitializer (java.util.List<java.util.List<hydra.java.syntax.VariableInitializer>> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrayInitializer)) {
      return false;
    }
    ArrayInitializer o = (ArrayInitializer) other;
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
  public int compareTo(ArrayInitializer other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
