// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class MethodReference_Array implements Serializable, Comparable<MethodReference_Array> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.MethodReference_Array");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.java.syntax.ArrayType value;

  public MethodReference_Array (hydra.java.syntax.ArrayType value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodReference_Array)) {
      return false;
    }
    MethodReference_Array o = (MethodReference_Array) other;
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
  public int compareTo(MethodReference_Array other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
