// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class MethodName implements Serializable, Comparable<MethodName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.MethodName");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.java.syntax.Identifier value;

  public MethodName (hydra.java.syntax.Identifier value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodName)) {
      return false;
    }
    MethodName o = (MethodName) other;
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
  public int compareTo(MethodName other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
