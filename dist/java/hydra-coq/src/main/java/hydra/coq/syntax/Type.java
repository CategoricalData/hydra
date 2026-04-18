// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class Type implements Serializable, Comparable<Type> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Type");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.coq.syntax.Term value;

  public Type (hydra.coq.syntax.Term value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type)) {
      return false;
    }
    Type o = (Type) other;
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
  public int compareTo(Type other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
