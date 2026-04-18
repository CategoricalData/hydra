// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class TypeComment implements Serializable, Comparable<TypeComment> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.TypeComment");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public TypeComment (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeComment)) {
      return false;
    }
    TypeComment o = (TypeComment) other;
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
  public int compareTo(TypeComment other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
