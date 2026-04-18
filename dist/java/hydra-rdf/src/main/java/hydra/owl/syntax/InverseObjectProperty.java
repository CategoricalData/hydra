// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

public class InverseObjectProperty implements Serializable, Comparable<InverseObjectProperty> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.InverseObjectProperty");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.owl.syntax.ObjectProperty value;

  public InverseObjectProperty (hydra.owl.syntax.ObjectProperty value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InverseObjectProperty)) {
      return false;
    }
    InverseObjectProperty o = (InverseObjectProperty) other;
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
  public int compareTo(InverseObjectProperty other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
