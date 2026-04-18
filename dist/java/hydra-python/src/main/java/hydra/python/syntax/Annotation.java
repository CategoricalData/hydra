// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class Annotation implements Serializable, Comparable<Annotation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.Annotation");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.python.syntax.Expression value;

  public Annotation (hydra.python.syntax.Expression value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Annotation)) {
      return false;
    }
    Annotation o = (Annotation) other;
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
  public int compareTo(Annotation other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
