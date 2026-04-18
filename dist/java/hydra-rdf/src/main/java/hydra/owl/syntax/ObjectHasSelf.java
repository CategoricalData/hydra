// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

public class ObjectHasSelf implements Serializable, Comparable<ObjectHasSelf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.ObjectHasSelf");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.owl.syntax.ObjectPropertyExpression value;

  public ObjectHasSelf (hydra.owl.syntax.ObjectPropertyExpression value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectHasSelf)) {
      return false;
    }
    ObjectHasSelf o = (ObjectHasSelf) other;
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
  public int compareTo(ObjectHasSelf other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
