// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class ObjectHasSelf implements Serializable, Comparable<ObjectHasSelf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectHasSelf");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression value;

  public ObjectHasSelf (hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression value) {
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
    return ((Comparable) value).compareTo(other.value);
  }
}
