// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class ObjectComplementOf implements Serializable, Comparable<ObjectComplementOf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectComplementOf");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.ext.org.w3.owl.syntax.ClassExpression value;

  public ObjectComplementOf (hydra.ext.org.w3.owl.syntax.ClassExpression value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectComplementOf)) {
      return false;
    }
    ObjectComplementOf o = (ObjectComplementOf) other;
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
  public int compareTo(ObjectComplementOf other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
