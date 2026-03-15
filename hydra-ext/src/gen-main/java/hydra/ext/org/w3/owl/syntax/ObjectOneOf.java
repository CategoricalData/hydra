// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class ObjectOneOf implements Serializable, Comparable<ObjectOneOf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectOneOf");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Individual> value;
  
  public ObjectOneOf (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Individual> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectOneOf)) {
      return false;
    }
    ObjectOneOf o = (ObjectOneOf) other;
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
  public int compareTo(ObjectOneOf other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
