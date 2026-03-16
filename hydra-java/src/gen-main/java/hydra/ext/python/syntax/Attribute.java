// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Attribute implements Serializable, Comparable<Attribute> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.python.syntax.Attribute");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.util.ConsList<hydra.ext.python.syntax.Name> value;
  
  public Attribute (hydra.util.ConsList<hydra.ext.python.syntax.Name> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Attribute)) {
      return false;
    }
    Attribute o = (Attribute) other;
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
  public int compareTo(Attribute other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
