// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class AmbiguousName implements Serializable, Comparable<AmbiguousName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.java.syntax.AmbiguousName");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.util.ConsList<hydra.ext.java.syntax.Identifier> value;
  
  public AmbiguousName (hydra.util.ConsList<hydra.ext.java.syntax.Identifier> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AmbiguousName)) {
      return false;
    }
    AmbiguousName o = (AmbiguousName) other;
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
  public int compareTo(AmbiguousName other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
