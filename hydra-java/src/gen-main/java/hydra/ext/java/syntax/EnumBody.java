// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class EnumBody implements Serializable, Comparable<EnumBody> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.java.syntax.EnumBody");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.util.ConsList<hydra.ext.java.syntax.EnumBody_Element> value;
  
  public EnumBody (hydra.util.ConsList<hydra.ext.java.syntax.EnumBody_Element> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumBody)) {
      return false;
    }
    EnumBody o = (EnumBody) other;
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
  public int compareTo(EnumBody other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
