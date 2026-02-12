// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class EnumBody implements Serializable, Comparable<EnumBody> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.EnumBody");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.java.syntax.EnumBody_Element> value;
  
  public EnumBody (java.util.List<hydra.ext.java.syntax.EnumBody_Element> value) {
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
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
