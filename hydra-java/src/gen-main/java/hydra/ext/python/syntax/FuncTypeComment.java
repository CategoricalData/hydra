// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class FuncTypeComment implements Serializable, Comparable<FuncTypeComment> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.FuncTypeComment");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.python.syntax.TypeComment value;
  
  public FuncTypeComment (hydra.ext.python.syntax.TypeComment value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FuncTypeComment)) {
      return false;
    }
    FuncTypeComment o = (FuncTypeComment) other;
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
  public int compareTo(FuncTypeComment other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
