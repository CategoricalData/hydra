// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class Code implements Serializable, Comparable<Code> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.Code");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.io.shex.syntax.Code_Elmt> value;
  
  public Code (java.util.List<hydra.ext.io.shex.syntax.Code_Elmt> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Code)) {
      return false;
    }
    Code o = (Code) other;
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
  public int compareTo(Code other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
