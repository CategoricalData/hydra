// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class SwitchBlock implements Serializable, Comparable<SwitchBlock> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.SwitchBlock");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.java.syntax.SwitchBlock_Pair> value;
  
  public SwitchBlock (java.util.List<hydra.ext.java.syntax.SwitchBlock_Pair> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SwitchBlock)) {
      return false;
    }
    SwitchBlock o = (SwitchBlock) other;
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
  public int compareTo(SwitchBlock other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
