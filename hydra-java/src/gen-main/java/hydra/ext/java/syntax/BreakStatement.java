// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class BreakStatement implements Serializable, Comparable<BreakStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.BreakStatement");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.Identifier> value;
  
  public BreakStatement (hydra.util.Maybe<hydra.ext.java.syntax.Identifier> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BreakStatement)) {
      return false;
    }
    BreakStatement o = (BreakStatement) other;
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
  public int compareTo(BreakStatement other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
