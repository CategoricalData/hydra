// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ContinueStatement implements Serializable, Comparable<ContinueStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ContinueStatement");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.Identifier> value;
  
  public ContinueStatement (hydra.util.Maybe<hydra.ext.java.syntax.Identifier> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ContinueStatement)) {
      return false;
    }
    ContinueStatement o = (ContinueStatement) other;
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
  public int compareTo(ContinueStatement other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
