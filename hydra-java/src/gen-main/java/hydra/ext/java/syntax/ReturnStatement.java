// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ReturnStatement implements Serializable, Comparable<ReturnStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ReturnStatement");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.Expression> value;
  
  public ReturnStatement (hydra.util.Maybe<hydra.ext.java.syntax.Expression> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReturnStatement)) {
      return false;
    }
    ReturnStatement o = (ReturnStatement) other;
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
  public int compareTo(ReturnStatement other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
