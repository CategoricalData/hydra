// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ForUpdate implements Serializable, Comparable<ForUpdate> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ForUpdate");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.java.syntax.StatementExpression> value;
  
  public ForUpdate (java.util.List<hydra.ext.java.syntax.StatementExpression> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForUpdate)) {
      return false;
    }
    ForUpdate o = (ForUpdate) other;
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
  public int compareTo(ForUpdate other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
