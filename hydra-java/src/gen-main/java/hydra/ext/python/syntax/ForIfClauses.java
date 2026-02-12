// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ForIfClauses implements Serializable, Comparable<ForIfClauses> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ForIfClauses");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.python.syntax.ForIfClause> value;
  
  public ForIfClauses (java.util.List<hydra.ext.python.syntax.ForIfClause> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForIfClauses)) {
      return false;
    }
    ForIfClauses o = (ForIfClauses) other;
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
  public int compareTo(ForIfClauses other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
