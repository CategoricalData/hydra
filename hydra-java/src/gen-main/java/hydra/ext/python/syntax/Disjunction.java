// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Disjunction implements Serializable, Comparable<Disjunction> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Disjunction");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.python.syntax.Conjunction> value;
  
  public Disjunction (java.util.List<hydra.ext.python.syntax.Conjunction> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Disjunction)) {
      return false;
    }
    Disjunction o = (Disjunction) other;
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
  public int compareTo(Disjunction other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
