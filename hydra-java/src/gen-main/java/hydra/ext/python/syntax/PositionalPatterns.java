// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class PositionalPatterns implements Serializable, Comparable<PositionalPatterns> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.PositionalPatterns");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.python.syntax.Pattern> value;
  
  public PositionalPatterns (java.util.List<hydra.ext.python.syntax.Pattern> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PositionalPatterns)) {
      return false;
    }
    PositionalPatterns o = (PositionalPatterns) other;
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
  public int compareTo(PositionalPatterns other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
