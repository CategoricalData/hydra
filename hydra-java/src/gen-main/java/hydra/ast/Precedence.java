// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * Operator precedence
 */
public class Precedence implements Serializable, Comparable<Precedence> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ast.Precedence");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final Integer value;
  
  public Precedence (Integer value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Precedence)) {
      return false;
    }
    Precedence o = (Precedence) other;
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
  public int compareTo(Precedence other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
