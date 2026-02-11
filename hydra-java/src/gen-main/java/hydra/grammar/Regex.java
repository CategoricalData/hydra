// Note: this is an automatically generated file. Do not edit.

package hydra.grammar;

import java.io.Serializable;

/**
 * A regular expression
 */
public class Regex implements Serializable, Comparable<Regex> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.grammar.Regex");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public Regex (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Regex)) {
      return false;
    }
    Regex o = (Regex) other;
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
  public int compareTo(Regex other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
