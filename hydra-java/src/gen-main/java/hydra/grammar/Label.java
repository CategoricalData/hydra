// Note: this is an automatically generated file. Do not edit.

package hydra.grammar;

import java.io.Serializable;

/**
 * A name for a pattern
 */
public class Label implements Serializable, Comparable<Label> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.grammar.Label");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public Label (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Label)) {
      return false;
    }
    Label o = (Label) other;
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
  public int compareTo(Label other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
