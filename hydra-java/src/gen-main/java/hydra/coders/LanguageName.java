// Note: this is an automatically generated file. Do not edit.

package hydra.coders;

import java.io.Serializable;

/**
 * The unique name of a language
 */
public class LanguageName implements Serializable, Comparable<LanguageName> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.coders.LanguageName");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public LanguageName (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LanguageName)) {
      return false;
    }
    LanguageName o = (LanguageName) other;
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
  public int compareTo(LanguageName other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
