// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ElementValuePair implements Serializable, Comparable<ElementValuePair> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ElementValuePair");
  
  public static final hydra.core.Name FIELD_NAME_KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.java.syntax.Identifier key;
  
  public final hydra.ext.java.syntax.ElementValue value;
  
  public ElementValuePair (hydra.ext.java.syntax.Identifier key, hydra.ext.java.syntax.ElementValue value) {
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ElementValuePair)) {
      return false;
    }
    ElementValuePair o = (ElementValuePair) other;
    return java.util.Objects.equals(
      this.key,
      o.key) && java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(key) + 3 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ElementValuePair other) {
    int cmp = 0;
    cmp = ((Comparable) key).compareTo(other.key);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) value).compareTo(other.value);
  }
  
  public ElementValuePair withKey(hydra.ext.java.syntax.Identifier key) {
    return new ElementValuePair(key, value);
  }
  
  public ElementValuePair withValue(hydra.ext.java.syntax.ElementValue value) {
    return new ElementValuePair(key, value);
  }
}
