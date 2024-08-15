// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class ElementValuePair implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.ElementValuePair");
  
  public static final hydra.core.Name FIELD_NAME_KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.langs.java.syntax.Identifier key;
  
  public final hydra.langs.java.syntax.ElementValue value;
  
  public ElementValuePair (hydra.langs.java.syntax.Identifier key, hydra.langs.java.syntax.ElementValue value) {
    java.util.Objects.requireNonNull((key));
    java.util.Objects.requireNonNull((value));
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ElementValuePair)) {
      return false;
    }
    ElementValuePair o = (ElementValuePair) (other);
    return key.equals(o.key) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode() + 3 * value.hashCode();
  }
  
  public ElementValuePair withKey(hydra.langs.java.syntax.Identifier key) {
    java.util.Objects.requireNonNull((key));
    return new ElementValuePair(key, value);
  }
  
  public ElementValuePair withValue(hydra.langs.java.syntax.ElementValue value) {
    java.util.Objects.requireNonNull((value));
    return new ElementValuePair(key, value);
  }
}