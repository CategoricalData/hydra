// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class KeyValuePattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.KeyValuePattern");
  
  public static final hydra.core.Name FIELD_NAME_KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.python.syntax.LiteralExpressionOrAttribute key;
  
  public final hydra.ext.python.syntax.Pattern value;
  
  public KeyValuePattern (hydra.ext.python.syntax.LiteralExpressionOrAttribute key, hydra.ext.python.syntax.Pattern value) {
    java.util.Objects.requireNonNull((key));
    java.util.Objects.requireNonNull((value));
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof KeyValuePattern)) {
      return false;
    }
    KeyValuePattern o = (KeyValuePattern) (other);
    return key.equals(o.key) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode() + 3 * value.hashCode();
  }
  
  public KeyValuePattern withKey(hydra.ext.python.syntax.LiteralExpressionOrAttribute key) {
    java.util.Objects.requireNonNull((key));
    return new KeyValuePattern(key, value);
  }
  
  public KeyValuePattern withValue(hydra.ext.python.syntax.Pattern value) {
    java.util.Objects.requireNonNull((value));
    return new KeyValuePattern(key, value);
  }
}