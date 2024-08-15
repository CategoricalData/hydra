// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class KeyValuePair implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.KeyValuePair");
  
  public static final hydra.core.Name FIELD_NAME_KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.langs.cypher.openCypher.PropertyKeyName key;
  
  public final hydra.langs.cypher.openCypher.Expression value;
  
  public KeyValuePair (hydra.langs.cypher.openCypher.PropertyKeyName key, hydra.langs.cypher.openCypher.Expression value) {
    java.util.Objects.requireNonNull((key));
    java.util.Objects.requireNonNull((value));
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof KeyValuePair)) {
      return false;
    }
    KeyValuePair o = (KeyValuePair) (other);
    return key.equals(o.key) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode() + 3 * value.hashCode();
  }
  
  public KeyValuePair withKey(hydra.langs.cypher.openCypher.PropertyKeyName key) {
    java.util.Objects.requireNonNull((key));
    return new KeyValuePair(key, value);
  }
  
  public KeyValuePair withValue(hydra.langs.cypher.openCypher.Expression value) {
    java.util.Objects.requireNonNull((value));
    return new KeyValuePair(key, value);
  }
}