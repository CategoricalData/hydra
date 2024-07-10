// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class KeyValuePair implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.KeyValuePair");
  
  public final hydra.langs.cypher.openCypher.PropertyKeyName key;
  
  public final hydra.langs.cypher.openCypher.Expression value;
  
  public KeyValuePair (hydra.langs.cypher.openCypher.PropertyKeyName key, hydra.langs.cypher.openCypher.Expression value) {
    if (key == null) {
      throw new IllegalArgumentException("null value for 'key' argument");
    }
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
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
    if (key == null) {
      throw new IllegalArgumentException("null value for 'key' argument");
    }
    return new KeyValuePair(key, value);
  }
  
  public KeyValuePair withValue(hydra.langs.cypher.openCypher.Expression value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    return new KeyValuePair(key, value);
  }
}