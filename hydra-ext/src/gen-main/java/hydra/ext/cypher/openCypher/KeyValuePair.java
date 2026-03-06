// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class KeyValuePair implements Serializable, Comparable<KeyValuePair> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.KeyValuePair");
  
  public static final hydra.core.Name KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.cypher.openCypher.PropertyKeyName key;
  
  public final hydra.ext.cypher.openCypher.Expression value;
  
  public KeyValuePair (hydra.ext.cypher.openCypher.PropertyKeyName key, hydra.ext.cypher.openCypher.Expression value) {
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof KeyValuePair)) {
      return false;
    }
    KeyValuePair o = (KeyValuePair) other;
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
  public int compareTo(KeyValuePair other) {
    int cmp = 0;
    cmp = ((Comparable) key).compareTo(other.key);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) value).compareTo(other.value);
  }
  
  public KeyValuePair withKey(hydra.ext.cypher.openCypher.PropertyKeyName key) {
    return new KeyValuePair(key, value);
  }
  
  public KeyValuePair withValue(hydra.ext.cypher.openCypher.Expression value) {
    return new KeyValuePair(key, value);
  }
}
