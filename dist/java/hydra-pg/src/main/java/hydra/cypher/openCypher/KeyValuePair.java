// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class KeyValuePair implements Serializable, Comparable<KeyValuePair> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.KeyValuePair");

  public static final hydra.core.Name KEY = new hydra.core.Name("key");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.cypher.openCypher.PropertyKeyName key;

  public final hydra.cypher.openCypher.Expression value;

  public KeyValuePair (hydra.cypher.openCypher.PropertyKeyName key, hydra.cypher.openCypher.Expression value) {
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
    cmp = hydra.util.Comparing.compare(
      key,
      other.key);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }

  public KeyValuePair withKey(hydra.cypher.openCypher.PropertyKeyName key) {
    return new KeyValuePair(key, value);
  }

  public KeyValuePair withValue(hydra.cypher.openCypher.Expression value) {
    return new KeyValuePair(key, value);
  }
}
