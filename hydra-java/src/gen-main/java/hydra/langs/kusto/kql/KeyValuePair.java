// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public class KeyValuePair implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/kusto/kql.KeyValuePair");
  
  public static final hydra.core.Name FIELD_NAME_KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String key;
  
  public final hydra.langs.kusto.kql.Expression value;
  
  public KeyValuePair (String key, hydra.langs.kusto.kql.Expression value) {
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
  
  public KeyValuePair withKey(String key) {
    java.util.Objects.requireNonNull((key));
    return new KeyValuePair(key, value);
  }
  
  public KeyValuePair withValue(hydra.langs.kusto.kql.Expression value) {
    java.util.Objects.requireNonNull((value));
    return new KeyValuePair(key, value);
  }
}