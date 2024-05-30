package hydra.langs.kusto.kql;

import java.io.Serializable;

public class KeyValuePair implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.KeyValuePair");
  
  public final String key;
  
  public final hydra.langs.kusto.kql.Expression value;
  
  public KeyValuePair (String key, hydra.langs.kusto.kql.Expression value) {
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
    return new KeyValuePair(key, value);
  }
  
  public KeyValuePair withValue(hydra.langs.kusto.kql.Expression value) {
    return new KeyValuePair(key, value);
  }
}