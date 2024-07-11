// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.format;

import java.io.Serializable;

/**
 * Wrapper struct to store key values
 */
public class KeyValue implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/format.KeyValue");
  
  public final String key;
  
  public final hydra.util.Opt<String> value;
  
  public KeyValue (String key, hydra.util.Opt<String> value) {
    java.util.Objects.requireNonNull((key));
    java.util.Objects.requireNonNull((value));
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof KeyValue)) {
      return false;
    }
    KeyValue o = (KeyValue) (other);
    return key.equals(o.key) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode() + 3 * value.hashCode();
  }
  
  public KeyValue withKey(String key) {
    java.util.Objects.requireNonNull((key));
    return new KeyValue(key, value);
  }
  
  public KeyValue withValue(hydra.util.Opt<String> value) {
    java.util.Objects.requireNonNull((value));
    return new KeyValue(key, value);
  }
}