// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Kvpair implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Kvpair");
  
  public static final hydra.core.Name FIELD_NAME_KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.python.syntax.Expression key;
  
  public final hydra.ext.python.syntax.Expression value;
  
  public Kvpair (hydra.ext.python.syntax.Expression key, hydra.ext.python.syntax.Expression value) {
    java.util.Objects.requireNonNull((key));
    java.util.Objects.requireNonNull((value));
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Kvpair)) {
      return false;
    }
    Kvpair o = (Kvpair) (other);
    return key.equals(o.key) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode() + 3 * value.hashCode();
  }
  
  public Kvpair withKey(hydra.ext.python.syntax.Expression key) {
    java.util.Objects.requireNonNull((key));
    return new Kvpair(key, value);
  }
  
  public Kvpair withValue(hydra.ext.python.syntax.Expression value) {
    java.util.Objects.requireNonNull((value));
    return new Kvpair(key, value);
  }
}