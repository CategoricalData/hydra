// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class Kvpair implements Serializable, Comparable<Kvpair> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Kvpair");
  
  public static final hydra.core.Name FIELD_NAME_KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.python.syntax.Expression key;
  
  public final hydra.ext.python.syntax.Expression value;
  
  public Kvpair (hydra.ext.python.syntax.Expression key, hydra.ext.python.syntax.Expression value) {
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Kvpair)) {
      return false;
    }
    Kvpair o = (Kvpair) other;
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
  public int compareTo(Kvpair other) {
    int cmp = 0;
    cmp = ((Comparable) key).compareTo(other.key);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) value).compareTo(other.value);
  }
  
  public Kvpair withKey(hydra.ext.python.syntax.Expression key) {
    return new Kvpair(key, value);
  }
  
  public Kvpair withValue(hydra.ext.python.syntax.Expression value) {
    return new Kvpair(key, value);
  }
}
