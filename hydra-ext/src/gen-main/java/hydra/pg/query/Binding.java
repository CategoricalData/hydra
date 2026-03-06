// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class Binding implements Serializable, Comparable<Binding> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.query.Binding");
  
  public static final hydra.core.Name KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.pg.query.Variable key;
  
  public final hydra.pg.query.Query value;
  
  public Binding (hydra.pg.query.Variable key, hydra.pg.query.Query value) {
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Binding)) {
      return false;
    }
    Binding o = (Binding) other;
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
  public int compareTo(Binding other) {
    int cmp = 0;
    cmp = ((Comparable) key).compareTo(other.key);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) value).compareTo(other.value);
  }
  
  public Binding withKey(hydra.pg.query.Variable key) {
    return new Binding(key, value);
  }
  
  public Binding withValue(hydra.pg.query.Query value) {
    return new Binding(key, value);
  }
}
