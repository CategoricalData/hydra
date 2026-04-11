// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class PropertyPattern implements Serializable, Comparable<PropertyPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.query.PropertyPattern");

  public static final hydra.core.Name KEY = new hydra.core.Name("key");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.pg.model.PropertyKey key;

  public final hydra.pg.query.PropertyValuePattern value;

  public PropertyPattern (hydra.pg.model.PropertyKey key, hydra.pg.query.PropertyValuePattern value) {
    this.key = key;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyPattern)) {
      return false;
    }
    PropertyPattern o = (PropertyPattern) other;
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
  public int compareTo(PropertyPattern other) {
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

  public PropertyPattern withKey(hydra.pg.model.PropertyKey key) {
    return new PropertyPattern(key, value);
  }

  public PropertyPattern withValue(hydra.pg.query.PropertyValuePattern value) {
    return new PropertyPattern(key, value);
  }
}
