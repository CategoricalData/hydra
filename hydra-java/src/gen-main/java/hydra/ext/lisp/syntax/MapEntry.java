// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A key-value pair in a map literal
 */
public class MapEntry implements Serializable, Comparable<MapEntry> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.MapEntry");

  public static final hydra.core.Name KEY = new hydra.core.Name("key");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  /**
   * The key expression
   */
  public final hydra.ext.lisp.syntax.Expression key;

  /**
   * The value expression
   */
  public final hydra.ext.lisp.syntax.Expression value;

  public MapEntry (hydra.ext.lisp.syntax.Expression key, hydra.ext.lisp.syntax.Expression value) {
    this.key = key;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MapEntry)) {
      return false;
    }
    MapEntry o = (MapEntry) other;
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
  public int compareTo(MapEntry other) {
    int cmp = 0;
    cmp = ((Comparable) key).compareTo(other.key);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) value).compareTo(other.value);
  }

  public MapEntry withKey(hydra.ext.lisp.syntax.Expression key) {
    return new MapEntry(key, value);
  }

  public MapEntry withValue(hydra.ext.lisp.syntax.Expression value) {
    return new MapEntry(key, value);
  }
}
