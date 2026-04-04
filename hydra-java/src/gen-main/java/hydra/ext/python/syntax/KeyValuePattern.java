// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class KeyValuePattern implements Serializable, Comparable<KeyValuePattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.python.syntax.KeyValuePattern");

  public static final hydra.core.Name KEY = new hydra.core.Name("key");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.ext.python.syntax.LiteralExpressionOrAttribute key;

  public final hydra.ext.python.syntax.Pattern value;

  public KeyValuePattern (hydra.ext.python.syntax.LiteralExpressionOrAttribute key, hydra.ext.python.syntax.Pattern value) {
    this.key = key;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof KeyValuePattern)) {
      return false;
    }
    KeyValuePattern o = (KeyValuePattern) other;
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
  public int compareTo(KeyValuePattern other) {
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

  public KeyValuePattern withKey(hydra.ext.python.syntax.LiteralExpressionOrAttribute key) {
    return new KeyValuePattern(key, value);
  }

  public KeyValuePattern withValue(hydra.ext.python.syntax.Pattern value) {
    return new KeyValuePattern(key, value);
  }
}
