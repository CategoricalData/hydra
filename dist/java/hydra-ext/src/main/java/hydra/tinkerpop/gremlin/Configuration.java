// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class Configuration implements Serializable, Comparable<Configuration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.Configuration");

  public static final hydra.core.Name KEY = new hydra.core.Name("key");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.tinkerpop.gremlin.KeywordOrIdentifier key;

  public final hydra.tinkerpop.gremlin.GenericLiteralArgument value;

  public Configuration (hydra.tinkerpop.gremlin.KeywordOrIdentifier key, hydra.tinkerpop.gremlin.GenericLiteralArgument value) {
    this.key = key;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Configuration)) {
      return false;
    }
    Configuration o = (Configuration) other;
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
  public int compareTo(Configuration other) {
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

  public Configuration withKey(hydra.tinkerpop.gremlin.KeywordOrIdentifier key) {
    return new Configuration(key, value);
  }

  public Configuration withValue(hydra.tinkerpop.gremlin.GenericLiteralArgument value) {
    return new Configuration(key, value);
  }
}
