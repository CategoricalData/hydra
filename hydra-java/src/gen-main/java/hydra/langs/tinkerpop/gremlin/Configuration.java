// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class Configuration implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.Configuration");
  
  public final hydra.langs.tinkerpop.gremlin.KeywordOrIdentifier key;
  
  public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value;
  
  public Configuration (hydra.langs.tinkerpop.gremlin.KeywordOrIdentifier key, hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value) {
    java.util.Objects.requireNonNull((key));
    java.util.Objects.requireNonNull((value));
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Configuration)) {
      return false;
    }
    Configuration o = (Configuration) (other);
    return key.equals(o.key) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode() + 3 * value.hashCode();
  }
  
  public Configuration withKey(hydra.langs.tinkerpop.gremlin.KeywordOrIdentifier key) {
    java.util.Objects.requireNonNull((key));
    return new Configuration(key, value);
  }
  
  public Configuration withValue(hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value) {
    java.util.Objects.requireNonNull((value));
    return new Configuration(key, value);
  }
}