package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public class PropertyPattern implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.PropertyPattern");
  
  public final hydra.langs.tinkerpop.propertyGraph.PropertyKey key;
  
  public final hydra.langs.tinkerpop.queries.PropertyValuePattern value;
  
  public PropertyPattern (hydra.langs.tinkerpop.propertyGraph.PropertyKey key, hydra.langs.tinkerpop.queries.PropertyValuePattern value) {
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyPattern)) {
      return false;
    }
    PropertyPattern o = (PropertyPattern) (other);
    return key.equals(o.key) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode() + 3 * value.hashCode();
  }
  
  public PropertyPattern withKey(hydra.langs.tinkerpop.propertyGraph.PropertyKey key) {
    return new PropertyPattern(key, value);
  }
  
  public PropertyPattern withValue(hydra.langs.tinkerpop.queries.PropertyValuePattern value) {
    return new PropertyPattern(key, value);
  }
}