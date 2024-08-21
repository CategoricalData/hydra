// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.queries;

import java.io.Serializable;

public class PropertyPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/queries.PropertyPattern");
  
  public static final hydra.core.Name FIELD_NAME_KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.org.apache.tinkerpop.propertyGraph.PropertyKey key;
  
  public final hydra.ext.org.apache.tinkerpop.queries.PropertyValuePattern value;
  
  public PropertyPattern (hydra.ext.org.apache.tinkerpop.propertyGraph.PropertyKey key, hydra.ext.org.apache.tinkerpop.queries.PropertyValuePattern value) {
    java.util.Objects.requireNonNull((key));
    java.util.Objects.requireNonNull((value));
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
  
  public PropertyPattern withKey(hydra.ext.org.apache.tinkerpop.propertyGraph.PropertyKey key) {
    java.util.Objects.requireNonNull((key));
    return new PropertyPattern(key, value);
  }
  
  public PropertyPattern withValue(hydra.ext.org.apache.tinkerpop.queries.PropertyValuePattern value) {
    java.util.Objects.requireNonNull((value));
    return new PropertyPattern(key, value);
  }
}