package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public class PropertyValue implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.PropertyValue");
  
  public final String value;
  
  public PropertyValue (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyValue)) {
      return false;
    }
    PropertyValue o = (PropertyValue) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}