package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class PropertyLookup implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.PropertyLookup");
  
  public final hydra.langs.cypher.openCypher.PropertyKeyName value;
  
  public PropertyLookup (hydra.langs.cypher.openCypher.PropertyKeyName value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyLookup)) {
      return false;
    }
    PropertyLookup o = (PropertyLookup) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}