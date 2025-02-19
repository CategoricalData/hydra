// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.stacspec.items;

import java.io.Serializable;

public class StacVersion implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.stacspec.items.StacVersion");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public StacVersion (String value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StacVersion)) {
      return false;
    }
    StacVersion o = (StacVersion) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}