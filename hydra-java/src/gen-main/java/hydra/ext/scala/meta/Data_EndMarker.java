// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_EndMarker implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Data_EndMarker");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public final hydra.ext.scala.meta.Data_Name name;
  
  public Data_EndMarker (hydra.ext.scala.meta.Data_Name name) {
    java.util.Objects.requireNonNull((name));
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_EndMarker)) {
      return false;
    }
    Data_EndMarker o = (Data_EndMarker) (other);
    return name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode();
  }
}