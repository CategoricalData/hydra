// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.avro.schema;

import java.io.Serializable;

public class Fixed implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.avro.schema.Fixed");
  
  public static final hydra.core.Name FIELD_NAME_SIZE = new hydra.core.Name("size");
  
  /**
   * an integer, specifying the number of bytes per value
   */
  public final Integer size;
  
  public Fixed (Integer size) {
    java.util.Objects.requireNonNull((size));
    this.size = size;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Fixed)) {
      return false;
    }
    Fixed o = (Fixed) (other);
    return size.equals(o.size);
  }
  
  @Override
  public int hashCode() {
    return 2 * size.hashCode();
  }
}