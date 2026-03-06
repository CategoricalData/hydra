// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class Create implements Serializable, Comparable<Create> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.Create");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.cypher.openCypher.Pattern value;
  
  public Create (hydra.ext.cypher.openCypher.Pattern value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Create)) {
      return false;
    }
    Create o = (Create) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Create other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
