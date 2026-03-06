// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class RelTypeName implements Serializable, Comparable<RelTypeName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.RelTypeName");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public RelTypeName (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelTypeName)) {
      return false;
    }
    RelTypeName o = (RelTypeName) other;
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
  public int compareTo(RelTypeName other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
