// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class Remove implements Serializable, Comparable<Remove> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.Remove");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.util.ConsList<hydra.ext.cypher.openCypher.RemoveItem> value;
  
  public Remove (hydra.util.ConsList<hydra.ext.cypher.openCypher.RemoveItem> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Remove)) {
      return false;
    }
    Remove o = (Remove) other;
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
  public int compareTo(Remove other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
