// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class GenericLiteralList implements Serializable, Comparable<GenericLiteralList> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralList");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral> value;
  
  public GenericLiteralList (hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenericLiteralList)) {
      return false;
    }
    GenericLiteralList o = (GenericLiteralList) other;
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
  public int compareTo(GenericLiteralList other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
