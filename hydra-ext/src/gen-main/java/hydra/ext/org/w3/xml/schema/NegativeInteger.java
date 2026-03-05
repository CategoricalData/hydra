// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.xml.schema;

import java.io.Serializable;

public class NegativeInteger implements Serializable, Comparable<NegativeInteger> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.xml.schema.NegativeInteger");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final java.math.BigInteger value;
  
  public NegativeInteger (java.math.BigInteger value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NegativeInteger)) {
      return false;
    }
    NegativeInteger o = (NegativeInteger) other;
    return this.value.compareTo(o.value) == 0;
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NegativeInteger other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
