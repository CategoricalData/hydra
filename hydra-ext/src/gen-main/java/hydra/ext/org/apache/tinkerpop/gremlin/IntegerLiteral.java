// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class IntegerLiteral implements Serializable, Comparable<IntegerLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.IntegerLiteral");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.math.BigInteger value;

  public IntegerLiteral (java.math.BigInteger value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IntegerLiteral)) {
      return false;
    }
    IntegerLiteral o = (IntegerLiteral) other;
    return this.value.compareTo(o.value) == 0;
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(IntegerLiteral other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
