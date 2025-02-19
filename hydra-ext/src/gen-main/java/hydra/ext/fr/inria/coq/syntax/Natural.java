// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

/**
 * A non-negative arbitrary-precision integer
 */
public class Natural implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.Natural");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.math.BigInteger value;
  
  public Natural (java.math.BigInteger value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Natural)) {
      return false;
    }
    Natural o = (Natural) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}