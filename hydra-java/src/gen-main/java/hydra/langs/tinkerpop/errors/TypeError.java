package hydra.langs.tinkerpop.errors;

import java.io.Serializable;

public class TypeError<T, V> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/errors.TypeError");
  
  /**
   * An expected type
   */
  public final T expectedType;
  
  /**
   * The actual value, which does not conform to the expected type
   */
  public final V actualValue;
  
  public TypeError (T expectedType, V actualValue) {
    this.expectedType = expectedType;
    this.actualValue = actualValue;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeError)) {
      return false;
    }
    TypeError o = (TypeError) (other);
    return expectedType.equals(o.expectedType) && actualValue.equals(o.actualValue);
  }
  
  @Override
  public int hashCode() {
    return 2 * expectedType.hashCode() + 3 * actualValue.hashCode();
  }
  
  public TypeError withExpectedType(T expectedType) {
    return new TypeError(expectedType, actualValue);
  }
  
  public TypeError withActualValue(V actualValue) {
    return new TypeError(expectedType, actualValue);
  }
}