// Note: this is an automatically generated file. Do not edit.

package hydra.util;

import java.io.Serializable;

/**
 * An empty record as a canonical unit value
 */
public class Unit implements Serializable {
  public Unit () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Unit)) {
      return false;
    }
    Unit o = (Unit) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}