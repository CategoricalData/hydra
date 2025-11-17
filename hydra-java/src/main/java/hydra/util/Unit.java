// Note: this is an automatically generated file. Do not edit.

package hydra.util;

import java.io.Serializable;

/**
 * An empty record as a canonical unit value.
 */
public class Unit implements Serializable {
  /**
   * Constructs a new Unit.
   */
  public Unit() {

  }
  
  /**
   * Checks if this Unit is equal to another object.
   *
   * @param other the object to compare to
   * @return true if the other object is also a Unit, false otherwise
   */
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Unit)) {
      return false;
    }
    Unit o = (Unit) (other);
    return true;
  }
  
  /**
   * Returns the hash code of this Unit.
   *
   * @return the hash code (always 0)
   */
  @Override
  public int hashCode() {
    return 0;
  }
}