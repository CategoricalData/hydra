package hydra.core;

import java.io.Serializable;

/**
 * An empty record as a canonical unit value
 */
public class Unit implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Unit");
  
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