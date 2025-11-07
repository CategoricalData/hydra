// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A type which pairs a 'first' type and a 'second' type
 */
public class PairType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.PairType");
  
  public static final hydra.core.Name FIELD_NAME_FIRST = new hydra.core.Name("first");
  
  public static final hydra.core.Name FIELD_NAME_SECOND = new hydra.core.Name("second");
  
  /**
   * The first component of the pair
   */
  public final hydra.core.Type first;
  
  /**
   * The second component of the pair
   */
  public final hydra.core.Type second;
  
  public PairType (hydra.core.Type first, hydra.core.Type second) {
    java.util.Objects.requireNonNull((first));
    java.util.Objects.requireNonNull((second));
    this.first = first;
    this.second = second;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PairType)) {
      return false;
    }
    PairType o = (PairType) (other);
    return first.equals(o.first) && second.equals(o.second);
  }
  
  @Override
  public int hashCode() {
    return 2 * first.hashCode() + 3 * second.hashCode();
  }
  
  public PairType withFirst(hydra.core.Type first) {
    java.util.Objects.requireNonNull((first));
    return new PairType(first, second);
  }
  
  public PairType withSecond(hydra.core.Type second) {
    java.util.Objects.requireNonNull((second));
    return new PairType(first, second);
  }
}
