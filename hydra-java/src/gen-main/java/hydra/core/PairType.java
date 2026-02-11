// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A type which pairs a 'first' type and a 'second' type
 */
public class PairType implements Serializable, Comparable<PairType> {
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
    this.first = first;
    this.second = second;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PairType)) {
      return false;
    }
    PairType o = (PairType) other;
    return java.util.Objects.equals(
      this.first,
      o.first) && java.util.Objects.equals(
      this.second,
      o.second);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(first) + 3 * java.util.Objects.hashCode(second);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PairType other) {
    int cmp = 0;
    cmp = ((Comparable) first).compareTo(other.first);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) second).compareTo(other.second);
  }
  
  public PairType withFirst(hydra.core.Type first) {
    return new PairType(first, second);
  }
  
  public PairType withSecond(hydra.core.Type second) {
    return new PairType(first, second);
  }
}
