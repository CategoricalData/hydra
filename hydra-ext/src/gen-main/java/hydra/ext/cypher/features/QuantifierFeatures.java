// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Quantifier expressions
 */
public class QuantifierFeatures implements Serializable, Comparable<QuantifierFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.QuantifierFeatures");
  
  public static final hydra.core.Name ALL = new hydra.core.Name("all");
  
  public static final hydra.core.Name ANY = new hydra.core.Name("any");
  
  public static final hydra.core.Name NONE = new hydra.core.Name("none");
  
  public static final hydra.core.Name SINGLE = new hydra.core.Name("single");
  
  /**
   * The ALL quantifier
   */
  public final Boolean all;
  
  /**
   * The ANY quantifier
   */
  public final Boolean any;
  
  /**
   * The NONE quantifier
   */
  public final Boolean none;
  
  /**
   * The SINGLE quantifier
   */
  public final Boolean single;
  
  public QuantifierFeatures (Boolean all, Boolean any, Boolean none, Boolean single) {
    this.all = all;
    this.any = any;
    this.none = none;
    this.single = single;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QuantifierFeatures)) {
      return false;
    }
    QuantifierFeatures o = (QuantifierFeatures) other;
    return java.util.Objects.equals(
      this.all,
      o.all) && java.util.Objects.equals(
      this.any,
      o.any) && java.util.Objects.equals(
      this.none,
      o.none) && java.util.Objects.equals(
      this.single,
      o.single);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(all) + 3 * java.util.Objects.hashCode(any) + 5 * java.util.Objects.hashCode(none) + 7 * java.util.Objects.hashCode(single);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(QuantifierFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) all).compareTo(other.all);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) any).compareTo(other.any);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) none).compareTo(other.none);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) single).compareTo(other.single);
  }
  
  public QuantifierFeatures withAll(Boolean all) {
    return new QuantifierFeatures(all, any, none, single);
  }
  
  public QuantifierFeatures withAny(Boolean any) {
    return new QuantifierFeatures(all, any, none, single);
  }
  
  public QuantifierFeatures withNone(Boolean none) {
    return new QuantifierFeatures(all, any, none, single);
  }
  
  public QuantifierFeatures withSingle(Boolean single) {
    return new QuantifierFeatures(all, any, none, single);
  }
}
