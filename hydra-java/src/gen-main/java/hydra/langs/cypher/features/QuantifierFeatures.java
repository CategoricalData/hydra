// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for quantifier expressions.
 */
public class QuantifierFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.QuantifierFeatures");
  
  /**
   * Whether to expect the ALL quantifier.
   */
  public final Boolean all;
  
  /**
   * Whether to expect the ANY quantifier.
   */
  public final Boolean any;
  
  /**
   * Whether to expect the exists() function.
   */
  public final Boolean exists;
  
  /**
   * Whether to expect the NONE quantifier.
   */
  public final Boolean none;
  
  /**
   * Whether to expect the SINGLE quantifier.
   */
  public final Boolean single;
  
  public QuantifierFeatures (Boolean all, Boolean any, Boolean exists, Boolean none, Boolean single) {
    if (all == null) {
      throw new IllegalArgumentException("null value for 'all' argument");
    }
    if (any == null) {
      throw new IllegalArgumentException("null value for 'any' argument");
    }
    if (exists == null) {
      throw new IllegalArgumentException("null value for 'exists' argument");
    }
    if (none == null) {
      throw new IllegalArgumentException("null value for 'none' argument");
    }
    if (single == null) {
      throw new IllegalArgumentException("null value for 'single' argument");
    }
    this.all = all;
    this.any = any;
    this.exists = exists;
    this.none = none;
    this.single = single;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QuantifierFeatures)) {
      return false;
    }
    QuantifierFeatures o = (QuantifierFeatures) (other);
    return all.equals(o.all) && any.equals(o.any) && exists.equals(o.exists) && none.equals(o.none) && single.equals(o.single);
  }
  
  @Override
  public int hashCode() {
    return 2 * all.hashCode() + 3 * any.hashCode() + 5 * exists.hashCode() + 7 * none.hashCode() + 11 * single.hashCode();
  }
  
  public QuantifierFeatures withAll(Boolean all) {
    if (all == null) {
      throw new IllegalArgumentException("null value for 'all' argument");
    }
    return new QuantifierFeatures(all, any, exists, none, single);
  }
  
  public QuantifierFeatures withAny(Boolean any) {
    if (any == null) {
      throw new IllegalArgumentException("null value for 'any' argument");
    }
    return new QuantifierFeatures(all, any, exists, none, single);
  }
  
  public QuantifierFeatures withExists(Boolean exists) {
    if (exists == null) {
      throw new IllegalArgumentException("null value for 'exists' argument");
    }
    return new QuantifierFeatures(all, any, exists, none, single);
  }
  
  public QuantifierFeatures withNone(Boolean none) {
    if (none == null) {
      throw new IllegalArgumentException("null value for 'none' argument");
    }
    return new QuantifierFeatures(all, any, exists, none, single);
  }
  
  public QuantifierFeatures withSingle(Boolean single) {
    if (single == null) {
      throw new IllegalArgumentException("null value for 'single' argument");
    }
    return new QuantifierFeatures(all, any, exists, none, single);
  }
}