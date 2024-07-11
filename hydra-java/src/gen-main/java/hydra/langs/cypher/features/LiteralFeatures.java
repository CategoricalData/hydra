// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for various types of literal values.
 */
public class LiteralFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.LiteralFeatures");
  
  /**
   * Whether to expect boolean literals (note: included by most if not all implementations).
   */
  public final Boolean boolean_;
  
  /**
   * Whether to expect double-precision floating-point literals.
   */
  public final Boolean double_;
  
  /**
   * Whether to expect integer literals.
   */
  public final Boolean integer;
  
  /**
   * Whether to expect list literals.
   */
  public final Boolean list;
  
  /**
   * Whether to expect map literals.
   */
  public final Boolean map;
  
  /**
   * Whether to expect the NULL literal.
   */
  public final Boolean null_;
  
  /**
   * Whether to expect string literals (note: included by most if not all implementations).
   */
  public final Boolean string;
  
  public LiteralFeatures (Boolean boolean_, Boolean double_, Boolean integer, Boolean list, Boolean map, Boolean null_, Boolean string) {
    java.util.Objects.requireNonNull((boolean_));
    java.util.Objects.requireNonNull((double_));
    java.util.Objects.requireNonNull((integer));
    java.util.Objects.requireNonNull((list));
    java.util.Objects.requireNonNull((map));
    java.util.Objects.requireNonNull((null_));
    java.util.Objects.requireNonNull((string));
    this.boolean_ = boolean_;
    this.double_ = double_;
    this.integer = integer;
    this.list = list;
    this.map = map;
    this.null_ = null_;
    this.string = string;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LiteralFeatures)) {
      return false;
    }
    LiteralFeatures o = (LiteralFeatures) (other);
    return boolean_.equals(o.boolean_) && double_.equals(o.double_) && integer.equals(o.integer) && list.equals(o.list) && map.equals(o.map) && null_.equals(o.null_) && string.equals(o.string);
  }
  
  @Override
  public int hashCode() {
    return 2 * boolean_.hashCode() + 3 * double_.hashCode() + 5 * integer.hashCode() + 7 * list.hashCode() + 11 * map.hashCode() + 13 * null_.hashCode() + 17 * string.hashCode();
  }
  
  public LiteralFeatures withBoolean(Boolean boolean_) {
    java.util.Objects.requireNonNull((boolean_));
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }
  
  public LiteralFeatures withDouble(Boolean double_) {
    java.util.Objects.requireNonNull((double_));
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }
  
  public LiteralFeatures withInteger(Boolean integer) {
    java.util.Objects.requireNonNull((integer));
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }
  
  public LiteralFeatures withList(Boolean list) {
    java.util.Objects.requireNonNull((list));
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }
  
  public LiteralFeatures withMap(Boolean map) {
    java.util.Objects.requireNonNull((map));
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }
  
  public LiteralFeatures withNull(Boolean null_) {
    java.util.Objects.requireNonNull((null_));
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }
  
  public LiteralFeatures withString(Boolean string) {
    java.util.Objects.requireNonNull((string));
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }
}