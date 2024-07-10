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
    if (boolean_ == null) {
      throw new IllegalArgumentException("null value for 'boolean' argument");
    }
    if (double_ == null) {
      throw new IllegalArgumentException("null value for 'double' argument");
    }
    if (integer == null) {
      throw new IllegalArgumentException("null value for 'integer' argument");
    }
    if (list == null) {
      throw new IllegalArgumentException("null value for 'list' argument");
    }
    if (map == null) {
      throw new IllegalArgumentException("null value for 'map' argument");
    }
    if (null_ == null) {
      throw new IllegalArgumentException("null value for 'null' argument");
    }
    if (string == null) {
      throw new IllegalArgumentException("null value for 'string' argument");
    }
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
    if (boolean_ == null) {
      throw new IllegalArgumentException("null value for 'boolean' argument");
    }
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }
  
  public LiteralFeatures withDouble(Boolean double_) {
    if (double_ == null) {
      throw new IllegalArgumentException("null value for 'double' argument");
    }
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }
  
  public LiteralFeatures withInteger(Boolean integer) {
    if (integer == null) {
      throw new IllegalArgumentException("null value for 'integer' argument");
    }
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }
  
  public LiteralFeatures withList(Boolean list) {
    if (list == null) {
      throw new IllegalArgumentException("null value for 'list' argument");
    }
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }
  
  public LiteralFeatures withMap(Boolean map) {
    if (map == null) {
      throw new IllegalArgumentException("null value for 'map' argument");
    }
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }
  
  public LiteralFeatures withNull(Boolean null_) {
    if (null_ == null) {
      throw new IllegalArgumentException("null value for 'null' argument");
    }
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }
  
  public LiteralFeatures withString(Boolean string) {
    if (string == null) {
      throw new IllegalArgumentException("null value for 'string' argument");
    }
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }
}