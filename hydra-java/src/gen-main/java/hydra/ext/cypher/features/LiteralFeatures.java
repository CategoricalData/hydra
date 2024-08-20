// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Various types of literal values
 */
public class LiteralFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/features.LiteralFeatures");
  
  public static final hydra.core.Name FIELD_NAME_BOOLEAN = new hydra.core.Name("boolean");
  
  public static final hydra.core.Name FIELD_NAME_DOUBLE = new hydra.core.Name("double");
  
  public static final hydra.core.Name FIELD_NAME_INTEGER = new hydra.core.Name("integer");
  
  public static final hydra.core.Name FIELD_NAME_LIST = new hydra.core.Name("list");
  
  public static final hydra.core.Name FIELD_NAME_MAP = new hydra.core.Name("map");
  
  public static final hydra.core.Name FIELD_NAME_NULL = new hydra.core.Name("null");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  /**
   * Boolean literals (note: included by most if not all implementations).
   */
  public final Boolean boolean_;
  
  /**
   * Double-precision floating-point literals
   */
  public final Boolean double_;
  
  /**
   * Integer literals
   */
  public final Boolean integer;
  
  /**
   * List literals
   */
  public final Boolean list;
  
  /**
   * Map literals
   */
  public final Boolean map;
  
  /**
   * The NULL literal
   */
  public final Boolean null_;
  
  /**
   * String literals (note: included by most if not all implementations).
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