// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.features;

import java.io.Serializable;

/**
 * Various types of literal values
 */
public class LiteralFeatures implements Serializable, Comparable<LiteralFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.features.LiteralFeatures");

  public static final hydra.core.Name BOOLEAN = new hydra.core.Name("boolean");

  public static final hydra.core.Name DOUBLE = new hydra.core.Name("double");

  public static final hydra.core.Name INTEGER = new hydra.core.Name("integer");

  public static final hydra.core.Name LIST = new hydra.core.Name("list");

  public static final hydra.core.Name MAP = new hydra.core.Name("map");

  public static final hydra.core.Name NULL = new hydra.core.Name("null");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

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
    LiteralFeatures o = (LiteralFeatures) other;
    return java.util.Objects.equals(
      this.boolean_,
      o.boolean_) && java.util.Objects.equals(
      this.double_,
      o.double_) && java.util.Objects.equals(
      this.integer,
      o.integer) && java.util.Objects.equals(
      this.list,
      o.list) && java.util.Objects.equals(
      this.map,
      o.map) && java.util.Objects.equals(
      this.null_,
      o.null_) && java.util.Objects.equals(
      this.string,
      o.string);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(boolean_) + 3 * java.util.Objects.hashCode(double_) + 5 * java.util.Objects.hashCode(integer) + 7 * java.util.Objects.hashCode(list) + 11 * java.util.Objects.hashCode(map) + 13 * java.util.Objects.hashCode(null_) + 17 * java.util.Objects.hashCode(string);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LiteralFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      boolean_,
      other.boolean_);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      double_,
      other.double_);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      integer,
      other.integer);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      list,
      other.list);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      map,
      other.map);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      null_,
      other.null_);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      string,
      other.string);
  }

  public LiteralFeatures withBoolean(Boolean boolean_) {
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }

  public LiteralFeatures withDouble(Boolean double_) {
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }

  public LiteralFeatures withInteger(Boolean integer) {
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }

  public LiteralFeatures withList(Boolean list) {
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }

  public LiteralFeatures withMap(Boolean map) {
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }

  public LiteralFeatures withNull(Boolean null_) {
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }

  public LiteralFeatures withString(Boolean string) {
    return new LiteralFeatures(boolean_, double_, integer, list, map, null_, string);
  }
}
