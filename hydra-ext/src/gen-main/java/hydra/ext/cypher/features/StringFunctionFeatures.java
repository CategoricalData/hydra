// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * String functions
 */
public class StringFunctionFeatures implements Serializable, Comparable<StringFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.StringFunctionFeatures");
  
  public static final hydra.core.Name BTRIM = new hydra.core.Name("btrim");
  
  public static final hydra.core.Name LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name LOWER = new hydra.core.Name("lower");
  
  public static final hydra.core.Name LTRIM = new hydra.core.Name("ltrim");
  
  public static final hydra.core.Name NORMALIZE = new hydra.core.Name("normalize");
  
  public static final hydra.core.Name REPLACE = new hydra.core.Name("replace");
  
  public static final hydra.core.Name REVERSE = new hydra.core.Name("reverse");
  
  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");
  
  public static final hydra.core.Name RTRIM = new hydra.core.Name("rtrim");
  
  public static final hydra.core.Name SPLIT = new hydra.core.Name("split");
  
  public static final hydra.core.Name SUBSTRING = new hydra.core.Name("substring");
  
  public static final hydra.core.Name TO_LOWER = new hydra.core.Name("toLower");
  
  public static final hydra.core.Name TO_STRING = new hydra.core.Name("toString");
  
  public static final hydra.core.Name TO_STRING_OR_NULL = new hydra.core.Name("toStringOrNull");
  
  public static final hydra.core.Name TO_UPPER = new hydra.core.Name("toUpper");
  
  public static final hydra.core.Name TRIM = new hydra.core.Name("trim");
  
  public static final hydra.core.Name UPPER = new hydra.core.Name("upper");
  
  /**
   * The btrim() function. Returns the given STRING with leading and trailing whitespace removed.; Returns the given STRING with leading and trailing trimCharacterString characters removed. Introduced in 5.20.
   */
  public final Boolean btrim;
  
  /**
   * The left() function. Returns a STRING containing the specified number (INTEGER) of leftmost characters in the given STRING.
   */
  public final Boolean left;
  
  /**
   * The lower() function. Returns the given STRING in lowercase. This function is an alias to the toLower() function, and it was introduced as part of Cypher's GQL conformance. Introduced in 5.21.
   */
  public final Boolean lower;
  
  /**
   * The ltrim() function. Returns the given STRING with leading whitespace removed.; Returns the given STRING with leading trimCharacterString characters removed. Introduced in 5.20.
   */
  public final Boolean ltrim;
  
  /**
   * The normalize() function. Returns the given STRING normalized according to the normalization CypherFunctionForm NFC. Introduced in 5.17.; Returns the given STRING normalized according to the specified normalization CypherFunctionForm. Introduced in 5.17.
   */
  public final Boolean normalize;
  
  /**
   * The replace() function. Returns a STRING in which all occurrences of a specified search STRING in the given STRING have been replaced by another (specified) replacement STRING.
   */
  public final Boolean replace;
  
  /**
   * The reverse() function. Returns a STRING in which the order of all characters in the given STRING have been reversed.
   */
  public final Boolean reverse;
  
  /**
   * The right() function. Returns a STRING containing the specified number of rightmost characters in the given STRING.
   */
  public final Boolean right;
  
  /**
   * The rtrim() function. Returns the given STRING with trailing whitespace removed.; Returns the given STRING with trailing trimCharacterString characters removed. Introduced in 5.20.
   */
  public final Boolean rtrim;
  
  /**
   * The split() function. Returns a LIST&lt;STRING&gt; resulting from the splitting of the given STRING around matches of the given delimiter.; Returns a LIST&lt;STRING&gt; resulting from the splitting of the given STRING around matches of any of the given delimiters.
   */
  public final Boolean split;
  
  /**
   * The substring() function. Returns a substring of the given STRING, beginning with a 0-based index start.; Returns a substring of a given length from the given STRING, beginning with a 0-based index start.
   */
  public final Boolean substring;
  
  /**
   * The toLower() function. Returns the given STRING in lowercase.
   */
  public final Boolean toLower;
  
  /**
   * The toString() function. Converts an INTEGER, FLOAT, BOOLEAN, POINT or temporal type (i.e. DATE, ZONED TIME, LOCAL TIME, ZONED DATETIME, LOCAL DATETIME or DURATION) value to a STRING.
   */
  public final Boolean toString;
  
  /**
   * The toStringOrNull() function. Converts an INTEGER, FLOAT, BOOLEAN, POINT or temporal type (i.e. DATE, ZONED TIME, LOCAL TIME, ZONED DATETIME, LOCAL DATETIME or DURATION) value to a STRING, or null if the value cannot be converted.
   */
  public final Boolean toStringOrNull;
  
  /**
   * The toUpper() function. Returns the given STRING in uppercase.
   */
  public final Boolean toUpper;
  
  /**
   * The trim() function. Returns the given STRING with leading and trailing whitespace removed.; Returns the given STRING with the leading and/or trailing trimCharacterString character removed. Introduced in 5.20.
   */
  public final Boolean trim;
  
  /**
   * The upper() function. Returns the given STRING in uppercase. This function is an alias to the toUpper() function, and it was introduced as part of Cypher's GQL conformance. Introduced in 5.21.
   */
  public final Boolean upper;
  
  public StringFunctionFeatures (Boolean btrim, Boolean left, Boolean lower, Boolean ltrim, Boolean normalize, Boolean replace, Boolean reverse, Boolean right, Boolean rtrim, Boolean split, Boolean substring, Boolean toLower, Boolean toString, Boolean toStringOrNull, Boolean toUpper, Boolean trim, Boolean upper) {
    this.btrim = btrim;
    this.left = left;
    this.lower = lower;
    this.ltrim = ltrim;
    this.normalize = normalize;
    this.replace = replace;
    this.reverse = reverse;
    this.right = right;
    this.rtrim = rtrim;
    this.split = split;
    this.substring = substring;
    this.toLower = toLower;
    this.toString = toString;
    this.toStringOrNull = toStringOrNull;
    this.toUpper = toUpper;
    this.trim = trim;
    this.upper = upper;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringFunctionFeatures)) {
      return false;
    }
    StringFunctionFeatures o = (StringFunctionFeatures) other;
    return java.util.Objects.equals(
      this.btrim,
      o.btrim) && java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.lower,
      o.lower) && java.util.Objects.equals(
      this.ltrim,
      o.ltrim) && java.util.Objects.equals(
      this.normalize,
      o.normalize) && java.util.Objects.equals(
      this.replace,
      o.replace) && java.util.Objects.equals(
      this.reverse,
      o.reverse) && java.util.Objects.equals(
      this.right,
      o.right) && java.util.Objects.equals(
      this.rtrim,
      o.rtrim) && java.util.Objects.equals(
      this.split,
      o.split) && java.util.Objects.equals(
      this.substring,
      o.substring) && java.util.Objects.equals(
      this.toLower,
      o.toLower) && java.util.Objects.equals(
      this.toString,
      o.toString) && java.util.Objects.equals(
      this.toStringOrNull,
      o.toStringOrNull) && java.util.Objects.equals(
      this.toUpper,
      o.toUpper) && java.util.Objects.equals(
      this.trim,
      o.trim) && java.util.Objects.equals(
      this.upper,
      o.upper);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(btrim) + 3 * java.util.Objects.hashCode(left) + 5 * java.util.Objects.hashCode(lower) + 7 * java.util.Objects.hashCode(ltrim) + 11 * java.util.Objects.hashCode(normalize) + 13 * java.util.Objects.hashCode(replace) + 17 * java.util.Objects.hashCode(reverse) + 19 * java.util.Objects.hashCode(right) + 23 * java.util.Objects.hashCode(rtrim) + 29 * java.util.Objects.hashCode(split) + 31 * java.util.Objects.hashCode(substring) + 37 * java.util.Objects.hashCode(toLower) + 41 * java.util.Objects.hashCode(toString) + 43 * java.util.Objects.hashCode(toStringOrNull) + 47 * java.util.Objects.hashCode(toUpper) + 53 * java.util.Objects.hashCode(trim) + 59 * java.util.Objects.hashCode(upper);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(StringFunctionFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) btrim).compareTo(other.btrim);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) left).compareTo(other.left);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) lower).compareTo(other.lower);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) ltrim).compareTo(other.ltrim);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) normalize).compareTo(other.normalize);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) replace).compareTo(other.replace);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) reverse).compareTo(other.reverse);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) right).compareTo(other.right);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) rtrim).compareTo(other.rtrim);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) split).compareTo(other.split);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) substring).compareTo(other.substring);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) toLower).compareTo(other.toLower);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) toString).compareTo(other.toString);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) toStringOrNull).compareTo(other.toStringOrNull);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) toUpper).compareTo(other.toUpper);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) trim).compareTo(other.trim);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) upper).compareTo(other.upper);
  }
  
  public StringFunctionFeatures withBtrim(Boolean btrim) {
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withLeft(Boolean left) {
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withLower(Boolean lower) {
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withLtrim(Boolean ltrim) {
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withNormalize(Boolean normalize) {
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withReplace(Boolean replace) {
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withReverse(Boolean reverse) {
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withRight(Boolean right) {
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withRtrim(Boolean rtrim) {
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withSplit(Boolean split) {
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withSubstring(Boolean substring) {
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withToLower(Boolean toLower) {
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withToString(Boolean toString) {
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withToStringOrNull(Boolean toStringOrNull) {
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withToUpper(Boolean toUpper) {
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withTrim(Boolean trim) {
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withUpper(Boolean upper) {
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
}
