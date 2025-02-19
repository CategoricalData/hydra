// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * String functions
 */
public class StringFunctionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.features.StringFunctionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_BTRIM = new hydra.core.Name("btrim");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_LOWER = new hydra.core.Name("lower");
  
  public static final hydra.core.Name FIELD_NAME_LTRIM = new hydra.core.Name("ltrim");
  
  public static final hydra.core.Name FIELD_NAME_NORMALIZE = new hydra.core.Name("normalize");
  
  public static final hydra.core.Name FIELD_NAME_REPLACE = new hydra.core.Name("replace");
  
  public static final hydra.core.Name FIELD_NAME_REVERSE = new hydra.core.Name("reverse");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public static final hydra.core.Name FIELD_NAME_RTRIM = new hydra.core.Name("rtrim");
  
  public static final hydra.core.Name FIELD_NAME_SPLIT = new hydra.core.Name("split");
  
  public static final hydra.core.Name FIELD_NAME_SUBSTRING = new hydra.core.Name("substring");
  
  public static final hydra.core.Name FIELD_NAME_TO_LOWER = new hydra.core.Name("toLower");
  
  public static final hydra.core.Name FIELD_NAME_TO_STRING = new hydra.core.Name("toString");
  
  public static final hydra.core.Name FIELD_NAME_TO_STRING_OR_NULL = new hydra.core.Name("toStringOrNull");
  
  public static final hydra.core.Name FIELD_NAME_TO_UPPER = new hydra.core.Name("toUpper");
  
  public static final hydra.core.Name FIELD_NAME_TRIM = new hydra.core.Name("trim");
  
  public static final hydra.core.Name FIELD_NAME_UPPER = new hydra.core.Name("upper");
  
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
    java.util.Objects.requireNonNull((btrim));
    java.util.Objects.requireNonNull((left));
    java.util.Objects.requireNonNull((lower));
    java.util.Objects.requireNonNull((ltrim));
    java.util.Objects.requireNonNull((normalize));
    java.util.Objects.requireNonNull((replace));
    java.util.Objects.requireNonNull((reverse));
    java.util.Objects.requireNonNull((right));
    java.util.Objects.requireNonNull((rtrim));
    java.util.Objects.requireNonNull((split));
    java.util.Objects.requireNonNull((substring));
    java.util.Objects.requireNonNull((toLower));
    java.util.Objects.requireNonNull((toString));
    java.util.Objects.requireNonNull((toStringOrNull));
    java.util.Objects.requireNonNull((toUpper));
    java.util.Objects.requireNonNull((trim));
    java.util.Objects.requireNonNull((upper));
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
    StringFunctionFeatures o = (StringFunctionFeatures) (other);
    return btrim.equals(o.btrim) && left.equals(o.left) && lower.equals(o.lower) && ltrim.equals(o.ltrim) && normalize.equals(o.normalize) && replace.equals(o.replace) && reverse.equals(o.reverse) && right.equals(o.right) && rtrim.equals(o.rtrim) && split.equals(o.split) && substring.equals(o.substring) && toLower.equals(o.toLower) && toString.equals(o.toString) && toStringOrNull.equals(o.toStringOrNull) && toUpper.equals(o.toUpper) && trim.equals(o.trim) && upper.equals(o.upper);
  }
  
  @Override
  public int hashCode() {
    return 2 * btrim.hashCode() + 3 * left.hashCode() + 5 * lower.hashCode() + 7 * ltrim.hashCode() + 11 * normalize.hashCode() + 13 * replace.hashCode() + 17 * reverse.hashCode() + 19 * right.hashCode() + 23 * rtrim.hashCode() + 29 * split.hashCode() + 31 * substring.hashCode() + 37 * toLower.hashCode() + 41 * toString.hashCode() + 43 * toStringOrNull.hashCode() + 47 * toUpper.hashCode() + 53 * trim.hashCode() + 59 * upper.hashCode();
  }
  
  public StringFunctionFeatures withBtrim(Boolean btrim) {
    java.util.Objects.requireNonNull((btrim));
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withLeft(Boolean left) {
    java.util.Objects.requireNonNull((left));
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withLower(Boolean lower) {
    java.util.Objects.requireNonNull((lower));
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withLtrim(Boolean ltrim) {
    java.util.Objects.requireNonNull((ltrim));
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withNormalize(Boolean normalize) {
    java.util.Objects.requireNonNull((normalize));
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withReplace(Boolean replace) {
    java.util.Objects.requireNonNull((replace));
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withReverse(Boolean reverse) {
    java.util.Objects.requireNonNull((reverse));
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withRight(Boolean right) {
    java.util.Objects.requireNonNull((right));
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withRtrim(Boolean rtrim) {
    java.util.Objects.requireNonNull((rtrim));
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withSplit(Boolean split) {
    java.util.Objects.requireNonNull((split));
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withSubstring(Boolean substring) {
    java.util.Objects.requireNonNull((substring));
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withToLower(Boolean toLower) {
    java.util.Objects.requireNonNull((toLower));
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withToString(Boolean toString) {
    java.util.Objects.requireNonNull((toString));
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withToStringOrNull(Boolean toStringOrNull) {
    java.util.Objects.requireNonNull((toStringOrNull));
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withToUpper(Boolean toUpper) {
    java.util.Objects.requireNonNull((toUpper));
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withTrim(Boolean trim) {
    java.util.Objects.requireNonNull((trim));
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
  
  public StringFunctionFeatures withUpper(Boolean upper) {
    java.util.Objects.requireNonNull((upper));
    return new StringFunctionFeatures(btrim, left, lower, ltrim, normalize, replace, reverse, right, rtrim, split, substring, toLower, toString, toStringOrNull, toUpper, trim, upper);
  }
}