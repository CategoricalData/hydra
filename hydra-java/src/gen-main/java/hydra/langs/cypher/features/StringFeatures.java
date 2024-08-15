// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for string functions.
 */
public class StringFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.StringFeatures");
  
  public static final hydra.core.Name FIELD_NAME_CHAR_LENGTH = new hydra.core.Name("char_length");
  
  public static final hydra.core.Name FIELD_NAME_CHARACTER_LENGTH = new hydra.core.Name("character_length");
  
  public static final hydra.core.Name FIELD_NAME_CONTAINS = new hydra.core.Name("contains");
  
  public static final hydra.core.Name FIELD_NAME_ENDS_WITH = new hydra.core.Name("endsWith");
  
  public static final hydra.core.Name FIELD_NAME_IN = new hydra.core.Name("in");
  
  public static final hydra.core.Name FIELD_NAME_STARTS_WITH = new hydra.core.Name("startsWith");
  
  public static final hydra.core.Name FIELD_NAME_TO_BOOLEAN = new hydra.core.Name("toBoolean");
  
  public static final hydra.core.Name FIELD_NAME_TO_BOOLEAN_OR_NULL = new hydra.core.Name("toBooleanOrNull");
  
  public static final hydra.core.Name FIELD_NAME_TO_FLOAT = new hydra.core.Name("toFloat");
  
  public static final hydra.core.Name FIELD_NAME_TO_FLOAT_OR_NULL = new hydra.core.Name("toFloatOrNull");
  
  public static final hydra.core.Name FIELD_NAME_TO_INTEGER = new hydra.core.Name("toInteger");
  
  public static final hydra.core.Name FIELD_NAME_TO_INTEGER_OR_NULL = new hydra.core.Name("toIntegerOrNull");
  
  /**
   * Whether to expect the char_length() function.
   */
  public final Boolean char_length;
  
  /**
   * Whether to expect the character_length() function.
   */
  public final Boolean character_length;
  
  /**
   * Whether to expect the contains() / CONTAINS aggregate function.
   */
  public final Boolean contains;
  
  /**
   * Whether to expect the endsWith() / ENDS WITH aggregate function.
   */
  public final Boolean endsWith;
  
  /**
   * Whether to expect the in() / IN aggregate function.
   */
  public final Boolean in;
  
  /**
   * Whether to expect the startsWith() / STARTS WITH aggregate function.
   */
  public final Boolean startsWith;
  
  /**
   * Whether to expect the toBoolean() function.
   */
  public final Boolean toBoolean;
  
  /**
   * Whether to expect the toBooleanOrNull() function.
   */
  public final Boolean toBooleanOrNull;
  
  /**
   * Whether to expect the toFloat() function.
   */
  public final Boolean toFloat;
  
  /**
   * Whether to expect the toFloatOrNull() function.
   */
  public final Boolean toFloatOrNull;
  
  /**
   * Whether to expect the toInteger() function.
   */
  public final Boolean toInteger;
  
  /**
   * Whether to expect the toIntegerOrNull() function.
   */
  public final Boolean toIntegerOrNull;
  
  public StringFeatures (Boolean char_length, Boolean character_length, Boolean contains, Boolean endsWith, Boolean in, Boolean startsWith, Boolean toBoolean, Boolean toBooleanOrNull, Boolean toFloat, Boolean toFloatOrNull, Boolean toInteger, Boolean toIntegerOrNull) {
    java.util.Objects.requireNonNull((char_length));
    java.util.Objects.requireNonNull((character_length));
    java.util.Objects.requireNonNull((contains));
    java.util.Objects.requireNonNull((endsWith));
    java.util.Objects.requireNonNull((in));
    java.util.Objects.requireNonNull((startsWith));
    java.util.Objects.requireNonNull((toBoolean));
    java.util.Objects.requireNonNull((toBooleanOrNull));
    java.util.Objects.requireNonNull((toFloat));
    java.util.Objects.requireNonNull((toFloatOrNull));
    java.util.Objects.requireNonNull((toInteger));
    java.util.Objects.requireNonNull((toIntegerOrNull));
    this.char_length = char_length;
    this.character_length = character_length;
    this.contains = contains;
    this.endsWith = endsWith;
    this.in = in;
    this.startsWith = startsWith;
    this.toBoolean = toBoolean;
    this.toBooleanOrNull = toBooleanOrNull;
    this.toFloat = toFloat;
    this.toFloatOrNull = toFloatOrNull;
    this.toInteger = toInteger;
    this.toIntegerOrNull = toIntegerOrNull;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringFeatures)) {
      return false;
    }
    StringFeatures o = (StringFeatures) (other);
    return char_length.equals(o.char_length) && character_length.equals(o.character_length) && contains.equals(o.contains) && endsWith.equals(o.endsWith) && in.equals(o.in) && startsWith.equals(o.startsWith) && toBoolean.equals(o.toBoolean) && toBooleanOrNull.equals(o.toBooleanOrNull) && toFloat.equals(o.toFloat) && toFloatOrNull.equals(o.toFloatOrNull) && toInteger.equals(o.toInteger) && toIntegerOrNull.equals(o.toIntegerOrNull);
  }
  
  @Override
  public int hashCode() {
    return 2 * char_length.hashCode() + 3 * character_length.hashCode() + 5 * contains.hashCode() + 7 * endsWith.hashCode() + 11 * in.hashCode() + 13 * startsWith.hashCode() + 17 * toBoolean.hashCode() + 19 * toBooleanOrNull.hashCode() + 23 * toFloat.hashCode() + 29 * toFloatOrNull.hashCode() + 31 * toInteger.hashCode() + 37 * toIntegerOrNull.hashCode();
  }
  
  public StringFeatures withChar_length(Boolean char_length) {
    java.util.Objects.requireNonNull((char_length));
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withCharacter_length(Boolean character_length) {
    java.util.Objects.requireNonNull((character_length));
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withContains(Boolean contains) {
    java.util.Objects.requireNonNull((contains));
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withEndsWith(Boolean endsWith) {
    java.util.Objects.requireNonNull((endsWith));
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withIn(Boolean in) {
    java.util.Objects.requireNonNull((in));
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withStartsWith(Boolean startsWith) {
    java.util.Objects.requireNonNull((startsWith));
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withToBoolean(Boolean toBoolean) {
    java.util.Objects.requireNonNull((toBoolean));
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withToBooleanOrNull(Boolean toBooleanOrNull) {
    java.util.Objects.requireNonNull((toBooleanOrNull));
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withToFloat(Boolean toFloat) {
    java.util.Objects.requireNonNull((toFloat));
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withToFloatOrNull(Boolean toFloatOrNull) {
    java.util.Objects.requireNonNull((toFloatOrNull));
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withToInteger(Boolean toInteger) {
    java.util.Objects.requireNonNull((toInteger));
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withToIntegerOrNull(Boolean toIntegerOrNull) {
    java.util.Objects.requireNonNull((toIntegerOrNull));
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
}