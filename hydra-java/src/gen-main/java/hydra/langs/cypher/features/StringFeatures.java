// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for string functions.
 */
public class StringFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.StringFeatures");
  
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
    if (char_length == null) {
      throw new IllegalArgumentException("null value for 'char_length' argument");
    }
    if (character_length == null) {
      throw new IllegalArgumentException("null value for 'character_length' argument");
    }
    if (contains == null) {
      throw new IllegalArgumentException("null value for 'contains' argument");
    }
    if (endsWith == null) {
      throw new IllegalArgumentException("null value for 'endsWith' argument");
    }
    if (in == null) {
      throw new IllegalArgumentException("null value for 'in' argument");
    }
    if (startsWith == null) {
      throw new IllegalArgumentException("null value for 'startsWith' argument");
    }
    if (toBoolean == null) {
      throw new IllegalArgumentException("null value for 'toBoolean' argument");
    }
    if (toBooleanOrNull == null) {
      throw new IllegalArgumentException("null value for 'toBooleanOrNull' argument");
    }
    if (toFloat == null) {
      throw new IllegalArgumentException("null value for 'toFloat' argument");
    }
    if (toFloatOrNull == null) {
      throw new IllegalArgumentException("null value for 'toFloatOrNull' argument");
    }
    if (toInteger == null) {
      throw new IllegalArgumentException("null value for 'toInteger' argument");
    }
    if (toIntegerOrNull == null) {
      throw new IllegalArgumentException("null value for 'toIntegerOrNull' argument");
    }
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
    if (char_length == null) {
      throw new IllegalArgumentException("null value for 'char_length' argument");
    }
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withCharacter_length(Boolean character_length) {
    if (character_length == null) {
      throw new IllegalArgumentException("null value for 'character_length' argument");
    }
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withContains(Boolean contains) {
    if (contains == null) {
      throw new IllegalArgumentException("null value for 'contains' argument");
    }
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withEndsWith(Boolean endsWith) {
    if (endsWith == null) {
      throw new IllegalArgumentException("null value for 'endsWith' argument");
    }
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withIn(Boolean in) {
    if (in == null) {
      throw new IllegalArgumentException("null value for 'in' argument");
    }
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withStartsWith(Boolean startsWith) {
    if (startsWith == null) {
      throw new IllegalArgumentException("null value for 'startsWith' argument");
    }
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withToBoolean(Boolean toBoolean) {
    if (toBoolean == null) {
      throw new IllegalArgumentException("null value for 'toBoolean' argument");
    }
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withToBooleanOrNull(Boolean toBooleanOrNull) {
    if (toBooleanOrNull == null) {
      throw new IllegalArgumentException("null value for 'toBooleanOrNull' argument");
    }
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withToFloat(Boolean toFloat) {
    if (toFloat == null) {
      throw new IllegalArgumentException("null value for 'toFloat' argument");
    }
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withToFloatOrNull(Boolean toFloatOrNull) {
    if (toFloatOrNull == null) {
      throw new IllegalArgumentException("null value for 'toFloatOrNull' argument");
    }
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withToInteger(Boolean toInteger) {
    if (toInteger == null) {
      throw new IllegalArgumentException("null value for 'toInteger' argument");
    }
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
  
  public StringFeatures withToIntegerOrNull(Boolean toIntegerOrNull) {
    if (toIntegerOrNull == null) {
      throw new IllegalArgumentException("null value for 'toIntegerOrNull' argument");
    }
    return new StringFeatures(char_length, character_length, contains, endsWith, in, startsWith, toBoolean, toBooleanOrNull, toFloat, toFloatOrNull, toInteger, toIntegerOrNull);
  }
}