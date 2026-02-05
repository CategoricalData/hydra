// Note: this is an automatically generated file. Do not edit.

package hydra.parsing;

import java.io.Serializable;

/**
 * An error which occurred while parsing
 */
public class ParseError implements Serializable, Comparable<ParseError> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.parsing.ParseError");
  
  public static final hydra.core.Name FIELD_NAME_MESSAGE = new hydra.core.Name("message");
  
  public static final hydra.core.Name FIELD_NAME_REMAINDER = new hydra.core.Name("remainder");
  
  /**
   * An error message
   */
  public final String message;
  
  /**
   * The remaining input at the point of failure
   */
  public final String remainder;
  
  public ParseError (String message, String remainder) {
    this.message = message;
    this.remainder = remainder;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParseError)) {
      return false;
    }
    ParseError o = (ParseError) (other);
    return java.util.Objects.equals(
      this.message,
      o.message) && java.util.Objects.equals(
      this.remainder,
      o.remainder);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(message) + 3 * java.util.Objects.hashCode(remainder);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ParseError other) {
    int cmp = 0;
    cmp = ((Comparable) (message)).compareTo(other.message);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (remainder)).compareTo(other.remainder);
  }
  
  public ParseError withMessage(String message) {
    return new ParseError(message, remainder);
  }
  
  public ParseError withRemainder(String remainder) {
    return new ParseError(message, remainder);
  }
}
