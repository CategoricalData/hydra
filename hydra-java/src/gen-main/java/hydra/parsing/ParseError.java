// Note: this is an automatically generated file. Do not edit.

package hydra.parsing;

import java.io.Serializable;

/**
 * An error which occurred while parsing
 */
public class ParseError implements Serializable {
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
    java.util.Objects.requireNonNull((message));
    java.util.Objects.requireNonNull((remainder));
    this.message = message;
    this.remainder = remainder;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParseError)) {
      return false;
    }
    ParseError o = (ParseError) (other);
    return message.equals(o.message) && remainder.equals(o.remainder);
  }
  
  @Override
  public int hashCode() {
    return 2 * message.hashCode() + 3 * remainder.hashCode();
  }
  
  public ParseError withMessage(String message) {
    java.util.Objects.requireNonNull((message));
    return new ParseError(message, remainder);
  }
  
  public ParseError withRemainder(String remainder) {
    java.util.Objects.requireNonNull((remainder));
    return new ParseError(message, remainder);
  }
}
