// Note: this is an automatically generated file. Do not edit.

package hydra.parsing;

/**
 * A successful parse result
 */
public class ParseSuccess<A> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.parsing.ParseSuccess");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name FIELD_NAME_REMAINDER = new hydra.core.Name("remainder");
  
  /**
   * The parsed value
   */
  public final A value;
  
  /**
   * The remaining unparsed input
   */
  public final String remainder;
  
  public ParseSuccess (A value, String remainder) {
    java.util.Objects.requireNonNull((value));
    java.util.Objects.requireNonNull((remainder));
    this.value = value;
    this.remainder = remainder;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParseSuccess)) {
      return false;
    }
    ParseSuccess o = (ParseSuccess) (other);
    return value.equals(o.value) && remainder.equals(o.remainder);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode() + 3 * remainder.hashCode();
  }
  
  public ParseSuccess withValue(A value) {
    java.util.Objects.requireNonNull((value));
    return new ParseSuccess(value, remainder);
  }
  
  public ParseSuccess withRemainder(String remainder) {
    java.util.Objects.requireNonNull((remainder));
    return new ParseSuccess(value, remainder);
  }
}
