// Note: this is an automatically generated file. Do not edit.

package hydra.parsing;

import java.io.Serializable;

/**
 * A successful parse result
 */
public class ParseSuccess<A> implements Serializable, Comparable<ParseSuccess<A>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.parsing.ParseSuccess");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name REMAINDER = new hydra.core.Name("remainder");

  /**
   * The parsed value
   */
  public final A value;

  /**
   * The remaining unparsed input
   */
  public final String remainder;

  public ParseSuccess (A value, String remainder) {
    this.value = value;
    this.remainder = remainder;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParseSuccess)) {
      return false;
    }
    ParseSuccess o = (ParseSuccess) other;
    return java.util.Objects.equals(
      this.value,
      o.value) && java.util.Objects.equals(
      this.remainder,
      o.remainder);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value) + 3 * java.util.Objects.hashCode(remainder);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ParseSuccess other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      value,
      other.value);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      remainder,
      other.remainder);
  }

  public ParseSuccess withValue(A value) {
    return new ParseSuccess(value, remainder);
  }

  public ParseSuccess withRemainder(String remainder) {
    return new ParseSuccess(value, remainder);
  }
}
