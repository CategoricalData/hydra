// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A string literal with quote style
 */
public class StringLiteral implements Serializable, Comparable<StringLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.StringLiteral");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name SINGLE_QUOTE = new hydra.core.Name("singleQuote");

  /**
   * The string value
   */
  public final String value;

  /**
   * Whether to use single quotes (true) or double quotes (false)
   */
  public final Boolean singleQuote;

  public StringLiteral (String value, Boolean singleQuote) {
    this.value = value;
    this.singleQuote = singleQuote;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringLiteral)) {
      return false;
    }
    StringLiteral o = (StringLiteral) other;
    return java.util.Objects.equals(
      this.value,
      o.value) && java.util.Objects.equals(
      this.singleQuote,
      o.singleQuote);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value) + 3 * java.util.Objects.hashCode(singleQuote);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(StringLiteral other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      value,
      other.value);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      singleQuote,
      other.singleQuote);
  }

  public StringLiteral withValue(String value) {
    return new StringLiteral(value, singleQuote);
  }

  public StringLiteral withSingleQuote(Boolean singleQuote) {
    return new StringLiteral(value, singleQuote);
  }
}
