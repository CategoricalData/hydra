// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class String_ implements Serializable, Comparable<String_> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.String");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name FIELD_NAME_QUOTE_STYLE = new hydra.core.Name("quoteStyle");
  
  public final String value;
  
  public final hydra.ext.python.syntax.QuoteStyle quoteStyle;
  
  public String_ (String value, hydra.ext.python.syntax.QuoteStyle quoteStyle) {
    this.value = value;
    this.quoteStyle = quoteStyle;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof String_)) {
      return false;
    }
    String_ o = (String_) other;
    return java.util.Objects.equals(
      this.value,
      o.value) && java.util.Objects.equals(
      this.quoteStyle,
      o.quoteStyle);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value) + 3 * java.util.Objects.hashCode(quoteStyle);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(String_ other) {
    int cmp = 0;
    cmp = ((Comparable) value).compareTo(other.value);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) quoteStyle).compareTo(other.quoteStyle);
  }
  
  public String_ withValue(String value) {
    return new String_(value, quoteStyle);
  }
  
  public String_ withQuoteStyle(hydra.ext.python.syntax.QuoteStyle quoteStyle) {
    return new String_(value, quoteStyle);
  }
}
