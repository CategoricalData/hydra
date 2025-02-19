// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class String_ implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.String");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name FIELD_NAME_QUOTE_STYLE = new hydra.core.Name("quoteStyle");
  
  public final String value;
  
  public final hydra.ext.python.syntax.QuoteStyle quoteStyle;
  
  public String_ (String value, hydra.ext.python.syntax.QuoteStyle quoteStyle) {
    java.util.Objects.requireNonNull((value));
    java.util.Objects.requireNonNull((quoteStyle));
    this.value = value;
    this.quoteStyle = quoteStyle;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof String_)) {
      return false;
    }
    String_ o = (String_) (other);
    return value.equals(o.value) && quoteStyle.equals(o.quoteStyle);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode() + 3 * quoteStyle.hashCode();
  }
  
  public String_ withValue(String value) {
    java.util.Objects.requireNonNull((value));
    return new String_(value, quoteStyle);
  }
  
  public String_ withQuoteStyle(hydra.ext.python.syntax.QuoteStyle quoteStyle) {
    java.util.Objects.requireNonNull((quoteStyle));
    return new String_(value, quoteStyle);
  }
}