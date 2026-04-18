// Note: this is an automatically generated file. Do not edit.

package hydra.xml.schema;

import java.io.Serializable;

public class Token implements Serializable, Comparable<Token> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.xml.schema.Token");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public Token (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Token)) {
      return false;
    }
    Token o = (Token) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Token other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
