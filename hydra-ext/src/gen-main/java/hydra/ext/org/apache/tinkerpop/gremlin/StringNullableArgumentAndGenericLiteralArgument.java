// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class StringNullableArgumentAndGenericLiteralArgument implements Serializable, Comparable<StringNullableArgumentAndGenericLiteralArgument> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument");
  
  public static final hydra.core.Name STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name LITERAL = new hydra.core.Name("literal");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument string;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument literal;
  
  public StringNullableArgumentAndGenericLiteralArgument (hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument string, hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument literal) {
    this.string = string;
    this.literal = literal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringNullableArgumentAndGenericLiteralArgument)) {
      return false;
    }
    StringNullableArgumentAndGenericLiteralArgument o = (StringNullableArgumentAndGenericLiteralArgument) other;
    return java.util.Objects.equals(
      this.string,
      o.string) && java.util.Objects.equals(
      this.literal,
      o.literal);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(string) + 3 * java.util.Objects.hashCode(literal);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(StringNullableArgumentAndGenericLiteralArgument other) {
    int cmp = 0;
    cmp = ((Comparable) string).compareTo(other.string);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) literal).compareTo(other.literal);
  }
  
  public StringNullableArgumentAndGenericLiteralArgument withString(hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument string) {
    return new StringNullableArgumentAndGenericLiteralArgument(string, literal);
  }
  
  public StringNullableArgumentAndGenericLiteralArgument withLiteral(hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument literal) {
    return new StringNullableArgumentAndGenericLiteralArgument(string, literal);
  }
}
